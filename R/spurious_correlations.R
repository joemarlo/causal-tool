library(tidyquant)

stock_symbols <- c("AAPL", "MSFT", "NFLX", "TSLA")
# Keep just the closing price
stock_prices <- tq_get(stock_symbols, from = "2010-01-01") %>%
  dplyr::select(symbol, date, close) %>% 
  group_by(symbol) %>% 
  mutate(daily.return = close / lag(close) - 1) %>% 
  ungroup()

tq_get('TB1YR', get = "economic.data", from = "1950-01-01") %>% 
  mutate(price = price / 100) %>% 
  ggplot(aes(x = date, y = price)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent)

tbills <- tq_get('TB1YR', get = "economic.data", from = "1950-01-01")


# rolling monthly sd
tbills %>% 
  mutate(price = price / 100) %>% 
  na.omit() %>% 
  tq_mutate(select = price, mutate_fun = runSD, n = 12) %>% 
  na.omit() %>% 
  # tq_performance(Ra = price, performance_fun = sd.annualized)
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent)

# annual sd
tbills %>% 
  mutate(price = price / 100) %>% 
  group_by(year = year(date)) %>% 
  mutate(annual.vol = sqrt(12)*sd(price)) %>% 
  filter(month(date) == 1) %>% 
  ggplot(aes(x = year, y = annual.vol)) +
  geom_line()

# annualized returns
stock_prices %>% 
  filter(year(date) == 2015) %>% 
  group_by(symbol) %>% 
  # tq_performance(Ra = daily.return, performance_fun = sd.annualized)
  tq_performance(Ra = daily.return, performance_fun = Return.annualized)




# fredr -------------------------------------------------------------------

library(tidyverse)

# https://fred.stlouisfed.org/categories
# https://fred.stlouisfed.org/docs/api/fred/category_children.html
# https://fred.stlouisfed.org/docs/api/fred/series.html


# library(fredr)
# fredr_set_key('cea428a6df3693db750a6a526cef15b3')
# fredr(
#   series_id = "UNRATE",
#   observation_start = as.Date("1990-01-01"),
#   observation_end = as.Date("2000-01-01")
# )

# get API key
source('R/api_key.R')

output <- httr::GET(
  paste0(
    'https://api.stlouisfed.org/fred/category/children?',
    'category_id=', 32641,
    '&api_key=', api_key,
    '&file_type=json'
  )
)

df <- jsonlite::fromJSON(httr::content(output, 'text')) %>% as.data.frame()
df


# get country ids
output <- httr::GET(
  paste0(
    'https://api.stlouisfed.org/fred/category/children?',
    'category_id=', 32264,
    '&api_key=', api_key,
    '&file_type=json'
  )
)

df <- jsonlite::fromJSON(httr::content(output, 'text')) %>% as.data.frame()
countries_ids <- df$categories.id

# sample the ids b/c api limits cause the process to take ~4min per country
countries_ids <- sample(countries_ids, size = 30, replace = FALSE)

total_api_calls <- 1
# for each country in the list, get the available series and return the data
all_countries <- map(seq_along(countries_ids), function(i_country){
  
  tryCatch({
    
    # api has rate limit of 120 requests per minute
    if(total_api_calls %% 120 == 0){
      writeLines("Sleeping due to api limit...")
      Sys.sleep(60)
    }
    
    # get the series available for the country
    output <- httr::GET(
      paste0(
        'https://api.stlouisfed.org/fred/category/series?',
        'category_id=', countries_ids[i_country],
        '&api_key=', api_key,
        '&file_type=json'
      )
    )
    
    # increment total calls
    total_api_calls <<- total_api_calls + 1
    
    # convert to dataframe and pull the series ids and series names
    # only include annual time series
    df <- jsonlite::fromJSON(httr::content(output, 'text')) %>% as.data.frame()
    series_ids <- df %>% filter(seriess.frequency == 'Annual') %>% pull(seriess.id)
    series_names <- df %>% filter(seriess.frequency == 'Annual') %>% pull(seriess.title)

    # for each series in the list, retrieve the data 
    time_series <- map(seq_along(series_ids), function(i_series) {

      tryCatch({
          
        # api has rate limit of 120 requests per minute
        if(total_api_calls %% 120 == 0){
          writeLines("Sleeping due to api limit...")
          Sys.sleep(60)
        }
          
        # get the time series data
        output <- httr::GET(
          paste0(
            'https://api.stlouisfed.org/fred/series/observations?',
            'series_id=', series_ids[i_series],
            '&api_key=', api_key,
            '&file_type=json'
          )
        )
        
        # increment total calls
        total_api_calls <<- total_api_calls + 1
        
        # convert to dataframe
        df <- jsonlite::fromJSON(httr::content(output, 'text')) %>% as.data.frame()
        
        # clean up names and values
        series_df <- df %>%
          mutate(date = as.Date(observations.date),
                 value = as.numeric(observations.value)) %>%
          select(date, value) %>%
          set_names(c("Date", str_replace_all(series_names[i_series], " ", "_")))
        
        return(series_df)
        },
        # if there is any error, warning, or otherwise then just return NULL
        error = function(e) return(NULL),
        warning = function(w) return(NULL),
        finally = function(f) return(NULL)
      )
    })
    
    return(time_series)
    },
    # if there is any error, warning, or otherwise then just return NULL
    error = function(e) return(NULL),
    warning = function(w) return(NULL),
    finally = function(f) return(NULL)
  )
})


# join the results within each country into a dataframe
results <- map(all_countries, function(country){
  # set names so join works
  names(country) <- seq_along(country)
  
  # remove nulls
  country <- country[sapply(country, function(item) !is.null(item))]
  
  # join
  country_df <- country %>% reduce(full_join, by = 'Date', copy = TRUE)
  
  return(country_df)
})

# join countries in one dataframe
names(results) <- seq_along(results)
results <- results %>% reduce(full_join, by = 'Date', copy = TRUE)

# results_1 <- results
# results <- full_join(results, results_1)

# filter to just 2000:2020
results_trimmed <- results %>% 
  arrange(desc(Date)) %>% 
  filter(Date %in% seq(as.Date('2000-01-01'), as.Date('2019-01-01'), by = 'year')) %>% 
  as_tibble()

# remove columns with NAs
results_trimmed <- results_trimmed[, colSums(is.na(results_trimmed)) == 0]

# compute correlations
results_trimmed_mat <- results_trimmed %>% select(-Date) %>% as.matrix()
results_trimmed_cor <- cor(results_trimmed_mat)

# convert back into a dataframe with paired columns
results_trimmed_cor[!lower.tri(results_trimmed_cor)] <- NA
highly_correlated_df <- data.frame(results_trimmed_cor) %>%
  rownames_to_column('Series_A') %>%
  as_tibble() %>% 
  pivot_longer(cols = -Series_A, values_to = 'Correlation', names_to = 'Series_B') %>%
  filter(Correlation > 0.85 & Correlation < 0.95)

# highly_correlated_df %>% write_csv('R/correlations.csv')

# filter out observations from the same country
# filter out population in Series B (b/c many are population pairs)
# sample to get 50 pairs
picks <- highly_correlated_df %>% 
  mutate(country_A = str_extract(Series_A, '_[^_]+$'),
         country_A = str_remove(country_A, '\\..*'),
         country_B = str_extract(Series_B, '_[^_]+$'),
         country_B = str_remove(country_B, '\\..*'),
         unit_A = str_extract(Series_A, '^[^_]+(?=_)'),
         unit_B = str_extract(Series_B, '^[^_]+(?=_)')) %>% 
  filter(country_A != country_B,
         unit_A != unit_B) %>% 
  select(Series_A, Series_B, Correlation) %>% 
  filter(!str_detect(Series_B, 'Population')) %>% 
  slice_sample(n = 50)

picks_series <- map(1:50, function(i){

  colA <- picks$Series_A[[i]]
  colB <- picks$Series_B[[i]]
  
  tryCatch({
  results_trimmed %>%
    select(Date, all_of(c(colA, colB)))
  }, error = function(e) return(NULL)
  )
})

# remove nulls b/c names
picks_series <- picks_series[sapply(picks_series, function(item) !is.null(item))]
names(picks_series) <- seq_along(picks_series)

picks_series <- map(seq_along(picks_series), function(i){
  df <- picks_series[[i]]
  df$name <- paste0(names(df)[2], names(df)[3], "_")
  df$id <- i
  df[, 2] <- scale(df[, 2])
  df[, 3] <- scale(df[, 3])
  colnames(df) <- c("Date", "Series_A", "Series_B", "Name", "ID")
  return(df)
})

bind_rows(picks_series) %>%
  pivot_longer(cols = starts_with("Series")) %>%
  ggplot(aes(x = Date, y = value, color = name, file = name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~Name) +
  theme(strip.text.x = element_text(size = 5))


# imdb --------------------------------------------------------------------



# wikipedia ---------------------------------------------------------------
# https://pageviews.toolforge.org/?project=en.wikipedia.org&platform=all-access&agent=user&redirects=0&range=latest-20&pages=Cat|Dog
# https://en.wikipedia.org/wiki/Wikipedia:Pageview_statistics

# CDC ---------------------------------------------------------------------



# CDC, IMDB
# https://wonder.cdc.gov/
# https://tylervigen.com/sources










# reduce list to a single dataframe, joining by date
names(time_series) <- seq_along(time_series)
time_series <- time_series[sapply(time_series, function(item) !is.null(item))]
time_series_df <- time_series %>% reduce(left_join, by = 'Date', copy = TRUE)
time_series_df %>% 
  pivot_longer(cols = -Date) %>%
  ggplot(aes(x = Date, y = value, group = name)) +
  geom_line() +
  geom_point()
time_series_mat <- time_series_df %>% select(-Date) %>% as.matrix()

cor(time_series_mat) %>% View



series_df %>% 
  ggplot(aes_string(x = "Date", y = colnames(series_df)[2])) +
  geom_line() +
  geom_point()









output <- map_dfr(1:100, function(id) {
  output <- httr::GET(
    paste0(
      'https://api.stlouisfed.org/fred/category/children?',
      'category_id=', id,
      '&api_key=', api_key,
      '&file_type=json'
    )
  )
  tryCatch(
    df <- jsonlite::fromJSON(httr::content(output, 'text')) %>% as.data.frame(),
    error = function(e) e
  )
  
  if (isTRUE(is.data.frame(df))){
    return(df)
  } else return(NULL)
})

View(output %>% filter(!is.na(categories.parent_id)))

output <- httr::GET(
  paste0(
    'https://api.stlouisfed.org/fred/category/children?',
    'category_id=', 13,
    '&api_key=', api_key,
    '&file_type=json'
  )
)
jsonlite::fromJSON(httr::content(output, 'text')) %>% as.data.frame()
