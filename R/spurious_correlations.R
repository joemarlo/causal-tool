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

library(fredr)
fredr_set_key('cea428a6df3693db750a6a526cef15b3')
fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2000-01-01")
)


source('R/api_key.R')
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
