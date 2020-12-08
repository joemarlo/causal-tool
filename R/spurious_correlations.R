library(tidyverse)
library(pageviews)
set.seed(44)
# https://pageviews.toolforge.org/?project=en.wikipedia.org&platform=all-access&agent=user&redirects=0&range=latest-20&pages=Cat|Dog
# https://en.wikipedia.org/wiki/Wikipedia:Pageview_statistics

# get popular articles
dates_to_test <- seq(as.Date("2015-10-01"), as.Date("2020-10-01"), by = 'month')
top_articles <- map_dfr(dates_to_test, function(date) {
  top_articles(
    project = "en.wikipedia",
    platform = "all",
    start = date,
    granularity = "monthly",
    reformat = TRUE
  ) %>%
    select(article)
})

# list of articles ot get data on
articles <- top_articles %>% 
  distinct() %>% 
  pull()

# get the page views of 500 random top articles + R + causal inference
articles_samp <- sample(articles, size = 5000) %>% 
  append(c('R (programming language)', 'Causal inference', "New York University"))
pv_results <- map_dfr(articles_samp, function(article){
  article_pageviews(
    project = "en.wikipedia",
    article = article,
    platform = "all",
    user_type = "all",
    start = "2015100100",
    end = '2020120800',
    reformat = TRUE,
    granularity = "monthly"
  )
})

# clean up names
pv_summ <- pv_results %>% 
  select(date, article, views) %>% 
  # as_tibble() %>% 
  distinct(date, article, views)
clean_names <- pv_summ %>%
  distinct(article) %>%
  mutate(new_name = janitor::make_clean_names(article))
pv_summ <- pv_summ %>%
  left_join(clean_names, by = 'article') %>%
  mutate(article = new_name) %>%
  select(-new_name) %>%
  as_tibble()
  
# compute correlations
pv_mat <- pv_summ %>% pivot_wider(values_from = 'views', names_from = 'article') %>% select(-date) %>% as.matrix()
pv_cor <- cor(pv_mat)

# convert back into a dataframe with paired columns
pv_cor[!lower.tri(pv_cor)] <- NA
pv_highly_cor <- data.frame(pv_cor) %>%
  rownames_to_column('Series_A') %>%
  as_tibble() %>% 
  pivot_longer(cols = -Series_A, values_to = 'Correlation', names_to = 'Series_B') %>%
  # filter(Correlation > 0.85 & Correlation < 0.95) %>%
  filter(str_detect(Series_A, 'r_programming_language') | str_detect(Series_A, 'causal_inference') | str_detect(Series_A, 'new_york_university')) %>% 
  filter(abs(Correlation) >= 0.5) %>% 
  arrange(desc(abs(Correlation)))
  # slice_head(n = 300)

# turn the highly correlated pairs into a long dataset including the monthly pageview values
pairs <- map2_dfr(.x = pv_highly_cor$Series_A,
         .y = pv_highly_cor$Series_B,
         .f = function(A, B){
           pv_summ %>% 
             filter(article %in% c(A, B)) %>% 
             mutate(id = paste0(A, ":", B))
})

# filter to just the highest correlations among causal inference
pv_highly_cor %>% 
  filter(str_detect(paste0(Series_A,Series_B), 'causal')) %>%
  filter(abs(Correlation) > 0.7) -> causal_high

# plot the correlated series
pairs %>% 
  # filter(str_detect(id, 'causal')) %>% 
  rowwise() %>%
  filter(any(str_detect(id, causal_high$Series_B))) %>% 
  group_by(id, article) %>% 
  mutate(views = scale(views)) %>% 
  ggplot(aes(x = date, y = views, color = article)) +
  geom_line() +
  facet_wrap(~id, scales = 'free_y') +
  theme(legend.position = 'none',
        strip.text = element_text(size = 6))


# interesting handpicked pairs
handpicked <- c(
  'causal_inference:emily_hampshire',
  'causal_inference:index_of_economic_freedom',
  'causal_inference:isabela_moner',
  'causal_inference:john_wick_film',
  'causal_inference:sons_of_anarchy',
  'new_york_university:afghanistan',
  # 'new_york_university:al_jazeera',
  # 'new_york_university:amazon_company',
  'new_york_university:bbc',
  'new_york_university:california',
  'new_york_university:causal_inference',
  'new_york_university:chicago',
  # 'new_york_university:chris_elliot',
  # 'new_york_university:google_drive',
  'new_york_university:microsoft',
  'new_york_university:you_tube',
  'new_york_university:zodiac',
  'r_programming_language:list_of_shameless_u_s_tv_series_episodes',
  'causal_inference:al_jazeera',
  'causal_inference:amazon_company',
  'causal_inference:bbc',
  'causal_inference:chris_elliott',
  'causal_inference:file_flag_of_brazil_svg',
  'causal_inference:google_drive',
  'causal_inference:jameela_jamil',
  'causal_inference:noah_reid',
  'causal_inference:sleep_paralysis',
  # 'causal_inference:suicide_methods',
  'causal_inference:zodiac'
)

handpicked <- sort(unique(handpicked))

# plot the handpicked ones
pairs %>% 
  filter(id %in% handpicked) %>% #count(id)
  group_by(article) %>% 
  mutate(views = scale(views)) %>% 
  ggplot(aes(x = date, y = views, color = article)) +
  geom_line() +
  facet_wrap(~id, scales = 'free_y') +
  theme(legend.position = 'none',
        strip.text = element_text(size = 6))

# check correlations again
pairs %>% 
  filter(id %in% handpicked) %>%
  group_by(id) %>% 
  group_modify(~{
    df <-  .x %>% 
      pivot_wider(names_from = 'article', values_from = 'views') %>% 
      select(-date)
    
    tibble(cor = cor(df[,1], df[,2]))
  })

# write out final dataset
pairs %>% 
  filter(id %in% handpicked) %>%
  mutate(date = as.Date(date)) %>% 
  write_csv("causal-tool/data/wikipedia_page_views.csv")
