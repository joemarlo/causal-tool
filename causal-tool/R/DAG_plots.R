DAG_one <- tibble(
  label = c('No heart\ndisease', 'Receives\ndrug'),
  x = c(0, 1),
  y = c(0, 1)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, size = 30) +
  geom_text(aes(label = label)) +
  geom_segment(data = tibble(x = 0.9, xend = 0.1,
                             y = 0.9, yend = 0.1),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, lineend = 'round', linejoin = 'mitre',
               size = 1.2,
               arrow = arrow(length = unit(0.04, "npc"))) +
  coord_cartesian(xlim = c(-0.25, 1.25), ylim = c(-0.25, 1.25)) +
  theme_void()

DAG_two <- tibble(
  label = c('Heart\ndisease', 'Does not\nreceive\ndrug'),
  x = c(0, -1),
  y = c(0, 1)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, size = 30) +
  geom_text(aes(label = label)) +
  geom_segment(data = tibble(x = -0.9, xend = -0.1,
                             y = 0.9, yend = 0.1),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, lineend = 'round', linejoin = 'mitre',
               size = 1.2,
               arrow = arrow(length = unit(0.04, "npc"))) +
  coord_cartesian(xlim = c(-1.25, 0.25), ylim = c(-0.25, 1.25)) +
  theme_void()

DAG_three <- tibble(
label = c('Receives\ndrug', 'No heart\ndisease', 'Does not\nreceive\ndrug', 'Heart\ndisease'),
  x = c(-1, -1, 1, 1),
  y = c(-1, -2, -1, -2)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, size = 30) +
  geom_text(aes(label = label)) +
  geom_segment(data = tibble(x = c(-1, 1), xend = c(-1, 1),
                             y = c(-1.3, -1.3), yend = c(-1.7, -1.7)),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, lineend = 'round', linejoin = 'mitre',
               size = 1.2,
               arrow = arrow(length = unit(0.04, "npc"))) +
  geom_segment(data = tibble(x = 0, xend = 0,
                             y = -0.75, yend = -2.25),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, linetype = 'dashed', 
               lineend = 'round', linejoin = 'mitre', size = 1.2) +
  coord_cartesian(xlim = c(-1.25, 1.25), ylim = c(-2.3, -0.7)) +
  theme_void()
  
DAG_four <- tibble(
  label = c('Cholesterol', 'Age', 'Smokes', 'Genetics', 'Heart\ndisease'),
  x = c(0, 0, -1, -1, 1),
  y = c(0, 1, -0, -1, 0)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, size = 30) +
  geom_text(aes(label = label)) +
  geom_segment(data = tibble(x = c(-0.8, -1, -0.8, 0.2, 0), xend = c(-0.2, -1, -0.2, 0.8, 0),
                             y = c(-0.8, -0.7, 0, 0, 0.7), yend = c(-0.2, -0.3, 0, 0, 0.3)),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, lineend = 'round', linejoin = 'mitre',
               size = 1.2,
               arrow = arrow(length = unit(0.04, "npc"))) +
  coord_cartesian(xlim = c(-1.25, 1.25), ylim = c(-1.25, 1.25)) +
  theme_void()
