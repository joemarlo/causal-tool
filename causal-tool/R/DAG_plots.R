DAG_one <- tibble(
  label = c('No heart\ndisease', 'Receives\ndrug'),
  x = c(0, 0),
  y = c(0, 1)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, size = 30) +
  geom_text(aes(label = label)) +
  geom_segment(data = tibble(x = 0, xend = 0,
                             y = 0.8, yend = 0.2),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, lineend = 'round', linejoin = 'mitre',
               size = 1.2,
               arrow = arrow(length = unit(0.04, "npc"))) +
  coord_cartesian(xlim = c(-0.10, 0.25), ylim = c(-0.25, 1.25)) +
  theme_void()

DAG_two <- tibble(
  label = c('Heart\ndisease', 'Does not\nreceive\ndrug'),
  x = c(0, 0),
  y = c(0, 1)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, size = 30) +
  geom_text(aes(label = label)) +
  geom_segment(data = tibble(x = 0, xend = 0,
                             y = 0.8, yend = 0.2),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, lineend = 'round', linejoin = 'mitre',
               size = 1.2,
               arrow = arrow(length = unit(0.04, "npc"))) +
  coord_cartesian(xlim = c(-0.25, 0.10), ylim = c(-0.25, 1.25)) +
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
  coord_cartesian(xlim = c(-1.7, 1.7), ylim = c(-2.3, -0.7)) +
  theme_void()

DAG_four <- tibble(
  label = c('Receives\ndrug A', 'No heart\ndisease',
            'Receives\ndrug B', 'Heart\ndisease',
            'Does not\nreceive\ndrug', 'Heart\ndisease'),
  x = c(-1, -1, 0, 0, 1, 1),
  y = c(-1, -2, -1, -2, -1, -2)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, size = 30) +
  geom_text(aes(label = label)) +
  geom_segment(data = tibble(x = c(-1, 0, 1), 
                             xend = c(-1, 0, 1),
                             y = rep(-1.3, 3), 
                             yend = rep(-1.7, 3)),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, lineend = 'round', linejoin = 'mitre',
               size = 1.2,
               arrow = arrow(length = unit(0.04, "npc"))) +
  geom_segment(data = tibble(x = c(-0.5, 0.5), 
                             xend = c(-0.5, 0.5),
                             y = rep(-0.75, 2),
                             yend = rep(-2.25, 2)),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, linetype = 'dashed', 
               lineend = 'round', linejoin = 'mitre', size = 1.2) +
  coord_cartesian(xlim = c(-1.25, 1.25), ylim = c(-2.3, -0.7)) +
  theme_void()

DAG_five <- tibble(
  label = c('No heart\ndisease', 'Receives\ndrug', 'Cholesterol', 'Age', 'Smokes', 'Genetics'),
  x = c(0, 0, 1, 1, 2, 2),
  y = c(0, 1, 0.5, 1.5, 1, 0)) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(shape = 21, size = 30) +
  geom_text(aes(label = label)) +
  geom_segment(data = tibble(x = c(0, 0.2, 0.8, 1.0, 1.80, 2.0, 1.80), 
                             xend = c(0, 0.8, 0.2, 1.0, 1.2, 2.0, 1.2),
                             y = c(0.8, 0.9, 0.35, 1.3, 0.9, 0.20, 0.1), 
                             yend = c(0.2, 0.65, 0.1, 0.70, 0.65, 0.80, 0.35)),
               aes(x = x, xend = xend, y = y, yend = yend),
               alpha = 0.5, lineend = 'round', linejoin = 'mitre',
               size = 1.2,
               arrow = arrow(length = unit(0.04, "npc"))) +
  coord_cartesian(xlim = c(-0.25, 2.25), ylim = c(-0.25, 1.75)) +
  theme_void()
