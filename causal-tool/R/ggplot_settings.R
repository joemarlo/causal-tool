
# build custom theme
theme_custom <- function()
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_rect(
      fill = "gray95",
      color = 'white'),
    strip.text = element_text(
      color = "gray30",
      size = 11,
      face = "bold"
    ),
    text = element_text(family = "Helvetica",
                        color = "gray30"),
    axis.title = element_text(
      hjust = 0.99),
      # size = 12),
    axis.title.x = element_text(vjust = -1),
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.justification = "right",
    plot.caption = element_text(face = "italic",
                                size = 8,
                                color = 'grey50')
  )

# set custom theme
theme_set(theme_custom())

# set default continuous colors
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

# set default discrete colors
scale_colour_discrete <- function(...) {
  scale_color_viridis(..., discrete = TRUE)
}
scale_color_discrete <- function(...) {
  scale_color_viridis(..., discrete = TRUE)
}
scale_fill_discrete <- function(...) {
  scale_fill_viridis(..., discrete = TRUE)
}
