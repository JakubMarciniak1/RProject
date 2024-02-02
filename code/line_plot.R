source(file.path(".", "\\resources\\Data_cleaning.R"))

library(ggplot2)


line_plot <- ggplot(
  clean_data,
  aes(
    x = year, y = salary,
    group = field, colour = field
  )
) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  geom_point(stat = "summary", fun = "mean") +
  theme_dark() +
  geom_smooth(method = "auto", se = FALSE)



line_plot
