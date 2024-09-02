
library(tidyverse)
library(tidyplots)
library(patchwork)

arrange_plots <- function(plots, widths = 30, heights = 30, ...) {
  plots %>%
    purrr::map(adjust_size, NA, NA) %>%
    purrr::map(remove_legend) %>%
    purrr::map(remove_caption) %>%
    patchwork::wrap_plots(
      widths = ggplot2::unit(widths, "mm"),
      heights = ggplot2::unit(heights, "mm"), ...
    )
}

# -------------------------------------------------------------------------
# Plots for Fig. 2a

gallery_plots <-
  list(
    eu_countries %>%
      tidyplot(x = area, y = population) %>%
      add_reference_lines(x = 2.5e5, y = 30) %>%
      add_data_points(white_border = TRUE),

    energy %>%
      dplyr::filter(year >= 2008) %>%
      tidyplot(x = year, y = power, color = energy_source) %>%
      add_barstack_relative(),

    study %>%
      tidyplot(x = treatment, y = score, color = treatment) %>%
      add_mean_dot(size = 2.5) %>%
      add_mean_bar(width = 0.03) %>%
      add_mean_value(),

    study %>%
      tidyplot(x = treatment, y = score, color = treatment) %>%
      add_boxplot() %>%
      add_data_points_beeswarm(),

    study %>%
      tidyplot(x = group, y = score, color = dose) %>%
      add_mean_bar(alpha = 0.3) %>%
      add_sem_errorbar() %>%
      add_data_points() %>%
      add_test_asterisks(hide_info = TRUE),

    time_course %>%
      tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
      add_mean_line() %>%
      add_sem_ribbon(),

    energy_week %>%
      tidyplot(x = date, y = power, color = energy_source) %>%
      add_areastack_absolute(),

    study %>%
      tidyplot(x = treatment, y = score, color = treatment) %>%
      add_violin() %>%
      add_data_points_beeswarm(),

    energy %>%
      tidyplot(y = power, color = energy_source) %>%
      add_donut(),

    climate %>%
      tidyplot(x = month, y = max_temperature, dodge_width = 0) %>%
      add_mean_line(group = year, alpha = 0.08) %>%
      add_mean_line() %>%
      adjust_x_axis(rotate_labels = 90),

    study %>%
      tidyplot(x = score, y = treatment, color = treatment) %>%
      add_mean_bar(alpha = 0.3) %>%
      add_sem_errorbar() %>%
      add_data_points(),

    climate %>%
      tidyplot(x = month, y = year, color = max_temperature) %>%
      add_heatmap(),

    energy_week %>%
      tidyplot(x = date, y = power, color = energy_source) %>%
      add_areastack_relative(),

    time_course %>%
      tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
      add_mean_line() %>%
      add_mean_dot(size = 1) %>%
      add_sem_errorbar(width = 2),

    energy %>%
      dplyr::filter(year >= 2008) %>%
      tidyplot(x = year, y = power, color = energy_source) %>%
      add_barstack_absolute()
  )

gallery_plots %>%
  arrange_plots(ncol = 5) %>%
  save_plot("Fig2a.pdf")

