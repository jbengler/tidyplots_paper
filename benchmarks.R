
library(tidyverse)
library(tidyplots)
library(patchwork)

benchmark <-
  . %>%
  rowwise() %>%
  mutate(
    characters = as.double(str_length(str_replace_all(code, "[\r\n]" , ""))),
    arguments = str_count(code, " = "),
    functions = str_count(code, "[a-zA-Z0-9_]{3,}\\("),
    words = str_count(code, "[a-zA-Z0-9_]{3,}"),
    lines = str_count(code, "\\n") + 1,
    plot = list(eval(parse(text = code)))
  ) %>%
  ungroup()


# -------------------------------------------------------------------------
# Metrics for Fig. 2b

code_examples <-
  tibble(
    code = c(
      'study %>%
tidyplot(x = group, y = score, color = dose) %>%
add_mean_bar(alpha = 0.3) %>%
add_sem_errorbar() %>%
add_data_points()',
      'study %>%
ggplot(mapping = aes(x = group, y = score, color = dose, fill = dose)) +
stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.8), alpha = 0.3, color = NA, width = 0.6) +
stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.8), width = 0.4) +
geom_point(position = position_dodge(width = 0.8))'
    )
  )

code_examples %>%
  benchmark()

code_examples %>%
  benchmark() %>%
  .$plot

# -------------------------------------------------------------------------
# Plots for Fig. 2c

plot_gallery <-
  tibble(
    code = c(
      'eu_countries %>%
tidyplot(x = area, y = population) %>%
add_reference_lines(x = 2.5e5, y = 30) %>%
add_data_points(white_border = TRUE)',
      'eu_countries %>%
ggplot(mapping = aes(x = area, y = population)) +
geom_vline(xintercept = 2.5e5, linetype = "dashed") +
geom_hline(yintercept = 30, linetype = "dashed") +
geom_point(shape = 21, color = "white", fill = "blue")',
      'energy %>%
dplyr::filter(year >= 2008) %>%
tidyplot(x = year, y = power, color = energy_source) %>%
add_barstack_relative()',
      'energy %>%
dplyr::filter(year >= 2008) %>%
ggplot(mapping = aes(x = year, y = power, fill = energy_source)) +
geom_col(position = position_fill())',
      'study %>%
tidyplot(x = treatment, y = score, color = treatment) %>%
add_mean_dot(size = 2.5) %>%
add_mean_bar(width = 0.03) %>%
add_mean_value()',
      'study %>%
ggplot(mapping = aes(x = treatment, y = score, color = treatment, fill = treatment, label = after_stat(y))) +
stat_summary(geom = "point", fun = mean, size = 4) +
stat_summary(geom = "bar", fun = mean, color = NA, width = 0.01) +
stat_summary(geom = "text", fun = mean, vjust = -2) +
scale_y_continuous(expand = expansion(mult = c(0, .1)))',
      'study %>%
tidyplot(x = treatment, y = score, color = treatment) %>%
add_boxplot() %>%
add_data_points_beeswarm()',
      'study %>%
ggplot(mapping = aes(x = treatment, y = score, color = treatment)) +
geom_boxplot(staplewidth = 0.8) +
ggbeeswarm::geom_beeswarm()',
      'study %>%
tidyplot(x = group, y = score, color = dose) %>%
add_mean_bar(alpha = 0.3) %>%
add_sem_errorbar() %>%
add_data_points() %>%
add_test_asterisks(hide_info = TRUE)',
      'study %>%
ggplot(mapping = aes(x = group, y = score, color = dose, fill = dose)) +
stat_summary(geom = "bar", fun = mean, position = position_dodge(width = 0.8), alpha = 0.3, color = NA, width = 0.6) +
stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(width = 0.8), width = 0.4) +
geom_point(position = position_dodge(width = 0.8)) +
ggpubr::geom_pwc(label = "p.signif", method = "t.test")',
      'time_course %>%
tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
add_mean_line() %>%
add_sem_ribbon()',
      'time_course %>%
ggplot(mapping = aes(x = day, y = score, fill = treatment, color = treatment)) +
stat_summary(geom = "line", fun = mean) +
stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.3, color = NA)',
      'energy_week %>%
tidyplot(x = date, y = power, color = energy_source) %>%
add_areastack_absolute()',
      'energy_week %>%
ggplot(mapping = aes(x = date, y = power, color = energy_source, fill = energy_source)) +
geom_area(alpha = 0.3)',
      'study %>%
tidyplot(x = treatment, y = score, color = treatment) %>%
add_violin() %>%
add_data_points_beeswarm()',
      'study %>%
ggplot(mapping = aes(x = treatment, y = score, color = treatment)) +
geom_violin(trim = FALSE) +
ggbeeswarm::geom_beeswarm()',
      'energy %>%
tidyplot(y = power, color = energy_source) %>%
add_donut()',
      'energy %>%
ggplot(mapping = aes(x = NA, y = power, fill = energy_source)) +
stat_summary(geom = "bar", fun = sum, position = position_fill()) +
coord_polar("y") +
scale_x_discrete(limits = function(x) c("", "", x)) +
theme_void()',
      'climate %>%
tidyplot(x = month, y = max_temperature, dodge_width = 0) %>%
add_mean_line(group = year, alpha = 0.08) %>%
add_mean_line() %>%
adjust_x_axis(rotate_labels = 90)',
      'climate %>%
ggplot(mapping = aes(x = month, y = max_temperature)) +
stat_summary(mapping = aes(group = year), geom = "line", fun = mean, alpha = 0.08) +
stat_summary(mapping = aes(group = NA),geom = "line", fun = mean)',
      'study %>%
tidyplot(x = score, y = treatment, color = treatment) %>%
add_mean_bar(alpha = 0.3) %>%
add_sem_errorbar() %>%
add_data_points()',
      'study %>%
ggplot(mapping = aes(x = score, y = treatment, color = treatment, fill = treatment)) +
stat_summary(geom = "bar", fun = mean, alpha = 0.3, color = NA, width = 0.6) +
stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.4) +
geom_point()',
      'climate %>%
tidyplot(x = month, y = year, color = max_temperature) %>%
add_heatmap()',
      'climate %>%
ggplot(mapping = aes(x = month, y = year, fill = max_temperature)) +
geom_tile() +
viridis::scale_fill_viridis()',
      'energy_week %>%
tidyplot(x = date, y = power, color = energy_source) %>%
add_areastack_relative()',
      'energy_week %>%
ggplot(mapping = aes(x = date, y = power, color = energy_source, fill = energy_source)) +
geom_area(alpha = 0.3, position = position_fill())',
      'time_course %>%
tidyplot(x = day, y = score, color = treatment, dodge_width = 0) %>%
add_mean_line() %>%
add_mean_dot(size = 1) %>%
add_sem_errorbar(width = 2)',
      'time_course %>%
ggplot(mapping = aes(x = day, y = score, fill = treatment, color = treatment)) +
stat_summary(geom = "line", fun = mean) +
stat_summary(geom = "point", fun = mean, size = 2) +
stat_summary(geom = "errorbar", fun.data = mean_se)',
      'energy %>%
dplyr::filter(year >= 2008) %>%
tidyplot(x = year, y = power, color = energy_source) %>%
add_barstack_absolute()',
      'energy %>%
dplyr::filter(year >= 2008) %>%
ggplot(mapping = aes(x = year, y = power, fill = energy_source)) +
geom_col()'
    ),
    tool = rep(c("tidyplots", "ggplot2"), times = 15),
    plot_number = 1:30
  )

plot_gallery %>%
  benchmark() %>%
  dplyr::filter(tool == "tidyplots") %>%
  .$plot %>%
  save_plot("single_plots/tidyplots.png")

plot_gallery %>%
  benchmark() %>%
  dplyr::filter(tool == "ggplot2") %>%
  .$plot %>%
  save_plot("single_plots/ggplot2.png")

plot_gallery %>%
  benchmark() %>%
  pivot_longer(cols = c(characters, arguments, functions, words, lines),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = fct_relevel(metric, "lines", "words", "characters", "functions")) %>%
  tidyplot(x = tool, y = value, color = tool) %>%
  add_violin(draw_quantiles = c(0.5)) %>%
  add_data_points_beeswarm(white_border = TRUE) %>%
  add_test_pvalue(method = "wilcoxon", hide_info = TRUE, bracket.nudge.y = 0.6) %>%
  adjust_colors(colors_discrete_metro) %>%
  adjust_y_axis_title("Count") %>%
  remove_x_axis_title() %>%
  remove_legend() %>%
  split_plot(metric, widths = 25, heights = 30, nrow = 1) %>%
  save_plot("Fig2c.pdf")
