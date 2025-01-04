
library(tidyverse)
library(tidyplots)

### Gene expression

gene_expression %>%
  tidyplot(x = sample, y = external_gene_name, color = expression) %>%
  add_heatmap(scale = "row", rasterize = TRUE) %>%
  adjust_size(height = 120) %>%
  sort_y_axis_labels(direction, -padj) %>%
  adjust_theme_details(legend.key.height = unit(1, "null")) %>%
  adjust_legend_title("Row Z-score") %>%
  remove_x_axis_title() %>%
  remove_y_axis_title() %>%
  save_plot("Fig3a.pdf")


### Volcano plot

df <-
  read_csv("https://tidyplots.org/data/differential-expression-analysis.csv") %>%
  mutate(
    neg_log10_padj = -log10(padj),
    direction = if_else(log2FoldChange > 0, "up", "down", NA),
    candidate = abs(log2FoldChange) >= 1 & padj < 0.05
  )

df %>%
  tidyplot(x = log2FoldChange, y = neg_log10_padj) %>%
  add_data_points(data = filter_rows(!candidate),
                  color = "lightgrey", rasterize = TRUE) %>%
  add_data_points(data = filter_rows(candidate, direction == "up"),
                  color = "#FF7777", alpha = 0.5, rasterize = TRUE) %>%
  add_data_points(data = filter_rows(candidate, direction == "down"),
                  color = "#7DA8E6", alpha = 0.5, rasterize = TRUE) %>%
  add_reference_lines(x = c(-1, 1), y = -log10(0.05)) %>%
  add_data_labels_repel(data = min_rows(padj, 6, by = direction), label = external_gene_name,
                        color = "#000000", min.segment.length = 0, background = TRUE) %>%
  adjust_x_axis_title("$Log[2]~fold~change$") %>%
  adjust_y_axis_title("$-Log[10]~italic(P)~adjusted$") %>%
  save_plot("Fig3b.pdf")


### Microbiome composition

df <-
  read_csv("https://tidyplots.org/data/microbiota.csv") %>%
  mutate(genus = fct_inorder(genus),
         sample = fct_reorder(sample, top, .desc = TRUE))

df %>%
  tidyplot(x = sample, y = rel_abundance, color = genus) %>%
  add_areastack_absolute(alpha = 0.6) %>%
  adjust_theme_details(legend.key.height = unit(3.4, "mm")) %>%
  adjust_theme_details(legend.key.width = unit(3.4, "mm")) %>%
  adjust_x_axis_title("Sample") %>%
  adjust_y_axis_title("Relative abundance") %>%
  remove_x_axis_labels() %>%
  remove_x_axis_ticks() %>%
  remove_legend_title() %>%
  save_plot("Fig3c.pdf")


### Principal component plot

df <-
  read_csv("https://tidyplots.org/data/pca-plot.csv")

p2 <-
  df %>%
  tidyplot(x = pc1, y = pc2, color = group) %>%
  add_data_points(size = 1.3, white_border = TRUE) %>%
  adjust_x_axis_title(paste0("Component 1 (", format_number(df$pc1_var*100), "%)")) %>%
  adjust_y_axis_title(paste0("Component 2 (", format_number(df$pc2_var*100), "%)")) %>%
  adjust_colors(colors_discrete_apple) %>%
  adjust_legend_position("top") %>%
  remove_legend_title() %>%
  save_plot("Fig3d.pdf")

### Correlation

library(tidyverse)
library(tidyplots)

df <-
  read_csv("https://tidyplots.org/data/correlation-matrix.csv")

df %>%
  tidyplot(x = x, y = y, color = correlation) %>%
  add_heatmap(rasterize = TRUE) %>%
  sort_x_axis_labels(order_x) %>%
  sort_y_axis_labels(order_y) %>%
  remove_x_axis() %>%
  remove_y_axis() %>%
  remove_legend_title() %>%
  adjust_legend_position("right") %>%
  adjust_colors(colors_continuous_inferno) %>%
  adjust_theme_details(legend.key.height = unit(1, "null")) %>%
  save_plot("Fig3e.pdf")


### Read alignment, relative

library(tidyverse)
library(tidyplots)

df <- read_csv("https://tidyplots.org/data/sequencing-qc-STAR.csv")

my_colors <- c("Uniquely mapped" = "#437bb1",
               "Mapped to multiple loci" = "#7cb5ec",
               "Mapped to too many loci" = "#f7a35c",
               "Unmapped: too short" = "#b1084c",
               "Unmapped: other" = "#7f0000")

df %>%
  tidyplot(x = reads, y = sample, color = category) %>%
  add_barstack_relative(reverse = TRUE) %>%
  theme_minimal_x() %>%
  adjust_colors(my_colors) %>%
  adjust_x_axis(title = "Percentage of reads", labels = scales::percent) %>%
  adjust_size(70, 50) %>%
  reorder_color_labels(names(my_colors)) %>%
  remove_legend_title() %>%
  remove_y_axis_title() %>%
  save_plot("Fig3f.pdf")


### Feature counts, relative

library(tidyverse)
library(tidyplots)

df <- read_csv("https://tidyplots.org/data/sequencing-qc-featureCounts.csv")

my_colors <- c("Assigned" = "#7cb5ec",
               "Unassigned_Ambiguity" = "#434348",
               "Unassigned_MultiMapping" = "#90ed7d",
               "Unassigned_NoFeatures" = "#f7a35c")

df %>%
  tidyplot(x = reads, y = sample, color = category) %>%
  add_barstack_relative(reverse = TRUE) %>%
  theme_minimal_x() %>%
  adjust_colors(my_colors) %>%
  adjust_x_axis(title = "Percentage of reads", labels = scales::percent) %>%
  adjust_size(70, 50) %>%
  reorder_color_labels(names(my_colors)) %>%
  remove_legend_title() %>%
  remove_y_axis_title() %>%
  save_plot("Fig3g.pdf")
