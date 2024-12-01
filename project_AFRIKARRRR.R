# Встановлення необхідних пакетів
install.packages("circlize")
install.packages("networkD3")
install.packages("ggraph")
install.packages("igraph")
install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")

# Завантаження бібліотек
library(circlize)
library(networkD3)
library(tidyverse)
library(ComplexHeatmap)
library(igraph)
library(ggraph)
library(reshape2)
library(htmltools)

# Завантаження даних
data <- read.csv("skillmigration.csv")

### 1. Heatmap###
# Агрегація даних для Heatmap
heatmap_data <- data[, c("skill_group_category", "wb_region", "net_per_10K_2019")]
heatmap_matrix <- reshape2::acast(heatmap_data, skill_group_category ~ wb_region, value.var = "net_per_10K_2019", fill = 0)

# Налаштування кольорової гами
col_fun <- colorRamp2(
  breaks = c(min(heatmap_matrix), 0, max(heatmap_matrix)),
  colors = c("blue", "white", "red")
)

# Побудова Heatmap без дендрограм
Heatmap(
  heatmap_matrix,
  name = "Net Migration",
  col = col_fun,
  show_column_names = TRUE,
  show_row_names = TRUE,
  column_names_gp = gpar(fontsize = 12, fontface = "bold"), 
  row_names_gp = gpar(fontsize = 12, fontface = "bold"),
  heatmap_legend_param = list(
    title = "Migration\n(per 10K)",
    title_gp = gpar(fontsize = 14, fontface = "bold"),
    labels_gp = gpar(fontsize = 12)
  ),
  border = TRUE,
  rect_gp = gpar(col = "black", lwd = 1),
  cluster_rows = FALSE,   # Прибирає дендрограму рядків
  cluster_columns = FALSE # Прибирає дендрограму колонок
)

### 2. Sankey Diagram ###
# Фільтрація даних для Europe & Central Asia
lac_data <- data %>%
  filter(wb_region == "Europe & Central Asia") %>%
  select(country_name, skill_group_category, net_per_10K_2019) %>%
  filter(!is.na(net_per_10K_2019))

# Агрегація міграції за країнами
lac_aggregated <- lac_data %>%
  group_by(country_name) %>%
  summarise(total_migration = sum(net_per_10K_2019)) %>%
  arrange(desc(total_migration)) %>%
  slice_head(n = 10)  # Топ-10 країн

# Фільтрація даних для обраних країн
filtered_data <- lac_data %>%
  filter(country_name %in% lac_aggregated$country_name)

# Підготовка зв'язків для Sankey Diagram
links <- filtered_data %>%
  group_by(country_name, skill_group_category) %>%
  summarise(value = sum(net_per_10K_2019, na.rm = TRUE)) %>%
  ungroup()

# Створення списку вузлів
nodes <- data.frame(name = unique(c(links$country_name, links$skill_group_category)))

# Перетворення в індекси
links$source <- match(links$country_name, nodes$name) - 1
links$target <- match(links$skill_group_category, nodes$name) - 1

head(links)

# Визначення кольорів для вузлів
# Кольори для країн (перші 10 вузлів)
country_colors <- rainbow(10)
# Кольори для навичок (решта вузлів)
skill_colors <- rainbow(length(unique(filtered_data$skill_group_category)))

# Об'єднання кольорів
node_colors <- c(country_colors, skill_colors)

# Задайте ширину і висоту графіка
sankey_width <- 1000  # Задайте ширину графіка
sankey_height <- 700  # Задайте висоту графіка

# Побудова Sankey Diagram з заданими розмірами
sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 14,
  nodeWidth = 30,
  colourScale = JS(
    sprintf(
      'd3.scaleOrdinal().domain(%s).range(%s)',
      jsonlite::toJSON(nodes$name),
      jsonlite::toJSON(node_colors)
    )
  ),
  width = sankey_width,
  height = sankey_height
)

# Відображення діаграми з більшою розміру
htmltools::browsable(
  htmltools::tagList(
    tags$h2("Sankey Diagram: Migration from Europe & Central Asia by Skills (2019)"),
    sankey
  )
)

### 3. Chord Diagram ###
# Фільтрація країн із регіону North America
ssa_data <- data %>%
  filter(wb_region == "North America") %>%
  select(country_name, skill_group_category, net_per_10K_2019) %>%
  filter(!is.na(net_per_10K_2019))

# Агрегація: сума міграції для кожної країни за групами навичок
ssa_aggregated <- ssa_data %>%
  group_by(country_name, skill_group_category) %>%
  summarise(total_migration = sum(net_per_10K_2019, na.rm = TRUE)) %>%
  ungroup()

# Формування матриці
chord_matrix <- reshape2::acast(ssa_aggregated, skill_group_category ~ country_name, 
                                value.var = "total_migration", fill = 0)

# Визначення кольорів для категорій навичок і країн
# Унікальні назви для категорій навичок і країн
skill_categories <- unique(ssa_aggregated$skill_group_category)
countries <- unique(ssa_aggregated$country_name)

# Задаємо кольори для навичок
skill_colors <- setNames(rainbow(length(skill_categories)), skill_categories)

# Генерація випадкових кольорів для країн
set.seed(228)  # Для відтворюваності результатів
country_colors <- setNames(sample(colors(), length(countries)), countries)

# Об'єднання кольорів
grid_colors <- c(skill_colors, country_colors)

# Створення Chord Diagram з фіксованими кольорами
chordDiagram(
  chord_matrix, 
  transparency = 0.5, 
  annotationTrack = c("grid", "name"), 
  grid.col = grid_colors
)

# Додати заголовок
title("Chord Diagram: Migration from North America by Skills (2019)")
