setwd("C:\\Users\\Oleksiy\\Desktop\\Programs\\project")
# Install required libraries
install.packages("dendextend")
install.packages("igraph")
library(readr)
library(dplyr)
library(ggdendro)
library(dendextend)
library(ggraph)
library(circlize)
library(igraph)
library(ggalluvial)
library(tidyverse)

# Load the data
data <- read_csv("football.csv")

# Check for missing values
colSums(is.na(data))

# Rename the "Pos." column for convenience
data <- data %>% rename(Position = Pos.)

# Check unique values in specific columns
unique(data$From.Club)
unique(data$To.Club)

# Filter data to remove invalid entries
data <- data %>%
  filter(!is.na(From.Club) & !is.na(To.Club) & !is.na(Fee) & Fee > 0)

# Review the processed data
summary(data)
head(data)

# 0. Calculate average transfer fees by club
club_data <- data %>%
  group_by(From.Club) %>%
  summarise(Average_Fee = mean(Fee, na.rm = TRUE))

# Plot a bar chart of average transfer fees
ggplot(club_data, aes(x = reorder(From.Club, Average_Fee), y = Average_Fee)) + 
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Average Transfer Fees by Club", 
    x = "Clubs", 
    y = "Average Transfer Fee (in million euros)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 1. Dendrogram: Average transfer fees by club
club_data <- data %>%
  group_by(From.Club) %>%
  summarise(Average_Fee = mean(Fee, na.rm = TRUE))

# Create the dendrogram
hc <- hclust(dist(club_data$Average_Fee))
dendrogram <- as.dendrogram(hc)
labels(dendrogram) <- club_data$From.Club[order.dendrogram(dendrogram)]  # Replace labels

# Enhanced Dendrogram Visualization

# Color clusters for better differentiation
dendrogram <- color_branches(dendrogram, k = 4)  # k = number of clusters

# Visualize the improved dendrogram
ggdendrogram(dendrogram, theme_dendro = TRUE) +
  ggtitle("Dendrogram of Selling Clubs") +
  labs(y = "Average Fee Distance", subtitle = "Clubs grouped by similar average transfer fees") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),  # Rotate and scale labels
    axis.title.x = element_blank(),  # Remove X-axis title
    axis.title.y = element_text(size = 12, face = "bold"),  # Adjust Y-axis title
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and bold the title
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic")  # Add a subtitle
  ) 

# 2. Arc Diagram: Relationships between clubs
# Select top 20 transfers by fee
top_20_transfers <- data %>%
  arrange(desc(Fee)) %>%
  select(From.Club, To.Club, Fee) %>%
  head(20)

# Create a graph
network <- graph_from_data_frame(d = top_20_transfers, directed = TRUE)

# Enhanced Arc Diagram with No X-Axis Below the Diagram
ggraph(network, layout = "linear") +
  geom_edge_arc(aes(edge_width = Fee, color = Fee), alpha = 0.8) +  # Arc thickness and color
  scale_edge_width(range = c(1, 6), name = "Transfer Amount (€M)") +  # Clearer edge width legend
  scale_edge_color_gradient(low = "lightblue", high = "darkblue", name = "Transfer Value (€M)") +  # Improved color legend
  geom_node_point(size = 6, color = "gold") +  # Node points
  geom_node_text(aes(label = name), size = 3, vjust = 1, hjust = 1, angle = 45, check_overlap = TRUE) +  # Node labels
  theme_minimal() +
  ggtitle("Arc Diagram of Top 20 Transfers Between Clubs") +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",  # Align legends horizontally
    legend.text = element_text(size = 10),  # Increase legend text size
    legend.title = element_text(face = "bold"),  # Bold legend title
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "italic"),
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank(),  # Remove axis titles
    panel.grid = element_blank()
  ) +
  labs(subtitle = "Top transfers by value (in million euros)") +
  guides(
    edge_color = guide_edge_colorbar(
      title = "Color Gradient (Transfer Value)", 
      barwidth = 10, 
      barheight = 0.5, 
      title.position = "left"
    ),
    edge_width = guide_legend(
      title = "Arc Thickness (Transfer Value)", 
      keywidth = 2, 
      keyheight = 0.5, 
      label.position = "bottom"
    )
  )

# 3. Network Diagram: Transfer network
# Selecting the 20 cheapest transfers
bottom_20_transfers <- data %>%
  arrange(Fee) %>%  # Sorting by ascending fee
  select(From.Club, To.Club, Fee) %>%
  head(20)

# Creating the graph
network <- graph_from_data_frame(d = bottom_20_transfers, directed = TRUE)

# Visualizing the network graph
ggraph(network, layout = "circle") +  # Circular layout
  geom_edge_link2(aes(width = Fee, color = Fee), alpha = 0.7) +  # Edges
  geom_node_point(size = 6, color = "gold") +  # Nodes as points
  geom_node_text(aes(label = name), size = 4, repel = TRUE, color = "black") +  # Node labels
  scale_edge_width(range = c(0.5, 4)) +  # Scaling edge width by transfer fee
  scale_edge_color_gradient(low = "lightgreen", high = "darkgreen", name = "Transfer Fee") +  # Edge color gradient
  theme_graph(base_family = "sans") +  # Graph-specific theme
  ggtitle("Network Diagram of the Cheapest Transfers") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(subtitle = "Top 20 Cheapest Transfers Between Clubs (in million euros)") +
  guides(
    edge_width = guide_legend(
      title = "Edge Width (based on fee)", 
      keywidth = 1.5, 
      keyheight = 1.5
    )
  )
