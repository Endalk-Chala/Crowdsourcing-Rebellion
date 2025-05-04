# ===== 1. Load libraries =====
library(readr)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)

# ===== 2. Load the CSV file =====
df <- read_csv("data/amhara_emergency_fund_donors_network_sample.csv")

# ===== 3. Define your key focus words =====
focus_words <- c("amhara", "support", "fano", "people", 
                 "donation", "genocide", "emergency", "fund", "work")

# ===== 4. Tokenize comments and filter to focus words only =====
tokens <- df %>%
  select(`Donor Name`, Comment) %>%
  unnest_tokens(word, Comment) %>%
  mutate(word = tolower(word)) %>%
  filter(str_detect(word, "[a-zA-Z]")) %>%
  filter(word %in% focus_words)

# ===== 5. Create edges between donor and keyword =====
edges <- tokens %>%
  distinct(`Donor Name`, word)

# ===== 6. Build the graph =====
graph <- graph_from_data_frame(edges, directed = FALSE)

# ===== 7. Classify node types and highlight focus words =====
donor_names_clean <- tolower(trimws(df$`Donor Name`))
V(graph)$name_clean <- tolower(trimws(V(graph)$name))
V(graph)$type <- ifelse(V(graph)$name_clean %in% donor_names_clean, "donor", "keyword")
V(graph)$highlight <- ifelse(V(graph)$name_clean %in% focus_words, "yes", "no")

# ===== 8. Plot network (clean layout, high contrast) =====
set.seed(123)
ggraph(graph, layout = "fr") +
  geom_edge_link(color = "gray80", alpha = 0.4) +
  geom_node_point(aes(color = highlight, shape = type), size = 5) +
  geom_node_text(
    aes(label = ifelse(type == "keyword", name, "")),
    repel = TRUE,
    size = 5,
    max.overlaps = Inf,
    color = "black"
  ) +
  scale_color_manual(values = c("yes" = "#d73027", "no" = "#4575b4")) +
  scale_shape_manual(values = c("donor" = 16, "keyword" = 17)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  ggtitle("Donor–Keyword Network (Filtered by Research-Relevant Terms)")

