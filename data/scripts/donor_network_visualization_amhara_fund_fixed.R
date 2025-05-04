# ===== 1. Install and load required packages (only install once) =====
install.packages(c("readr", "dplyr", "tidytext", "stringr", "tidyr", 
                   "igraph", "ggraph", "ggplot2", "here"))

library(readr)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(here)

# ===== 2. Read CSV from your data folder =====
df <- read_csv(here("data", "amhara_emergency_fund_donors_network_sample.csv"))

# ===== 3. Tokenize and clean comment words =====
tokens <- df %>%
  select(`Donor Name`, Comment) %>%
  unnest_tokens(word, Comment) %>%
  filter(!word %in% stop_words$word) %>%
  filter(str_detect(word, "[a-zA-Z]"))

# ===== 4. Skip frequency filtering to include all words =====
top_words <- tokens  # use all tokens, no filtering

# ===== 5. Create donor–keyword edges =====
edges <- top_words %>%
  distinct(`Donor Name`, word)

# ===== 6. Build graph =====
graph <- graph_from_data_frame(edges, directed = FALSE)

# ===== 7. Normalize names and classify nodes =====
donor_names_clean <- tolower(trimws(df$`Donor Name`))
V(graph)$name_clean <- tolower(trimws(V(graph)$name))

V(graph)$type <- ifelse(V(graph)$name_clean %in% donor_names_clean, "donor", "keyword")

# ===== 8. Highlight key keywords =====
key_terms <- c("fano", "amhara", "protest", "genocide", "freedom", "unity", "justice")
V(graph)$highlight <- ifelse(
  V(graph)$type == "keyword" & V(graph)$name_clean %in% key_terms,
  "highlight", "normal"
)

# ===== 9. Plot network graph =====
ggraph(graph, layout = "fr") +
  geom_edge_link(alpha = 0.2, edge_width = 0.4) +
  geom_node_point(aes(color = highlight, shape = type), size = 5) +
  geom_node_text(
    aes(label = ifelse(highlight == "highlight" | degree(graph) > 1, name, "")),
    repel = TRUE,
    size = 4,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c("highlight" = "red", "normal" = "black")) +
  scale_shape_manual(values = c("donor" = 16, "keyword" = 17)) +
  theme_minimal(base_size = 14) +
  ggtitle("Donor–Keyword Network: Highlighting FANO, Amhara, Protest, etc.")

# ===== 10. Optional: Export as PNG =====
ggsave("output/amhara_donor_network_v2.png", width = 12, height = 10, dpi = 300)
