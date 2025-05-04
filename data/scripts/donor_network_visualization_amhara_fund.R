# ===== 1. Install and load required packages =====
install.packages(c("readr", "dplyr", "tidytext", "stringr", "tidyr", "igraph", "ggraph", "ggplot2", "here"))
library(readr)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(here)

# ===== 2. Read the donor data CSV file =====
df <- read_csv(here("data", "amhara_emergency_fund_donors_network_sample.csv"))

# ===== 3. Tokenize comment text =====
tokens <- df %>%
  select(`Donor Name`, Comment) %>%
  unnest_tokens(word, Comment) %>%
  filter(!word %in% stop_words$word) %>%
  filter(str_detect(word, "[a-zA-Z]"))

# ===== 4. Include all keywords (even rare ones) =====
top_words <- tokens %>%
  count(word, sort = TRUE) %>%
  filter(n >= 1)  # include even rare but meaningful terms

# ===== 5. Create edges between Donor and Keyword =====
edges <- tokens %>%
  semi_join(top_words, by = "word") %>%
  distinct(`Donor Name`, word)

# ===== 6. Create the network graph =====
graph <- graph_from_data_frame(edges, directed = FALSE)

# ===== 7. Normalize node names and classify type =====
V(graph)$label_clean <- tolower(trimws(V(graph)$name))
V(graph)$type <- ifelse(V(graph)$label_clean %in% tolower(df$`Donor Name`), "donor", "keyword")

# ===== 8. Define and flag important keywords =====
key_terms <- c("fano", "amhara", "protest", "genocide", "freedom", "unity", "justice")
V(graph)$highlight <- ifelse(
  V(graph)$type == "keyword" & V(graph)$label_clean %in% key_terms,
  "highlight",
  "normal"
)

# ===== 9. Add node degree to prioritize label display =====
V(graph)$degree <- degree(graph)

# ===== 10. Plot the network graph =====
ggraph(graph, layout = "fr") +
  geom_edge_link(alpha = 0.2, edge_width = 0.5) +
  geom_node_point(aes(color = highlight, shape = type), size = 4) +
  geom_node_text(
    aes(label = ifelse(highlight == "highlight" | degree > 2, name, "")),
    repel = TRUE,
    size = 3,
    max.overlaps = Inf
  ) +
  scale_color_manual(values = c("highlight" = "red", "normal" = "gray")) +
  scale_shape_manual(values = c("donor" = 16, "keyword" = 17)) +
  theme_void() +
  ggtitle("Donor–Keyword Network: Highlighting FANO, Amhara, Protest, etc.")

# ===== 11. Optional: Save as high-res PNG =====
ggsave("output/amhara_donor_network.png", width = 10, height = 8, dpi = 300)


