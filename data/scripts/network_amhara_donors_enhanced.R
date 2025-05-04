# Load libraries
library(igraph)
library(tidyverse)
library(here)

# Read the data
df <- read_csv(here("data", "amhara_gofundme.csv"))

# Clean the data
df_clean <- df %>%
  filter(!is.na(`Organizer Name`), !is.na(`Campaign Title`))

# Create edge list
edges <- df_clean %>%
  select(`Organizer Name`, `Campaign Title`)

# Create graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Calculate campaign count per organizer
organizer_freq <- df_clean %>%
  count(`Organizer Name`, name = "campaign_count")

# Set node size based on campaign frequency
V(g)$size <- ifelse(V(g)$name %in% organizer_freq$`Organizer Name`,
                    organizer_freq$campaign_count[match(V(g)$name, organizer_freq$`Organizer Name`)] * 3,
                    5)

# Save the plot as PNG
png(filename = here("output", "amhara_gofundme_network.png"), width = 1000, height = 800)

# Plot inside the PNG
plot(g,
     vertex.label.cex = 0.6,
     edge.arrow.size = 0.4,
     main = "Amhara GoFundMe Network: Organizers & Campaigns")

# Turn off PNG device
dev.off()
