# Load libraries
library(igraph)
library(tidyverse)
library(here)

# Read the GoFundMe CSV file from the 'data' folder
df <- read_csv(here("data", "amhara_gofundme.csv"))

# Preview the column names and first few rows (for debugging)
print(colnames(df))
print(head(df))

# Clean the data: remove any rows with missing Organizer or Campaign Title
df_clean <- df %>%
  filter(!is.na(`Organizer Name`), !is.na(`Campaign Title`))

# Select edges: each edge connects an organizer to a campaign
edges <- df_clean %>%
  select(`Organizer Name`, `Campaign Title`)

# Create an undirected graph from the edge list
g <- graph_from_data_frame(edges, directed = FALSE)

# Plot the network
plot(g,
     vertex.size = 5,
     vertex.label.cex = 0.6,
     edge.arrow.size = 0.4,
     main = "Organizer-Campaign Network: Amhara GoFundMe")


