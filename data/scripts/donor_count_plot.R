# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

# Load the dataset
campaigns <- read_csv("data/amhara_gofundme.csv")

# Clean number of donors and reorder campaign titles in ascending order
campaigns <- campaigns %>%
  mutate(
    `Number of Donors` = parse_number(`Number of Donors`),
    `Campaign Title` = fct_reorder(`Campaign Title`, `Number of Donors`, .desc = FALSE)
  )

# Create the plot
ggplot(campaigns, aes(x = `Number of Donors`, y = `Campaign Title`)) +
  geom_col(fill = "#1f78b4") +
  labs(
    title = "Number of Donors to Amhara Diaspora GoFundMe Campaigns",
    x = "Number of Donors",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
