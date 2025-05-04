# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)

# Load your data
campaigns <- read_csv("data/amhara_gofundme.csv")

# Ensure sorting by total raised in DESCENDING order
campaigns <- campaigns %>%
  mutate(`Campaign Title` = fct_reorder(`Campaign Title`, `Total Amount Raised ($)`, .desc = TRUE))

# Clean bar plot
ggplot(campaigns, aes(x = `Total Amount Raised ($)`, y = `Campaign Title`)) +
  geom_col(fill = "#1f78b4") +
  labs(
    title = "Funds Raised by Amhara Diaspora GoFundMe Campaigns",
    x = "Total Raised (USD)",
    y = NULL
  ) +
  scale_x_continuous(labels = scales::dollar_format()) +  # Format axis with dollar signs
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )
