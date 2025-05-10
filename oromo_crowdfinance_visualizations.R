# ─────────────────────────────────────────────────────
# oromo_crowdfinance_visualizations.R
# Project: OromoDiasporaFunding
# ─────────────────────────────────────────────────────

library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

# 1. Create figures folder if missing
if (!dir.exists("figures")) dir.create("figures")

# 2. Load the data
oromo_df <- read_csv("data/Oromo_Diaspora_Fundraising_Campaigns.csv") %>%
  mutate(across(everything(), ~ if(is.character(.)) str_trim(.) else .)) %>% 
  mutate(
    `Total Amount Raised ($)` = parse_number(`Total Amount Raised ($)`),
    `Number of Donors`        = as.numeric(`Number of Donors`)
  )

# ─────────────────────────────────────────────────────
# 3. Plot: Total Amount Raised
#    – drop NAs and reorder
# ─────────────────────────────────────────────────────
plot_funds <- oromo_df %>%
  filter(!is.na(`Total Amount Raised ($)`)) %>%
  ggplot(aes(
    x = `Total Amount Raised ($)`,
    y = fct_reorder(`Campaign Title`, `Total Amount Raised ($)`, .na_rm = TRUE)
  )) +
  geom_col(fill = "#1f78b4") +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Oromo Diaspora Campaigns by Total Funds Raised",
    x = "Total Raised (USD)",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y     = element_text(size = 12),
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background= element_rect(fill = "white", color = NA),
    plot.margin     = margin(t = 20, r = 20, b = 20, l = 20)
  )

ggsave("figures/oromo_campaigns_by_funds.png", plot_funds,
       width = 10, height = 6, dpi = 300)

# ─────────────────────────────────────────────────────
# 4. Plot: Number of Donors
#    – drop NAs and reorder
# ─────────────────────────────────────────────────────
plot_donors <- oromo_df %>%
  filter(!is.na(`Number of Donors`)) %>%
  ggplot(aes(
    x = `Number of Donors`,
    y = fct_reorder(`Campaign Title`, `Number of Donors`, .na_rm = TRUE)
  )) +
  geom_col(fill = "#1f78b4") +
  labs(
    title = "Oromo Diaspora Campaigns by Number of Donors",
    x = "Number of Donors",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y     = element_text(size = 12),
    plot.title      = element_text(face = "bold", hjust = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background= element_rect(fill = "white", color = NA),
    plot.margin     = margin(t = 20, r = 20, b = 20, l = 20)
  )

ggsave("figures/oromo_campaigns_by_donors.png", plot_donors,
       width = 10, height = 6, dpi = 300)




