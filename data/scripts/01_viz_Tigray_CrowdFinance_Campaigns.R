# ─────────────────────────────────────────────────────
# 01_viz_Tigray_CrowdFinance_Campaigns.R
# Project: TigrayCrowdFinanceViz.Rproj
# ─────────────────────────────────────────────────────

# 1. Load libraries
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(forcats)
library(scales)

# 2. Ensure the figures/ directory exists
if (!dir.exists("figures")) dir.create("figures")

# 3. Read the Tigray crowdfunding campaigns data
data_path <- "data/tigray_crowdfinance_campaigns.csv"
if (!file.exists(data_path)) stop("Data file not found: ", data_path)

raw <- read_csv(data_path, col_types = cols(.default = "c"))

# 4. Inspect column names (uncomment to debug)
# print(names(raw))

# 5. Rename and parse numeric columns
df <- raw %>%
  rename(
    campaign = `Campaign Title`,
    donors   = `Number of Donors`,
    amount   = `Total Amount Raised ($)`
  ) %>%
  mutate(
    donors = as.numeric(donors),
    amount = parse_number(amount)
  )

# 6. Check parsing
if (any(is.na(df$donors))) warning("Some donor counts could not be parsed.")
if (any(is.na(df$amount))) warning("Some amounts could not be parsed.")

# ─────────────────────────────────────────────────────
# 7. Plot: Total Amount Raised
# ─────────────────────────────────────────────────────
plot_funds <- df %>%
  filter(!is.na(amount)) %>%
  ggplot(aes(
    x = amount,
    y = fct_reorder(campaign, amount, .na_rm = TRUE)
  )) +
  geom_col(fill = "#1f78b4") +
  scale_x_continuous(labels = dollar_format(prefix = "$", big.mark = ",")) +
  labs(
    title = "Tigray Diaspora Campaigns by Total Funds Raised",
    x     = "Total Raised (USD)",
    y     = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y     = element_text(size = 12),
    plot.title      = element_text(face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background= element_rect(fill = "white", color = NA),
    plot.margin     = margin(t = 40, r = 20, b = 20, l = 20)
  )

ggsave(
  filename = "figures/tigray_campaigns_by_funds.png",
  plot     = plot_funds,
  width    = 10,
  height   = 6,
  dpi      = 300
)

# ─────────────────────────────────────────────────────
# 8. Plot: Number of Donors
# ─────────────────────────────────────────────────────
plot_donors <- df %>%
  filter(!is.na(donors)) %>%
  ggplot(aes(
    x = donors,
    y = fct_reorder(campaign, donors, .na_rm = TRUE)
  )) +
  geom_col(fill = "#1f78b4") +
  scale_x_continuous(labels = comma_format()) +
  labs(
    title = "Tigray Diaspora Campaigns by Number of Donors",
    x     = "Number of Donors",
    y     = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y     = element_text(size = 12),
    plot.title      = element_text(face = "bold", hjust = 0.5, margin = margin(b = 15)),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background= element_rect(fill = "white", color = NA),
    plot.margin     = margin(t = 40, r = 20, b = 20, l = 20)
  )

ggsave(
  filename = "figures/tigray_campaigns_by_donors.png",
  plot     = plot_donors,
  width    = 10,
  height   = 6,
  dpi      = 300
)

# ─────────────────────────────────────────────────────
# End of script
# ─────────────────────────────────────────────────────
