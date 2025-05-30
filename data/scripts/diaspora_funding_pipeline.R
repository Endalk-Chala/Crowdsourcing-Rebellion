# diaspora_funding_pipeline.R
# Project: DiasporaFundFlowSimulation.Rproj

# 0. Create necessary directories
dir.create("data",     showWarnings = FALSE, recursive = TRUE)
dir.create("figures",  showWarnings = FALSE, recursive = TRUE)
dir.create("output",   showWarnings = FALSE, recursive = TRUE)

# 1. Load libraries
library(readr)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(ggplot2)
library(scales)
library(fitdistrplus)

# 2. Define paths to cleaned CSVs
paths <- c(
  Oromo  = "data/cleaned_oromo_simple.csv",
  Amhara = "data/cleaned_amhara_simple.csv",
  Tigray = "data/cleaned_tigray_simple.csv"
)

# 3. Read & normalize each dataset into a unified dataframe
hist_df <- imap_dfr(paths, function(path, grp) {
  df <- read_csv(path, show_col_types = FALSE)
  amt_col   <- intersect(names(df), c("Total Amount Raised", "Amount Raised"))
  start_col <- intersect(names(df), c("Start Date", "Start"))
  end_col   <- intersect(names(df), c("End Date",   "End"))
  if (length(amt_col)   != 1) stop("Could not detect Amount column in ", path)
  if (length(start_col) != 1) stop("Could not detect Start column in ", path)
  if (length(end_col)   != 1) stop("Could not detect End column in ",   path)
  
  df2 <- df %>%
    rename_with(~"Amount",    all_of(amt_col)) %>%
    rename_with(~"Start_raw", all_of(start_col)) %>%
    rename_with(~"End_raw",   all_of(end_col)) %>%
    mutate(
      Group = grp,
      Start = as.Date(Start_raw, tryFormats = c("%Y-%m-%d","%d-%b-%y","%m/%d/%Y")),
      End   = as.Date(End_raw,   tryFormats = c("%Y-%m-%d","%d-%b-%y","%m/%d/%Y"))
    ) %>%
    filter(
      !is.na(Amount),
      !is.na(Start), !is.na(End),
      Start <= End
    )
  # keep only essential columns via base subsetting
  df2[c("Group","Amount","Start","End")]
})

# 4. Expand each campaign into monthly observations
hist_monthly <- hist_df %>%
  mutate(
    MonthList = map2(
      floor_date(Start, "month"),
      floor_date(End,   "month"),
      ~ seq(.x, .y, by = "month")
    ),
    per_month = Amount / map_int(MonthList, length)
  ) %>%
  unnest(cols = MonthList) %>%
  rename(Month = MonthList) %>%
  group_by(Group, Month) %>%
  summarise(
    MonthlyRaised = sum(per_month, na.rm = TRUE),
    .groups = "drop"
  )

# 5. Check for valid data
if (nrow(hist_monthly) == 0) {
  stop("No valid monthly data found. Check that your CSVs have correct date formats.")
}

# 6. Fit Gamma distributions and simulate forward
n_sims <- 500
last_month <- max(hist_monthly$Month)
future_months <- seq(
  last_month + months(1),
  by = "month",
  length.out = 12
)

sim_summary <- hist_monthly %>%
  group_by(Group) %>%
  summarise(
    fit = list(fitdist(MonthlyRaised + 1e-6, "gamma")),
    .groups = "drop"
  ) %>%
  mutate(
    sims = map(fit, ~ {
      p <- .x$estimate
      replicate(
        n_sims,
        rgamma(
          length(future_months),
          shape = p["shape"],
          rate  = p["rate"]
        )
      )
    }),
    summary = map(sims, ~ {
      m <- .
      tibble(
        Month   = future_months,
        Mean    = rowMeans(m),
        Lower95 = apply(m, 1, quantile, 0.025),
        Upper95 = apply(m, 1, quantile, 0.975)
      )
    })
  ) %>%
  dplyr::select(Group, summary) %>%
  tidyr::unnest(cols = summary)

# 7. Plot historical and forecasted series
p <- ggplot() +
  geom_line(
    data = hist_monthly,
    aes(x = Month, y = MonthlyRaised, color = Group),
    size = 1.2
  ) +
  geom_line(
    data = sim_summary,
    aes(x = Month, y = Mean, color = Group),
    linetype = "dashed",
    size = 1
  ) +
  geom_ribbon(
    data = sim_summary,
    aes(x = Month, ymin = Lower95, ymax = Upper95, fill = Group),
    alpha = 0.2,
    color = NA
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  scale_y_continuous(
    labels = dollar_format(prefix = "$", big.mark = ",")
  ) +
  scale_color_manual(
    values = c(
      "Amhara" = "#E69F00",
      "Oromo"  = "#D55E00",
      "Tigray" = "#0072B2"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Amhara" = "#E69F00",
      "Oromo"  = "#D55E00",
      "Tigray" = "#0072B2"
    )
  ) +
  labs(
    title    = "Diaspora Fundraising: Historical & Forecasted Monthly Totals",
    subtitle = "Solid = observed; dashed = forecast mean; ribbon = 95% CI",
    x        = NULL,
    y        = "Funds Raised (USD)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.background   = element_rect(fill = "gray98"),
    panel.grid.major   = element_line(color = "white"),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    legend.position    = "bottom",
    legend.title       = element_blank(),
    plot.title         = element_text(face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(hjust = 0.5)
  )

# 8. Save plot to file
ggsave(
  filename = "figures/diaspora_fundraising_journal.png",
  plot     = p,
  width    = 8,
  height   = 6,
  dpi      = 300
)
