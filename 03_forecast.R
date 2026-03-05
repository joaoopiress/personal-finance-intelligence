## =============================================================
## Personal Finance Intelligence — Spending Forecast
## scripts/03_forecast.R
## =============================================================

library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(forecast)
library(ggplot2)
library(tidyr)

# Setup results directory
set.seed(42)
RESULTS_DIR <- here::here("analysis")
dir.create(RESULTS_DIR, showWarnings = FALSE)

cat("=== Spending Forecast ===\n\n")

# Database connection
con <- dbConnect(
  Postgres(),
  host     = "localhost",
  port     = 5432,
  dbname   = "finance",
  user     = "your-user",
  password = "your-password"
)

# Load monthly totals
monthly <- dbGetQuery(con, "
  SELECT
    TO_CHAR(date, 'YYYY-MM') AS month,
    SUM(amount)              AS total_spent
  FROM transactions
  GROUP BY month
  ORDER BY month
") |> as_tibble()

# Load category-level monthly totals
cat_monthly <- dbGetQuery(con, "
  SELECT
    TO_CHAR(t.date, 'YYYY-MM') AS month,
    c.name                     AS category,
    SUM(t.amount)              AS total
  FROM transactions t
  JOIN categories c ON t.category_id = c.id
  GROUP BY month, c.name
  ORDER BY month
") |> as_tibble()

dbDisconnect(con)

cat("Monthly data summary:\n")
print(monthly)

# Build time series object
ts_data <- ts(monthly$total_spent, frequency = 12,
              start = c(year(as.Date(paste0(monthly$month[1], "-01"))),
                        month(as.Date(paste0(monthly$month[1], "-01")))))

# ── Method 1: Holt-Winters Exponential Smoothing ─────────────
cat("\n► Holt-Winters forecast...\n")
hw_model    <- HoltWinters(ts_data, gamma = FALSE)
hw_forecast <- forecast(hw_model, h = 3)

next_month_hw <- round(hw_forecast$mean[1], 2)
cat(sprintf("  Next month forecast (Holt-Winters): $%.2f\n", next_month_hw))
cat(sprintf("  95%% Prediction Interval: [$%.2f - $%.2f]\n",
            round(hw_forecast$lower[1, 2], 2),
            round(hw_forecast$upper[1, 2], 2)))

# ── Method 2: Linear Trend Model ─────────────────────────────
cat("\n► Linear Trend model...\n")
monthly_lm    <- monthly |> mutate(t = row_number())
lm_model      <- lm(total_spent ~ t, data = monthly_lm)
next_t        <- data.frame(t = nrow(monthly_lm) + 1)
next_month_lm <- predict(lm_model, next_t, interval = "prediction")[1]

cat(sprintf("  Next month forecast (Linear Trend): $%.2f\n",
            round(next_month_lm, 2)))
cat(sprintf("  R-squared = %.3f | Trend: %+.1f $/month\n",
            summary(lm_model)$r.squared,
            coef(lm_model)["t"]))

# ── Method 3: 3-Month Moving Average ─────────────────────────
last3       <- tail(monthly$total_spent, 3)
ma_forecast <- mean(last3)
cat(sprintf("\n► 3-Month Moving Average: $%.2f\n", ma_forecast))

# ── Ensemble Model (Average of 3 Methods) ────────────────────
ensemble <- mean(c(next_month_hw, next_month_lm, ma_forecast))
cat(sprintf("\n★ Ensemble Forecast (Combined Result): $%.2f\n", ensemble))

# ── Category-Level Analysis ──────────────────────────────────
cat("\n► Category-level projections (next month)...\n")

cat_forecasts <- cat_monthly |>
  group_by(category) |>
  summarise(
    avg_monthly = round(mean(total), 2),
    last_month  = round(last(total), 2),
    trend       = round((last(total) - first(total)) / n(), 2),
    forecast    = round(mean(tail(total, 3)), 2),
    .groups     = "drop"
  ) |>
  arrange(desc(forecast))

print(cat_forecasts)

# Save category forecasts to CSV
write.csv(cat_forecasts,
          file.path(RESULTS_DIR, "category_forecasts.csv"),
          row.names = FALSE)

# ── Visualization ─────────────────────────────────────────────
# Prepare plot data
forecast_df <- tibble(
  month  = monthly$month,
  actual = monthly$total_spent,
  type   = "Actual"
)

future_months <- seq.Date(
  as.Date(paste0(tail(monthly$month, 1), "-01")) %m+% months(1),
  by = "month", length.out = 3
) |> format("%Y-%m")

forecast_pts <- tibble(
  month  = future_months,
  actual = as.numeric(hw_forecast$mean),
  type   = "Forecast"
)

plot_data <- bind_rows(forecast_df, forecast_pts)

# Generate forecast plot
p <- ggplot(plot_data, aes(x = month, y = actual, group = 1, color = type)) +
  geom_line(data = filter(plot_data, type == "Actual"),
            linewidth = 1.4, color = "#3B82F6") +
  geom_point(data = filter(plot_data, type == "Actual"),
             size = 3, color = "#3B82F6") +
  geom_line(data = filter(plot_data, type == "Forecast"),
            linewidth = 1.2, linetype = "dashed", color = "#F97316") +
  geom_point(data = filter(plot_data, type == "Forecast"),
             size = 3, color = "#F97316") +
  geom_vline(xintercept = nrow(monthly) + 0.5, linetype = "dotted",
             color = "gray40", linewidth = 0.8) +
  annotate("text", x = nrow(monthly) + 0.6, y = max(plot_data$actual) * 0.95,
           label = "Forecast Area", hjust = 0, color = "#F97316", size = 3.5) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title    = "Monthly Spending Trend",
       subtitle = "Ensemble Prediction | Personal Finance Intelligence",
       x        = "Month",
       y        = "Total Spending ($)",
       color    = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

# Save plot to analysis directory
ggsave(file.path(RESULTS_DIR, "spending_forecast.png"), p,
       width = 9, height = 5, dpi = 150)

cat("\n✓ Forecast visualization saved to analysis/spending_forecast.png\n")
cat(sprintf("\n★ SUMMARY: Next month ensemble forecast: $%.2f\n", ensemble))