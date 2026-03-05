# Personal Finance Intelligence
![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
![PostgreSQL](https://img.shields.io/badge/PostgreSQL-316192?style=flat&logo=postgresql&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-blue?style=flat&logo=r&logoColor=white)

A personal finance analytics system built with R, PostgreSQL, and Shiny. Transactions are loaded from CSV or user input (through app.R), cleaned and stored in a database, automatically classified into spending categories using a machine learning model trained iteratively, and visualised in a live web application — where users can also add and delete transactions in real time.

---

## Dashboard Preview

![Dashboard Demo](assets/demo.gif)

---

## What This Project Does

This system:

1. **Ingests** raw transaction CSVs and loads them into PostgreSQL
2. **Classifies** each transaction automatically using a trained ML model
3. **Suggests** a category in real time as the user types a description
4. **Forecasts** next month's spending using an ensemble of 3 statistical models
5. **Visualises** everything in a live Shiny dashboard with add/delete functionality

---

## Project Structure

```
personal-finance-intelligence/
│
├── app.R                        # Shiny application — UI + Server
├── 01_data_pipeline.R           # CSV ingestion, cleaning, PostgreSQL insertion
├── 02_ml_classifier.R           # ML model training + real-time prediction function
├── 03_forecast.R                # Spending forecast (3 methods + ensemble)
│
├── www/
│   └── index.html               # Custom dark-theme HTML template for Shiny
│
├── sql/
│   ├── schema.sql               # PostgreSQL schema (3 tables + indexes + seed data)
│   └── queries.sql              # 10 standalone analytical queries
│
├── data/
│   └── transactions.csv         # Input data (date, description, amount, category)
```

---

## Tech Stack

| Layer | Technology |
|---|---|
| Database | PostgreSQL (local) |
| Data Pipeline | R · `readr` · `dplyr` · `DBI` · `RPostgres` |
| Text Features | R · `tidytext` · `stringr` |
| ML Models | R · `e1071` (Naive Bayes) · `nnet` (Logistic Regression) |
| Forecasting | R · `forecast` (Holt-Winters) · OLS · Moving Average |
| Web Application | R Shiny · `bslib` · `DT` · `ggplot2` |
| UI Template | Custom HTML/CSS (dark theme) injected via `htmlTemplate()` |

---

## Quick Start

### 1 — Install R packages

```r
install.packages(c(
  "shiny", "bslib", "DBI", "RPostgres", "dplyr", "ggplot2",
  "scales", "here", "DT", "readr", "lubridate", "stringr",
  "tidytext", "tidyr", "e1071", "nnet", "caret",
  "forecast", "jsonlite"
))
```

### 2 — Set up PostgreSQL

Create a local PostgreSQL database called `finance` and apply the schema:

```bash
psql -U postgres -c "CREATE DATABASE finance;"
psql -U postgres -d finance -f sql/schema.sql
```

### 3 — Load transaction data

Run the pipeline script to ingest the CSV into PostgreSQL:

```r
source("01_data_pipeline.R")
```

### 4 — Train the ML model

```r
source("02_ml_classifier.R")
```

### 5 — Launch the Shiny app

```r
shiny::runApp("app.R")
```

> The app is now live at `http://127.0.0.1:PORT` in your browser.

---

## Database Schema

Three tables in PostgreSQL:

```
categories    — lookup table (13 categories with colour codes and essential/discretionary flag)
transactions  — core table (date, description, amount, category_id, ML prediction, confidence)
budgets       — optional monthly budget targets per category
```

**Key design decisions:**
- `NUMERIC(12,2)` for amounts — exact precision, no floating-point errors
- `TIMESTAMPTZ` for created_at — timezone-aware timestamps
- `ON CONFLICT DO NOTHING` for idempotent category seeding
- Indexes on `date`, `category_id`, and `amount DESC` for fast analytical queries

---

## Machine Learning: Transaction Classifier

The classifier was built **iteratively** across 3 attempts — reflecting a real experimentation process.

### Attempt 1 — Keyword Rules (Baseline)
Hand-crafted regex patterns per category, tailored to Portuguese merchants (Pingo Doce, Galp, MEO, TAP, etc.).
- Fast and interpretable
- Fails on ambiguous or unknown merchants

### Attempt 2 — Naive Bayes
Document-term matrix from tokenised descriptions + Laplace smoothing.
- Learns statistical patterns from training data
- Better generalisation than pure rules

### Attempt 3 — Multinomial Logistic Regression ✅ Best Model
Same text features + engineered numeric features:

| Feature | Logic |
|---|---|
| Top-80 unigrams | Binary word presence from merchant descriptions |
| `log_amount` | Captures order-of-magnitude differences |
| `is_round` | Round numbers often indicate bills or rent |
| `is_large` | Flags transactions > €200 (rent, flights) |
| `amount_bucket` | 6-level ordinal: tiny / small / medium / large / xlarge / huge |

**Real-time prediction:** The trained model is exposed as `predict_transaction_category(desc, amount)`, called live in the Shiny app as the user types — automatically suggesting a category before they submit.

---

## Spending Forecast

Next month's spending is predicted by training **three independent models** on historical monthly totals, then combining their outputs into a single ensemble estimate. Each model captures a different signal in the data — averaging them reduces the risk of any one model being wrong.

---

### Method 1 — Holt-Winters Exponential Smoothing

```r
hw_model    <- HoltWinters(ts_data, gamma = FALSE)
hw_forecast <- forecast(hw_model, h = 3)
```

Assigns exponentially decreasing weights to past months — recent months matter more than older ones. If spending spiked in March, that has more influence on the April forecast than what happened in January.

`gamma = FALSE` disables the seasonal component, which is appropriate here since a few months of data are not enough to establish a reliable seasonal pattern.

This is the only method that natively produces a **95% prediction interval**, giving both an optimistic lower bound and a pessimistic upper bound alongside the point estimate:

```
Example output:
  Point forecast:  €1,250
  Lower bound:      €980
  Upper bound:     €1,520
```

The Shiny dashboard uses this method directly for its live forecast panel, recalculating whenever a new transaction is added.

---

### Method 2 — Linear Trend (OLS Regression)

```r
monthly_lm    <- monthly |> mutate(t = row_number())
lm_model      <- lm(total_spent ~ t, data = monthly_lm)
next_month_lm <- predict(lm_model, data.frame(t = nrow(monthly_lm) + 1))
```

Fits a straight line through the monthly spending values and extrapolates it one step forward. If spending has been growing by ~€50/month, this model projects that trend to continue.

The R² coefficient measures how well the linear trend explains the variance in the data — a value close to 1.0 indicates a very consistent upward or downward trend.

```
Example with 4 months of data:
  Jan: €1,100
  Feb: €1,150   → trend: +€50/month
  Mar: €1,200
  Apr: €1,250
  May (forecast): €1,300
```

---

### Method 3 — 3-Month Moving Average

```r
last3       <- tail(monthly$total_spent, 3)
ma_forecast <- mean(last3)
```

The simplest of the three. Takes the mean of the last 3 months and uses it as the next month's prediction. Works well when spending is stable with no clear trend, and is robust against one-off anomalous months (e.g., an unusually expensive trip) distorting the forecast.

```
Example:
  Feb: €1,150
  Mar: €1,200
  Apr: €1,250
  Average → May forecast: €1,200
```

---

### Ensemble — Combining All Three

```r
ensemble <- mean(c(next_month_hw, next_month_lm, ma_forecast))
```

The final forecast is the simple mean of the three model outputs. Each model has different strengths and weaknesses:

| Model | Works best when... | Less reliable when... |
|---|---|---|
| Holt-Winters | Spending has a gradual trend | Very few months of data |
| Linear Trend | Growth or decline is consistent | Spending is irregular |
| Moving Average | Spending is stable month-to-month | There is a strong upward trend |

By averaging the three, individual model errors tend to cancel out — producing a more stable and reliable estimate than any single model alone. This is a standard technique in forecasting known as **forecast combination**.

The Shiny dashboard shows the forecast dynamically — it recalculates in real time as new transactions are added.

---

## Shiny Application Features

The `app.R` is a full CRUD application connected live to PostgreSQL:

- **Add transactions** via sidebar form — with real-time ML category suggestion as you type
- **Delete transactions** with confirmation modal
- **Filter by month** — all KPIs, charts and tables update reactively
- **KPI cards** — total spent, average transaction, top category, month-over-month delta
- **Forecast panel** — Holt-Winters point estimate with 95% prediction interval and range bar
- **Category breakdown** — horizontal bar chart of top 5 categories
- **Monthly trend** — bar chart with LOESS smoothing trend line
- **Spending donut** — sidebar chart with custom category colours
- **Category forecast table** — predicted vs historical average per category
- **Recent transactions table** — interactive DataTable with delete button per row

---

## SQL Queries

`sql/queries.sql` contains 10 ready-to-run PostgreSQL queries for ad-hoc analysis directly in DataGrip or pgAdmin — independent of the Shiny app. The app executes equivalent queries inline via `DBI`, but this file serves as a standalone analytical reference.

| Query | Description | Used in code |
|---|---|---|
| Q1 — Monthly Summary | Total spent, transaction count and averages per month | `03_forecast.R`, `app.R` |
| Q2 — Spending by Category | All-time totals, averages and % of total per category | `app.R` (donut chart, category plot) |
| Q3 — Monthly by Category | Monthly breakdown per category (pivot-style) | `03_forecast.R` (category forecasts) |
| Q4 — Top 10 Expenses | Largest individual transactions with category | Ad-hoc analysis |
| Q5 — Recurring Detection | Merchants appearing in 3+ months with total paid | Ad-hoc analysis |
| Q6 — Month-over-Month | Spending change and % delta using `LAG()` window function | `app.R` (KPI delta indicator) |
| Q7 — Essential vs Discretionary | Split between essential and discretionary categories | Ad-hoc analysis |
| Q8 — Budget vs Actual | Variance against budget targets *(requires budgets table populated)* | Ad-hoc analysis |
| Q9 — Merchant Drill-down | Filter transactions by merchant keyword (e.g. cafés) | Ad-hoc analysis |
| Q10 — Rolling Average | 7-day rolling average of daily spending using window functions | Ad-hoc analysis |

---

## Key Insights from the Data

- The ML classifier improves accuracy significantly across iterations, demonstrating the value of feature engineering over simple rule-based approaches
- Keyword rules work well for known Portuguese merchants but fail for generic descriptions — motivating the move to ML
- The ensemble forecast provides more stable estimates than any single model alone, especially with limited monthly data points
- Real-time category suggestion in the Shiny app reduces manual classification effort to near zero

---

## Extending the Project

- **Replace sample data** with a real bank export (Millennium BCP, Montepio, Revolut CSV)
- **Add budget alerts** — populate the `budgets` table and use Query 8 to track overspending
- **Deploy to shinyapps.io** — the app is fully self-contained and ready to deploy
- **Add anomaly detection** — flag transactions that are unusually large for their category
- **Connect to Revolut API** — automate transaction ingestion without CSV exports

---

## Author

**João Pires**
[LinkedIn](www.linkedin.com/in/joaoo-piress) · [GitHub](https://github.com/joaoopiress)

---

*Transaction data used in development is synthetic. No real financial data is included in this repository.*
