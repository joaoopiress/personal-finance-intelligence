## =============================================================
## Personal Finance Intelligence — Data Pipeline
## scripts/01_data_pipeline.R
##
## Loads CSV → cleans data → inserts into PostgreSQL
## =============================================================

library(DBI)
library(RPostgres)
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# ── Configuration ─────────────────────────────────────────────
DATA_DIR  <- "path"
SQL_DIR   <- "path"
CSV_PATH  <- file.path(DATA_DIR, "transactions.csv")

cat("=== Personal Finance Intelligence: Data Pipeline ===\n\n")

# ── Step 1: Database Connection ───────────────────────────────
cat("[1/5] Connecting to Postgres database...\n")
con <- dbConnect(
  Postgres(),
  host     = "localhost",
  port     = 5432,
  dbname   = "finance",
  user     = "your-user",
  password = "your-password"   
)

# Load and prepare schema
schema_sql <- readLines(file.path(SQL_DIR, "schema.sql")) |> paste(collapse = "\n")
statements <- strsplit(schema_sql, ";\\s*\n")[[1]]
statements <- trimws(statements)
statements <- statements[nchar(statements) > 10]

# Execute schema statements
for (stmt in statements) {
  tryCatch(dbExecute(con, stmt), error = function(e) NULL)
}
cat("   ✓ Schema applied\n")

# ── Step 2: Load Raw CSV ──────────────────────────────────────
cat("[2/5] Loading raw transaction data...\n")
raw <- read_csv(CSV_PATH, show_col_types = FALSE)
cat(sprintf("   ✓ %d rows loaded\n", nrow(raw)))

# ── Step 3: Clean & Validate ──────────────────────────────────
cat("[3/5] Cleaning and validating data...\n")

clean <- raw |>
  # Standardize column names
  rename_with(tolower) |>
  # Parse and validate dates
  mutate(date = as.Date(date)) |>
  filter(!is.na(date)) |>
  # Clean description: uppercase, trim, remove special chars
  mutate(
    description = str_to_upper(str_trim(description)),
    description = str_replace_all(description, "[^A-Z0-9 ]", "")
  ) |>
  # Ensure amount is numeric and positive
  mutate(amount = as.numeric(amount)) |>
  filter(!is.na(amount), amount > 0) |>
  # Normalize category names
  mutate(
    category_raw = str_to_lower(str_trim(category)),
    category_raw = str_replace_all(category_raw, "\\s+", "_")
  ) |>
  # Flag recurring payments
  mutate(
    is_recurring = as.integer(
      str_detect(description,
                 "SUBSCRIPTION|PREMIUM|MEMBERSHIP|BILL|PAYMENT|WIRELESS|INTERNET")
    )
  ) |>
  select(date, description, amount, category_raw, is_recurring)

cat(sprintf("   ✓ %d valid rows after cleaning\n", nrow(clean)))

# ── Step 4: Map Categories to IDs ─────────────────────────────
cat("[4/5] Mapping categories...\n")

cat_lookup <- dbGetQuery(con, "SELECT id, name FROM categories") |>
  rename(category_id = id, category_raw = name)

clean <- clean |>
  left_join(cat_lookup, by = "category_raw") |>
  mutate(
    category_id = if_else(is.na(category_id),
                          # Default to 'uncategorized' if mapping fails
                          dbGetQuery(con, "SELECT id FROM categories WHERE name='uncategorized'")$id,
                          category_id
    )
  )

cat(sprintf("   ✓ Unmapped categories defaulted to 'uncategorized'\n"))

# ── Step 5: Insert into Database ──────────────────────────────
cat("[5/5] Inserting into database...\n")

# Clear existing data for idempotent reloads
dbExecute(con, "DELETE FROM transactions")

insert_df <- clean |>
  mutate(
    date          = as.character(date),
    predicted_cat = NA_character_,
    confidence    = NA_real_,
    notes         = NA_character_,
    created_at    = as.character(Sys.time())
  ) |>
  select(date, description, amount, category_id, category_raw,
         predicted_cat, confidence, is_recurring, notes, created_at)

dbWriteTable(con, "transactions", insert_df, append = TRUE)

# Verification
n_inserted <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM transactions")$n
cat(sprintf("   ✓ %d transactions inserted\n\n", as.integer(n_inserted)))

# ── Summary Report ────────────────────────────────────────────
cat("=== Pipeline Summary ===\n")
summary_q <- dbGetQuery(con, "
  SELECT
    TO_CHAR(date, 'YYYY-MM') AS month,
    COUNT(*)::integer AS transactions,
    ROUND(SUM(amount), 2) AS total_spent
  FROM transactions
  GROUP BY month
  ORDER BY month
")
print(summary_q)

dbDisconnect(con)

cat("\n✓ Pipeline complete.\n")
