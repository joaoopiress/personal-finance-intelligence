## =============================================================
## Personal Finance Intelligence — ML Transaction Classifier
## scripts/02_ml_classifier.R
##
## Iterative approach:
##   Attempt 1: Keyword rule-based classifier (baseline)
##   Attempt 2: Naive Bayes on raw tokens
##   Attempt 3: Logistic Regression with engineered features
## =============================================================

library(DBI)
library(RPostgres)
library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)
library(e1071)       # Naive Bayes
library(nnet)        # Multinomial Logistic Regression
library(caret)
library(jsonlite)

# Setup output directory
OUT_DIR <- here::here("analysis")
dir.create(OUT_DIR, showWarnings = FALSE)

cat("=== ML Transaction Classifier ===\n\n")

# Database connection
con <- dbConnect(
  Postgres(),
  host     = "localhost",
  port     = 5432,
  dbname   = "finance",
  user     = "your-user",
  password = "your-password"
)

# Load data
df <- dbGetQuery(con, "
  SELECT t.id, t.description, t.amount, c.name AS category
  FROM transactions t
  JOIN categories c ON t.category_id = c.id
")
dbDisconnect(con)

cat(sprintf("Loaded %d labelled transactions\n", nrow(df)))
cat("Category distribution:\n")
print(table(df$category))
cat("\n")

## ─────────────────────────────────────────────────────────────
## ATTEMPT 1: Keyword Rule-Based Classifier (Baseline)
## ─────────────────────────────────────────────────────────────
cat("── Attempt 1: Keyword Rule-Based Classifier ──\n")

keyword_classify <- function(description) {
  desc <- str_to_upper(description)
  case_when(
    str_detect(desc, "RENT|LEASE|MORTGAGE|HOUSING|CONDO")                        ~ "rent",
    str_detect(desc, "GROCER|SUPERMARKET|CONTINENTE|PINGO DOCE|AUCHAN|LIDL|ALDI") ~ "groceries",
    str_detect(desc, "COFFEE|CAFE|MCDONALDS|UBER EATS|GLOVO|RESTAURANT|BURGER")   ~ "food_drink",
    str_detect(desc, "UBER|BOLT|GALP|REPSOL|BP|PRIO|METRO|TRAIN|PARKING")         ~ "transport",
    str_detect(desc, "NETFLIX|SPOTIFY|DISNEY|APPLE|AMAZON PRIME|HBO|SUBSCR")      ~ "subscriptions",
    str_detect(desc, "IKEA|LEROY MERLIN|CONFORAMA|CASA|HOME")                     ~ "home",
    str_detect(desc, "AMAZON|WORTEN|FNAC|ZARA|SHOPPING|PURCHASE")                 ~ "shopping",
    str_detect(desc, "PHARMACY|HEALTH|CLINIC|GYM|FITNESS|HOSPITAL")               ~ "health",
    str_detect(desc, "EDP|ENDESA|WATER|ELECTRICITY|MEO|NOS|VODAFONE|BILL")        ~ "utilities",
    str_detect(desc, "AIRLINE|TAP|HOTEL|AIRBNB|TRAVEL|TRIP|FLIGHT")               ~ "travel",
    TRUE                                                                          ~ "uncategorized"
  )
}

df$pred_attempt1 <- keyword_classify(df$description)
acc1 <- mean(df$pred_attempt1 == df$category)
cat(sprintf("   Accuracy: %.1f%%\n\n", acc1 * 100))

## ─────────────────────────────────────────────────────────────
## FEATURE ENGINEERING (Shared by Attempts 2 & 3)
## ─────────────────────────────────────────────────────────────
cat("── Feature Engineering ──\n")

# Tokenization
tokens <- df |>
  mutate(row_id = row_number()) |>
  unnest_tokens(word, description) |>
  filter(!str_detect(word, "^[0-9]+$"))

# Create vocabulary (Top 80 words)
vocab <- tokens |>
  count(word, sort = TRUE) |>
  slice_head(n = 80) |>
  pull(word)

# Document-Term Matrix
dtm <- tokens |>
  filter(word %in% vocab) |>
  mutate(present = 1L) |>
  pivot_wider(id_cols = row_id, names_from = word,
              values_from = present, values_fill = 0L)

# Feature matrix construction
feature_df <- df |>
  mutate(row_id = row_number()) |>
  left_join(dtm, by = "row_id") |>
  mutate(
    across(all_of(vocab[vocab %in% names(dtm)]), ~replace_na(.x, 0L)),
    log_amount    = log1p(amount),
    is_round      = as.integer(amount == round(amount, -1)),
    is_large      = as.integer(amount > 200),
    amount_bucket = cut(amount, breaks = c(0,10,25,50,100,250,Inf),
                        labels = c("tiny","small","medium","large","xlarge","huge"),
                        include.lowest = TRUE)
  )

feature_cols <- c(
  intersect(vocab, names(feature_df)),
  "log_amount", "is_round", "is_large"
)

cat(sprintf("   Feature matrix: %d rows x %d features\n\n",
            nrow(feature_df), length(feature_cols)))

# Data split (80/20)
set.seed(42)
split_idx <- createDataPartition(feature_df$category, p = 0.8, list = FALSE)
train_df  <- feature_df[ split_idx, ]
test_df   <- feature_df[-split_idx, ]

## ─────────────────────────────────────────────────────────────
## ATTEMPT 2: Naive Bayes
## ─────────────────────────────────────────────────────────────
cat("── Attempt 2: Naive Bayes ──\n")

X_train  <- as.matrix(train_df[, feature_cols])
X_test   <- as.matrix(test_df[, feature_cols])

nb_model <- naiveBayes(X_train, as.factor(train_df$category), laplace = 1)
pred_nb  <- predict(nb_model, X_test)
acc2     <- mean(pred_nb == test_df$category)

cat(sprintf("   Test Accuracy: %.1f%%\n", acc2 * 100))
cat("   Confusion Matrix:\n")
print(table(Predicted = pred_nb, Actual = test_df$category))
cat("\n")

## ─────────────────────────────────────────────────────────────
## ATTEMPT 3: Multinomial Logistic Regression (Best Model)
## ─────────────────────────────────────────────────────────────
cat("── Attempt 3: Multinomial Logistic Regression ──\n")

# Prepare dummy variables for amount buckets
train_full <- train_df |>
  mutate(amount_bucket = as.character(amount_bucket)) |>
  bind_cols(
    model.matrix(~amount_bucket - 1,
                 data = data.frame(amount_bucket = as.factor(train_df$amount_bucket)))
  )

test_full <- test_df |>
  mutate(amount_bucket = as.character(amount_bucket)) |>
  bind_cols(
    model.matrix(~amount_bucket - 1,
                 data = data.frame(amount_bucket = as.factor(test_df$amount_bucket)))
  )

bucket_cols  <- grep("^amount_bucket", names(train_full), value = TRUE)
all_features <- c(feature_cols, bucket_cols)
all_features <- all_features[all_features %in% names(train_full)]

# Train model
lr_model <- multinom(
  as.factor(category) ~ .,
  data    = cbind(category = train_df$category,
                  as.data.frame(train_full[, all_features])),
  trace   = FALSE,
  MaxNWts = 5000
)

# Evaluate model
pred_lr <- predict(lr_model, newdata = as.data.frame(test_full[, all_features]))
acc3    <- mean(pred_lr == test_df$category)
cat(sprintf("   Test Accuracy: %.1f%%\n", acc3 * 100))

# Standardize category levels for confusion matrix
all_categories <- unique(df$category)
pred_lr_factor <- factor(pred_lr, levels = all_categories)
test_category_factor <- factor(test_df$category, levels = all_categories)

cm <- confusionMatrix(pred_lr_factor, test_category_factor)
print(cm)
cat("   Per-Class Statistics:\n")
print(cm$byClass[, c("Precision","Recall","F1")])
cat("\n")

## ─────────────────────────────────────────────────────────────
## Results Comparison
## ─────────────────────────────────────────────────────────────
cat("── Model Comparison ──\n")
results <- data.frame(
  attempt   = c("Attempt 1: Keywords", "Attempt 2: Naive Bayes",
                "Attempt 3: Logistic Regression"),
  accuracy = round(c(acc1, acc2, acc3) * 100, 1)
)
print(results)
cat("\n")

## ─────────────────────────────────────────────────────────────
## Export Best Model Results
## ─────────────────────────────────────────────────────────────
cat("── Exporting Results ──\n")

# Process full matrix for final export
full_mat <- feature_df |>
  mutate(amount_bucket = as.character(amount_bucket)) |>
  bind_cols(
    model.matrix(~amount_bucket - 1,
                 data = data.frame(amount_bucket = as.factor(feature_df$amount_bucket)))
  )

bucket_cols2  <- grep("^amount_bucket", names(full_mat), value = TRUE)
all_features2 <- c(feature_cols, bucket_cols2)
all_features2 <- all_features2[all_features2 %in% names(full_mat)]

# Generate predictions and confidence scores
probs      <- predict(lr_model, newdata = as.data.frame(full_mat[, all_features2]),
                      type = "probs")
preds      <- predict(lr_model, newdata = as.data.frame(full_mat[, all_features2]))
confidence <- apply(probs, 1, max)

export_df <- df |>
  mutate(
    predicted_category = as.character(preds),
    confidence         = round(confidence, 3),
    correct            = predicted_category == category
  )

# Write output files
write.csv(export_df, file.path(OUT_DIR, "predictions.csv"), row.names = FALSE)

accuracy_summary <- list(
  model_comparison = results,
  best_model       = "Logistic Regression",
  best_accuracy    = acc3,
  per_class        = as.data.frame(cm$byClass[, c("Precision","Recall","F1")])
)
write_json(accuracy_summary, file.path(OUT_DIR, "model_summary.json"), pretty = TRUE)

cat("   ✓ Predictions exported to analysis/predictions.csv\n")
cat("=== ML Classifier complete ===\n")

## =============================================================
## SHINY APP INTEGRATION
## Real-time prediction function
## =============================================================
cat("── Initializing Real-Time Prediction Function ──\n")

predict_transaction_category <- function(new_desc, new_amount) {
  
  # Error handling to prevent app crashes; fallback to Attempt 1
  tryCatch({
    
    # 1. Initialize feature vector
    new_data <- as.data.frame(matrix(0, nrow = 1, ncol = length(all_features2)))
    names(new_data) <- all_features2
    
    # 2. Feature Engineering: Text (Bag of Words)
    tokens_new <- unlist(str_extract_all(tolower(new_desc), "[a-z]+"))
    
    for (w in tokens_new) {
      if (w %in% vocab && w %in% names(new_data)) {
        new_data[1, w] <- 1L
      }
    }
    
    # 3. Feature Engineering: Numeric
    if ("log_amount" %in% names(new_data)) new_data$log_amount <- log1p(new_amount)
    if ("is_round" %in% names(new_data)) new_data$is_round <- as.integer(new_amount == round(new_amount, -1))
    if ("is_large" %in% names(new_data)) new_data$is_large <- as.integer(new_amount > 200)
    
    # 4. Feature Engineering: Amount Bucket
    bucket <- as.character(cut(new_amount, breaks = c(0,10,25,50,100,250,Inf),
                               labels = c("tiny","small","medium","large","xlarge","huge"),
                               include.lowest = TRUE))
    
    bucket_col <- paste0("amount_bucket", bucket)
    if (bucket_col %in% names(new_data)) new_data[1, bucket_col] <- 1L
    
    # 5. Predict using Logistic Regression
    prediction <- predict(lr_model, newdata = new_data)
    return(as.character(prediction))
    
  }, error = function(e) {
    # Fallback to keyword classifier on error
    return(keyword_classify(new_desc))
  })
}

cat("   ✓ Function predict_transaction_category() is ready for Shiny.\n")
