-- ============================================================
-- Personal Finance Intelligence — Analytical Queries
-- PostgreSQL version
-- ============================================================

-- ── 1. MONTHLY SPENDING SUMMARY ─────────────────────────────
SELECT
    TO_CHAR(date, 'YYYY-MM')         AS month,
    COUNT(*)::integer                AS num_transactions,
    ROUND(SUM(amount), 2)            AS total_spent,
    ROUND(AVG(amount), 2)            AS avg_transaction,
    ROUND(MAX(amount), 2)            AS largest_transaction
FROM transactions
GROUP BY month
ORDER BY month;

-- ── 2. SPENDING BY CATEGORY (ALL TIME) ──────────────────────
SELECT
    c.name                           AS category,
    COUNT(t.id)::integer             AS transactions,
    ROUND(SUM(t.amount), 2)          AS total_spent,
    ROUND(AVG(t.amount), 2)          AS avg_per_transaction,
    ROUND(SUM(t.amount) * 100.0 /
        (SELECT SUM(amount) FROM transactions), 1) AS pct_of_total
FROM transactions t
JOIN categories c ON t.category_id = c.id
GROUP BY c.name
ORDER BY total_spent DESC;

-- ── 3. MONTHLY SPENDING BY CATEGORY (PIVOT-STYLE) ───────────
SELECT
    TO_CHAR(t.date, 'YYYY-MM')       AS month,
    c.name                           AS category,
    ROUND(SUM(t.amount), 2)          AS total_spent
FROM transactions t
JOIN categories c ON t.category_id = c.id
GROUP BY month, c.name
ORDER BY month, total_spent DESC;

-- ── 4. TOP 10 LARGEST SINGLE EXPENSES ───────────────────────
SELECT
    t.date,
    t.description,
    ROUND(t.amount, 2)               AS amount,
    c.name                           AS category
FROM transactions t
JOIN categories c ON t.category_id = c.id
ORDER BY t.amount DESC
LIMIT 10;

-- ── 5. RECURRING EXPENSE DETECTION ──────────────────────────
-- Transactions that appear in 3+ months = likely recurring
SELECT
    description,
    COUNT(DISTINCT TO_CHAR(date, 'YYYY-MM'))::integer AS months_seen,
    ROUND(AVG(amount), 2)            AS avg_amount,
    ROUND(SUM(amount), 2)            AS total_paid
FROM transactions
GROUP BY description
HAVING COUNT(DISTINCT TO_CHAR(date, 'YYYY-MM')) >= 3
ORDER BY total_paid DESC;

-- ── 6. MONTH-OVER-MONTH SPENDING CHANGE ─────────────────────
WITH monthly AS (
    SELECT
        TO_CHAR(date, 'YYYY-MM')     AS month,
        SUM(amount)                  AS total
    FROM transactions
    GROUP BY month
)
SELECT
    month,
    ROUND(total, 2)                  AS total_spent,
    ROUND(total - LAG(total) OVER (ORDER BY month), 2)        AS mom_change,
    ROUND((total - LAG(total) OVER (ORDER BY month)) * 100.0
        / LAG(total) OVER (ORDER BY month), 1)                AS mom_pct_change
FROM monthly
ORDER BY month;

-- ── 7. ESSENTIAL vs DISCRETIONARY SPLIT ─────────────────────
SELECT
    CASE WHEN c.is_essential = 1 THEN 'Essential' ELSE 'Discretionary' END AS expense_type,
    ROUND(SUM(t.amount), 2)          AS total_spent,
    ROUND(SUM(t.amount) * 100.0 /
        (SELECT SUM(amount) FROM transactions), 1)            AS pct_of_total
FROM transactions t
JOIN categories c ON t.category_id = c.id
GROUP BY expense_type;

-- ── 8. BUDGET vs ACTUAL (requires budget table populated) ────
SELECT
    c.name                           AS category,
    b.budget_amt                     AS budgeted,
    ROUND(SUM(t.amount), 2)          AS actual_spent,
    ROUND(SUM(t.amount) - b.budget_amt, 2) AS variance,
    CASE
        WHEN SUM(t.amount) <= b.budget_amt THEN '✅ Under'
        ELSE '⚠️ Over'
    END                              AS status
FROM budgets b
JOIN categories c  ON b.category_id = c.id
LEFT JOIN transactions t ON t.category_id = c.id
    AND TO_CHAR(t.date, 'YYYY-MM') = b.month
WHERE b.month = '2024-06'  -- Change to the month you want to analyse
GROUP BY c.name, b.budget_amt
ORDER BY variance DESC;

-- ── 9. COFFEE SHOP DEEP DIVE (example merchant drill-down) ──
SELECT
    date,
    description,
    ROUND(amount, 2) AS amount
FROM transactions
WHERE description ILIKE '%CAFE%'
   OR description ILIKE '%DELTA%'
   OR description ILIKE '%STARBUCKS%'
   OR description ILIKE '%COFFEE%'
ORDER BY date;

-- ── 10. SPENDING VELOCITY (7-DAY ROLLING AVERAGE) ───────────
SELECT
    date,
    ROUND(amount, 2)                 AS daily_spend,
    ROUND(AVG(amount) OVER (
        ORDER BY date
        ROWS BETWEEN 6 PRECEDING AND CURRENT ROW
    ), 2)                            AS rolling_7d_avg
FROM (
    SELECT date, SUM(amount) AS amount
    FROM transactions
    GROUP BY date
) daily
ORDER BY date;
