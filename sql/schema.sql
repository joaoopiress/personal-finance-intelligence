-- =============================================================
-- Personal Finance Intelligence — PostgreSQL Schema
-- =============================================================

-- -------------------------------------------------------------
-- CATEGORIES
-- -------------------------------------------------------------
CREATE TABLE IF NOT EXISTS categories (
    id           SERIAL       PRIMARY KEY,
    name         TEXT         NOT NULL UNIQUE,
    description  TEXT,
    color_hex    TEXT         DEFAULT '#6B7280',
    is_essential SMALLINT     DEFAULT 0            -- 1 = essential expense
);

-- Seed categories
INSERT INTO categories (name, description, color_hex, is_essential) VALUES
    ('rent',          'Housing / rent payments',             '#EF4444', 1),
    ('groceries',     'Supermarkets and food stores',        '#22C55E', 1),
    ('food_drink',    'Restaurants, cafes, takeaway',        '#F97316', 0),
    ('transport',     'Uber, Lyft, gas, transit',            '#3B82F6', 1),
    ('subscriptions', 'Streaming, SaaS, memberships',       '#8B5CF6', 0),
    ('health',        'Pharmacy, gym, medical',              '#EC4899', 1),
    ('shopping',      'Retail, Amazon, general merchandise', '#F59E0B', 0),
    ('utilities',     'Electric, internet, phone',           '#06B6D4', 1),
    ('travel',        'Flights, hotels',                     '#14B8A6', 0),
    ('home',          'Home improvement, furniture',         '#84CC16', 0),
    ('transfers',     'Peer transfers, Venmo, Zelle',        '#94A3B8', 0),
    ('cash',          'ATM withdrawals',                     '#6B7280', 0),
    ('uncategorized', 'Needs manual review',                 '#D1D5DB', 0)
ON CONFLICT (name) DO NOTHING;

-- -------------------------------------------------------------
-- TRANSACTIONS
-- -------------------------------------------------------------
CREATE TABLE IF NOT EXISTS transactions (
    id            SERIAL          PRIMARY KEY,
    date          DATE            NOT NULL,
    description   TEXT            NOT NULL,
    amount        NUMERIC(12, 2)  NOT NULL CHECK (amount > 0),
    category_id   INTEGER         REFERENCES categories(id),
    category_raw  TEXT,                        -- original label from CSV / ML model
    predicted_cat TEXT,                        -- ML model prediction
    confidence    NUMERIC(4, 3),               -- model confidence score (0–1)
    is_recurring  SMALLINT        DEFAULT 0,   -- 1 = recurring expense
    notes         TEXT,
    created_at    TIMESTAMPTZ     DEFAULT NOW()
);

-- -------------------------------------------------------------
-- BUDGETS
-- -------------------------------------------------------------
CREATE TABLE IF NOT EXISTS budgets (
    id          SERIAL          PRIMARY KEY,
    category_id INTEGER         NOT NULL REFERENCES categories(id),
    month       CHAR(7)         NOT NULL,      -- format: YYYY-MM
    budget_amt  NUMERIC(12, 2)  NOT NULL,
    UNIQUE (category_id, month)
);

-- -------------------------------------------------------------
-- INDEXES
-- -------------------------------------------------------------
CREATE INDEX IF NOT EXISTS idx_transactions_date     ON transactions(date);
CREATE INDEX IF NOT EXISTS idx_transactions_category ON transactions(category_id);
CREATE INDEX IF NOT EXISTS idx_transactions_amount   ON transactions(amount DESC);