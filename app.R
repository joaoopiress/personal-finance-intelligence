# ==========================================
# 1. SETUP & LIBRARIES
# ==========================================
library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(dplyr)
library(ggplot2)
library(scales)
library(here)
library(DT)

# --- GLOBAL DESIGN CONFIG ---
dark_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    text = element_text(color = "#E2E8F0", family = "sans", size = 14),
    axis.text = element_text(color = "#94A3B8"),
    panel.grid.major = element_line(color = "#FFFFFF1A"), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# --- TREND PLOT FUNCTION ---
generate_trend_plot <- function(df_monthly) {
  df_monthly <- df_monthly[order(df_monthly$month), ]
  df_monthly$date_label <- format(as.Date(df_monthly$month), "%b %y")
  df_monthly$month_f <- factor(df_monthly$date_label, levels = df_monthly$date_label)
  df_monthly$highlight <- ifelse(df_monthly$month == max(df_monthly$month), "current", "past")
  
  ggplot(df_monthly, aes(x = month_f, y = total, group = 1)) +
    geom_col(aes(fill = highlight), width = 0.7, show.legend = FALSE) +
    geom_line(stat = "smooth", method = "loess", formula = y ~ x, 
              color = "#F87171", linetype = "dashed", linewidth = 1, alpha = 0.8) +
    scale_fill_manual(values = c("current" = "#38BDF8", "past" = "#38BDF833")) +
    scale_y_continuous(labels = label_number(prefix = "€", scale = 1e-3, suffix = "k")) +
    dark_theme + 
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 0, size = 10), panel.grid.major.x = element_blank())
}

# External Scripts
source(here("02_ml_classifier.R")) 
source(here("03_forecast.R"))

# Database Helper
get_db_conn <- function() {
  dbConnect(Postgres(), dbname = "finance", host = "localhost", port = 5432, user = "your-user", password = "your-password")
}

# ==========================================
# 2. UI
# ==========================================
ui <- bootstrapPage(
  theme = bs_theme(version = 5, preset = "darkly"),
  htmlTemplate(
    here("www", "index.html"),
    kpi_total = textOutput("kpi_total", inline = TRUE),
    kpi_avg = textOutput("kpi_avg", inline = TRUE),
    kpi_avg_delta = uiOutput("kpi_avg_delta", inline = TRUE),
    kpi_top = textOutput("kpi_top", inline = TRUE),
    forecast_ensemble_ui = uiOutput("forecast_ensemble_ui"),
    chart_categories = plotOutput("cat_plot", height = "300px"),
    chart_forecast = plotOutput("forecast_plot", height = "300px"),
    chart_donut_sidebar = plotOutput("sidebar_donut", height = "350px"),
    table_forecast = uiOutput("table_forecast"),
    table_recent = DTOutput("recent_table"),
    month_filter_ui = selectInput("month_filter", NULL, choices = c("All Months" = "all"), width = "180px"),
    form_ui = tagList(
      numericInput("amount", "Amount (€)", value = 0, min = 0, step = 0.01),
      textInput("desc", "Description", placeholder = "e.g., Starbucks..."),
      selectInput("category", "Category", choices = NULL),
      actionButton("save_btn", "Save Transaction", class = "btn-success w-100 mt-3")
    )
  )
)

# ==========================================
# 3. SERVER
# ==========================================
server <- function(input, output, session) {
  
  refresh_trigger <- reactiveVal(0)
  action_state <- reactiveValues(target_id = NULL)
  
  # --- 3.1 INITIALIZATION ---
  conn <- get_db_conn()
  db_cats <- dbGetQuery(conn, "SELECT id, name FROM categories")
  updateSelectInput(session, "category", choices = setNames(db_cats$id, db_cats$name))
  
  db_months <- dbGetQuery(conn, "SELECT DISTINCT TO_CHAR(date, 'YYYY-MM') AS m FROM transactions ORDER BY m DESC")
  if(nrow(db_months) > 0) {
    real_dates <- as.Date(paste0(db_months$m, "-01"))
    formatted_names <- gsub("\\.", "", toupper(format(real_dates, "%b-%Y")))
    month_choices <- c("All Months" = "all", setNames(db_months$m, formatted_names))
    updateSelectInput(session, "month_filter", choices = month_choices)
  }
  dbDisconnect(conn)
  
  # --- 3.2 ML CATEGORY SUGGESTION ---
  observeEvent(input$desc, {
    req(nchar(input$desc) >= 3)
    pred_cat <- predict_transaction_category(input$desc, input$amount)
    pred_id <- db_cats$id[tolower(db_cats$name) == tolower(pred_cat)]
    if(length(pred_id) > 0) updateSelectInput(session, "category", selected = pred_id)
  })
  
  # --- 3.3 CREATE (INSERT) ---
  observeEvent(input$save_btn, {
    req(input$amount > 0, input$desc != "")
    tryCatch({
      conn <- get_db_conn()
      dbExecute(conn, "INSERT INTO transactions (date, description, amount, category_id) VALUES (CURRENT_DATE, $1, $2, $3)", 
                list(input$desc, input$amount, as.integer(input$category)))
      dbDisconnect(conn)
      updateTextInput(session, "desc", value = "")
      updateNumericInput(session, "amount", value = 0)
      refresh_trigger(refresh_trigger() + 1)
    }, error = function(e) showNotification(e$message, type = "error"))
  })
  
  # --- 3.4 DELETE ---
  observeEvent(input$delete_id, {
    action_state$target_id <- input$delete_id
    showModal(modalDialog(title = "Confirm Deletion", "Delete this transaction?", footer = tagList(modalButton("Cancel"), actionButton("confirm_delete", "Delete", class = "btn-danger"))))
  })
  observeEvent(input$confirm_delete, {
    req(action_state$target_id)
    conn <- get_db_conn()
    dbExecute(conn, "DELETE FROM transactions WHERE id = $1", list(action_state$target_id))
    dbDisconnect(conn)
    removeModal()
    refresh_trigger(refresh_trigger() + 1)
  })
  
  # --- 3.5 KPI DATA REACTIVE ---
  kpi_data <- reactive({
    refresh_trigger()
    req(input$month_filter)
    conn <- get_db_conn()
    base_q <- "SELECT t.amount, c.name as category, t.date FROM transactions t JOIN categories c ON t.category_id = c.id"
    df <- if (input$month_filter != "all") dbGetQuery(conn, paste0(base_q, " WHERE TO_CHAR(t.date, 'YYYY-MM') = $1"), list(input$month_filter)) else dbGetQuery(conn, base_q)
    dbDisconnect(conn)
    return(df)
  })
  
  # --- 3.6 KPI RENDERERS ---
  output$kpi_total <- renderText({ format(sum(kpi_data()$amount), nsmall = 2, big.mark = ",") })
  output$kpi_avg <- renderText({ format(round(mean(kpi_data()$amount), 2), nsmall = 2) })
  
  output$kpi_avg_delta <- renderUI({
    refresh_trigger()
    req(input$month_filter != "all")
    conn <- get_db_conn()
    curr_date <- as.Date(paste0(input$month_filter, "-01"))
    prev_month <- format(seq(curr_date, length = 2, by = "-1 months")[2], "%Y-%m")
    prev_val <- dbGetQuery(conn, "SELECT AVG(amount) as avg FROM transactions WHERE TO_CHAR(date, 'YYYY-MM') = $1", list(prev_month))$avg
    dbDisconnect(conn)
    if (is.na(prev_val) || prev_val == 0) return(HTML("<span style='color: var(--muted);'>No prior data</span>"))
    pct <- ((mean(kpi_data()$amount) - prev_val) / prev_val) * 100
    color <- if(pct > 0) "var(--red)" else "var(--green)"
    HTML(sprintf("<span style='color: %s; font-weight: 600;'>%s %.1f%% vs last month</span>", color, if(pct>0) "↑" else "↓", abs(pct)))
  })
  
  output$kpi_top <- renderText({
    df <- kpi_data()
    if(nrow(df) == 0) return("None")
    top <- df %>% group_by(category) %>% summarize(total = sum(amount)) %>% arrange(desc(total)) %>% slice(1)
    toupper(top$category)
  })
  
  # --- 3.7 CHART RENDERERS ---
  output$cat_plot <- renderPlot({
    df <- kpi_data() %>% group_by(category) %>% summarize(total = sum(amount)) %>% arrange(desc(total)) %>% head(5)
    req(nrow(df) > 0)
    ggplot(df, aes(x = reorder(category, total), y = total)) + 
      geom_col(fill = "#38BDF8") + 
      coord_flip() + 
      labs(x = "Category", y = "Total Spent (€)") + 
      dark_theme
  }, bg = "transparent")
  
  output$sidebar_donut <- renderPlot({
    refresh_trigger()
    conn <- get_db_conn()
    df <- dbGetQuery(conn, "
      SELECT c.name as category, COALESCE(SUM(t.amount), 0) as total 
      FROM categories c
      LEFT JOIN transactions t ON t.category_id = c.id 
      GROUP BY c.name
    ")
    dbDisconnect(conn)
    req(nrow(df) > 0)
    
    df$category <- tolower(trimws(df$category))
    custom_colors <- c("rent" = "#EF4444", "groceries" = "#10B981", "travel" = "#06B6D4", "utilities" = "#0EA5E9", "shopping" = "#F59E0B", "food_drink" = "#F97316", "transport" = "#3B82F6", "home" = "#84CC16", "health" = "#EC4899", "subscriptions" = "#8B5CF6")
    df$category <- factor(df$category, levels = names(custom_colors))
    
    ggplot(df, aes(x = 1.5, y = total, fill = category)) + 
      geom_col(color = "#0F172A", linewidth = 1.5) + 
      coord_polar(theta = "y") + 
      xlim(c(0, 2.5)) + 
      scale_fill_manual(values = custom_colors, drop = FALSE) + 
      dark_theme + 
      theme(
        axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 9, color = "#94A3B8"),
        legend.spacing.x = unit(0.2, 'cm'), legend.margin = margin(t = 10)
      ) +
      guides(fill = guide_legend(ncol = 3, byrow = TRUE))
  }, bg = "transparent")
  
  # --- 3.8 FORECAST UI RENDERERS ---
  output$table_forecast <- renderUI({
    refresh_trigger()
    conn <- get_db_conn()
    df <- dbGetQuery(conn, "SELECT c.name as category, SUM(t.amount) as total, TO_CHAR(t.date, 'YYYY-MM') as month FROM transactions t JOIN categories c ON t.category_id = c.id GROUP BY category, month")
    dbDisconnect(conn)
    if(nrow(df) == 0) return(div("No data..."))
    stats <- df %>% group_by(category) %>% summarise(avg_val = mean(total), pred_val = mean(tail(total, 2))) %>% mutate(pct = ((pred_val - avg_val) / avg_val) * 100)
    
    tags$table(style="width: 100%;", tags$tbody(lapply(1:nrow(stats), function(i) {
      row <- stats[i, ]; color <- if(row$pct < -1) "#34D399" else if(row$pct > 1) "#F87171" else "#64748B"
      tags$tr(style="border-bottom: 1px solid rgba(255,255,255,0.05);", tags$td(style="padding: 12px 0;", row$category), tags$td(style=paste0("text-align: right; color: ", color), paste0("€", round(row$pred_val), if(row$pct > 1) " ↑" else " ↓")))
    })))
  })
  
  output$forecast_ensemble_ui <- renderUI({
    refresh_trigger()
    conn <- get_db_conn()
    df_monthly <- dbGetQuery(conn, "SELECT DATE_TRUNC('month', date)::DATE as month, SUM(amount) as total FROM transactions GROUP BY month ORDER BY month ASC")
    dbDisconnect(conn)
    req(nrow(df_monthly) >= 3)
    
    ts_data <- ts(df_monthly$total, frequency = 12)
    hw_f <- forecast::forecast(HoltWinters(ts_data, gamma = (nrow(df_monthly) >= 24)), h = 1)
    p_val <- round(as.numeric(hw_f$mean), 0)
    l_val <- round(as.numeric(hw_f$lower[, 2]), 0)
    u_val <- round(as.numeric(hw_f$upper[, 2]), 0)
    
    track_min <- round(l_val * 0.8, -2); track_max <- round(u_val * 1.2, -2)
    pos_pct <- min(max(((p_val - track_min) / (track_max - track_min)) * 100, 0), 100)
    next_month <- format(max(df_monthly$month) + months(1), "%B %Y")
    
    tagList(
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
          div(div(style = "font-size: 18px; font-weight: bold; color: #E2E8F0;", paste("Spending Forecast —", next_month)),
              div(style = "font-size: 11px; color: #94A3B8;", "Based on 6-month trend analysis with exponential smoothing")),
          span(style = "background: rgba(56, 189, 248, 0.1); color: #38BDF8; padding: 4px 12px; border-radius: 20px; font-size: 11px; border: 1px solid rgba(56, 189, 248, 0.2); font-weight: 600;", "● ETS + LINEAR BLEND")
      ),
      div(style = "display: grid; grid-template-columns: 1fr 1fr 1fr; text-align: center; margin-bottom: 25px;",
          div(div(style = "font-size: 10px; color: #94A3B8; margin-bottom: 5px;", "LOWER BOUND (95%)"), div(style = "font-size: 24px; font-weight: bold; color: #10B981;", paste0("€", format(l_val, big.mark = ",")))),
          div(div(style = "font-size: 10px; color: #38BDF8; font-weight: bold; margin-bottom: 5px;", "POINT FORECAST"), div(style = "font-size: 32px; font-weight: bold; color: #38BDF8;", paste0("€", format(p_val, big.mark = ",")))),
          div(div(style = "font-size: 10px; color: #94A3B8; margin-bottom: 5px;", "UPPER BOUND (95%)"), div(style = "font-size: 24px; font-weight: bold; color: #F87171;", paste0("€", format(u_val, big.mark = ","))))
      ),
      div(style = "position: relative; height: 6px; background: #334155; border-radius: 3px; margin: 0 15px;",
          div(style = paste0("position: absolute; left: 0; width: ", pos_pct, "%; height: 100%; background: #38BDF8; border-radius: 3px; opacity: 0.4;")),
          div(style = paste0("position: absolute; left: ", pos_pct, "%; top: 50%; transform: translate(-50%, -50%); width: 14px; height: 14px; background: #38BDF8; border-radius: 50%; border: 3px solid #0F172A; box-shadow: 0 0 15px rgba(56, 189, 248, 0.6);")))
    )
  })
  
  output$forecast_plot <- renderPlot({
    refresh_trigger(); conn <- get_db_conn()
    df_monthly <- dbGetQuery(conn, "SELECT DATE_TRUNC('month', date)::DATE as month, SUM(amount) as total FROM transactions GROUP BY month ORDER BY month DESC LIMIT 6")
    dbDisconnect(conn); req(nrow(df_monthly) >= 2)
    generate_trend_plot(df_monthly[order(df_monthly$month), ])
  }, bg = "transparent")
  
  # --- 3.9 DATA TABLE RENDERER ---
  output$recent_table <- renderDT({
    refresh_trigger(); req(input$month_filter); conn <- get_db_conn()
    query <- "SELECT t.id, TO_CHAR(t.date, 'YYYY-MM-DD') AS Date, t.description AS Description, t.amount AS Amount, c.name AS Category FROM transactions t JOIN categories c ON t.category_id = c.id"
    df <- if (input$month_filter != "all") dbGetQuery(conn, paste0(query, " WHERE TO_CHAR(t.date, 'YYYY-MM') = $1 ORDER BY t.date DESC LIMIT 10"), list(input$month_filter)) else dbGetQuery(conn, paste0(query, " ORDER BY t.date DESC LIMIT 10"))
    dbDisconnect(conn); req(nrow(df) > 0)
    df$Actions <- sprintf('<button class="btn btn-sm btn-outline-danger" onclick="Shiny.setInputValue(\'delete_id\', %d)">🗑️</button>', df$id)
    datatable(df, escape = FALSE, rownames = FALSE, options = list(dom = 't', columnDefs = list(list(visible = FALSE, targets = 0))))
  })
}

shinyApp(ui, server)