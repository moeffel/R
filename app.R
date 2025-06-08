###############################################################################
#  Football-Scouting Dashboard                                               ##
#  ------------------------------------------------------------------------- ##

##### 1 · Libraries ###########################################################
# Each library (package) extends R with specific abilities.
# If you see “Error: package ‘XYZ’ not found”, install once via:
# > install.packages("XYZ")

library(shiny)       # Build interactive web applications in R
library(tidyverse)   # Meta-package: dplyr, tidyr, readr, ggplot2, …
library(readr)       # read_csv() — fast, explicit CSV import
library(plotly)      # Convert static ggplots OR build directly as HTML widgets
library(DT)          # DataTables JS → interactive <table>
library(bslib)       # Bootstrap-5 theming utilities + value_box helper
library(bsicons)     # Free Bootstrap SVG icons (graph-up, award-fill, …)
library(sass)        # Compile SCSS (Sassy CSS) into plain CSS on the fly


##### 2 · Helper functions ####################################################
# Small, reusable utilities keep the body of the server() nice and tidy.

# -- parse_euro --------------------------------------------------------------
# Convert “12.345,67” (comma decimal, dot thousand) → 12345.67 (numeric).
# readr::parse_number() understands many locales when told the separators.
parse_euro <- function(x) {
  parse_number(
    x,
    locale = locale(grouping_mark = ".", decimal_mark = ",")
  )
}

# -- fmt_euro ----------------------------------------------------------------
# Friendly wrapper to format a single numeric as “€12.345”.
# Handles NA, ±Inf, or even non-numeric input gracefully → returns "N/A".
fmt_euro <- function(x) {
  n <- suppressWarnings(as.numeric(x))  # coerce; suppress “NAs introduced” msg
  ifelse(
    is.na(n) | !is.finite(n),           # TRUE/FALSE vector
    "N/A",
    paste0(
      "€",
      format(
        round(n, 0),                    # no cents
        big.mark = ".",                 # thousands separator
        decimal.mark = ",",             # (not used after round)
        scientific = FALSE
      )
    )
  )
}

# -- make_vb -----------------------------------------------------------------
# Sugar-coat bslib::value_box(): pass a title, value, icon-name, and colour
# and you get a Bootstrap-flavoured “KPI card”.
make_vb <- function(title, value, icon, theme) {
  bslib::value_box(
    title    = tags$small(title),                     # micro-title
    value    = value,                                # literal string
    showcase = bsicons::bs_icon(icon, size = "1.4em"),  # left-hand icon
    theme    = theme,                                # "primary" / "info" / …
    class    = "mb-0"                                # remove bottom margin
  )
}

##### 3 · Data preparation (incl. Age Corridor) ###############################
# All heavy lifting² (I/O + modelling) happens exactly once on startup.
prep_data <- function() {
  
  ## 3.1 – load raw CSVs -----------------------------------------------------
  # col_types = "c" → treat *every* column as character; we control parsing.
  players    <- read_csv("players.csv",           col_types = cols(.default = "c"))
  transfers  <- read_csv("transfers.csv",         col_types = cols(.default = "c"))
  valuations <- read_csv("player_valuations.csv", col_types = cols(.default = "c"))
  
  ## 3.2 – clean numeric & date columns --------------------------------------
  # Convert text numbers to numeric (transfer fees, market values)
  transfers  <- transfers  %>%
    mutate(transfer_fee        = parse_euro(transfer_fee))
  
  valuations <- valuations %>%
    mutate(market_value_in_eur = parse_euro(market_value_in_eur))
  
  # Helper that tries multiple date formats (TM uses mixed “YYYY-MM-DD” vs
  # “DD.MM.YYYY”)
  to_date <- function(x) as.Date(x, tryFormats = c("%Y-%m-%d", "%d.%m.%Y"))
  
  # apply to every relevant column
  players   <- players   %>%
    mutate(date_of_birth            = to_date(date_of_birth),
           contract_expiration_date = to_date(contract_expiration_date))
  
  transfers  <- transfers  %>%
    mutate(transfer_date = to_date(transfer_date))
  
  valuations <- valuations %>%
    mutate(date         = to_date(date))
  
  ## 3.3 – last market value and last transfer fee ---------------------------
  # For each player_id keep the *latest* valuation / transfer row
  last_val <- valuations %>%
    group_by(player_id) %>%
    slice_max(order_by = date, n = 1, with_ties = FALSE) %>%   # fastest way
    ungroup() %>%
    select(player_id, market_value = market_value_in_eur)
  
  last_fee <- transfers  %>%
    group_by(player_id) %>%
    slice_max(order_by = transfer_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(player_id, transfer_fee)
  
  ## 3.4 – working dataframe --------------------------------------------------
  # Start with players.csv, join the two “last_*” tables, compute ages, …
  df <- players %>%
    select(player_id,
           name,
           position,
           date_of_birth,
           contract_expiration_date) %>%
    
    # Compute numeric age & contract years in *decimal* form
    mutate(
      age = as.numeric(difftime(Sys.Date(),
                                date_of_birth,
                                units = "days")) / 365.25,
      contract_remaining_years =
        as.numeric(difftime(contract_expiration_date,
                            Sys.Date(),
                            units = "days")) / 365.25
    ) %>%
    
    left_join(last_val, by = "player_id") %>%
    left_join(last_fee, by = "player_id")
  
  ## 3.5 – base FMV model (log-linear) ---------------------------------------
  # Build a very naïve Ordinary-Least-Squares: log(MV) ~ age + position + …
  df <- df %>%
    mutate(
      log_market_value = ifelse(!is.na(market_value) & market_value > 0,
                                log1p(market_value), NA_real_)
    )
  
  fit <- NULL
  sufficient_rows      <- nrow(df %>% filter(!is.na(log_market_value))) > 10
  at_least_two_levels  <- n_distinct(df$position, na.rm = TRUE) > 1
  
  if (sufficient_rows && at_least_two_levels) {
    fit <- lm(
      log_market_value ~ age + position + contract_remaining_years,
      data      = df,
      na.action = na.exclude   # keeps row count stable
    )
  }
  
  # Predict back on the FULL df; fall back to +10 %
  df$fair_market_value_model <- if (!is.null(fit)) {
    
    # Make sure factor levels in newdata match the training model
    newdata <- df %>%
      mutate(position = factor(position,
                               levels = fit$xlevels$position))
    
    expm1(predict(fit, newdata = newdata, na.action = na.pass))
    
  } else {
    df$market_value * 1.10   # emergency fallback, rarely hit
  }
  
  ## 3.6 – age-dependent corridor (upper / lower bounds) ---------------------
  # Business rule from your spec (“≤20 y → 0.5 × … 2.5 × TMV” etc.)
  df <- df %>%
    mutate(
      upper = case_when(age <= 20 ~ 2.5,
                        age <= 24 ~ 2.0,
                        age <= 28 ~ 1.6,
                        age <= 32 ~ 1.3,
                        TRUE      ~ 1.1) * market_value,
      lower = case_when(age <= 20 ~ 0.50,
                        age <= 24 ~ 0.60,
                        age <= 28 ~ 0.70,
                        age <= 32 ~ 0.80,
                        TRUE      ~ 0.90) * market_value,
      # Clamp: max(lower, min(model, upper))
      fair_market_value_model =
        ifelse(!is.na(market_value),
               pmin(pmax(fair_market_value_model, lower), upper),
               fair_market_value_model)
    ) %>%
    
    # 3.7 – quick scouting metrics ------------------------------------------
  mutate(
    mv_to_fmv_ratio  = market_value / fair_market_value_model,
    opportunity_score = round(((20 - age) / 10) + (1 - mv_to_fmv_ratio), 2),
    opportunity_flag  = case_when(
      opportunity_score >= 2 ~ "High Priority Target",
      opportunity_score >= 1 ~ "Notable Prospect",
      TRUE                   ~ "Standard Profile"
    )
  )
  
  df   # return value of prep_data()
}

# Create once; every session re-uses the same tibble in RAM
master_data <- prep_data()

##### 4 · Theme + custom SCSS #################################################
theme <- bs_theme(version = 5, bootswatch = "pulse")   # “pulse” palette

# Optionally compile your own SCSS (wow-effect gradient, club badges, …)
scss_path <- "www/soccer.scss"
if (file.exists(scss_path)) {
  theme <- bs_add_rules(theme, sass::sass_file(scss_path))
} else {
  warning("Custom SCSS 'www/soccer.scss' not found – using plain Bootstrap.")
}

##### 5 · UI ##################################################################
# Distinct positions = checkboxes; same for opportunity flags
pos_choices  <- sort(unique(master_data$position))
flag_choices <- c("High Priority Target", "Notable Prospect", "Standard Profile")

ui <- page_sidebar(
  title = "Football Scouting Dashboard",
  theme = theme,
  
  # ---------- Sidebar: all user inputs live here ----------------------------
  sidebar = sidebar(
    tags$h6("Filter"),
    
    # Age: dual mechanism (slider + numeric) for precision -------------------
    sliderInput("age_slide", NULL,
                min   = floor(min(master_data$age, na.rm = TRUE)),
                max   = ceiling(max(master_data$age, na.rm = TRUE)),
                value = range(master_data$age, na.rm = TRUE)),
    numericInput("age_min", "Min age",
                 floor(min(master_data$age, na.rm = TRUE))),
    numericInput("age_max", "Max age",
                 ceiling(max(master_data$age, na.rm = TRUE))),
    
    # Paid transfer fee ------------------------------------------------------
    sliderInput("fee_slide", NULL,
                min   = floor(min(master_data$transfer_fee, na.rm = TRUE)),
                max   = ceiling(max(master_data$transfer_fee, na.rm = TRUE)),
                value = range(master_data$transfer_fee, na.rm = TRUE),
                step = 1e6, pre = "€", sep = "."),
    numericInput("fee_min", "Min fee (€)",
                 floor(min(master_data$transfer_fee, na.rm = TRUE))),
    numericInput("fee_max", "Max fee (€)",
                 ceiling(max(master_data$transfer_fee, na.rm = TRUE))),
    
    # Position checkboxes + “select all” toggle ------------------------------
    div(
      checkboxGroupInput("pos", "Positions",
                         choices  = pos_choices,
                         selected = pos_choices),
      actionLink("pos_all", "Select / unselect all", class = "small")
    ),
    
    # Scouting flag checkboxes ----------------------------------------------
    div(
      checkboxGroupInput("flag", "Scouting flag",
                         choices  = flag_choices,
                         selected = flag_choices),
      actionLink("flag_all", "Select / unselect all", class = "small")
    ),
    
    # Reset → restore *all* inputs to default values -------------------------
    actionButton("reset", "Reset filters", class = "btn-primary mt-2")
  ),
  
  # ---------- Main column layout -------------------------------------------
  layout_columns(
    col_widths = 12,   # one big column
    
    # KPI card (player header + three value boxes) ---------------------------
    card(
      card_body(
        uiOutput("player_header"),
        layout_columns(
          uiOutput("vb_fmv"),
          uiOutput("vb_mv"),
          uiOutput("vb_opp")
        )
      )
    ),
    
    # Plot & table inside tabset --------------------------------------------
    navset_card_tab(
      nav_panel("Plot",  plotlyOutput("bubble")),
      nav_panel("Table", DTOutput("tbl"))
    )
  )
)

##### 6 · Server ##############################################################
server <- function(input, output, session) {
  
  ## 6.1 – “select/unselect all” logic for checkbox groups -------------------
  observeEvent(input$pos_all, {
    # If everything is selected, unselect all → else select all
    updateCheckboxGroupInput(
      session, "pos",
      selected = if (length(input$pos) == length(pos_choices))
        character(0) else pos_choices
    )
  })
  observeEvent(input$flag_all, {
    updateCheckboxGroupInput(
      session, "flag",
      selected = if (length(input$flag) == length(flag_choices))
        character(0) else flag_choices
    )
  })
  
  ## 6.2 – keep sliders & numeric inputs synchronised ------------------------
  sync <- function(slider, num_min, num_max) {
    # When user drags slider → update numeric boxes
    observeEvent(input[[slider]], {
      updateNumericInput(session, num_min, value = input[[slider]][1])
      updateNumericInput(session, num_max, value = input[[slider]][2])
    }, ignoreInit = TRUE)
    # When user types numbers → move slider
    observeEvent(c(input[[num_min]], input[[num_max]]), {
      updateSliderInput(session, slider,
                        value = c(input[[num_min]], input[[num_max]]))
    }, ignoreInit = TRUE)
  }
  sync("age_slide", "age_min", "age_max")
  sync("fee_slide", "fee_min", "fee_max")
  
  ## 6.3 – Reset button resets *all* inputs ----------------------------------
  observeEvent(input$reset, {
    updateSliderInput(session, "age_slide",
                      value = range(master_data$age, na.rm = TRUE))
    updateSliderInput(session, "fee_slide",
                      value = range(master_data$transfer_fee, na.rm = TRUE))
    updateCheckboxGroupInput(session, "pos",  selected = pos_choices)
    updateCheckboxGroupInput(session, "flag", selected = flag_choices)
  })
  
  ## 6.4 – Reactive subset of rows based on UI filters -----------------------
  # *reactive()* re-runs automatically whenever an input it touches changes.
  filt <- reactive({
    d <- master_data %>%
      filter(between(age, input$age_slide[1],     input$age_slide[2]),
             between(transfer_fee,
                     input$fee_slide[1],
                     input$fee_slide[2]))
    if (length(input$pos))  d <- d %>% filter(position        %in% input$pos)
    if (length(input$flag)) d <- d %>% filter(opportunity_flag %in% input$flag)
    d
  })
  
  ## 6.5 – Which player is currently “active”? -------------------------------
  sel_id <- reactiveVal(master_data$player_id[1])      # init to first row
  
  # When user clicks a bubble …
  observeEvent(event_data("plotly_click", source = "src"), {
    sel_id(event_data("plotly_click", source = "src")$customdata)
  })
  # … or clicks a row in the DT table
  observeEvent(input$tbl_rows_selected, {
    sel_id(filt()$player_id[input$tbl_rows_selected])
  })
  # Convenience reactive: current player row
  player <- reactive(filt() %>% filter(player_id == sel_id()))
  
  ## 6.6 – Value boxes -------------------------------------------------------
  output$vb_fmv <- renderUI({
    pd <- player(); if (nrow(pd) == 0) return(NULL)
    make_vb("Fair MV", fmt_euro(pd$fair_market_value_model),
            "graph-up-arrow", "primary")
  })
  output$vb_mv <- renderUI({
    pd <- player(); if (nrow(pd) == 0) return(NULL)
    make_vb("TM value", fmt_euro(pd$market_value),
            "tag-fill", "info")
  })
  output$vb_opp <- renderUI({
    pd <- player(); if (nrow(pd) == 0) return(NULL)
    icon  <- ifelse(pd$opportunity_flag == "High Priority Target",
                    "award-fill", "binoculars-fill")
    theme <- ifelse(pd$opportunity_flag == "High Priority Target",
                    "success", "secondary")
    make_vb("Opportunity", pd$opportunity_score, icon, theme)
  })
  
  ## 6.7 – Player header with coloured progress bar --------------------------
  output$player_header <- renderUI({
    pd <- player(); if (nrow(pd) == 0) return(h5("Select a player…"))
    
    vals <- c(pd$market_value,
              pd$fair_market_value_model,
              pd$transfer_fee)
    vals[is.na(vals)] <- 0
    
    pct <- if (max(vals) == 0) c(0, 0, 0)
    else round(100 * vals / max(vals), 1)
    
    # Return *HTML tags* (not text) via tagList()
    tagList(
      h5(pd$name,
         tags$small(sprintf("(%.1f yrs, %s)", pd$age, pd$position),
                    class = "text-muted")),
      div(class = "progress", style = "height:4px;",
          div(class = "progress-bar bg-info",    style = paste0("width:", pct[1], "%")),
          div(class = "progress-bar bg-primary", style = paste0("width:", pct[2], "%")),
          div(class = "progress-bar bg-warning", style = paste0("width:", pct[3], "%")))
    )
  })
  
  ## 6.8 – Bubble plot -------------------------------------------------------
  output$bubble <- renderPlotly({
    
    # Base scatter (one trace per position automatically generated)
    p <- plot_ly(
      filt(),
      x     = ~age,
      y     = ~transfer_fee,
      type  = "scatter",
      mode  = "markers",
      size  = ~market_value,            # bubble area
      sizes = c(8, 40),                 # px
      color = ~position,                # map to distinct colours
      customdata = ~player_id,          # invisible field to identify point
      source     = "src",               # channel for event_data()
      text  = ~paste0("<b>", name, "</b>",
                      "<br>Fair MV: ", fmt_euro(fair_market_value_model),
                      "<br>TM value: ", fmt_euro(market_value)),
      hoverinfo = "text"
    )
    
    # Add a **single** border spec → avoids “line.width” warning spam
    p <- style(
      p,
      marker.line.width = 0.5,
      marker.line.color = "rgba(0,0,0,0.5)"
    )
    
    p %>% event_register("plotly_click") %>%   # make points clickable
      layout(
        xaxis = list(title = "Age"),
        yaxis = list(title = "Transfer Fee (€)")
      )
  })
  
  ## 6.9 – Data table (DT) ---------------------------------------------------
  output$tbl <- renderDT({
    
    dat <- filt() %>% select(
      Name          = name,
      Age           = age,
      Pos           = position,
      `MV (€)`      = market_value,
      `Fair MV (€)` = fair_market_value_model,
      `MV/FMV`      = mv_to_fmv_ratio,
      `Fee (€)`     = transfer_fee,
      `Opp.Score`   = opportunity_score,
      Flag          = opportunity_flag
    )
    
    datatable(
      dat,
      selection = "single",     # click → highlight → send row index
      rownames  = FALSE,
      options   = list(
        pageLength  = 10,
        columnDefs  = list(list(className = "dt-center", targets = "_all"))
      )
    ) %>%
      formatCurrency(
        c("MV (€)", "Fair MV (€)", "Fee (€)"),
        "€", interval = 3, mark = ".", digits = 0
      ) %>%
      formatPercentage("MV/FMV", 0)
  })
}

##### 7 · Launch the app ######################################################
# *This* is the only function call actually executed when you “Run App”.
shinyApp(ui, server)

