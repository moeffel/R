###############################################################################
#  Football-Scouting Dashboard – FMV + Age Corridor + Custom SCSS (English)   #
###############################################################################

##### 1 · Libraries ###########################################################
library(shiny)      # Web-framework
library(tidyverse)  # Data wrangling (dplyr, tidyr, ggplot2, …)
library(readr)      # Fast CSV import
library(plotly)     # Interactive plots
library(DT)         # Interactive HTML tables
library(bslib)      # Bootstrap 5 theming + value_box()
library(bsicons)    # Bootstrap icons
library(sass)       # Compile SCSS at runtime (bs_add_rules)

##### 2 · Helper functions ####################################################
# Parse European-style numbers (e.g. "12.345,67")
parse_euro <- function(x) parse_number(
  x, locale = locale(grouping_mark = ".", decimal_mark = ","))

# Safe money formatter → returns "N/A" if x is NA / Inf / char
fmt_euro <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(n) | !is.finite(n),
    "N/A",
    paste0("€",
           format(round(n, 0),
                  big.mark = ".", decimal.mark = ",", scientific = FALSE)))
}

# Convenience wrapper for bslib value boxes
make_vb <- function(title, value, icon, theme) {
  bslib::value_box(
    title    = tags$small(title),
    value    = value,
    showcase = bsicons::bs_icon(icon, size = "1.4em"),
    theme    = theme,
    class    = "mb-0")
}

##### 3 · Data preparation (incl. Age Corridor) ###############################
prep_data <- function() {
  
  ## 3.1 – load raw CSVs -----------------------------------------------------
  players    <- read_csv("players.csv",           col_types = cols(.default = "c"))
  transfers  <- read_csv("transfers.csv",         col_types = cols(.default = "c"))
  valuations <- read_csv("player_valuations.csv", col_types = cols(.default = "c"))
  
  ## 3.2 – clean numeric & date columns --------------------------------------
  transfers  <- transfers  %>% mutate(transfer_fee        = parse_euro(transfer_fee))
  valuations <- valuations %>% mutate(market_value_in_eur = parse_euro(market_value_in_eur))
  
  to_date <- function(x) as.Date(x, tryFormats = c("%Y-%m-%d", "%d.%m.%Y"))
  
  players   <- players   %>%
    mutate(date_of_birth            = to_date(date_of_birth),
           contract_expiration_date = to_date(contract_expiration_date))
  transfers  <- transfers  %>% mutate(transfer_date = to_date(transfer_date))
  valuations <- valuations %>% mutate(date         = to_date(date))
  
  ## 3.3 – last market value and last transfer fee ---------------------------
  last_val <- valuations %>% group_by(player_id) %>%
    slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
    select(player_id, market_value = market_value_in_eur)
  
  last_fee <- transfers  %>% group_by(player_id) %>%
    slice_max(order_by = transfer_date, n = 1, with_ties = FALSE) %>%
    select(player_id, transfer_fee)
  
  ## 3.4 – working dataframe --------------------------------------------------
  df <- players %>%
    select(player_id, name, position, date_of_birth, contract_expiration_date) %>%
    mutate(
      age = as.numeric(difftime(Sys.Date(), date_of_birth, units = "days")) / 365.25,
      contract_remaining_years =
        as.numeric(difftime(contract_expiration_date, Sys.Date(), units = "days")) / 365.25
    ) %>%
    left_join(last_val, by = "player_id") %>%
    left_join(last_fee, by = "player_id")
  
  ## 3.5 – base FMV model (log-linear) ---------------------------------------
  df <- df %>% mutate(
    log_market_value = ifelse(!is.na(market_value) & market_value > 0,
                              log1p(market_value), NA_real_)
  )
  
  fit <- NULL
  # Fit only if enough rows *and* at least two positions are available
  if (nrow(df %>% filter(!is.na(log_market_value))) > 10 &&
      n_distinct(df$position, na.rm = TRUE) > 1) {
    fit <- lm(log_market_value ~ age + position + contract_remaining_years,
              data = df, na.action = na.exclude)
  }
  
  df$fair_market_value_model <- if (!is.null(fit)) {
    expm1(predict(
      fit,
      newdata = df %>%
        mutate(position = factor(position, levels = fit$xlevels$position)),
      na.action = na.pass))
  } else {
    df$market_value * 1.10   # very rare fallback
  }
  
  ## 3.6 – age-dependent corridor (upper / lower bounds) ---------------------
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
      # clamp the model output into that corridor
      fair_market_value_model =
        ifelse(!is.na(market_value),
               pmin(pmax(fair_market_value_model, lower), upper),
               fair_market_value_model)
    ) %>%
    ## 3.7 – quick scouting metrics -----------------------------------------
  mutate(
    mv_to_fmv_ratio  = market_value / fair_market_value_model,
    opportunity_score = round(((20 - age) / 10) + (1 - mv_to_fmv_ratio), 2),
    opportunity_flag  = case_when(
      opportunity_score >= 2 ~ "High Priority Target",
      opportunity_score >= 1 ~ "Notable Prospect",
      TRUE                   ~ "Standard Profile")
  )
  
  df
}

master_data <- prep_data()

##### 4 · Theme + custom SCSS (wow effect) ####################################
theme <- bs_theme(version = 5, bootswatch = "pulse")

# If you created "www/soccer.scss", compile & inject it here
scss_path <- "www/soccer.scss"
if (file.exists(scss_path)) {
  theme <- bs_add_rules(theme, sass::sass_file(scss_path))
} else {
  warning("Custom SCSS 'www/soccer.scss' not found – running with default theme.")
}

##### 5 · UI ##################################################################
pos_choices  <- sort(unique(master_data$position))
flag_choices <- c("High Priority Target", "Notable Prospect", "Standard Profile")

ui <- page_sidebar(
  title = "Football Scouting Dashboard",
  theme = theme,
  
  # ---------- sidebar with filters ------------------------------------------
  sidebar = sidebar(
    tags$h6("Filter"),
    
    # Age filter – slider + numeric boxes ------------------------------------
    sliderInput("age_slide", NULL,
                min   = floor(min(master_data$age, na.rm = TRUE)),
                max   = ceiling(max(master_data$age, na.rm = TRUE)),
                value = range(master_data$age, na.rm = TRUE)),
    numericInput("age_min", "Min age",
                 floor(min(master_data$age, na.rm = TRUE))),
    numericInput("age_max", "Max age",
                 ceiling(max(master_data$age, na.rm = TRUE))),
    
    # Fee filter -------------------------------------------------------------
    sliderInput("fee_slide", NULL,
                min   = floor(min(master_data$transfer_fee, na.rm = TRUE)),
                max   = ceiling(max(master_data$transfer_fee, na.rm = TRUE)),
                value = range(master_data$transfer_fee, na.rm = TRUE),
                step = 1e6, pre = "€", sep = "."),
    numericInput("fee_min", "Min fee (€)",
                 floor(min(master_data$transfer_fee, na.rm = TRUE))),
    numericInput("fee_max", "Max fee (€)",
                 ceiling(max(master_data$transfer_fee, na.rm = TRUE))),
    
    # Checkboxes – position ---------------------------------------------------
    div(
      checkboxGroupInput("pos", "Positions",
                         choices  = pos_choices,
                         selected = pos_choices),
      actionLink("pos_all", "Select / unselect all", class = "small")
    ),
    
    # Checkboxes – scouting flag ---------------------------------------------
    div(
      checkboxGroupInput("flag", "Scouting flag",
                         choices  = flag_choices,
                         selected = flag_choices),
      actionLink("flag_all", "Select / unselect all", class = "small")
    ),
    
    # Reset button -----------------------------------------------------------
    actionButton("reset", "Reset filters", class = "btn-primary mt-2")
  ),
  
  # ---------- main area -----------------------------------------------------
  layout_columns(
    col_widths = 12,
    
    # Player card ------------------------------------------------------------
    card(
      card_body(
        uiOutput("player_header"),
        layout_columns(uiOutput("vb_fmv"),
                       uiOutput("vb_mv"),
                       uiOutput("vb_opp"))
      )
    ),
    
    # Plot and table ---------------------------------------------------------
    navset_card_tab(
      nav_panel("Plot",  plotlyOutput("bubble")),
      nav_panel("Table", DTOutput("tbl"))
    )
  )
)

##### 6 · Server ##############################################################
server <- function(input, output, session) {
  
  ## 6.1 – select / unselect all checkboxes ----------------------------------
  observeEvent(input$pos_all, {
    updateCheckboxGroupInput(session, "pos",
                             selected = if (length(input$pos) == length(pos_choices)) character(0) else pos_choices)
  })
  observeEvent(input$flag_all, {
    updateCheckboxGroupInput(session, "flag",
                             selected = if (length(input$flag) == length(flag_choices)) character(0) else flag_choices)
  })
  
  ## 6.2 – sync sliders with numeric inputs ----------------------------------
  sync <- function(slider, num_min, num_max) {
    # Slider → numeric
    observeEvent(input[[slider]], {
      updateNumericInput(session, num_min, value = input[[slider]][1])
      updateNumericInput(session, num_max, value = input[[slider]][2])
    }, ignoreInit = TRUE)
    # Numeric → slider
    observeEvent(c(input[[num_min]], input[[num_max]]), {
      updateSliderInput(session, slider,
                        value = c(input[[num_min]], input[[num_max]]))
    }, ignoreInit = TRUE)
  }
  sync("age_slide", "age_min", "age_max")
  sync("fee_slide", "fee_min", "fee_max")
  
  ## 6.3 – reset button ------------------------------------------------------
  observeEvent(input$reset, {
    updateSliderInput(session, "age_slide",
                      value = range(master_data$age, na.rm = TRUE))
    updateSliderInput(session, "fee_slide",
                      value = range(master_data$transfer_fee, na.rm = TRUE))
    updateCheckboxGroupInput(session, "pos",  selected = pos_choices)
    updateCheckboxGroupInput(session, "flag", selected = flag_choices)
  })
  
  ## 6.4 – reactive filtered dataset ----------------------------------------
  filt <- reactive({
    d <- master_data %>%
      filter(between(age, input$age_slide[1], input$age_slide[2]),
             between(transfer_fee, input$fee_slide[1], input$fee_slide[2]))
    if (length(input$pos))  d <- d %>% filter(position        %in% input$pos)
    if (length(input$flag)) d <- d %>% filter(opportunity_flag %in% input$flag)
    d
  })
  
  ## 6.5 – keep track of selected player ID ----------------------------------
  sel_id <- reactiveVal(master_data$player_id[1])
  observeEvent(event_data("plotly_click", source = "src"),
               sel_id(event_data("plotly_click", source = "src")$customdata))
  observeEvent(input$tbl_rows_selected,
               sel_id(filt()$player_id[input$tbl_rows_selected]))
  player <- reactive(filt() %>% filter(player_id == sel_id()))
  
  ## 6.6 – value boxes -------------------------------------------------------
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
    make_vb("Opportunity", pd$opportunity_score,
            icon, theme)
  })
  
  ## 6.7 – player header progress bar ----------------------------------------
  output$player_header <- renderUI({
    pd <- player(); if (nrow(pd) == 0) return(h5("Select a player…"))
    vals <- c(pd$market_value, pd$fair_market_value_model, pd$transfer_fee)
    vals[is.na(vals)] <- 0
    pct <- if (max(vals) == 0) c(0, 0, 0) else round(100 * vals / max(vals), 1)
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
  
  ## 6.8 – bubble plot (marker border via style()) ---------------------------
  output$bubble <- renderPlotly({
    
    # Base scatter WITHOUT marker.line.* to avoid "line.width" warnings
    p <- plot_ly(
      filt(),
      x     = ~age,
      y     = ~transfer_fee,
      type  = "scatter",
      mode  = "markers",
      size  = ~market_value,
      sizes = c(8, 40),
      color = ~position,
      customdata = ~player_id,
      source     = "src",
      text = ~paste0("<b>", name, "</b>",
                     "<br>Fair MV: ", fmt_euro(fair_market_value_model),
                     "<br>TM value: ", fmt_euro(market_value)),
      hoverinfo = "text"
    )
    
    # Add ONE border spec for all traces → no warning spam
    p <- style(p,
               marker.line.width = 0.5,
               marker.line.color = "rgba(0,0,0,0.5)")
    
    p %>% event_register("plotly_click") %>%
      layout(xaxis = list(title = "Age"),
             yaxis = list(title = "Transfer Fee (€)"))
  })
  
  ## 6.9 – data table --------------------------------------------------------
  output$tbl <- renderDT({
    dat <- filt() %>% select(
      Name = name, Age = age, Pos = position,
      `MV (€)`        = market_value,
      `Fair MV (€)`   = fair_market_value_model,
      `MV/FMV`        = mv_to_fmv_ratio,
      `Fee (€)`       = transfer_fee,
      `Opp.Score`     = opportunity_score,
      Flag            = opportunity_flag)
    
    datatable(
      dat,
      selection = "single",
      rownames  = FALSE,
      options   = list(
        pageLength  = 10,
        columnDefs  = list(list(className = "dt-center", targets = "_all"))
      )
    ) %>%
      formatCurrency(c("MV (€)", "Fair MV (€)", "Fee (€)"),
                     "€", interval = 3, mark = ".", digits = 0) %>%
      formatPercentage("MV/FMV", 0)
  })
}

##### 7 · Launch the app ######################################################
shinyApp(ui, server)
