###############################################################################
#  Football-Scouting Dashboard (v6 — Final Corrected Code)                  #
###############################################################################
#  ‣ This is the complete, single-file application code.                      #
#  ‣ Fee scaling kept, but 'k' suffix removed from UI for clarity.            #
#  ‣ Hover-over tooltip in the plot now shows transfer fee and opp. score.    #
#  ‣ An "Reset All" button has been added to the sidebar.                     #
###############################################################################


##### 1 · Libraries ###########################################################
# -----------------------------------------------------------------------------
# Load all necessary packages for the application.
# -----------------------------------------------------------------------------
library(shiny)        ; library(tidyverse) ; library(readr)
library(lubridate)    ; library(plotly)    ; library(DT)
library(bslib)        ; library(bsicons)   ; library(sass)


##### 2 · Helper functions ####################################################
# -----------------------------------------------------------------------------
# Custom functions to format numbers and create UI components.
# -----------------------------------------------------------------------------

# Parses European-formatted numbers (e.g., "1.000,50") into numeric values.
parse_euro <- function(x) parse_number(x, locale = locale(grouping_mark=".", decimal_mark=","))

# Formats numeric values into a European currency string (e.g., "€1.000").
fmt_euro   <- function(x){
  n <- suppressWarnings(as.numeric(x))
  ifelse(is.na(n) | !is.finite(n), "N/A",
         paste0("€", format(round(n,0), big.mark=".", decimal.mark=",", scientific=FALSE)))
}

# Creates a bslib value box, a UI component for displaying key metrics.
make_vb <- function(title, value, icon, theme){
  bslib::value_box(tags$small(title), value,
                   showcase = bsicons::bs_icon(icon, size="1.4em"),
                   theme    = theme,
                   class    = "mb-0")
}

# Infix operator for providing a default value if an object is NULL.
`%||%` <- function(a,b) if (is.null(a)) b else a


##### 3 · Data preparation ####################################################
# -----------------------------------------------------------------------------
# A function to load, clean, and pre-process all required data.
# -----------------------------------------------------------------------------
prep_data <- function(){
  ## 3.1 · Read raw CSVs ------------------------------------------------------
  players      <- read_csv("players.csv"          , col_types = cols(.default="c"))
  transfers    <- read_csv("transfers.csv"        , col_types = cols(.default="c"))
  valuations   <- read_csv("player_valuations.csv", col_types = cols(.default="c"))
  appearances  <- read_csv("appearances.csv"      , col_types = cols(.default="c"))
  competitions <- read_csv("competitions.csv"     , col_types = cols(.default="c"))
  
  ## 3.2 · Clean numerics & dates --------------------------------------------
  transfers  <- transfers  %>% mutate(transfer_fee        = parse_euro(transfer_fee))
  valuations <- valuations %>% mutate(market_value_in_eur = parse_euro(market_value_in_eur))
  
  to_date <- function(x) as.Date(x, tryFormats = c("%Y-%m-%d","%d.%m.%Y"))
  players      <- players      %>% mutate(date_of_birth = to_date(date_of_birth),
                                          contract_expiration_date = to_date(contract_expiration_date))
  transfers    <- transfers    %>% mutate(transfer_date = to_date(transfer_date))
  valuations   <- valuations   %>% mutate(date          = to_date(date))
  appearances  <- appearances  %>% mutate(match_date    = to_date(date))
  
  ## 3.3 · Latest MV & fee ----------------------------------------------------
  last_val <- valuations %>% group_by(player_id) %>% slice_max(date, n=1, with_ties=FALSE) %>%
    ungroup() %>% transmute(player_id, market_value = market_value_in_eur)
  
  # Transfergebühr durch 1000 teilen, um sie in Tausendern darzustellen
  last_fee <- transfers  %>% group_by(player_id) %>% slice_max(transfer_date,n=1,with_ties=FALSE) %>%
    ungroup() %>% transmute(player_id, transfer_fee = transfer_fee / 1000)
  
  ## 3.4 · One-year performance snapshot -------------------------------------
  perf <- appearances %>%
    filter(!is.na(match_date) & match_date >= Sys.Date()-365) %>%
    mutate(across(c(minutes_played,goals,assists), as.numeric)) %>%
    left_join(competitions %>% distinct(competition_id, league_name=name),
              by="competition_id") %>%
    group_by(player_id) %>%
    summarise(season_minutes = sum(minutes_played,na.rm=TRUE),
              season_goals   = sum(goals ,na.rm=TRUE),
              season_assists = sum(assists,na.rm=TRUE),
              league_main    = names(sort(table(league_name),decreasing=TRUE))[1],
              .groups="drop")
  
  ## 3.4.1 · Simple league-strength coefficients -----------------------------
  league_strength <- tribble(
    ~league_main,           ~coef,
    "Premier League",       1.00,
    "La Liga",              1.00,
    "Bundesliga",           0.95,
    "Serie A",              0.95,
    "Ligue 1",              0.90,
    "Eredivisie",           0.85,
    "Liga Portugal",        0.85,
    "MLS",                  0.75,
    "Belgian Pro League",   0.75,
    "Other",                0.60)
  
  perf <- perf %>%
    mutate(league_main = replace_na(league_main,"Other")) %>%
    left_join(league_strength, by="league_main") %>%
    mutate(league_coef = replace_na(coef,0.60),
           prod_per90  = ifelse(season_minutes > 0,
                                (season_goals + season_assists)/(season_minutes/90),
                                NA_real_)) %>%
    select(-coef)
  
  ## 3.5 · Assemble master dataframe -----------------------------------------
  df <- players %>%
    transmute(player_id,name,position,date_of_birth,contract_expiration_date) %>%
    mutate(age = as.numeric(difftime(Sys.Date(),date_of_birth,units="days"))/365.25,
           contract_remaining_years =
             as.numeric(difftime(contract_expiration_date,Sys.Date(),units="days"))/365.25) %>%
    left_join(last_val, by="player_id") %>%
    left_join(last_fee, by="player_id") %>%
    left_join(perf    , by="player_id")
  
  ## 3.6 · FMV model (log-linear) --------------------------------------------
  df <- df %>% mutate(log_market_value = ifelse(market_value > 0, log1p(market_value), NA_real_))
  model_fmv <- NULL
  if(nrow(df %>% filter(!is.na(log_market_value))) > 30){
    model_fmv <- lm(log_market_value ~ age + position + contract_remaining_years +
                      I(season_minutes/1000) + prod_per90*league_coef,
                    data=df, na.action=na.exclude)
  }
  df$fair_market_value_model <- if(!is.null(model_fmv)){
    predict_df <- df %>% mutate(position = factor(position, levels=model_fmv$xlevels$position))
    expm1(predict(model_fmv, newdata=predict_df, na.action=na.pass))
  } else NA_real_
  
  ## 3.7 · Age corridor & opportunity metric ---------------------------------
  df <- df %>%
    mutate(
      upper = case_when(age<=20~2.5, age<=24~2.0, age<=28~1.6, age<=32~1.3, TRUE~1.1) * market_value,
      lower = case_when(age<=20~0.50,age<=24~0.60,age<=28~0.70,age<=32~0.80,TRUE~0.90) * market_value,
      fair_market_value_model = pmin(pmax(fair_market_value_model,lower),upper),
      fair_market_value_model = ifelse(is.na(fair_market_value_model), market_value*1.10, fair_market_value_model),
      mv_to_fmv_ratio = market_value / fair_market_value_model,
      
      minutes_sc = case_when(season_minutes >=2500 ~1,
                             season_minutes >=1500 ~0.6,
                             season_minutes >= 500 ~0.3,
                             TRUE ~0),
      prod_sc    = case_when(prod_per90 >=0.6 ~1,
                             prod_per90 >=0.3 ~0.6,
                             prod_per90 >  0  ~0.3,
                             TRUE ~0),
      
      opportunity_score = round(
        0.45*(1 - mv_to_fmv_ratio) +  # cheaper than FMV
          0.25*(30 - age)/30      +  # younger is better
          0.15*minutes_sc         +  # reliable minutes
          0.15*prod_sc, 2),
      
      opportunity_flag = if_else(
        is.na(opportunity_score), 
        NA_character_,
        case_when(
          opportunity_score < 0     ~ "overvalued",
          opportunity_score < 0.40  ~ "fair valued",
          TRUE                      ~ "undervalued"
        )
      )
    )
  df
}

# Execute the data preparation function to create the main dataframe.
master_data <- prep_data()


##### 4 · Theme ###############################################################
# -----------------------------------------------------------------------------
# Define the visual theme for the application using bslib and SASS.
# -----------------------------------------------------------------------------
theme <- bs_theme(version=5, bootswatch="pulse")
if(file.exists("www/soccer.scss"))
  theme <- bs_add_rules(theme, sass::sass_file("www/soccer.scss"))


##### 5 · UI ##################################################################
# -----------------------------------------------------------------------------
# Define the User Interface (UI) of the application.
# -----------------------------------------------------------------------------

# Pre-calculate ranges for sliders to avoid issues with NA values.
min_age <- floor(min(master_data$age, na.rm=TRUE))
max_age <- ceiling(max(master_data$age, na.rm=TRUE))
# Werte für Gebühren sind nun in Tausendern
min_fee <- 0
max_fee <- ceiling(max(master_data$transfer_fee, na.rm=TRUE))
pos_choices  <- sort(unique(master_data$position))
flag_choices <- c("undervalued","fair valued","overvalued")

ui <- page_sidebar(
  title = "Football Scouting Dashboard",
  theme = theme,
  sidebar = sidebar(
    tags$h6("Filter"),
    
    ## Age slider + boxes -----------------------------------------------------
    sliderInput("age_slide", NULL, min=min_age, max=max_age, value=c(min_age, max_age)),
    numericInput("age_min", "Min age", min_age, width="49%"),
    numericInput("age_max", "Max age", max_age, width="49%"),
    
    ## Fee slider + boxes -----------------------------------------------------
    sliderInput("fee_slide", NULL, min=min_fee, max=max_fee, value=c(min_fee, max_fee),
                step=1000, pre="€", sep="."),
    numericInput("fee_min", "Min fee (€)", min_fee, width="49%"),
    numericInput("fee_max", "Max fee (€)", max_fee, width="49%"),
    
    ## Position check-boxes ---------------------------------------------------
    div(
      checkboxGroupInput("pos", "Positions", choices=pos_choices, selected=pos_choices),
      actionLink("pos_all", "Select / unselect all", class="small d-block text-end")
    ),
    
    ## Valuation flag check-boxes --------------------------------------------
    div(
      checkboxGroupInput("flag", "Valuation", choices=flag_choices, selected=flag_choices),
      actionLink("flag_all", "Select / unselect all", class="small d-block text-end")
    ),
    
    # KORREKTUR: Reset-Button hinzugefügt
    hr(),
    actionButton("reset_all", "Reset All Filters", 
                 icon = icon("refresh"), class = "btn-primary w-100")
    
  ),
  layout_columns(
    col_widths = 12,
    card(card_body(uiOutput("player_header"),
                   layout_columns(uiOutput("vb_fmv"), uiOutput("vb_mv"), uiOutput("vb_opp")))),
    navset_card_tab(
      nav_panel("Plot" , plotlyOutput("bubble")),
      nav_panel("Table", DTOutput("tbl"))
    )
  )
)


##### 6 · Server ##############################################################
# -----------------------------------------------------------------------------
# Define the server-side logic for the application.
# -----------------------------------------------------------------------------
server <- function(input, output, session){
  
  ## 6.1 · Select / unselect all & Reset -------------------------------------
  observeEvent(input$pos_all ,{
    updateCheckboxGroupInput(session, "pos",
                             selected = if(length(input$pos) == length(pos_choices)) character(0) else pos_choices)
  })
  observeEvent(input$flag_all,{
    updateCheckboxGroupInput(session, "flag",
                             selected = if(length(input$flag) == length(flag_choices)) character(0) else flag_choices)
  })
  
  # KORREKTUR: Logik für den Reset-Button
  observeEvent(input$reset_all, {
    updateSliderInput(session, "age_slide", value = c(min_age, max_age))
    updateSliderInput(session, "fee_slide", value = c(min_fee, max_fee))
    updateCheckboxGroupInput(session, "pos", selected = pos_choices)
    updateCheckboxGroupInput(session, "flag", selected = flag_choices)
  })
  
  ## 6.2 · Keep sliders & numeric boxes in sync (ROBUST VERSION) -------------
  sync <- function(slider, minbox, maxbox, lo, hi) {
    observeEvent(input[[slider]], {
      if (!isTRUE(all.equal(isolate(input[[minbox]]), input[[slider]][1]))) {
        updateNumericInput(session, minbox, value = input[[slider]][1])
      }
      if (!isTRUE(all.equal(isolate(input[[maxbox]]), input[[slider]][2]))) {
        updateNumericInput(session, maxbox, value = input[[slider]][2])
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(c(input[[minbox]], input[[maxbox]]), {
      mn <- input[[minbox]] %||% lo
      mx <- input[[maxbox]] %||% hi
      
      mn <- max(lo, mn, na.rm = TRUE)
      mx <- min(hi, mx, na.rm = TRUE)
      
      if (mn > mx) {
        if (!isTRUE(all.equal(mn, isolate(input[[slider]][1])))) {
          mx <- mn
          updateNumericInput(session, maxbox, value = mx)
        } else {
          mn <- mx
          updateNumericInput(session, minbox, value = mn)
        }
      }
      
      new_slider_vals <- c(mn, mx)
      
      if (!isTRUE(all.equal(isolate(input[[slider]]), new_slider_vals))) {
        updateSliderInput(session, slider, value = new_slider_vals)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  }
  
  sync("age_slide", "age_min", "age_max", min_age, max_age)
  sync("fee_slide", "fee_min", "fee_max", min_fee, max_fee)
  
  ## 6.3 · Reactive filtered data (FINAL FIX) --------------------------------
  filt <- reactive({
    master_data %>%
      mutate(transfer_fee = replace_na(transfer_fee, 0)) %>%
      filter(between(age, input$age_slide[1], input$age_slide[2]),
             between(transfer_fee, input$fee_slide[1], input$fee_slide[2]),
             position %in% input$pos,
             opportunity_flag %in% input$flag | is.na(opportunity_flag)) %>%
      mutate(line_width = 0.5) 
  })
  
  ## 6.4 · Selected player tracking ------------------------------------------
  sel_id <- reactiveVal(master_data$player_id[1])
  
  observeEvent(event_data("plotly_click", source="src"),
               sel_id(event_data("plotly_click", source="src")$customdata))
  
  observeEvent(input$tbl_rows_selected, {
    sel_id(filt()$player_id[input$tbl_rows_selected])
  })
  
  player <- reactive(filt() %>% filter(player_id == sel_id()))
  
  ## 6.5 · KPI boxes ----------------------------------------------------------
  output$vb_fmv <- renderUI({
    pd <- player(); if(nrow(pd) == 0) return(NULL)
    make_vb("Fair MV", fmt_euro(pd$fair_market_value_model),
            "graph-up-arrow", "primary")
  })
  output$vb_mv <- renderUI({
    pd <- player(); if(nrow(pd) == 0) return(NULL)
    make_vb("TM value", fmt_euro(pd$market_value), "tag-fill", "info")
  })
  output$vb_opp <- renderUI({
    pd <- player(); if(nrow(pd) == 0) return(NULL)
    
    flag_val <- pd$opportunity_flag
    display_val <- if (is.na(flag_val)) "N/A" else flag_val
    
    make_vb("Valuation", display_val,
            switch(display_val, 
                   "undervalued" = "arrow-down-circle",
                   "fair valued" = "dash-circle", 
                   "overvalued"  = "arrow-up-circle",
                   "N/A"         = "question-circle"),
            switch(display_val, 
                   "undervalued" = "success",
                   "fair valued" = "info", 
                   "overvalued"  = "danger",
                   "N/A"         = "secondary"))
  })
  
  ## 6.6 · Player header ------------------------------------------------------
  output$player_header <- renderUI({
    pd <- player(); if(nrow(pd) == 0) return(h5("Select a player…"))
    vals <- c(pd$market_value, pd$fair_market_value_model, pd$transfer_fee)
    vals[is.na(vals)] <- 0
    pct <- if(max(vals) == 0) c(0,0,0) else round(100*vals/max(vals), 1)
    tagList(
      h5(pd$name, tags$small(sprintf("(%.1f yrs, %s)", pd$age, pd$position), class="text-muted")),
      div(class="progress", style="height:4px;",
          div(class="progress-bar bg-info",    style=paste0("width:", pct[1], "%")),
          div(class="progress-bar bg-primary", style=paste0("width:", pct[2], "%")),
          div(class="progress-bar bg-warning", style=paste0("width:", pct[3], "%")))
    )
  })
  
  ## 6.7 · Bubble plot (FINAL FIX) -------------------------------------------
  output$bubble <- renderPlotly({
    plot_ly(filt(), x=~age, y=~transfer_fee, type="scatter", mode="markers",
            size=~market_value, sizes=c(8,40),
            color=~position, customdata=~player_id, source="src",
            # KORREKTUR: Hover-Text um Gebühr und Score erweitert
            text=~paste0("<b>", name, "</b>",
                         "<br>TM value: ", fmt_euro(market_value),
                         "<br>Fair MV: ", fmt_euro(fair_market_value_model),
                         "<br>Transfer Fee: ", fmt_euro(transfer_fee),
                         "<br>Opp. Score: ", round(opportunity_score, 2)),
            hoverinfo="text",
            marker = list(
              line = list(
                width = ~line_width, 
                color = "rgba(0,0,0,0.5)"
              )
            )
    ) %>%
      event_register("plotly_click") %>%
      layout(xaxis=list(title="Age"), yaxis=list(title="Transfer Fee (in €)"))
  })
  
  ## 6.8 · Data table ---------------------------------------------------------
  output$tbl <- renderDT({
    dat <- filt() %>%
      select(-line_width) %>% # Remove the helper column before displaying
      mutate(season_minutes = replace_na(season_minutes, NA_real_),
             prod_per90     = replace_na(prod_per90, NA_real_)) %>%
      select(Name=name, Age=age, Pos=position,
             `MV (€)`=market_value, `Fair MV (€)`=fair_market_value_model,
             `MV/FMV`=mv_to_fmv_ratio, `Fee (€)`=transfer_fee,
             Min=season_minutes, `Prod/90`=prod_per90,
             `Opp.Score`=opportunity_score, Flag=opportunity_flag)
    datatable(dat, selection="single", rownames=FALSE,
              options=list(pageLength=10,
                           columnDefs=list(list(className="dt-center", targets="_all")))) %>%
      formatCurrency(c("MV (€)","Fair MV (€)","Fee (€)"), "€", 3, ".", 0) %>%
      formatPercentage("MV/FMV", 0) %>%
      formatRound(c("Min","Prod/90","Opp.Score"), 2)
  })
}


##### 7 · Launch the app ######################################################
# -----------------------------------------------------------------------------
# Start the Shiny application.
# -----------------------------------------------------------------------------
shinyApp(ui, server)
