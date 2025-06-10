###############################################################################
#                                                                             #
#                  Football Player Scouting Dashboard (v13)                     #
#                                                                             #
###############################################################################
#                                                                             #
# Project:      Final University Presentation Version                         #
# Author:       AI Assistant (Gemini) & User Collaboration                    #
# Date:         June 2024                                                     #
#                                                                             #
# Description:  This Shiny application provides an interactive dashboard for  #
#               scouting football players. It allows users to filter players  #
#               based on various metrics and visualizes the data through an   #
#               interactive bubble chart and a data table. The dashboard      #
#               calculates a custom "Opportunity Score" to identify           #
#               potentially undervalued players.                               #
#                                                                             #
# Change Log:   v13 -> Players with no Opportunity Score are now excluded     #
#               from the bubble chart and data table entirely.                #
#                                                                             #
###############################################################################


##### 1 · Load Required Libraries #############################################
# -----------------------------------------------------------------------------
# Libraries are collections of pre-written code (like toolkits or plugins) 
# that provide special functions for our app.
# -----------------------------------------------------------------------------
library(shiny)        # The core framework for building the interactive web app.
library(tidyverse)    # A powerful collection of tools for data manipulation and cleaning (e.g., dplyr, readr).
library(readr)        # A fast and friendly way to read rectangular data like CSV files.
library(lubridate)    # Simplifies working with dates and times.
library(plotly)       # Creates beautiful, interactive graphs and charts.
library(DT)           # Provides an interactive, feature-rich data table.
library(bslib)        # Advanced tools for theming and creating modern user interfaces in Shiny.
library(bsicons)      # Provides easy access to Bootstrap icons for use in the UI.
library(sass)         # A pre-processor for CSS, used for advanced custom styling.


##### 2 · Helper Functions ####################################################
# -----------------------------------------------------------------------------
# These are custom-built functions to perform repetitive tasks, keeping the
# main code cleaner and more readable.
# -----------------------------------------------------------------------------

# Converts text that looks like European currency (e.g., "1.234,56") into a plain number.
parse_euro <- function(x) parse_number(x, locale = locale(grouping_mark=".", decimal_mark=","))

# Takes a number and formats it back into a user-friendly European currency string.
fmt_euro   <- function(x){
  n <- suppressWarnings(as.numeric(x))
  ifelse(is.na(n) | !is.finite(n), "N/A",
         paste0("€", format(round(n,0), big.mark=".", decimal.mark=",", scientific=FALSE)))
}

# Creates one of the summary "Value Boxes" at the top of the dashboard.
make_vb <- function(title, value, icon, theme){
  bslib::value_box(tags$small(title), value,
                   showcase = bsicons::bs_icon(icon, size="1.4em"),
                   theme    = theme,
                   class    = "mb-0")
}

# A custom operator to provide a default value if an object is missing (NULL).
`%||%` <- function(a,b) if (is.null(a)) b else a


##### 3 · Data Preparation ####################################################
# -----------------------------------------------------------------------------
# This function loads, cleans, merges, and engineers all the necessary 
# features for the application from the raw data files.
# -----------------------------------------------------------------------------
prep_data <- function(){
  
  ## 3.1 · Read Raw Data Files (CSVs) -----------------------------------------
  # This loads the data from the spreadsheet-like CSV files into R data frames.
  players      <- read_csv("players.csv"          , col_types = cols(.default="c"))
  transfers    <- read_csv("transfers.csv"        , col_types = cols(.default="c"))
  valuations   <- read_csv("player_valuations.csv", col_types = cols(.default="c"))
  appearances  <- read_csv("appearances.csv"      , col_types = cols(.default="c"))
  competitions <- read_csv("competitions.csv"     , col_types = cols(.default="c"))
  
  ## 3.2 · Clean Numeric and Date Columns -------------------------------------
  # Raw data is often stored as text. This step converts text for money and 
  # dates into proper number and date formats for calculations and plotting.
  transfers  <- transfers  %>% mutate(transfer_fee        = parse_euro(transfer_fee))
  valuations <- valuations %>% mutate(market_value_in_eur = parse_euro(market_value_in_eur))
  
  to_date <- function(x) as.Date(x, tryFormats = c("%Y-%m-%d","%d.%m.%Y"))
  players      <- players      %>% mutate(date_of_birth = to_date(date_of_birth),
                                          contract_expiration_date = to_date(contract_expiration_date))
  transfers    <- transfers    %>% mutate(transfer_date = to_date(transfer_date))
  valuations   <- valuations   %>% mutate(date          = to_date(date))
  appearances  <- appearances  %>% mutate(match_date    = to_date(date))
  
  ## 3.3 · Get Latest Market Value & Transfer Fee -----------------------------
  # A player can have multiple entries over time. We only want their most recent value.
  last_val <- valuations %>% group_by(player_id) %>% slice_max(date, n=1, with_ties=FALSE) %>%
    ungroup() %>% transmute(player_id, market_value = market_value_in_eur)
  
  # We scale the transfer fee by dividing by 1000 to make the numbers on the plot axes more readable.
  last_fee <- transfers  %>% group_by(player_id) %>% slice_max(transfer_date,n=1,with_ties=FALSE) %>%
    ungroup() %>% transmute(player_id, transfer_fee = transfer_fee / 1000)
  
  ## 3.4 · Create a 1-Year Performance Snapshot -------------------------------
  # Here, we calculate each player's performance stats (minutes, goals, assists) over the last 365 days.
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
  
  ## 3.4.1 · Define Simple League-Strength Coefficients -----------------------
  # This is a simple model to adjust player stats based on the strength of the league they play in.
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
  
  ## 3.5 · Assemble the Master Data Frame -------------------------------------
  # This crucial step combines all the cleaned data from different files into one main table.
  df <- players %>%
    transmute(player_id,name,position,date_of_birth,contract_expiration_date) %>%
    mutate(age = as.numeric(difftime(Sys.Date(),date_of_birth,units="days"))/365.25,
           contract_remaining_years =
             as.numeric(difftime(contract_expiration_date,Sys.Date(),units="days"))/365.25) %>%
    left_join(last_val, by="player_id") %>%
    left_join(last_fee, by="player_id") %>%
    left_join(perf    , by="player_id")
  
  ## 3.6 · Fair Market Value (FMV) Model --------------------------------------
  # This section builds a simple statistical model (linear regression) to predict a 
  # "Fair Market Value" based on player stats like age, position, and performance.
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
  
  ## 3.7 · Age Corridor & Custom "Opportunity Score" --------------------------
  # This is the core of our custom analysis. We create a score to find good transfer targets.
  df <- df %>%
    mutate(
      # Define an age-based "fair value" corridor to keep the model realistic.
      upper = case_when(age<=20~2.5, age<=24~2.0, age<=28~1.6, age<=32~1.3, TRUE~1.1) * market_value,
      lower = case_when(age<=20~0.50,age<=24~0.60,age<=28~0.70,age<=32~0.80,TRUE~0.90) * market_value,
      fair_market_value_model = pmin(pmax(fair_market_value_model,lower),upper),
      fair_market_value_model = ifelse(is.na(fair_market_value_model), market_value*1.10, fair_market_value_model),
      mv_to_fmv_ratio = market_value / fair_market_value_model,
      
      # Create simple scores for playing time and productivity.
      minutes_sc = case_when(season_minutes >=2500 ~1,
                             season_minutes >=1500 ~0.6,
                             season_minutes >= 500 ~0.3,
                             TRUE ~0),
      prod_sc    = case_when(prod_per90 >=0.6 ~1,
                             prod_per90 >=0.3 ~0.6,
                             prod_per90 >  0  ~0.3,
                             TRUE ~0),
      
      # Combine everything into a final weighted "Opportunity Score".
      opportunity_score = round(
        0.45*(1 - mv_to_fmv_ratio) +  # Value for money (cheaper than FMV is better).
          0.25*(30 - age)/30      +  # Age (younger is better).
          0.15*minutes_sc         +  # Playing time (reliable minutes are good).
          0.15*prod_sc, 2),         # Productivity (goals/assists are good).
      
      # Assign a human-readable flag based on the score.
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

# Run the data preparation function once when the app starts.
master_data <- prep_data()


##### 4 · Application Theme ###################################################
# -----------------------------------------------------------------------------
# This section defines the app's color scheme and overall visual style.
# -----------------------------------------------------------------------------
theme <- bs_theme(version=5, bootswatch="pulse")
if(file.exists("www/soccer.scss"))
  theme <- bs_add_rules(theme, sass::sass_file("www/soccer.scss"))


##### 5 · User Interface (UI) #################################################
# -----------------------------------------------------------------------------
# The UI defines the layout and appearance of the application, like an HTML file.
# It determines what the user sees and interacts with.
# -----------------------------------------------------------------------------

# Pre-calculate filter ranges to initialize the UI controls.
min_age <- floor(min(master_data$age, na.rm = TRUE))
max_age <- ceiling(max(master_data$age, na.rm = TRUE))
min_fee <- 0
max_fee <- ceiling(max(master_data$transfer_fee, na.rm = TRUE))
min_opp_score <- round(min(master_data$opportunity_score, na.rm = TRUE), 1)
max_opp_score <- round(max(master_data$opportunity_score, na.rm = TRUE), 1)
pos_choices  <- sort(unique(master_data$position))
flag_choices <- c("undervalued", "fair valued", "overvalued")

ui <- page_sidebar(
  title = "Football Player Scouting Dashboard",
  theme = theme,
  
  # The sidebar contains all the input controls for filtering the data.
  sidebar = sidebar(
    tags$h6("Filters"),
    
    # Age filter with a slider and two number boxes.
    sliderInput("age_slide", NULL, min = min_age, max = max_age, value = c(min_age, max_age)),
    numericInput("age_min", "Min Age", min_age, width = "49%"),
    numericInput("age_max", "Max Age", max_age, width = "49%"),
    
    # Transfer fee filter, also with a slider and number boxes.
    sliderInput("fee_slide", NULL, min = min_fee, max = max_fee, value = c(min_fee, max_fee),
                step = 1000, pre = "€", sep = "."),
    numericInput("fee_min", "Min Fee (€)", min_fee, width = "49%"),
    numericInput("fee_max", "Max Fee (€)", max_fee, width = "49%"),
    
    # Opportunity Score filter.
    sliderInput("opp_score_slide", "Opportunity Score", 
                min = min_opp_score, max = max_opp_score, 
                value = c(min_opp_score, max_opp_score), step = 0.1),
    numericInput("opp_score_min", "Min Score", min_opp_score, width = "49%"),
    numericInput("opp_score_max", "Max Score", max_opp_score, width = "49%"),
    
    # Checkboxes for selecting player positions.
    div(
      checkboxGroupInput("pos", "Positions", choices = pos_choices, selected = pos_choices),
      actionLink("pos_all", "Select / Deselect All", class = "small d-block text-end")
    ),
    
    # Checkboxes for selecting the valuation flag.
    div(
      checkboxGroupInput("flag", "Valuation", choices = flag_choices, selected = flag_choices),
      actionLink("flag_all", "Select / Deselect All", class = "small d-block text-end")
    ),
    
    # A button to reset all filters to their default state.
    hr(),
    actionButton("reset_all", "Reset All Filters", 
                 icon = icon("refresh"), class = "btn-primary w-100")
    
  ),
  
  # The main content area of the page.
  layout_columns(
    col_widths = 12,
    # A card to display information about the selected player.
    card(card_body(uiOutput("player_header"),
                   layout_columns(uiOutput("vb_fmv"), uiOutput("vb_mv"), uiOutput("vb_opp")))),
    # A tabbed view to switch between the plot and the data table.
    navset_card_tab(
      nav_panel("Plot" , plotlyOutput("bubble")),
      nav_panel("Table", DTOutput("tbl"))
    )
  )
)


##### 6 · Server Logic ########################################################
# -----------------------------------------------------------------------------
# The server contains the instructions that tell the app how to react to user
# input. It's the "brain" of the application.
# -----------------------------------------------------------------------------
server <- function(input, output, session){
  
  ## 6.1 · Input Control Logic (Select All & Reset) ---------------------------
  # This block handles clicks on the "Select/Deselect All" and "Reset" buttons.
  observeEvent(input$pos_all ,{
    updateCheckboxGroupInput(session, "pos",
                             selected = if(length(input$pos) == length(pos_choices)) character(0) else pos_choices)
  })
  observeEvent(input$flag_all,{
    updateCheckboxGroupInput(session, "flag",
                             selected = if(length(input$flag) == length(flag_choices)) character(0) else flag_choices)
  })
  
  # When the reset button is clicked, update all inputs back to their original values.
  observeEvent(input$reset_all, {
    updateSliderInput(session, "age_slide", value = c(min_age, max_age))
    updateSliderInput(session, "fee_slide", value = c(min_fee, max_fee))
    updateSliderInput(session, "opp_score_slide", value = c(min_opp_score, max_opp_score))
    updateCheckboxGroupInput(session, "pos", selected = pos_choices)
    updateCheckboxGroupInput(session, "flag", selected = flag_choices)
  })
  
  ## 6.2 · Sync Sliders with Numeric Inputs -----------------------------------
  # This custom function keeps the sliders and their corresponding number boxes perfectly in sync.
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
          mx <- mn; updateNumericInput(session, maxbox, value = mx)
        } else {
          mn <- mx; updateNumericInput(session, minbox, value = mn)
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
  sync("opp_score_slide", "opp_score_min", "opp_score_max", min_opp_score, max_opp_score)
  
  ## 6.3 · Reactive Filtered Data ---------------------------------------------
  # This is a core concept of Shiny. `filt()` is a special object that holds the
  # player data. It automatically re-filters this data whenever a UI filter is changed.
  filt <- reactive({
    master_data %>%
      # NEW: Explicitly remove any player that does not have an opportunity score.
      filter(!is.na(opportunity_score)) %>%
      # Then, apply the user's filters.
      mutate(transfer_fee = replace_na(transfer_fee, 0)) %>%
      filter(
        between(age, input$age_slide[1], input$age_slide[2]),
        between(transfer_fee, input$fee_slide[1], input$fee_slide[2]),
        between(opportunity_score, input$opp_score_slide[1], input$opp_score_slide[2]),
        position %in% input$pos,
        opportunity_flag %in% input$flag
      ) %>%
      # Add helper columns needed for plotting.
      mutate(
        # A constant for the bubble outline width.
        line_width = 0.5,
        # An aggressive, exponential scaling for bubble size to make high-opportunity
        # players visually stand out much more clearly.
        bubble_size = case_when(
          opportunity_score < 0.1  ~ 4, # Small, constant size for poor opportunities
          TRUE                     ~ 5 + (opportunity_score * 8)^2 # Strong exponential scaling
        )
      ) 
  })
  
  ## 6.4 · Selected Player Tracking -------------------------------------------
  # This section keeps track of which player is currently selected by the user.
  sel_id <- reactiveVal() # Start with no player selected.
  
  observeEvent(event_data("plotly_click", source="src"),
               sel_id(event_data("plotly_click", source="src")$customdata))
  
  observeEvent(input$tbl_rows_selected, {
    sel_id(filt()$player_id[input$tbl_rows_selected])
  })
  
  # A reactive object that holds only the data for the single selected player.
  player <- reactive(filt() %>% filter(player_id == sel_id()))
  
  ## 6.5 · Render the KPI Value Boxes -----------------------------------------
  # This code generates the three summary boxes at the top and ensures they
  # update when a new player is selected.
  output$vb_fmv <- renderUI({
    pd <- player(); if(nrow(pd) == 0) return(NULL)
    make_vb("Fair Market Value", fmt_euro(pd$fair_market_value_model),
            "graph-up-arrow", "primary")
  })
  output$vb_mv <- renderUI({
    pd <- player(); if(nrow(pd) == 0) return(NULL)
    make_vb("Transfermarkt Value", fmt_euro(pd$market_value), "tag-fill", "info")
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
  
  ## 6.6 · Render the Player Header -------------------------------------------
  # Creates the header with the selected player's name and a progress bar.
  output$player_header <- renderUI({
    # If no player is selected, show a prompt.
    if(is.null(sel_id()) || nrow(player()) == 0) {
      return(h5("Select a player from the plot or table..."))
    }
    
    pd <- player()
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
  
  ## 6.7 · Render the Interactive Bubble Plot ---------------------------------
  # This code block generates the main plot using the `plotly` library.
  output$bubble <- renderPlotly({
    plot_ly(
        data = filt(), 
        x = ~age, 
        y = ~transfer_fee, 
        size = ~bubble_size,  # Bubble size is mapped to our custom score.
        color = ~position,      # Bubbles are colored by player position.
        customdata = ~player_id, # Store player ID for click events.
        source = "src",         # A name for the plot source.
        # Define the text that appears when hovering over a bubble.
        text = ~paste0("<b>", name, "</b>",
                      "<br>TM Value: ", fmt_euro(market_value),
                      "<br>Fair MV: ", fmt_euro(fair_market_value_model),
                      "<br>Transfer Fee: ", fmt_euro(transfer_fee),
                      "<br>Opp. Score: ", round(opportunity_score, 2)),
        hoverinfo = "text",
        type = "scatter", 
        mode = "markers",
        marker = list(line = list(width = ~line_width, color = "rgba(0,0,0,0.5)"))
    ) %>%
      event_register("plotly_click") %>%
      layout(xaxis = list(title = "Player Age"), 
             yaxis = list(title = "Transfer Fee (in thousands €)"))
  })
  
  ## 6.8 · Render the Data Table ----------------------------------------------
  # This renders the interactive data table at the bottom of the page.
  output$tbl <- renderDT({
    dat <- filt() %>%
      select(-line_width, -bubble_size) %>% # Remove helper columns before displaying.
      mutate(season_minutes = replace_na(season_minutes, NA_real_),
             prod_per90     = replace_na(prod_per90, NA_real_)) %>%
      # Select and rename columns for a clean presentation.
      select(Name=name, Age=age, Pos=position,
             `MV (€)`=market_value, `Fair MV (€)`=fair_market_value_model,
             `MV/FMV`=mv_to_fmv_ratio, `Fee (€)`=transfer_fee,
             `Mins`=season_minutes, `Prod/90`=prod_per90,
             `Opp. Score`=opportunity_score, Flag=opportunity_flag)
    
    datatable(dat, selection="single", rownames=FALSE,
              options=list(pageLength=10,
                           columnDefs=list(list(className="dt-center", targets="_all")))) %>%
      # Apply special formatting to currency and percentage columns.
      formatCurrency(c("MV (€)","Fair MV (€)","Fee (€)"), "€", 3, ".", 0) %>%
      formatPercentage("MV/FMV", 0) %>%
      formatRound(c("Mins","Prod/90","Opp. Score"), 2)
  })
}


##### 7 · Launch the Application ##############################################
# -----------------------------------------------------------------------------
# This final line takes the UI and the Server logic and runs the application.
# -----------------------------------------------------------------------------
shinyApp(ui, server)
