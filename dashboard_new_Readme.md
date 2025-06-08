# Football Scouting Dashboard: Code Explanation

This document provides a detailed explanation of the R Shiny application designed for football scouting. It focuses on data preparation, Fair Market Value (FMV) estimation, and the interactive user interface.

## 1. Libraries

The application utilizes several R packages:

*   **`shiny`**: The core framework for building interactive web applications in R.
*   **`tidyverse`**: A collection of R packages for data science, including:
    *   `dplyr`: For data manipulation (filtering, mutating, joining).
    *   `tidyr`: For tidying data.
    *   `ggplot2` (implicitly used by `plotly`): For creating static plots (though `plotly` makes them interactive).
    *   `readr`: For fast and efficient reading of CSV files.
*   **`lubridate`**: For easier manipulation of date and time objects.
*   **`plotly`**: For creating interactive charts and visualizations.
*   **`DT`**: For rendering R data frames as interactive HTML tables in the Shiny app.
*   **`bslib`**: For theming Shiny apps with Bootstrap 5 and providing UI components like `value_box()`.
*   **`bsicons`**: For easily adding Bootstrap icons to the UI.
*   **`sass`**: For compiling SCSS (Sassy CSS) files at runtime, allowing for more advanced custom styling.

```r
# ##### 1 · Libraries ###########################################################
library(shiny)      # Web-framework
library(tidyverse)  # Data wrangling (dplyr, tidyr, ggplot2, …)
library(readr)      # Fast CSV import
library(plotly)     # Interactive plots
library(DT)         # Interactive HTML tables
library(bslib)      # Bootstrap 5 theming + value_box()
library(bsicons)    # Bootstrap icons
library(sass)       # Compile SCSS at runtime (bs_add_rules)

2. Helper Functions

These are small, reusable functions to simplify common tasks:

parse_euro(x):

Takes a character string x representing a number in European format (e.g., "12.345,67").

Uses readr::parse_number() to convert it into a numeric value.

The locale argument specifies that "." is the grouping mark (for thousands) and "," is the decimal mark.

suppressWarnings() is used to prevent warnings if parsing fails for some inputs.

# Parse European-style numbers (e.g. "12.345,67")
parse_euro <- function(x) parse_number(
  x, locale = locale(grouping_mark = ".", decimal_mark = ","))
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END

fmt_euro(x):

Takes a numeric value x.

Formats it as a Euro currency string (e.g., "€12.346").

First, it tries to convert x to numeric robustly using as.numeric(x). suppressWarnings() handles cases where x might not be convertible.

If x is NA, Inf, -Inf, or cannot be converted to a finite number, it returns "N/A".

Otherwise, it rounds the number to 0 decimal places, formats it with "." as the thousands separator and "," as the decimal mark (though no decimals are shown after rounding), and prepends "€". scientific = FALSE ensures numbers aren't displayed in scientific notation.

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
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END

make_vb(title, value, icon, theme):

A convenience wrapper for creating bslib::value_box() components.

title: Text for the value box title (displayed small).

value: The main value to display.

icon: The name of a bsicons::bs_icon() to display.

theme: The color theme for the value box (e.g., "primary", "info", "success").

It standardizes the icon size and adds a CSS class mb-0 (margin-bottom: 0).

# Convenience wrapper for bslib value boxes
make_vb <- function(title, value, icon, theme) {
  bslib::value_box(
    title    = tags$small(title),
    value    = value,
    showcase = bsicons::bs_icon(icon, size = "1.4em"),
    theme    = theme,
    class    = "mb-0")
}
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
3. Data Preparation (prep_data() function)

This is the core data processing function. It loads raw data, cleans it, engineers new features, and estimates the Fair Market Value (FMV).

prep_data <- function() {
  # ... (function body)
}
master_data <- prep_data() # Execute the function to load and prepare data globally
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
3.1 Load Raw CSVs

Reads data from several CSV files: players.csv, transfers.csv, player_valuations.csv.

readr::read_csv() is used for fast loading.

col_types = cols(.default = "c") reads all columns as character type initially. This prevents read_csv from incorrectly guessing column types (e.g., reading an ID column as numeric when it should be character) and allows for more controlled type conversion later.

## 3.1 – load raw CSVs -----------------------------------------------------
  players    <- read_csv("players.csv",           col_types = cols(.default = "c"))
  transfers  <- read_csv("transfers.csv",         col_types = cols(.default = "c"))
  valuations <- read_csv("player_valuations.csv", col_types = cols(.default = "c"))
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
3.2 Clean Numeric & Date Columns

Numeric Parsing:

The transfer_fee column in the transfers data and market_value_in_eur in valuations are converted from character strings (potentially in European format) to numeric values using the parse_euro helper function.

Date Parsing:

A local helper function to_date(x) is defined to convert character strings to Date objects. It tries two common formats: YYYY-MM-DD and DD.MM.YYYY.

This function is applied to date_of_birth and contract_expiration_date in the players data, transfer_date in transfers, and date in valuations.

## 3.2 – clean numeric & date columns --------------------------------------
  transfers  <- transfers  %>% mutate(transfer_fee        = parse_euro(transfer_fee))
  valuations <- valuations %>% mutate(market_value_in_eur = parse_euro(market_value_in_eur))

  to_date <- function(x) as.Date(x, tryFormats = c("%Y-%m-%d", "%d.%m.%Y"))

  players   <- players   %>%
    mutate(date_of_birth            = to_date(date_of_birth),
           contract_expiration_date = to_date(contract_expiration_date))
  transfers  <- transfers  %>% mutate(transfer_date = to_date(transfer_date))
  valuations <- valuations %>% mutate(date         = to_date(date))
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
3.3 Last Market Value and Last Transfer Fee

last_val: For each player (group_by(player_id)), this code finds the most recent market valuation.

slice_max(order_by = date, n = 1, with_ties = FALSE) selects the row with the maximum (latest) date. with_ties = FALSE ensures only one row is picked if there are multiple entries on the same latest date (it picks the first one it encounters).

It then renames market_value_in_eur to market_value.

last_fee: Similarly, for each player, it finds the most recent transfer.

slice_max(order_by = transfer_date, n = 1, with_ties = FALSE) selects the row with the latest transfer_date.

It selects the player_id and transfer_fee.

## 3.3 – last market value and last transfer fee ---------------------------
  last_val <- valuations %>% group_by(player_id) %>%
    slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
    select(player_id, market_value = market_value_in_eur)

  last_fee <- transfers  %>% group_by(player_id) %>%
    slice_max(order_by = transfer_date, n = 1, with_ties = FALSE) %>%
    select(player_id, transfer_fee)
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
3.4 Working Dataframe (df)

A primary data frame df is created, starting with selected columns from players_raw (which seems to be a typo and should likely be players).

Columns selected: player_id, name, position, date_of_birth, contract_expiration_date.

New Features Calculated:

age: Calculated as the difference between the current system date (Sys.Date()) and the player's date_of_birth, converted to years.

contract_remaining_years: Calculated as the difference between the contract_expiration_date and Sys.Date(), converted to years.

The last_val (containing latest market values) and last_fee (containing latest transfer fees) are joined to this df by player_id.

## 3.4 – working dataframe --------------------------------------------------
  df <- players %>% # Assuming players_raw was a typo and players is the intended dataframe
    select(player_id, name, position, date_of_birth, contract_expiration_date) %>%
    mutate(
      age = as.numeric(difftime(Sys.Date(), date_of_birth, units = "days")) / 365.25,
      contract_remaining_years =
        as.numeric(difftime(contract_expiration_date, Sys.Date(), units = "days")) / 365.25
    ) %>%
    left_join(last_val, by = "player_id") %>%
    left_join(last_fee, by = "player_id")
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
3.5 Base FMV Model (Log-Linear Regression)

This is the core of the Fair Market Value estimation.

Log-Transform Market Value:

A new column log_market_value is created. It's the natural logarithm of market_value plus 1 (log1p()). This transformation is common for monetary values in regression models because:

It can help normalize the distribution of the target variable if it's skewed (common for market values).

It can stabilize variance.

It makes the model estimate multiplicative effects (a unit change in a predictor leads to a percentage change in market value), which often makes more sense for financial data.

The transformation is only applied if market_value is not NA and is greater than 0. Otherwise, log_market_value is NA_real_.

df <- df %>% mutate(
    log_market_value = ifelse(!is.na(market_value) & market_value > 0,
                              log1p(market_value), NA_real_)
  )
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END

Fit Linear Model:

A linear regression model (lm()) is fitted to predict log_market_value.

The predictors are:

age: Player's age.

position: Player's playing position (treated as a categorical variable).

contract_remaining_years: Years remaining on the player's contract.

fit <- NULL: The fit object (which will store the model) is initialized to NULL.

Conditional Fitting: The model is only fitted if there are more than 10 rows with non-NA log_market_value and there are more than one distinct player positions (n_distinct(df$position, na.rm = TRUE) > 1). This is a basic check to ensure there's enough data and variability (especially for the categorical position predictor) to fit a meaningful model.

na.action = na.exclude: This tells lm to exclude rows with NA values in any of the model variables during fitting but to keep track of them so that predict() can later return NA for these rows.

fit <- NULL
  # Fit only if enough rows *and* at least two positions are available
  if (nrow(df %>% filter(!is.na(log_market_value))) > 10 &&
      n_distinct(df$position, na.rm = TRUE) > 1) {
    fit <- lm(log_market_value ~ age + position + contract_remaining_years,
              data = df, na.action = na.exclude)
  }
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END

Predict Fair Market Value:

A new column fair_market_value_model is created.

If the model (fit) was successfully trained:

predict(fit, newdata = df, na.action = na.pass): Predictions are made for all players in the df.

newdata = df %>% mutate(position = factor(position, levels = fit$xlevels$position)): This is crucial. When predicting on new data (or the same data), factor levels for categorical predictors (like position) must match the levels seen during training. fit$xlevels$position stores the factor levels from the training data. This line ensures the position column in df is treated as a factor with those specific levels.

na.action = na.pass: Ensures that if any predictor value needed for a prediction is NA, the prediction for that row will also be NA.

expm1(): Since the model predicted log_market_value (which was log(market_value + 1)), expm1() is used to reverse the transformation: exp(prediction) - 1. This gives the predicted market value on the original scale.

If the model was NOT trained (fallback):

df$market_value * 1.10: A simple fallback is used where the FMV is estimated as 110% of the player's current market_value. This is a very basic heuristic used if the model couldn't be built.

df$fair_market_value_model <- if (!is.null(fit)) {
    expm1(predict(
      fit,
      newdata = df %>%
        mutate(position = factor(position, levels = fit$xlevels$position)),
      na.action = na.pass))
  } else {
    df$market_value * 1.10   # very rare fallback
  }
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
3.6 Age-Dependent Corridor (Upper/Lower Bounds for FMV)

This step refines the model-predicted FMV by applying an "age corridor". The idea is that the market behaves differently for players of different ages, and the raw model output might sometimes be unrealistic. This corridor imposes plausible upper and lower bounds on the FMV based on the player's current market value and age.

upper bound calculation:

A multiplier is determined based on age:

Age <= 20: Multiplier = 2.5

Age 20-24: Multiplier = 2.0

Age 24-28: Multiplier = 1.6

Age 28-32: Multiplier = 1.3

Age > 32: Multiplier = 1.1

The upper bound is this multiplier times the player's current market_value.

lower bound calculation:

A similar age-based multiplier is used:

Age <= 20: Multiplier = 0.50

Age 20-24: Multiplier = 0.60

Age 24-28: Multiplier = 0.70

Age 28-32: Multiplier = 0.80

Age > 32: Multiplier = 0.90

The lower bound is this multiplier times market_value.

Clamping FMV:

The fair_market_value_model (from step 3.5) is then "clamped" or constrained to be within these lower and upper bounds.

pmax(fair_market_value_model, lower): Takes the maximum of the model's prediction and the calculated lower bound (ensuring FMV isn't below lower).

pmin(..., upper): Takes the minimum of the previous result and the calculated upper bound (ensuring FMV isn't above upper).

This clamping is only applied if market_value is not NA. If market_value is NA, the fair_market_value_model remains as predicted by the model (or its fallback).

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
    )
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
3.7 Quick Scouting Metrics

Finally, some simple scouting metrics are calculated:

mv_to_fmv_ratio: The ratio of the player's current market_value to their estimated fair_market_value_model. A ratio > 1 suggests the player might be overvalued by the market compared to the model; < 1 suggests undervaluation.

opportunity_score: A simple heuristic score.

(20 - age) / 10: This part gives higher scores to younger players (e.g., a 15-year-old gets (20-15)/10 = 0.5; a 25-year-old gets (20-25)/10 = -0.5).

(1 - mv_to_fmv_ratio): This part gives higher scores if the player is considered undervalued by the model (i.e., mv_to_fmv_ratio is low).

The sum is rounded to 2 decimal places.

opportunity_flag: A categorical flag based on the opportunity_score:

High Priority Target: Score >= 2

Notable Prospect: Score >= 1

Standard Profile: Otherwise

## 3.7 – quick scouting metrics -----------------------------------------
  mutate(
    mv_to_fmv_ratio  = market_value / fair_market_value_model,
    opportunity_score = round(((20 - age) / 10) + (1 - mv_to_fmv_ratio), 2),
    opportunity_flag  = case_when(
      opportunity_score >= 2 ~ "High Priority Target",
      opportunity_score >= 1 ~ "Notable Prospect",
      TRUE                   ~ "Standard Profile")
  )

  df # Return the prepared dataframe
}
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END

The prep_data() function returns the fully processed df, which is then stored in the global master_data variable for use throughout the Shiny app.

4. Theme + Custom SCSS

bslib::bs_theme(): Initializes a Bootstrap 5 theme.

version = 5: Specifies Bootstrap version 5.

bootswatch = "pulse": Uses the "pulse" Bootswatch theme as a base, providing a pre-designed look and feel.

Custom SCSS:

It checks if a file named soccer.scss exists in a www subdirectory.

If it exists, sass::sass_file(scss_path) compiles the SCSS file into CSS, and bslib::bs_add_rules() injects these custom CSS rules into the theme. This allows for fine-grained styling beyond the Bootswatch theme.

If the file doesn't exist, a warning is issued.

# ##### 4 · Theme + custom SCSS (wow effect) ####################################
theme <- bs_theme(version = 5, bootswatch = "pulse")

# If you created "www/soccer.scss", compile & inject it here
scss_path <- "www/soccer.scss"
if (file.exists(scss_path)) {
  theme <- bs_add_rules(theme, sass::sass_file(scss_path))
} else {
  warning("Custom SCSS 'www/soccer.scss' not found – running with default theme.")
}
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
5. User Interface (UI)

The UI is defined using shiny and bslib functions. It creates a page with a sidebar for filters and a main area for displaying data.

page_sidebar(): The main page layout, with a title and the defined theme.

Sidebar (sidebar = sidebar(...)):

Contains various input controls for filtering the data.

Age Filter:

sliderInput("age_slide", ...): A range slider for age.

numericInput("age_min", ...) and numericInput("age_max", ...): Numeric input boxes that will be synced with the age slider.

Fee Filter:

sliderInput("fee_slide", ...): A range slider for transfer fees.

numericInput("fee_min", ...) and numericInput("fee_max", ...): Numeric input boxes synced with the fee slider.

Position Filter:

checkboxGroupInput("pos", ...): Checkboxes for selecting player positions. pos_choices (derived from unique positions in master_data) populates the choices.

actionLink("pos_all", ...): A link to select/deselect all positions.

Scouting Flag Filter:

checkboxGroupInput("flag", ...): Checkboxes for selecting scouting flags. flag_choices (predefined categories) populates the choices.

actionLink("flag_all", ...): A link to select/deselect all flags.

Reset Button:

actionButton("reset", ...): A button to reset all filters to their default states.

Main Area (layout_columns(...)):

Player Card:

A card() component to display details of a selected player.

uiOutput("player_header"): Dynamically generated header for the player card (name, age, position, progress bar).

layout_columns(...) nests more columns for value boxes:

uiOutput("vb_fmv"): Value box for Fair Market Value.

uiOutput("vb_mv"): Value box for current Transfermarkt (TM) Value.

uiOutput("vb_opp"): Value box for Opportunity Score.

Plot and Table Tabs:

navset_card_tab(): Creates a card with tabs.

nav_panel("Plot", plotlyOutput("bubble")): A tab containing an interactive bubble plot.

nav_panel("Table", DTOutput("tbl")): A tab containing an interactive data table.

# ##### 5 · UI ##################################################################
# ... (UI definition code) ...
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
6. Server Logic

The server function contains the logic that makes the app interactive. It responds to user inputs, filters data, and renders outputs.

# ##### 6 · Server ##############################################################
server <- function(input, output, session) {
  # ... (server logic code) ...
}
IGNORE_WHEN_COPYING_START
content_copy
download
Use code with caution.
R
IGNORE_WHEN_COPYING_END
6.1 Select/Unselect All Checkboxes

Two observeEvent blocks handle clicks on the "Select / unselect all" links for position and flag filters.

updateCheckboxGroupInput(): Modifies the selected choices. If all are currently selected, it deselects all (by setting selected = character(0)). Otherwise, it selects all available choices.

6.2 Sync Sliders with Numeric Inputs

A local helper function sync(slider, num_min, num_max) is defined to create two-way binding between a slider and two numeric input boxes (for min/max values of the range).

Slider to Numeric: observeEvent(input[[slider]], ...) updates the numeric inputs when the slider changes.

Numeric to Slider: observeEvent(c(input[[num_min]], input[[num_max]]), ...) updates the slider when either numeric input changes.

ignoreInit = TRUE prevents these observers from firing when the app first starts.

This sync function is called for the age and fee filters.

6.3 Reset Button

observeEvent(input$reset, ...): When the "Reset filters" button is clicked:

updateSliderInput() resets the age and fee sliders to their initial full ranges.

updateCheckboxGroupInput() resets the position and flag checkboxes to select all choices.

6.4 Reactive Filtered Dataset (filt())

This is a reactive expression, meaning its value is automatically re-calculated whenever its dependencies (input values) change.

It starts with the master_data.

Applies filters based on the current values of:

input$age_slide: Filters players within the selected age range using dplyr::between().

input$fee_slide: Filters players within the selected transfer fee range.

input$pos: Filters players whose position is in the list of selected positions (if any are selected).

input$flag: Filters players whose opportunity_flag is in the list of selected flags (if any are selected).

Returns the filtered data frame.

6.5 Keep Track of Selected Player ID

sel_id <- reactiveVal(master_data$player_id[1]): A reactiveVal named sel_id is created to store the player_id of the currently selected player. It's initialized with the ID of the first player in master_data.

Plot Click: observeEvent(event_data("plotly_click", source = "src"), ...):

Listens for click events on the Plotly bubble plot (which has source = "src").

event_data("plotly_click", source = "src")$customdata: Retrieves the customdata associated with the clicked point. In the bubble plot, customdata is set to player_id.

Updates sel_id() with the clicked player's ID.

Table Row Selection: observeEvent(input$tbl_rows_selected, ...):

Listens for row selection events in the DT table.

input$tbl_rows_selected gives the index of the selected row in the currently displayed data in the table (which is filt()).

Updates sel_id() with the player_id from the corresponding row in filt().

player <- reactive(...): A reactive expression that filters filt() to get the data row for the player whose ID matches sel_id(). This provides the complete data for the selected player.

6.6 Value Boxes

Three renderUI blocks create the value boxes for the selected player:

output$vb_fmv: Displays "Fair MV" using fmt_euro(pd$fair_market_value_model).

output$vb_mv: Displays "TM value" (Transfermarkt value) using fmt_euro(pd$market_value).

output$vb_opp: Displays "Opportunity" score. The icon and theme color change based on the opportunity_flag.

pd <- player(); if (nrow(pd) == 0) return(NULL): Ensures that rendering only proceeds if a player is actually selected and their data (pd) is available.

The make_vb helper function is used.

6.7 Player Header Progress Bar

output$player_header: Renders the header for the player card.

If no player is selected (nrow(pd) == 0), it displays "Select a player…".

Otherwise, it displays:

Player's name, age (formatted to one decimal place), and position.

A simple progress bar made of three div elements, where the width of each segment represents the relative proportion of market_value, fair_market_value_model, and transfer_fee to the maximum of these three values. NAs are treated as 0 for this visualization.

6.8 Bubble Plot

output$bubble <- renderPlotly(...): Renders the interactive bubble plot.

Data: Uses the filtered data from filt().

Aesthetics:

x = ~age

y = ~transfer_fee

size = ~market_value: Bubble size is proportional to market value.

color = ~position: Bubbles are colored by player position.

customdata = ~player_id: Stores player_id with each point, used for click events.

source = "src": A unique identifier for this plot, allowing event_data to specifically target events from it.

text = ~paste0(...): Defines the hover text, showing player name, FMV, and TM value (formatted with fmt_euro).

Marker Border: style(p, marker.line.width = 0.5, marker.line.color = "rgba(0,0,0,0.5)") adds a border to all markers in the plot. This is a way to apply styling to all traces after the initial plot_ly call, sometimes avoiding warnings about applying line properties to non-line markers if done directly in plot_ly.

event_register("plotly_click"): Explicitly registers the plotly_click event so that event_data() can capture it.

layout(...): Customizes the axis titles.

6.9 Data Table

output$tbl <- renderDT(...): Renders the interactive data table using the DT package.

Data: Uses the filtered data from filt().

select(...): Selects and renames columns for display in the table (e.g., name becomes "Name", market_value becomes "MV (€)").

datatable(...): Creates the DT object.

selection = "single": Allows only single row selection.

rownames = FALSE: Hides row names.

options = list(...):

pageLength = 10: Shows 10 rows per page.

columnDefs = list(list(className = "dt-center", targets = "_all")): Centers the text in all columns.

formatCurrency(...): Formats specified columns ("MV (€)", "Fair MV (€)", "Fee (€)") as Euro currency.

formatPercentage(...): Formats the "MV/FMV" column as a percentage.

7. Launch the App

shinyApp(ui, server): Runs the Shiny application, combining the defined UI and server logic.

This detailed breakdown covers the structure and functionality of the provided R Shiny dashboard code, with a special emphasis on how the Fair Market Value is derived through data processing and a linear regression model, followed by an age-based corridor adjustment.
