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

*(The R code for loading these libraries is located in the `app.R` file.)*

## 2. Helper Functions

These are small, reusable functions to simplify common tasks:

*   **`parse_euro(x)`**:
    *   Takes a character string `x` representing a number in European format (e.g., "12.345,67").
    *   Uses `readr::parse_number()` to convert it into a numeric value, specifying "." as the grouping mark and "," as the decimal mark.
    *   `suppressWarnings()` prevents warnings if parsing fails.

*   **`fmt_euro(x)`**:
    *   Formats a numeric value `x` as a Euro currency string (e.g., "€12.346").
    *   Returns "N/A" if `x` is `NA`, non-finite, or non-convertible to numeric.
    *   Otherwise, rounds to 0 decimal places, formats with "." as thousands separator, and prepends "€".

*   **`make_vb(title, value, icon, theme)`**:
    *   A wrapper for `bslib::value_box()`, standardizing its appearance (small title, icon size, no bottom margin).

*(The R code for these helper functions is located in the `app.R` file.)*

## 3. Data Preparation (`prep_data()` function)

This function is the heart of the data processing pipeline. It is executed once when the app starts, and its result is stored in `master_data`.

### 3.1 Load Raw CSVs
*   A helper function `read_robust_csv` is defined to load CSVs (`players.csv`, `transfers.csv`, `valuations.csv`). It uses `tryCatch` to handle file reading errors, returning an empty `tibble` and a warning if a file can't be loaded.
*   All columns are initially read as character type (`cols(.default = "c")`) to prevent type misinterpretation.
*   The application stops if the essential `players.csv` cannot be loaded.

### 3.2 Clean Numeric & Date Columns
*   **Numeric Parsing**:
    *   An internal helper `ensure_numeric_col` ensures specified columns exist and are converted to numeric. It uses `parse_euro` for `transfer_fee` and `market_value_in_eur`.
*   **Date Parsing**:
    *   `to_date_robust(x_val)` converts character strings to `Date` objects, trying common R formats first, then `lubridate::parse_date_time` for more flexibility.
    *   `ensure_date_col` applies this robust date parsing to relevant date columns in the loaded tables (`date_of_birth`, `contract_expiration_date`, `transfer_date`, `date`).

### 3.3 Last Market Value and Last Transfer Fee
*   **`last_val`**: For each player, it extracts the most recent (latest `date`) market valuation from the `valuations` data. Rows with missing `player_id` or `date` are filtered out first.
*   **`last_fee`**: Similarly, it extracts the most recent (latest `transfer_date`) transfer fee for each player from the `transfers` data.
*   If the source tibbles are empty or lack necessary columns, these will result in empty tibbles with the correct column types.

### 3.4 Working Dataframe (`df`)
*   The main dataframe `df` is initialized with selected columns from the `players` data. Essential columns are checked, and if missing, they are added as `NA` with a warning.
*   **Calculated Features**:
    *   `age`: Player's current age in years.
    *   `contract_remaining_years`: Years remaining on the player's contract. If the contract is expired or the expiration date is `NA`, this is set to 0.
*   `last_val` and `last_fee` are `left_join`ed to `df`. `player_id` is explicitly cast to character type before joining to ensure compatibility. Joined monetary values are also cast to numeric.

### 3.5 Base FMV Model (Log-Linear Regression)
This section details the estimation of a player's Fair Market Value (FMV) using a linear regression model.

1.  **Log-Transform Target Variable**:
    *   The `market_value` is log-transformed (`log1p(market_value)`) to create `log_market_value`. This is a common practice for monetary values in regression to stabilize variance and model multiplicative effects. It's only applied to positive market values.

2.  **Conditional Model Fitting**:
    *   A linear model (`lm()`) is trained to predict `log_market_value`.
    *   The predictors are `age`, `position_model` (player's position, treated as a factor with "Unknown" for NAs), and `contract_remaining_years`.
    *   The model is only fitted if there are more than 10 valid data points (after filtering NAs in predictors and target) and more than one distinct position category (to ensure the `position_model` factor has enough levels).
    *   `tryCatch` is used to handle potential errors during the `lm()` call.

3.  **Prediction and Fallback**:
    *   If the model (`fit`) is successfully trained:
        *   Predictions for `log_market_value` are made on the entire dataset.
        *   Crucially, when predicting, the `position_model` factor in the `newdata` is explicitly set to have the same levels as those observed during model training (from `fit$model$position_model`). This prevents errors if new or unseen positions appear.
        *   The `expm1()` function is used to transform the predictions back from the log scale to the original monetary scale (`fair_market_value_model`).
    *   If model training fails or the conditions aren't met, a fallback mechanism is used: `fair_market_value_model` is set to `market_value * 1.10`.
    *   The predicted FMV is floored at 0. If FMV is still `NA` but `market_value` exists, the fallback is applied again.

### 3.6 Age-Dependent Corridor for FMV
The model-derived FMV is further refined by an "age corridor" to ensure values are within plausible bounds relative to the player's current market value and age.
*   Age-specific multipliers are defined to calculate `upper_bound_val` and `lower_bound_val` based on `market_value`. Younger players typically have wider corridors (e.g., a 20-year-old's FMV can be between 0.5x and 2.5x their market value).
*   The `fair_market_value_model` is then "clamped" using `pmin(pmax(fmv, lower_bound_val), upper_bound_val)` to stay within this corridor. This adjustment is only applied if all necessary components (market value, FMV, bounds) are non-NA.

### 3.7 Quick Scouting Metrics
*   **`mv_to_fmv_ratio`**: Ratio of current `market_value` to the estimated `fair_market_value_model`.
*   **`opportunity_score`**: A heuristic score calculated as `round(((20 - age) / 10) + (1 - mv_to_fmv_ratio), 2)`. It favors younger players and those whose market value is low relative to their FMV (i.e., `mv_to_fmv_ratio` is low). If `mv_to_fmv_ratio` is NA, it's treated as 1 for this calculation.
*   **`opportunity_flag`**: A categorical flag ("High Priority Target", "Notable Prospect", "Standard Profile") based on the `opportunity_score`.

The `prep_data()` function returns the final `df`.

*(The R code for data preparation is located in the `app.R` file.)*

## 4. Theme + Custom SCSS

*   A Bootstrap 5 theme is initialized using `bslib::bs_theme(bootswatch = "pulse")`.
*   The app attempts to load and apply custom styles from `www/soccer.scss` if the file exists.

*(The R code for theming is located in the `app.R` file.)*

## 5. User Interface (UI)

The UI is built using `shiny::page_sidebar` for the overall layout.

*   **Sidebar**: Contains various input controls for filtering:
    *   **Age Filter**: A `sliderInput("age_slide")` and two `numericInput`s (`age_min`, `age_max`) for range selection, styled to appear side-by-side.
    *   **Fee Filter**: Similar `sliderInput("fee_slide")` and numeric inputs (`fee_min`, `fee_max`) for transfer fees.
    *   **Position Filter**: `checkboxGroupInput("pos")` allowing multiple position selections, with an "Select / unselect all" `actionLink`. Only shown if position choices are available.
    *   **Scouting Flag Filter**: `checkboxGroupInput("flag")` for `opportunity_flag`.
    *   **Reset Button**: `actionButton("reset")` to revert all filters.
*   **Main Area**:
    *   **Player Card**: A `bslib::card` displays information for the currently selected player.
        *   `uiOutput("player_header")`: Shows player name, age, position, and a visual progress bar comparing market value, FMV, and transfer fee.
        *   Three `bslib::value_box` components (rendered via `uiOutput("vb_fmv")`, etc.) display FMV, market value, and opportunity score.
    *   **Data Exploration Tabs**: A `bslib::navset_card_tab` contains:
        *   A "Plot" panel with `plotlyOutput("bubble_plot_main")`.
        *   A "Table" panel with `DTOutput("data_table_main")`.

*(The R code for the UI is located in the `app.R` file.)*

## 6. Server Logic

The `server` function defines the app's backend logic and reactivity.

### 6.1 Select/Unselect All Checkboxes
*   `observeEvent` listeners for `input$pos_all` and `input$flag_all` toggle the selection state of the position and flag `checkboxGroupInput`s.

### 6.2 Sync Sliders with Numeric Inputs
*   A helper function `sync_slider_numeric` creates a two-way binding between the range sliders (e.g., `age_slide`) and their corresponding min/max `numericInput` fields (e.g., `age_min`, `age_max`).
*   This ensures that changing the slider updates the numeric boxes, and changing a numeric box updates the slider, while also respecting the overall min/max bounds of the sliders.

### 6.3 Reset Button
*   An `observeEvent` for `input$reset` updates all filter inputs (sliders and checkboxes) back to their initial default values.

### 6.4 Reactive Filtered Dataset (`filtered_data_reactive`)
*   This `reactive` expression filters the `master_data` based on the current selections in `input$age_slide`, `input$fee_slide`, `input$pos`, and `input$flag`.
*   For fee and age filters, `NA` values are included if the respective slider's lower bound is at its absolute minimum, implying the user hasn't actively filtered out records with missing values.
*   If no options are selected in a checkbox group (e.g., all positions deselected), it results in no rows being returned for that filter step.

### 6.5 Keep Track of Selected Player ID
*   `selected_player_id_reactive <- reactiveVal()`: Stores the ID of the currently selected player.
*   **Initialization & Update Logic**: An `observe` block initializes this `reactiveVal` with the first player from the `filtered_data_reactive` list. It also updates the selection if the currently selected player is no longer in the filtered list (e.g., due to filter changes) or clears the selection if the filtered list becomes empty.
*   The `selected_player_id_reactive` is updated by:
    *   Clicks on the Plotly bubble plot (via `event_data("plotly_click", source = "bubble_plot_source_id")`). The `customdata` attribute of the plot holds the `player_id`.
    *   Row selection in the DT table (via `input$data_table_main_rows_selected`).
*   `selected_player_details_reactive <- reactive(...)`: A reactive expression that fetches the full data row for the `selected_player_id_reactive()` from the original `master_data`.

### 6.6 Value Boxes
*   `output$vb_fmv`, `output$vb_mv`, `output$vb_opp` use `renderUI` to display `bslib::value_box` components for the selected player's FMV, market value, and opportunity score, using the `make_vb` helper and data from `selected_player_details_reactive()`.

### 6.7 Player Header
*   `output$player_header`: Dynamically renders the player's name, age, position. It also includes a simple horizontal progress bar composed of three segments, where the width of each segment visually represents the player's market value, FMV, and last transfer fee relative to the maximum of these three values.

### 6.8 Bubble Plot
*   `output$bubble_plot_main <- renderPlotly(...)`: Renders the interactive bubble plot.
    *   Data is taken from `filtered_data_reactive()`.
    *   Temporary `_display` columns are created to handle NAs specifically for plotting aesthetics (e.g., `market_value_display` for bubble size defaults to 1 if `market_value` is NA or zero).
    *   Aesthetics: age (x), transfer fee (y), market value (size), position (color).
    *   `customdata` is set to `player_id` for click interactivity, and `source` is set to "bubble_plot_source_id" to uniquely identify events from this plot.
    *   `event_register("plotly_click")` enables click event capture.
    *   Layout includes axis titles, a horizontal legend, adjusted margins, and pan as the drag mode. Markers have a slight opacity and border.

### 6.9 Data Table
*   `output$data_table_main <- renderDT(...)`: Renders an interactive data table using `DT::datatable`.
    *   Data is from `filtered_data_reactive()`.
    *   Columns are selected and renamed for a user-friendly display. The `player_id` column is included in the selected data but then hidden in the displayed table using `columnDefs` (it's still available for row selection logic).
    *   Features include single row selection, no row names, top-row filtering (`filter = "top"`), responsive extension for better mobile viewing, and a custom search placeholder.
    *   `formatCurrency` and `formatPercentage` are used to style numeric columns appropriately.

*(The R code for the Server logic is located in the `app.R` file.)*

## 7. Launch the App

*   `shinyApp(ui, server)`: This command starts the Shiny application, bringing the UI and server logic together to create the interactive web dashboard.
