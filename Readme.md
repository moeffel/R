# Football Player Scouting Dashboard - README

## 1. Overall Purpose

This R Shiny dashboard is designed as a tool for football (soccer) player scouting. It processes data from various sources (player information, transfers, market valuations, club details, and match appearances) to provide insights into player valuation, performance, and potential transfer opportunities. The dashboard allows users to filter players based on multiple criteria and view detailed statistics, including model-derived fair market values and an "Opportunity Score" to highlight promising targets.

## 2. Code Structure (`app.R`)

The application is contained within a single `app.R` file, a common practice for Shiny apps.

### 2.1. Packages
The following R packages are used:
-   **shiny**: Core package for building interactive web applications.
-   **tidyverse**: A collection of R packages for data science (includes `dplyr` for data manipulation, `readr` for reading data, `ggplot2` for plotting - though `plotly` is used for interactive plots here).
-   **plotly**: For creating interactive charts, specifically the main bubble plot.
-   **DT**: For rendering interactive HTML tables.
-   **lubridate**: For easier handling of date and time data.
-   **scales**: For formatting numbers (e.g., currency, percentages) in plots and tables.
-   **bslib**: For modern Bootstrap 5 theming and UI components like cards and accordions.

### 2.2. `prep_data()` Function
This is the primary data processing engine of the dashboard. It's called once when the app starts to prepare the `master_data` tibble.

**Key Steps within `prep_data()`:**

1.  **Data Loading (`read_robust_csv`)**:
    *   Reads multiple CSV files (`players.csv`, `transfers.csv`, `player_valuations.csv`, `clubs.csv`, `appearances.csv`).
    *   Uses a custom function `read_robust_csv` which reads all columns as characters initially to prevent type inference errors and includes error handling for missing files or read issues.

2.  **Data Cleaning & Type Conversion**:
    *   Converts relevant columns to numeric types (e.g., `transfer_fee`, `market_value_in_eur`, performance stats).
    *   Uses a custom function `safe_as_date` to convert date columns (`date_of_birth`, `contract_expiration_date`, `transfer_date`, `valuation_date`, `appearance_date`) robustly.
    *   Handles missing values (NAs) appropriately, often by initializing columns with `NA` if they are missing or if source data is empty.

3.  **Feature Engineering**:
    *   **`age`**: Calculated from `date_of_birth` relative to the current system date (`Sys.Date()`).
    *   **`get_season()`**: A helper function to determine the football season (e.g., "2022/2023") based on a given date. Assumes seasons run July-June.
    *   **Previous Season Performance**:
        *   For each transfer, identifies the immediately preceding football season.
        *   Aggregates `goals`, `assists`, `minutes_played`, and `games` (`n_distinct(game_id)`) from the `appearances` data for each player in that previous season.
        *   These are stored as `prev_season_goals`, `prev_season_assists`, `prev_season_minutes`, `prev_season_games`.
    *   **`contract_remaining_years`**:
        *   Calculated at the time of a player's last transfer.
        *   Determined by the difference between `contract_expiration_date` (from `players.csv`) and `transfer_date`.
        *   Capped at a maximum of 5 years. If the contract had already expired or was not available, it's handled as 0 or NA.
    *   **`log_market_value`**: Natural logarithm of `market_value` (using `log1p` which is `log(1+x)` to handle potential zeros gracefully if market value could be 0, though current logic uses `market_value > 0`). This transformation is often used in regression to normalize skewed data.
    *   **`market_value_plot`**: A version of `market_value` used for sizing bubbles in the plot, ensuring it's at least 1 to avoid issues with zero or NA sizes.
    *   **Interaction Terms (`inv_age_x_minutes`, `inv_age_x_goals_assists`)**:
        *   **Purpose**: These terms are created for use in the regression models. They aim to capture the idea that the impact of performance (minutes played, goals+assists) might differ depending on a player's age. For instance, high minutes for a very young player might be valued differently than for an older, established player.
        *   **Calculation**:
            *   `inv_age_x_minutes = (1 / (age + 0.001)) * prev_season_minutes`
            *   `inv_age_x_goals_assists = (1 / (age + 0.001)) * (prev_season_goals + prev_season_assists)`
            *   The `(1 / (age + 0.001))` part represents the inverse of age (with a small constant `0.001` added to avoid division by zero if age could theoretically be 0, though practically player ages will be positive). This gives higher weight to younger players.

4.  **Modeling**:
    *   **Fair Market Value Model (`model_fair_mv`)**:
        *   **Type**: Linear Regression model (`lm` function).
        *   **Target Variable**: `log_market_value` (the log-transformed current market value of players).
        *   **Predictors**:
            *   `age`
            *   `position` (treated as a categorical variable/factor)
            *   `contract_remaining_years`
            *   `prev_season_minutes`
            *   `prev_season_goals`
            *   `prev_season_assists`
            *   `inv_age_x_minutes` (interaction term)
            *   `inv_age_x_goals_assists` (interaction term)
        *   **Training**: The model is trained using players from the dataset who have non-missing values for `log_market_value` and all predictor variables. It requires a minimum number of observations and distinct positions for stability.
        *   **Output Column (`fair_market_value_model`)**: The model predicts `log_market_value`. This prediction is then back-transformed using `expm1` (equivalent to `exp(x) - 1`) to get the fair market value on the original currency scale. Predicted values less than 0 are set to 0. This represents the model's estimate of what a player's market value *should* be based on their characteristics and performance.

    *   **Predicted Transaction Fee Model (`model_transaction_fee`)**:
        *   **Type**: Generalized Linear Model (`glm` function) with a Gamma family and a log link. This statistical model is suitable for response variables (like transfer fees) that are continuous, strictly positive, and often right-skewed.
        *   **Target Variable**: `transfer_fee` (actual transfer fees paid for players).
        *   **Predictors**:
            *   `age`
            *   `market_value` (the player's current market value)
            *   `position` (as a factor)
            *   `contract_remaining_years`
            *   `prev_season_minutes`
            *   `prev_season_goals`
            *   `prev_season_assists`
        *   **Training**: Trained on players with known, positive transfer fees and complete predictor information.
        *   **Output Column (`predicted_transaction_fee`)**: The model predicts the expected transfer fee for a player given their attributes. Predictions are on the original currency scale. Values less than 0 are set to 0.

5.  **Metric Calculation (Derived Scores and Flags)**:
    *   **Valuation Ratios**:
        *   `mv_to_fair_value_ratio = market_value / fair_market_value_model`: Compares a player's actual current market value to their modeled fair market value. A ratio < 1 suggests potential undervaluation by the market relative to the model.
        *   `fee_to_fair_value_ratio = transfer_fee / fair_market_value_model`: Compares a player's last transfer fee to their modeled fair market value. A ratio < 1 suggests the transfer fee paid was less than their modeled fair value.
    *   **Component Scores for Opportunity Score**:
        *   `potential_score_age`: A score from 0 to 4, where younger players get higher scores (e.g., age <= 20 gets 4, age 25-26 gets 1, age > 26 gets 0).
        *   `performance_score_minutes`: A score from 0 to 4 based on `prev_season_minutes` (e.g., >=2500 mins gets 4, 1000-1800 mins gets 2).
        *   `productivity_per_90 = (prev_season_goals + prev_season_assists) / (prev_season_minutes / 90)`: Calculates combined goals and assists per 90 minutes played in the previous season.
        *   `performance_score_productivity`: A score from 0 to 3 based on `productivity_per_90` (e.g., >= 0.6 G+A/90 gets 3).
    *   **`opportunity_score`**:
        *   **Purpose**: A composite, weighted score designed to highlight potentially good scouting opportunities.
        *   **Calculation**:
            `round((potential_score_age * 0.35) +`
            `(performance_score_minutes * 0.15) +`
            `(performance_score_productivity * 0.15) +`
            `(score_for_mv_ratio * 0.20) +`
            `(score_for_fee_ratio * 0.15), 1)`
            *   `score_for_mv_ratio`: 2 if `mv_to_fair_value_ratio` <= 0.8; 1 if `mv_to_fair_value_ratio` <= 1.0; else 0. (Favors players whose current market value is low compared to their modeled fair value).
            *   `score_for_fee_ratio`: 2 if `fee_to_fair_value_ratio` <= 0.7; 1 if `fee_to_fair_value_ratio` <= 1.0; else 0. (Favors players whose last transfer fee was low compared to their modeled fair value).
        *   The weights (0.35, 0.15, etc.) determine the relative importance of each component.
    *   **`opportunity_flag`**: A categorical label assigned based on the `opportunity_score`:
        *   `High Priority Target`: if `opportunity_score` >= 2.0
        *   `Notable Prospect`: if `opportunity_score` >= 1.2
        *   `Standard Profile`: otherwise
    *   **`transaction_valuation_status`**: Classifies the player's last transfer deal based on the `transfer_fee` relative to the `fair_market_value_model`:
        *   `Undervalued (vs Fair MV)`: if `transfer_fee` < 0.9 * `fair_market_value_model`
        *   `Overvalued (vs Fair MV)`: if `transfer_fee` > 1.1 * `fair_market_value_model`
        *   `Fairly Valued (vs Fair MV)`: otherwise (if fee is between 90% and 110% of Fair MV)

6.  **Final Output**: The `prep_data` function returns a single, comprehensive tibble (`data_out`) containing all raw, processed, and derived data for each player. This tibble is named `master_data` in the global environment of the app.

### 2.3. `make_bubble_plot()` Function
*   Takes the filtered data as input.
*   Creates an interactive scatter plot (bubble plot) using `plotly`.
    *   X-axis: Player `age`.
    *   Y-axis: Last `transfer_fee`.
    *   Color: Player `position`.
    *   Size: `market_value_plot` (derived from current market value).
    *   Hover Text: Displays detailed player information on mouse-over.
    *   `customdata`: Stores `player_id` for click interactivity.

### 2.4. Global Definitions
*   **`master_data`**: The main dataset prepared by `prep_data()`.
*   **`ui_ready`**: A boolean flag indicating if `master_data` was successfully loaded and is suitable for building the UI.
*   Default values and choices for UI input elements (sliders, select inputs) are derived from `master_data` if available, otherwise, hardcoded defaults are used. This makes the filters adapt to the range of the loaded data.
*   **`theme_obj`**: Defines the visual theme for the app using `bslib::bs_theme`, incorporating Google Fonts and a Bootswatch theme ("pulse"). It also attempts to load custom SASS styles from `www/custom_styles.scss`.

### 2.5. User Interface (`ui`)
Defined using `bslib::page_sidebar`.
*   **Title**: "Football Player Scouting Dashboard".
*   **Custom CSS**: Includes CSS to adjust the display of icons within `bslib::value_box` components.
*   **Sidebar (`sidebar`)**:
    *   Contains input controls organized into an `accordion`.
    *   **Base Filters**: Sliders for `age` and `transfer_fee`, and a `selectInput` for `position`.
    *   **Valuation & Scouting Filters**: `selectInput` for `transaction_valuation` (Deal Valuation) and `opportunity` (Scouting Flag).
*   **Main Content Area (`layout_columns`)**:
    *   Organized into two main rows using `layout_columns`.
    *   **Top Row (Player Detail Card)**:
        *   `card` with `id = "player_detail_card"`.
        *   Header (`card_header`): Displays the selected player's name, age, and position (`uiOutput("playerDetailCardHeader")`).
        *   Body (`card_body`):
            *   Contains three `bslib::value_box` elements arranged horizontally (`layout_columns`) to show:
                *   Fair MV (Model) (`uiOutput("vbPlayerFairMV")`)
                *   Current Market Value (`uiOutput("vbPlayerCurrentMV")`)
                *   Opportunity Score (`uiOutput("vbPlayerOpportunityScore")`)
            *   Additional player details (`uiOutput("playerDetailExtraInfo")`) like last fee, deal valuation, remaining contract, and previous season stats.
    *   **Bottom Row (Data Exploration Tabset)**:
        *   `navset_card_tab` for tabbed navigation.
        *   **"Player Overview Plot" Tab**: Contains the interactive bubble plot (`plotlyOutput("bubblePlot")`). The height is dynamically calculated to better fit the viewport.
        *   **"Detailed Player Table" Tab**: Contains the interactive data table (`DTOutput("playerTable")`).

*   **Footer**: Displays dashboard version and a tagline.

### 2.6. Server Logic (`server`)
Contains the reactive logic that drives the dashboard.

1.  **`filtered_data()` (Reactive Expression)**:
    *   This is the core reactive data source for most outputs.
    *   It takes `master_data` and filters it based on the current values of all user inputs from the sidebar (age range, transfer fee range, position, transaction valuation, and opportunity flag).
    *   It re-executes automatically whenever any of these input values change.

2.  **`selected_player_id()` (Reactive Value)**:
    *   Stores the `player_id` of the player currently selected by the user, either by clicking on the bubble plot or selecting a row in the DT table.
    *   Initialized to `NULL`.

3.  **Event Observers (`observeEvent`)**:
    *   `input$playerTable_rows_selected`: Updates `selected_player_id()` when a row is selected in the `playerTable`.
    *   `event_data("plotly_click", source = "bubblePlotSource")`: Updates `selected_player_id()` when a point (bubble) is clicked on the `bubblePlot`.

4.  **`player_details()` (Reactive Expression)**:
    *   Filters `master_data` to get the row(s) corresponding to the `selected_player_id()`.
    *   `slice_head(n=1)` ensures only one player's details are used if multiple matches occur (though `player_id` should be unique).

5.  **Output Rendering Functions**:
    *   **Player Detail Card Outputs**:
        *   `output$playerDetailCardHeader <- renderUI({...})`: Dynamically generates the HTML for the header of the player detail card based on `player_details()`.
        *   `render_player_value_box()`: A helper function to create `bslib::value_box` UIs. It's used by:
            *   `output$vbPlayerFairMV`: Displays the "Fair MV (Model)".
            *   `output$vbPlayerCurrentMV`: Displays the "Current Market Value".
            *   `output$vbPlayerOpportunityScore`: Displays the "Opportunity Score", with icon and theme dynamically changing based on the `opportunity_flag`.
        *   `output$playerDetailExtraInfo <- renderUI({...})`: Generates HTML for additional details (last fee, contract, previous season stats) for the selected player.
    *   **Main Data View Outputs**:
        *   `output$bubblePlot <- renderPlotly({...})`:
            *   Uses `filtered_data()` as input.
            *   Calls `make_bubble_plot()` to generate the plot.
            *   Registers `plotly_click` event for interactivity.
            *   Configures the plot mode bar (removes some default buttons).
        *   `output$playerTable <- renderDT({...})`:
            *   Uses `filtered_data()` as input.
            *   Selects specific columns for display and sorts by `opportunity_score` descending.
            *   Applies custom formatting to numeric columns (currency, percentages, years, scores) using helper functions (`format_dt_num`, etc.) and `scales` package functions.
            *   Maps internal column names to more user-friendly display names.
            *   Configures `DT::datatable` options (page length, search, scroll, language, hides `player_id` column).
            *   Applies conditional row styling using `formatStyle` based on `opportunity_flag` (background color, font weight) and `transaction_valuation_status` (text color).
            *   Uses `server = TRUE` for server-side processing, which is generally better for larger tables.

### 2.7. `shinyApp(ui, server)`
*   This final line runs the Shiny application by combining the UI and server logic.

## 3. Data Requirements (CSV Files)

The dashboard expects the following CSV files in the same directory as `app.R` (or paths specified in `prep_data`):
*   `players.csv`: Player-specific information (ID, name, date of birth, position, contract details, etc.).
*   `transfers.csv`: Transfer history (player ID, date, fee, from/to clubs).
*   `player_valuations.csv`: Historical market valuations (player ID, date, market value).
*   `clubs.csv`: Club details (club ID, name).
*   `appearances.csv`: Player match appearance data (player ID, game ID, date, goals, assists, minutes played).

The quality and completeness of these input files directly impact the accuracy and usefulness of the dashboard's outputs and models.
