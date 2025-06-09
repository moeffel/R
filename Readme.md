# Football Scouting Dashboard: Code Explanation (Advanced FMV & Opportunity Score Version)

This document provides a detailed explanation of the R Shiny application designed for football scouting. This version incorporates more advanced data features, including player performance metrics and league strength adjustments, into its Fair Market Value (FMV) estimation and a revised Opportunity Score.

## 1. Libraries

The application utilizes several R packages:

*   **`shiny`**: Core framework for web applications.
*   **`tidyverse`**: For comprehensive data manipulation (`dplyr`, `tidyr`), reading data (`readr`), and plotting foundations.
*   **`lubridate`**: For easier date and time manipulation.
*   **`plotly`**: For creating interactive charts.
*   **`DT`**: For rendering interactive HTML tables.
*   **`bslib`**: For modern Bootstrap 5 theming and UI components like `value_box`.
*   **`bsicons`**: For Bootstrap icons.
*   **`sass`**: For compiling SCSS to CSS for custom styling.
    *   *(Note: `glmnet` was in the library list of the "NEW CODE" but not used in its FMV model, which uses `lm`. If a Ridge/Lasso regression were implemented, `glmnet` would be essential.)*

*(The R code for loading these libraries is located in the `app.R` file.)*

## 2. Helper Functions

These utility functions simplify common tasks:

*   **`parse_euro(x)`**: Converts European-formatted number strings (e.g., "12.345,67") to numeric values.
*   **`fmt_euro(x)`**: Formats numeric values into Euro currency strings (e.g., "€12.345"), handling `NA` or non-finite inputs gracefully by returning "N/A".
*   **`make_vb(title, value, icon, theme)`**: A wrapper for `bslib::value_box()` to create consistently styled Key Performance Indicator (KPI) cards.
*   **`%||%` (Infix Operator)**: A custom operator that returns the right-hand side `b` if the left-hand side `a` is `NULL`; otherwise, it returns `a`. Useful for providing default values.

*(The R code for these helper functions is located in the `app.R` file.)*

## 3. Data Preparation (`prep_data()` function)

This is the central function for all data loading, cleaning, feature engineering, and modeling. It's executed once when the Shiny application starts.

### 3.1 Load Raw CSVs
*   Data is loaded from multiple CSV files:
    *   `players.csv`: Core player information.
    *   `transfers.csv`: Player transfer history.
    *   `player_valuations.csv`: Historical market valuations (e.g., from Transfermarkt).
    *   `appearances.csv`: Player match appearance data (minutes, goals, assists).
    *   `competitions.csv`: Information about leagues and competitions.
*   All columns are initially read as character type to ensure controlled parsing later.

### 3.2 Clean Numerics & Dates
*   `transfer_fee` and `market_value_in_eur` are converted to numeric using `parse_euro`.
*   Date columns (`date_of_birth`, `contract_expiration_date`, `transfer_date`, `date` in valuations, `match_date` from appearances) are converted to `Date` objects using a helper function that attempts multiple common date formats.
*   Performance statistics from `appearances` (`minutes_played`, `goals`, `assists`) are converted to numeric.

### 3.3 Latest Market Value (MV) & Transfer Fee
*   For each player, the most recent `market_value` is extracted from `valuations`.
*   Similarly, the most recent `transfer_fee` is extracted from `transfers`.

### 3.4 One-Year Performance Snapshot & League Strength
This section introduces performance metrics and adjusts for league difficulty.

*   **Performance Aggregation (`perf` table)**:
    *   Filters `appearances` for matches within the last 365 days.
    *   For each player, it calculates:
        *   `season_minutes`: Total minutes played.
        *   `season_goals`: Total goals scored.
        *   `season_assists`: Total assists provided.
        *   `league_main`: The primary league the player participated in (determined by the most frequent league name in their appearances).
*   **League Strength Coefficients (`league_strength` table & join)**:
    *   A predefined `tribble` (a way to create a small data frame inline) assigns strength coefficients to major leagues (e.g., Premier League = 1.0, Bundesliga = 0.95, Other = 0.60).
    *   This `league_strength` is joined to the `perf` data based on `league_main`. A default coefficient (0.60) is applied if a league is not in the predefined list or if `league_main` is NA (then categorized as "Other").
    *   `league_coef`: The resulting league strength coefficient for the player.
    *   `prod_per90`: Player productivity (goals + assists) per 90 minutes, calculated as `(season_goals + season_assists) / (season_minutes / 90)`. If `season_minutes` is 0, `prod_per90` will be NA (or 0 depending on handling).

### 3.5 Assemble Master Dataframe (`df`)
*   The main `df` is created starting with player attributes from `players.csv`.
*   `age` and `contract_remaining_years` are calculated.
*   The `last_val`, `last_fee`, and the processed performance data (`perf` including `league_coef` and `prod_per90`) are all `left_join`ed to this `df`.

### 3.6 Fair Market Value (FMV) Model (Log-Linear Regression)
This version uses a more sophisticated linear model for FMV estimation, incorporating performance data.

1.  **Target Variable**:
    *   `log_market_value`: The natural logarithm of `market_value` plus 1 (`log1p(market_value)`) is used as the target variable for the regression. This is done only if `market_value > 0`.

2.  **Model Specification**:
    *   A linear model (`lm()`) is fitted:
        `log_market_value ~ age + position + contract_remaining_years + I(season_minutes/1000) + prod_per90 * league_coef`
    *   **Predictors**:
        *   `age`: Player's age.
        *   `position`: Player's position (categorical).
        *   `contract_remaining_years`: Contract duration.
        *   `I(season_minutes/1000)`: Total season minutes played (scaled by dividing by 1000 for better coefficient interpretability). The `I()` function ensures the division is performed before being included in the model formula.
        *   `prod_per90 * league_coef`: An interaction term between player productivity per 90 minutes and their league's strength coefficient. This allows the model to value productivity differently based on the league's toughness (e.g., a goal in a top league might be valued higher).
    *   The model (`model_fmv`) is only trained if there are more than 30 rows with valid `log_market_value` and predictor data. `na.action = na.exclude` is used.

3.  **Prediction**:
    *   If `model_fmv` is successfully trained, predictions are made on the full `df`.
    *   The `position` factor in the prediction data is aligned with the levels from the training data (`model_fmv$xlevels$position`) to prevent errors.
    *   `expm1()` transforms the log-scale predictions back to the original monetary scale for `fair_market_value_model`.
    *   If model training fails or conditions aren't met, `fair_market_value_model` is initially set to `NA_real_`.

### 3.7 Age Corridor & Opportunity Metric/Flag
This section refines the FMV and calculates the final scouting metrics.

*   **Age Corridor**:
    *   Similar to the previous version, `upper` and `lower` bounds for FMV are calculated based on age-specific multipliers applied to the player's `market_value`.
    *   The `fair_market_value_model` is then clamped to stay within this corridor: `pmin(pmax(fair_market_value_model, lower), upper)`.
    *   **Fallback**: If `fair_market_value_model` is still `NA` after the model prediction and corridor application (e.g., if `market_value` was `NA`, making the corridor bounds `NA`), it's set to `market_value * 1.10` as a final fallback.
*   **Ratios & Scores**:
    *   `mv_to_fmv_ratio`: `market_value / fair_market_value_model`.
    *   **Performance Scores (`minutes_sc`, `prod_sc`)**:
        *   `minutes_sc`: A scaled score (0, 0.3, 0.6, 1) based on `season_minutes` played. More minutes get a higher score.
        *   `prod_sc`: A scaled score (0, 0.3, 0.6, 1) based on `prod_per90`. Higher productivity gets a higher score.
    *   **`opportunity_score` (New Formula)**:
        `round(0.45*(1 - mv_to_fmv_ratio) + 0.25*(30 - age)/30 + 0.15*minutes_sc + 0.15*prod_sc, 2)`
        This score is now a weighted sum:
        *   `45%` weight: Player being cheaper than FMV (`1 - mv_to_fmv_ratio`). A higher value here means `market_value` is lower than `fair_market_value_model`.
        *   `25%` weight: Youthfulness (`(30 - age)/30`). Players younger than 30 get a positive contribution.
        *   `15%` weight: Reliable minutes played (`minutes_sc`).
        *   `15%` weight: Productivity score (`prod_sc`).
*   **`opportunity_flag` (Valuation based on `opportunity_score`)**:
    *   The flag is now interpreted as a valuation status based on the new `opportunity_score`:
        *   `opportunity_score < 0`: "overvalued" (implies a poor opportunity based on the score's components)
        *   `opportunity_score < 0.40` (and >= 0): "fair valued"
        *   `opportunity_score >= 0.40`: "undervalued" (implies a good opportunity)
    *   **Interpretation and Limitations of this `opportunity_flag`**:
        *   This flag is an **indirect valuation**. It's not a direct comparison of a transaction price to FMV. Instead, it reflects whether the player represents a "good opportunity" according to the weighted `opportunity_score` formula.
        *   A player flagged as "undervalued" here means their combined profile (age, market value relative to model FMV, minutes, productivity) suggests they are a good target. It primarily indicates that their *current market value* might be low for their modeled FMV and performance/age profile.
        *   "Overvalued" suggests the opposite – their current market value might be high relative to their FMV and performance/age, or they are older with less upside according to the score.
        *   This is different from a flag that would be set by comparing an *actual transfer fee paid* to the FMV (which was done in some previous iterations of the code). This version's flag is more about assessing current market standing versus a modeled fair value.
        *   The thresholds (`<0`, `<0.40`) are subjective and determine the distribution of these flags. They may need adjustment based on the typical range of `opportunity_score` values observed in the dataset to provide meaningful categorizations.

The `prep_data()` function returns the final `df`.

*(The R code for data preparation is located in the `app.R` file.)*

## 4. Theme

*   A Bootstrap 5 theme is initialized using `bslib::bs_theme(bootswatch = "pulse")`.
*   Custom styles from `www/soccer.scss` are applied if the file exists.

*(The R code for theming is located in the `app.R` file.)*

## 5. User Interface (UI)

The UI structure remains similar to previous versions, with a filter sidebar and a main display area.

*   **Filter Inputs**: Sliders and numeric inputs for `age` and `transfer_fee`; checkbox groups for `position` and the `opportunity_flag` (now labeled "Valuation" in the UI, with choices "undervalued", "fair valued", "overvalued").
*   **Main Area**: Player header card with value boxes (FMV, Market Value, and a value box now explicitly labeled "Valuation" displaying the `opportunity_flag` string and an icon based on it). Tabs for the Plotly bubble chart and the DT data table.

*(The R code for the UI is located in the `app.R` file.)*

## 6. Server Logic

The server function handles interactivity.

*   **Filter Synchronization**: Logic for syncing sliders with numeric inputs and for "select/unselect all" checkbox links.
*   **Reset Button**: Resets all filters.
*   **Reactive Filtering (`filt`)**: Filters `master_data` based on user inputs.
*   **Selected Player Tracking (`sel_id`, `player`)**: Manages which player is currently selected via plot clicks or table row selections.
*   **Output Rendering**:
    *   `renderUI` for value boxes and the player header. The "Valuation" value box (`output$vb_opp`) now displays the `opportunity_flag` string and changes its icon/theme based on whether the flag is "undervalued", "fair valued", or "overvalued".
    *   `renderPlotly` for the bubble chart.
    *   `renderDT` for the data table. The "Flag" column in the table displays the `opportunity_flag`.

*(The R code for the Server logic is located in the `app.R` file.)*

## 7. Launch the App

*   `shinyApp(ui, server)` starts the application.

*(The R code for launching the app is located in the `app.R` file.)*
