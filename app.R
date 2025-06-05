# -------------------- Packages --------------------
library(shiny)
library(tidyverse) 
library(plotly)
library(DT)
library(lubridate)
library(scales)
library(bslib)

# -------------------- Helper Functions --------------------

prep_data <- function(players_path = "players.csv",
                      transfers_path = "transfers.csv",
                      valuations_path = "player_valuations.csv",
                      clubs_path = "clubs.csv",
                      appearances_path = "appearances.csv") {
  
  # Robust function to read CSV files, handling potential errors.
  # Reads all columns as character initially to prevent type inference issues.
  read_robust_csv <- function(path, name) {
    tryCatch(
      readr::read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE),
      error = function(e) {
        # If reading fails, issue a warning and return an empty placeholder tibble.
        warning(paste("Error loading", name, "from", path, ":", e$message), call. = FALSE)
        tibble(placeholder_file_not_loaded = character()) 
      }
    )
  }
  # Load raw data from CSV files.
  players_raw <- read_robust_csv(players_path, "players.csv")
  transfers_raw_input <- read_robust_csv(transfers_path, "transfers.csv")
  player_valuations_raw <- read_robust_csv(valuations_path, "player_valuations.csv")
  clubs_raw <- read_robust_csv(clubs_path, "clubs.csv")
  appearances_raw <- read_robust_csv(appearances_path, "appearances.csv")
  
  # Process transfers data.
  transfers <- transfers_raw_input
  if (nrow(transfers) > 0) {
    if ("transfer_fee" %in% names(transfers)) {
      transfers <- transfers %>% mutate(transfer_fee = as.numeric(transfer_fee)) 
    } else {
      # Warn if 'transfer_fee' column is missing and initialize it with NA.
      warning("Column 'transfer_fee' not in 'transfers_raw_input'. Initializing with NA.", call. = FALSE)
      transfers <- transfers %>% mutate(transfer_fee = NA_real_)
    }
  } else {
    # If transfers data is empty, create a skeleton tibble.
    transfers <- tibble(player_id = character(), transfer_date = as.Date(NA), 
                        transfer_fee = NA_real_, from_club_id = character(), to_club_id = character()) 
  }
  
  # Safely convert columns to date format.
  safe_as_date <- function(date_vector, col_name) {
    if (is.null(date_vector) || length(date_vector) == 0 || all(is.na(date_vector))) return(as.Date(rep(NA, length(date_vector))))
    parsed_dates <- lubridate::as_date(date_vector)
    # Warn if some values couldn't be parsed as dates.
    if (sum(is.na(parsed_dates)) > sum(is.na(date_vector))) warning(paste("Some values in '", col_name, "' could not be interpreted as dates."), call. = FALSE)
    return(parsed_dates)
  }
  
  # Process players data.
  players <- players_raw
  if (nrow(players) > 0) {
    if("date_of_birth" %in% names(players)) players <- players %>% mutate(date_of_birth = safe_as_date(date_of_birth, "date_of_birth")) else players$date_of_birth = as.Date(NA)
    if("contract_expiration_date" %in% names(players)) players <- players %>% mutate(contract_expiration_date = safe_as_date(contract_expiration_date, "contract_expiration_date")) else players$contract_expiration_date = as.Date(NA)
  } else {
    players <- tibble(player_id = character(), name=character(), date_of_birth = as.Date(NA), contract_expiration_date = as.Date(NA), position=character(), country_of_citizenship=character()) 
  }
  
  # Process player valuations data.
  player_valuations <- player_valuations_raw
  if (nrow(player_valuations) > 0) {
    if("date" %in% names(player_valuations)) player_valuations <- player_valuations %>% mutate(date = safe_as_date(date, "valuation_date")) else player_valuations$date = as.Date(NA)
    if("market_value_in_eur" %in% names(player_valuations)) player_valuations$market_value_in_eur = as.numeric(player_valuations$market_value_in_eur) else player_valuations$market_value_in_eur = NA_real_
  } else {
    player_valuations <- tibble(player_id = character(), date = as.Date(NA), market_value_in_eur = NA_real_) 
  }
  
  # Convert transfer_date in transfers data.
  if (nrow(transfers) > 0 && "transfer_date" %in% names(transfers)) {
    transfers <- transfers %>% mutate(transfer_date = safe_as_date(transfer_date, "transfer_date"))
  } else if (nrow(transfers) > 0) { 
    transfers$transfer_date = as.Date(NA)
  }
  
  # Process appearances data.
  appearances <- appearances_raw
  if (nrow(appearances) > 0) {
    if("date" %in% names(appearances)) appearances <- appearances %>% mutate(appearance_date = safe_as_date(date, "appearance_date")) else appearances$appearance_date = as.Date(NA)
    if(!"game_id" %in% names(appearances)) appearances$game_id <- NA_character_ 
    appearances <- appearances %>% mutate(across(c(goals, assists, minutes_played), as.numeric))
  } else {
    appearances <- tibble(player_id=character(), appearance_date = as.Date(NA), goals=NA_real_, assists=NA_real_, minutes_played=NA_real_, game_id=NA_character_) 
  }
  
  # Create player dataframe with age and selected columns.
  req_cols_player_df <- c("player_id", "name", "date_of_birth", "position", "country_of_citizenship")
  if (nrow(players) > 0 && all(req_cols_player_df %in% names(players))) {
    player_df <- players %>% 
      mutate(age = if_else(is.na(date_of_birth), NA_real_, as.numeric(difftime(Sys.Date(), date_of_birth, units = "days")) / 365.25)) %>%
      select(player_id, name, age, position, nationality = country_of_citizenship)
  } else { 
    player_df <- tibble(player_id = character(), name = character(), age = NA_real_ , position = character(), nationality = character()) 
  }
  
  # Get last valuation for each player.
  req_cols_last_val <- c("player_id", "date", "market_value_in_eur")
  if (nrow(player_valuations) > 0 && all(req_cols_last_val %in% names(player_valuations))) {
    last_valuation <- player_valuations %>% 
      filter(!is.na(player_id) & !is.na(date) & !is.na(market_value_in_eur)) %>% 
      group_by(player_id) %>%
      filter(date == max(date, na.rm = TRUE)) %>% slice_head(n = 1) %>% ungroup() %>%
      select(player_id, market_value = market_value_in_eur, valuation_date = date)
  } else { 
    last_valuation <- tibble(player_id = character(), market_value = NA_real_, valuation_date = as.Date(NA)) 
  }
  
  # Function to determine the football season based on a date.
  get_season <- function(date_vec) {
    if (is.null(date_vec) || length(date_vec) == 0) return(rep(NA_character_, length(date_vec)))
    season_str <- rep(NA_character_, length(date_vec)); valid_indices <- !is.na(date_vec) 
    if (any(valid_indices)) {
      year_val <- lubridate::year(date_vec[valid_indices]); month_val <- lubridate::month(date_vec[valid_indices])
      # Season typically runs from July to June of next year.
      season_str[valid_indices] <- dplyr::if_else(month_val >= 7, paste0(year_val, "/", year_val + 1), paste0(year_val - 1, "/", year_val))
    }
    return(season_str)
  }
  
  # Aggregate player performance from the previous season relative to their transfer date.
  player_performance_prev_season <- tibble() 
  req_cols_appearances <- c("player_id", "appearance_date", "goals", "assists", "minutes_played", "game_id")
  
  if (nrow(appearances) > 0 && all(req_cols_appearances %in% names(appearances)) &&
      nrow(transfers) > 0 && "transfer_date" %in% names(transfers) && "player_id" %in% names(transfers)) {
    appearances_with_season <- appearances %>% filter(!is.na(appearance_date)) %>% mutate(appearance_season = get_season(appearance_date))
    # Identify the previous season for each transfer.
    transfers_with_prev_season_id <- transfers %>%
      filter(!is.na(transfer_date) & !is.na(player_id)) %>%
      mutate(transfer_actual_season = get_season(transfer_date),
             transfer_season_start_year = as.numeric(str_extract(transfer_actual_season, "^\\d{4}"))) %>%
      filter(!is.na(transfer_season_start_year)) %>% 
      mutate(prev_season_id = paste0(transfer_season_start_year - 1, "/", transfer_season_start_year)) %>%
      distinct(player_id, transfer_date, prev_season_id) 
    
    if (nrow(appearances_with_season) > 0 && nrow(transfers_with_prev_season_id) > 0) {
      # Summarize performance by season.
      performance_by_season <- appearances_with_season %>%
        filter(!is.na(appearance_season)) %>% 
        group_by(player_id, appearance_season) %>%
        summarise(prev_season_goals = sum(goals, na.rm = TRUE), prev_season_assists = sum(assists, na.rm = TRUE),
                  prev_season_minutes = sum(minutes_played, na.rm = TRUE), prev_season_games = n_distinct(game_id, na.rm = TRUE), .groups = "drop")
      # Join performance data with transfers based on the previous season ID.
      player_performance_prev_season <- transfers_with_prev_season_id %>%
        left_join(performance_by_season, by = c("player_id", "prev_season_id" = "appearance_season")) %>%
        mutate(across(c(prev_season_goals, prev_season_assists, prev_season_minutes, prev_season_games), ~replace_na(., 0))) %>%
        select(player_id, transfer_date, prev_season_goals, prev_season_assists, prev_season_minutes, prev_season_games)
      if(nrow(player_performance_prev_season) > 0) message("Previous season performance data aggregated.")
    }
  } 
  # If no previous season performance data, create a skeleton.
  if(nrow(player_performance_prev_season) == 0) { 
    player_performance_prev_season <- tibble(player_id = character(), transfer_date = as.Date(NA), prev_season_goals = NA_real_, 
                                             prev_season_assists = NA_real_, prev_season_minutes = NA_real_, prev_season_games = NA_real_)
  }
  
  # Get last transfer details for each player, including contract information.
  req_cols_last_transfer <- c("player_id", "transfer_date", "transfer_fee", "from_club_id", "to_club_id")
  if (nrow(transfers) > 0 && all(req_cols_last_transfer %in% names(transfers))) {
    player_contracts <- if(nrow(players) > 0 && "player_id" %in% names(players) && "contract_expiration_date" %in% names(players)) {
      players %>% select(player_id, contract_expiration_date) %>% filter(!is.na(contract_expiration_date)) %>% distinct(player_id, .keep_all = TRUE)
    } else { tibble(player_id = character(), contract_expiration_date = as.Date(NA)) }
    
    last_transfer_base <- transfers %>% filter(!is.na(player_id) & !is.na(transfer_date)) %>% group_by(player_id) %>%
      filter(transfer_date == max(transfer_date, na.rm = TRUE)) %>% slice_head(n = 1) %>% ungroup()
    
    # Calculate remaining contract years at the time of transfer.
    lt <- last_transfer_base %>% left_join(player_contracts, by = "player_id") %>%
      mutate(contract_remaining_days = if_else(!is.na(contract_expiration_date) & !is.na(transfer_date), as.numeric(difftime(contract_expiration_date, transfer_date, units = "days")), NA_real_),
             contract_remaining_years = case_when(is.na(contract_remaining_days) ~ NA_real_, contract_remaining_days < 0 ~ 0, TRUE ~ pmin(contract_remaining_days / 365.25, 5))) # Cap at 5 years
    
    # Join with previous season performance.
    if(nrow(player_performance_prev_season) > 0 && "player_id" %in% names(player_performance_prev_season) && "transfer_date" %in% names(player_performance_prev_season)){
      lt <- lt %>% left_join(player_performance_prev_season, by = c("player_id", "transfer_date"))
    } else { 
      lt <- lt %>% mutate(prev_season_goals = NA_real_, prev_season_assists = NA_real_, prev_season_minutes = NA_real_, prev_season_games = NA_real_)
    }
    last_transfer <- lt %>% select(player_id, transfer_fee, transfer_date, from_club_id, to_club_id, contract_remaining_years, 
                                   any_of(c("prev_season_goals", "prev_season_assists", "prev_season_minutes", "prev_season_games"))) 
  } else { 
    last_transfer <- tibble(player_id = character(), transfer_fee = NA_real_, transfer_date = as.Date(NA), from_club_id = character(), 
                            to_club_id = character(), contract_remaining_years = NA_real_, prev_season_goals = NA_real_, 
                            prev_season_assists = NA_real_, prev_season_minutes = NA_real_, prev_season_games = NA_real_)
  }
  
  # Process clubs data.
  clubs <- if(nrow(clubs_raw) > 0 && "club_id" %in% names(clubs_raw) && "name" %in% names(clubs_raw)){ 
    clubs_raw %>% select(club_id, club_name = name) 
  } else { tibble(club_id = character(), club_name = character())}
  
  # Join all processed dataframes.
  if (nrow(player_df) > 0) {
    data_intermediate <- player_df %>% 
      left_join(last_valuation, by = "player_id") %>%
      left_join(last_transfer,  by = "player_id") %>%
      filter(!is.na(age)) 
  } else { data_intermediate <- tibble() }
  
  # Define expected output columns and create a skeleton dataframe with correct types.
  expected_data_out_cols <- c("player_id", "name", "age", "position", "nationality", "market_value", "valuation_date", "transfer_fee", "transfer_date", "from_club_id", "to_club_id", "club_name", "contract_remaining_years", "prev_season_goals", "prev_season_assists", "prev_season_minutes", "prev_season_games", "log_market_value", "market_value_plot", "fair_market_value_model", "predicted_transaction_fee", "mv_to_fair_value_ratio", "fee_to_fair_value_ratio", "potential_score_age", "performance_score_minutes", "productivity_per_90", "performance_score_productivity", "opportunity_score", "opportunity_flag", "transaction_valuation_status", "inv_age_x_minutes", "inv_age_x_goals_assists") 
  data_out_skeleton <- setNames(data.frame(matrix(ncol = length(expected_data_out_cols), nrow = 0)), expected_data_out_cols) %>% 
    mutate(across(c(player_id, name, position, nationality, from_club_id, to_club_id, club_name, opportunity_flag, transaction_valuation_status), as.character),
           across(c(age, market_value, transfer_fee, contract_remaining_years, prev_season_goals, prev_season_assists, prev_season_minutes, prev_season_games, log_market_value, market_value_plot, fair_market_value_model, predicted_transaction_fee, mv_to_fair_value_ratio, fee_to_fair_value_ratio, potential_score_age, performance_score_minutes, productivity_per_90, performance_score_productivity, opportunity_score, inv_age_x_minutes, inv_age_x_goals_assists), as.numeric), 
           across(c(valuation_date, transfer_date), as.Date))
  data_out <- data_out_skeleton 
  
  # Populate the output dataframe and perform initial transformations.
  if (nrow(data_intermediate) > 0) {
    temp_data_out <- data_intermediate %>% 
      left_join(clubs, by = c("to_club_id" = "club_id")) %>% # Join with club names
      mutate(position = ifelse(is.na(position) | position == "", "Unknown", as.character(position)), # Handle missing positions
             market_value = as.numeric(market_value), 
             market_value_plot = ifelse(is.na(market_value) | market_value <= 0, 1, market_value), # For plot size, avoid 0 or NA
             log_market_value = ifelse(!is.na(market_value) & market_value > 0, log1p(market_value), NA_real_), # Log-transform market value
             across(any_of(c("prev_season_goals", "prev_season_assists", "prev_season_minutes", "prev_season_games")), ~replace_na(as.numeric(.), 0))) 
    data_out <- dplyr::bind_rows(data_out_skeleton, temp_data_out) 
  } else { warning("No usable intermediate data after joins.", call. = FALSE) }
  
  # Create interaction terms for models (inverse age * performance metrics).
  if (nrow(data_out) > 0) {
    data_out <- data_out %>%
      mutate(
        age = as.numeric(age), 
        prev_season_minutes = as.numeric(prev_season_minutes),
        prev_season_goals = as.numeric(prev_season_goals),
        prev_season_assists = as.numeric(prev_season_assists),
        inv_age_x_minutes = ifelse(age > 0, (1 / (age + 0.001)) * prev_season_minutes, 0), # Added small constant to age to avoid division by zero
        inv_age_x_goals_assists = ifelse(age > 0, (1 / (age + 0.001)) * (prev_season_goals + prev_season_assists), 0)
      )
  } else { 
    data_out$inv_age_x_minutes <- NA_real_
    data_out$inv_age_x_goals_assists <- NA_real_
  }
  
  # Model for Fair Market Value (Linear Model on log_market_value).
  model_fair_mv <- NULL 
  predictors_fair_mv <- c("age", "position", "contract_remaining_years", "prev_season_minutes", "prev_season_goals", "prev_season_assists", "inv_age_x_minutes", "inv_age_x_goals_assists") 
  actual_predictors_fair_mv <- intersect(predictors_fair_mv, names(data_out)) 
  
  if("log_market_value" %in% names(data_out) && length(actual_predictors_fair_mv) >= 2 && nrow(data_out) > 10){
    data_for_fair_mv <- data_out %>%  
      filter(!is.na(log_market_value), complete.cases(across(all_of(actual_predictors_fair_mv))), position != "Unknown") %>% # Ensure no NAs in predictors or target
      mutate(position = factor(position)) 
    if(nrow(data_for_fair_mv) > (length(actual_predictors_fair_mv) + 10) && n_distinct(data_for_fair_mv$position) > 1){ # Check for sufficient data and variance
      formula_fair_mv_str <- paste("log_market_value ~", paste(actual_predictors_fair_mv, collapse=" + ")) 
      model_fair_mv <- tryCatch(lm(as.formula(formula_fair_mv_str), data = data_for_fair_mv, na.action = na.exclude), error = function(e){ warning(paste("Fair MV Model failed:",e$message), call.=F); NULL})
      if(!is.null(model_fair_mv)) message("Fair MV Model (with interactions) trained.")
    } else { warning("Not enough data/variance for Fair MV Model.", call.=F)}
  } else { warning("Missing columns/data for Fair MV Model.", call.=F)}
  
  # Predict Fair Market Value using the trained model.
  data_out$fair_market_value_model <- NA_real_ 
  if (!is.null(model_fair_mv) && nrow(data_out) > 0) {
    predict_data_fair_mv <- data_out 
    # Ensure factor levels for position match those in the model.
    if ("position" %in% names(predict_data_fair_mv) && "position" %in% names(model_fair_mv$xlevels) && !is.null(model_fair_mv$xlevels[["position"]])) { 
      predict_data_fair_mv$position <- factor(predict_data_fair_mv$position, levels = model_fair_mv$xlevels[["position"]])
    }
    predicted_log_fmv <- tryCatch(predict(model_fair_mv, newdata = predict_data_fair_mv, na.action = na.pass), error = function(e) {warning(paste("Pred Fair MV:", e$message), call. = F); rep(NA_real_, nrow(predict_data_fair_mv))}) 
    data_out$fair_market_value_model <- ifelse(is.na(predicted_log_fmv), NA_real_, expm1(predicted_log_fmv)) # Back-transform from log scale
    data_out <- data_out %>% mutate(fair_market_value_model = ifelse(!is.na(fair_market_value_model) & fair_market_value_model < 0, 0, fair_market_value_model)) # Ensure non-negative values
  }
  
  # Model for Predicted Transaction Fee (GLM with Gamma distribution).
  model_transaction_fee <- NULL
  predictors_transaction_fee <- c("age", "market_value", "position", "contract_remaining_years", "prev_season_minutes", "prev_season_goals", "prev_season_assists")
  actual_predictors_transaction_fee <- intersect(predictors_transaction_fee, names(data_out))
  
  if("transfer_fee" %in% names(data_out) && length(actual_predictors_transaction_fee) >= 2 && nrow(data_out) > 10){
    data_for_transaction_fee <- data_out %>%
      filter(!is.na(transfer_fee) & transfer_fee > 0, complete.cases(across(all_of(actual_predictors_transaction_fee))), position != "Unknown") %>% # Filter for valid transfer fees
      mutate(position = factor(position)) 
    if(nrow(data_for_transaction_fee) > (length(actual_predictors_transaction_fee) + 10) && n_distinct(data_for_transaction_fee$position) > 1){
      formula_transaction_fee_str <- paste("transfer_fee ~", paste(actual_predictors_transaction_fee, collapse=" + "))
      model_transaction_fee <- tryCatch(glm(as.formula(formula_transaction_fee_str), data = data_for_transaction_fee, family = Gamma(link = "log"), na.action = na.exclude, control = glm.control(epsilon = 1e-7, maxit = 100, trace = FALSE)), error = function(e){ warning(paste("GLM Trans. Fee failed:", e$message), call.=F); NULL })
      if(!is.null(model_transaction_fee)) message("GLM Trans. Fee trained.")
    } else {warning("Not enough data/variance for Trans.Fee GLM.", call.=F)}
  } else {warning("Missing columns/data for Trans.Fee GLM.", call.=F)}
  
  # Predict Transaction Fee using the trained GLM.
  data_out$predicted_transaction_fee <- NA_real_ 
  if (!is.null(model_transaction_fee) && nrow(data_out) > 0) {
    predict_data_transaction <- data_out
    if ("position" %in% names(predict_data_transaction) && "position" %in% names(model_transaction_fee$xlevels) && !is.null(model_transaction_fee$xlevels[["position"]])) {
      predict_data_transaction$position <- factor(predict_data_transaction$position, levels = model_transaction_fee$xlevels[["position"]])
    }
    predicted_fees <- tryCatch(predict(model_transaction_fee, newdata = predict_data_transaction, type = "response", na.action = na.pass), error = function(e) {warning(paste("Pred Trans. Fee:", e$message), call. = F); rep(NA_real_, nrow(predict_data_transaction))})
    data_out$predicted_transaction_fee <- predicted_fees
    data_out <- data_out %>% mutate(predicted_transaction_fee = ifelse(!is.na(predicted_transaction_fee) & predicted_transaction_fee < 0, 0, predicted_transaction_fee)) # Ensure non-negative
  }
  
  # Initialize metrics columns if they don't exist.
  metrics_cols <- c("mv_to_fair_value_ratio", "fee_to_fair_value_ratio", "potential_score_age", "performance_score_minutes", "productivity_per_90", "performance_score_productivity", "opportunity_score")
  for(col_add in metrics_cols) {
    if(!col_add %in% names(data_out)) data_out[[col_add]] <- NA_real_
  }
  if(!"opportunity_flag" %in% names(data_out)) data_out$opportunity_flag <- NA_character_
  if(!"transaction_valuation_status" %in% names(data_out)) data_out$transaction_valuation_status <- NA_character_
  
  # Calculate various scores and flags based on data and model outputs.
  if ("fair_market_value_model" %in% names(data_out) && nrow(data_out) > 0) {
    data_out <- data_out %>%
      mutate(across(c(market_value, fair_market_value_model, transfer_fee, prev_season_minutes, prev_season_goals, prev_season_assists, age), as.numeric),
             mv_to_fair_value_ratio = ifelse(fair_market_value_model > 0 & !is.na(market_value), market_value / fair_market_value_model, NA_real_), 
             fee_to_fair_value_ratio = ifelse(fair_market_value_model > 0 & !is.na(transfer_fee), transfer_fee / fair_market_value_model, NA_real_), 
             potential_score_age = case_when(age <= 20 ~ 4, age <= 22 ~ 3, age <= 24 ~ 2, age <= 26 ~ 1, TRUE ~ 0),
             performance_score_minutes = case_when(is.na(prev_season_minutes) ~ 0, prev_season_minutes >= 2500 ~ 4, prev_season_minutes >= 1800 ~ 3, prev_season_minutes >= 1000 ~ 2, prev_season_minutes > 200 ~ 1, TRUE ~ 0),
             productivity_per_90 = ifelse(!is.na(prev_season_minutes) & prev_season_minutes > 0, (replace_na(prev_season_goals, 0) + replace_na(prev_season_assists, 0)) / (prev_season_minutes / 90), 0),
             performance_score_productivity = case_when(is.na(productivity_per_90) ~ 0, productivity_per_90 >= 0.6 ~ 3, productivity_per_90 >= 0.4 ~ 2, productivity_per_90 >= 0.2 ~ 1, TRUE ~ 0)) %>% 
      mutate(opportunity_score = round((potential_score_age * 0.35) + (performance_score_minutes * 0.15) + (performance_score_productivity * 0.15) + (ifelse(!is.na(mv_to_fair_value_ratio) & mv_to_fair_value_ratio <= 0.8, 2, ifelse(!is.na(mv_to_fair_value_ratio) & mv_to_fair_value_ratio <= 1, 1, 0)) * 0.2) + (ifelse(!is.na(fee_to_fair_value_ratio) & fee_to_fair_value_ratio <= 0.7, 2, ifelse(!is.na(fee_to_fair_value_ratio) & fee_to_fair_value_ratio <= 1, 1, 0)) * 0.15), 1), 
             opportunity_flag = case_when(
               is.na(opportunity_score) ~ NA_character_,
               opportunity_score >= 2.0 ~ "High Priority Target",
               opportunity_score >= 1.2 ~ "Notable Prospect",
               TRUE ~ "Standard Profile"
             ),
             transaction_valuation_status = case_when( 
               !is.na(fair_market_value_model) & !is.na(transfer_fee) & transfer_fee < 0.9 * fair_market_value_model ~ "Undervalued (vs Fair MV)",
               !is.na(fair_market_value_model) & !is.na(transfer_fee) & transfer_fee > 1.1 * fair_market_value_model ~ "Overvalued (vs Fair MV)",
               !is.na(fair_market_value_model) & !is.na(transfer_fee) ~ "Fairly Valued (vs Fair MV)",
               TRUE ~ NA_character_
             )
      )
  }
  
  # Ensure all expected columns are present in the final dataframe, adding NA columns if missing.
  current_cols <- names(data_out)
  missing_final_cols <- setdiff(expected_data_out_cols, current_cols)
  if(length(missing_final_cols) > 0) {
    na_df_cols <- setNames(data.frame(matrix(NA, nrow = nrow(data_out), ncol = length(missing_final_cols))), missing_final_cols)
    for(m_col in missing_final_cols) {
      na_df_cols[[m_col]] <- methods::as(na_df_cols[[m_col]], class(data_out_skeleton[[m_col]])) # Match type from skeleton
    }
    data_out <- bind_cols(data_out, na_df_cols)
  }
  # Select and order columns as defined in expected_data_out_cols.
  data_out <- data_out %>% select(all_of(expected_data_out_cols))
  return(data_out)
}

# Function to create the bubble plot using plotly.
make_bubble_plot <- function(plot_data_raw) {
  plot_data <- plot_data_raw %>%
    filter(!is.na(age) & !is.na(transfer_fee) & !is.na(position) & !is.na(market_value_plot)) # Filter NAs for essential plot aesthetics
  
  if (nrow(plot_data) == 0) {
    # Return an empty plot if no data after filtering.
    return(plotly_empty(type = "scatter", mode = "markers") %>% layout(title = list(text = "No data for Bubble Plot after NA filtering.", yref="paper", y=0.5)))
  }
  
  # Ensure all columns needed for hover text and plot aesthetics are present.
  cols_for_hover_and_plot <- c("name", "club_name", "market_value", "transfer_fee", "fair_market_value_model", "transaction_valuation_status", "age", "position", "market_value_plot", "contract_remaining_years", "prev_season_goals", "prev_season_assists", "prev_season_minutes", "opportunity_score", "opportunity_flag", "player_id")
  for(col_p in cols_for_hover_and_plot){
    if(!col_p %in% names(plot_data)){
      default_val <- if(col_p %in% c("name", "club_name", "transaction_valuation_status", "position", "opportunity_flag", "player_id")) NA_character_ else NA_real_
      plot_data[[col_p]] <- default_val
    }
  }
  
  # Prepare data for plotting, including constructing hover text.
  plot_data_final <- plot_data %>%
    mutate(across(c(market_value, fair_market_value_model, transfer_fee, age, opportunity_score, contract_remaining_years, prev_season_minutes, prev_season_goals, prev_season_assists), as.numeric),
           hover_text = paste(
             "<b>Name:</b>", ifelse(is.na(name), "N/A", name),
             "<br><b>Club:</b>", ifelse(is.na(club_name), "N/A", club_name),
             "<br><b>Position:</b>", ifelse(is.na(position), "N/A", position),
             "<br><b>Age:</b>", ifelse(is.na(age), "N/A", round(age,1)),
             "<br><b>Current Market Value:</b>", ifelse(is.na(market_value), "N/A", scales::dollar(market_value, prefix="€", big.mark=".", decimal.mark=",", accuracy=1)),
             "<br><b>Fair MV (Model):</b>", ifelse(is.na(fair_market_value_model), "N/A", scales::dollar(fair_market_value_model, prefix="€", big.mark=".", decimal.mark=",", accuracy=1)), 
             "<br><b>Transfer Fee:</b>", ifelse(is.na(transfer_fee), "N/A", scales::dollar(transfer_fee, prefix="€", big.mark=".", decimal.mark=",", accuracy=1)),
             "<br><b>Deal Valuation:</b>", ifelse(is.na(transaction_valuation_status), "N/A", transaction_valuation_status), 
             "<br><b>Opportunity Score:</b>", ifelse(is.na(opportunity_score), "N/A", opportunity_score),
             "<br><b>Scouting Flag:</b>", ifelse(is.na(opportunity_flag), "N/A", opportunity_flag),
             "<br><b>Remaining Contract:</b>", ifelse(is.na(contract_remaining_years), "N/A", paste(round(contract_remaining_years,1), "Yrs")),
             "<br><b>Prev. Season Mins:</b>", ifelse(is.na(prev_season_minutes), "N/A", prev_season_minutes),
             "<br><b>Prev. Season G+A:</b>", ifelse(is.na(prev_season_goals) | is.na(prev_season_assists), "N/A", replace_na(prev_season_goals,0) + replace_na(prev_season_assists,0))))
  
  # Create the plotly scatter plot.
  p <- plot_ly(data  = plot_data_final, x = ~age, y = ~transfer_fee, type  = "scatter", mode  = "markers",
               color = ~position, size  = ~market_value_plot, sizes = c(6, 45), # Bubble size based on market_value_plot
               text  = ~hover_text, hoverinfo = "text", customdata = ~player_id, source = "bubblePlotSource", # For click events
               marker = list(sizemin = 6, line = list(width = 0.5, color = 'rgba(0,0,0,0.5)'), opacity = 0.75)) 
  p %>% layout(title = NULL, xaxis = list(title = "Age (Years)", gridcolor = "rgba(0,0,0,0.05)"),
               yaxis = list(title = "Transfer Fee (€)", tickformat = ",.0f", gridcolor = "rgba(0,0,0,0.05)"), 
               legend = list(title = list(text = '<b>Position</b>'), orientation = "h", y = -0.2, x = 0.5, xanchor="center", font = list(size=10)),
               paper_bgcolor = "transparent", plot_bgcolor = "transparent", clickmode = "event+select")
}

# Prepare master data once at the start of the app.
master_data <- prep_data() 
# Flag to indicate if UI can be fully initialized based on data availability.
ui_ready <- !is.null(master_data) && nrow(master_data) > 0 && ncol(master_data) > 0 && ("player_id" %in% names(master_data))

# Define default and dynamic ranges for UI slider inputs.
default_min_age <- 16; default_max_age <- 40
default_min_fee <- 0; default_max_fee <- 100000000 # 100 Million

min_age_val <- if (ui_ready && "age" %in% names(master_data) && any(!is.na(master_data$age))) floor(min(master_data$age, na.rm = TRUE)) else default_min_age
max_age_val <- if (ui_ready && "age" %in% names(master_data) && any(!is.na(master_data$age))) ceiling(max(master_data$age, na.rm = TRUE)) else default_max_age
if (!is.finite(min_age_val) || !is.finite(max_age_val) || min_age_val >= max_age_val) { min_age_val <- default_min_age; max_age_val <- default_max_age }

min_fee_val <- if(ui_ready && "transfer_fee" %in% names(master_data) && any(!is.na(master_data$transfer_fee))) floor(min(master_data$transfer_fee, na.rm=T)) else default_min_fee
max_fee_val <- if(ui_ready && "transfer_fee" %in% names(master_data) && any(!is.na(master_data$transfer_fee))) ceiling(max(master_data$transfer_fee, na.rm=T)) else default_max_fee
if (!is.finite(min_fee_val) || !is.finite(max_fee_val) || min_fee_val >= max_fee_val) { min_fee_val <- default_min_fee; max_fee_val <- default_max_fee; if (max_fee_val <= min_fee_val) max_fee_val <- min_fee_val + 1000000 }

# Define choices for selectInput filters, based on available data.
position_choices <- if (ui_ready && "position" %in% names(master_data) && 
                        n_distinct(master_data$position[!is.na(master_data$position) & master_data$position != "Unknown"]) > 0) { 
  c("All", sort(unique(master_data$position[!is.na(master_data$position) & master_data$position != "Unknown"])))
} else { c("All") }

transaction_valuation_choices <- if (ui_ready && "transaction_valuation_status" %in% names(master_data) && n_distinct(master_data$transaction_valuation_status, na.rm = TRUE) > 0) {
  c("All", sort(unique(na.omit(master_data$transaction_valuation_status))))
} else { 
  c("All", "Undervalued (vs Fair MV)", "Fairly Valued (vs Fair MV)", "Overvalued (vs Fair MV)") 
}

defined_opportunity_flags <- c("High Priority Target", "Notable Prospect", "Standard Profile")
opportunity_choices <- c("All", defined_opportunity_flags)


# Define theme using bslib, including custom SASS if available.
custom_css_path <- "www/custom_styles.scss" # Assumes a 'www' subfolder for static assets
theme_obj <- bslib::bs_theme(version = 5, bootswatch = "pulse", primary = "#00529B", secondary = "#FFC107", base_font = bslib::font_google("Open Sans"), heading_font = bslib::font_google("Montserrat"), font_scale = 0.9)

if (file.exists(custom_css_path)) {
  theme_obj <- theme_obj %>% bslib::bs_add_rules(sass::sass_file(custom_css_path))
} else {
  warning(paste("Custom SASS file not found at:", custom_css_path, ". Proceeding without custom styles from this file."), call. = FALSE)
}

# -------------------- UI Definition --------------------
ui <- page_sidebar(title = "Football Player Scouting Dashboard", theme = theme_obj,
                   # Add custom CSS for value box icons
                   tags$head(
                     tags$style(HTML("
                       .bslib-value-box .bslib-value-box-showcase {
                         max-width: 4em; 
                       }
                       .bslib-value-box .bslib-value-box-showcase svg {
                         width: 100%;
                         height: 100%;
                         max-width: 2.5em; 
                         max-height: 2.5em;
                       }
                     "))
                   ),
                   sidebar = sidebar(position = "left", open = "always", width = 300, bg = "#f8f9fa",
                                     # Accordion for organizing filters.
                                     accordion(id = "filters_accordion", open = TRUE, 
                                               accordion_panel("Base Filters", icon = bsicons::bs_icon("sliders"),
                                                               sliderInput("age", "Age", min = min_age_val, max = max_age_val, value = c(min_age_val, max_age_val), step = 1, ticks = FALSE),
                                                               sliderInput("transfer_fee", "Paid Transfer Fee (€)", min = min_fee_val, max = max_fee_val, value = c(min_fee_val, max_fee_val), step = 100000, pre = "€", sep = ".", ticks=FALSE),
                                                               selectInput("position", "Position", choices = position_choices, selected = "All")),
                                               accordion_panel("Valuation & Scouting Filters", icon = bsicons::bs_icon("search-heart"),
                                                               selectInput("transaction_valuation", "Deal Valuation (vs Fair MV)", choices = transaction_valuation_choices, selected = "All"), 
                                                               selectInput("opportunity", "Scouting Flag", choices = opportunity_choices, selected = "All")))), 
                   # Main layout using columns for cards.
                   # The layout_columns now defines two main rows.
                   layout_columns(
                     col_widths = 12, # Each main element (Player Detail Card, Tabset Card) takes full width
                     row_heights = c(1, 4), # Relative heights: Player Detail smaller, Tabset larger
                     # Card for Player Details and Value Boxes.
                     card(
                       id = "player_detail_card", 
                       style="min-height: 180px; max-height: 250px; overflow-y: auto;", # Allow scrolling if content exceeds max-height
                       card_header(uiOutput("playerDetailCardHeader")),
                       card_body(
                         padding = "10px", 
                         layout_columns(col_widths = c(4,4,4), gap="10px", 
                                        uiOutput("vbPlayerFairMV"), 
                                        uiOutput("vbPlayerCurrentMV"), 
                                        uiOutput("vbPlayerOpportunityScore")), 
                         uiOutput("playerDetailExtraInfo")
                       )
                     ),
                     # Tabset Card for Bubble Plot and Detail Table
                     navset_card_tab(
                       id = "main_data_view",
                       full_screen = TRUE, # Allow the entire tabset card to go full screen
                       title = "Data Exploration",
                       nav_panel(
                         title = "Player Overview Plot",
                         icon = bsicons::bs_icon("bar-chart-line-fill"),
                         plotlyOutput("bubblePlot", height = "calc(100vh - 350px)") # Dynamic height calculation
                       ),
                       nav_panel(
                         title = "Detailed Player Table",
                         icon = bsicons::bs_icon("table"),
                         DTOutput("playerTable")
                       )
                     )
                   ), 
                   # Footer.
                   tags$footer( class = "text-center text-muted mt-3 mb-2", "Dashboard v2.9.5 | Data-driven Scouting Insights")) # Incremented version

# -------------------- Server Logic --------------------
server <- function(input, output, session) {
  # Reactive expression to filter data based on user inputs.
  filtered_data <- reactive({
    req(ui_ready, master_data, nrow(master_data) > 0) # Ensure data is loaded and ready
    d_filtered <- master_data
    # Apply age filter.
    if(!is.na(input$age[1])) d_filtered <- d_filtered %>% filter(age >= input$age[1])
    if(!is.na(input$age[2])) d_filtered <- d_filtered %>% filter(age <= input$age[2])
    # Apply transfer fee filter.
    if("transfer_fee" %in% names(d_filtered) && any(!is.na(d_filtered$transfer_fee))){
      if(!is.na(input$transfer_fee[1])) d_filtered <- d_filtered %>% filter(is.na(transfer_fee) | transfer_fee >= input$transfer_fee[1])
      if(!is.na(input$transfer_fee[2])) d_filtered <- d_filtered %>% filter(is.na(transfer_fee) | transfer_fee <= input$transfer_fee[2])
    }
    # Apply position filter.
    if (input$position != "All") d_filtered <- d_filtered %>% filter(position == input$position) 
    # Apply transaction valuation filter.
    if (input$transaction_valuation != "All" && "transaction_valuation_status" %in% names(d_filtered)) d_filtered <- d_filtered %>% filter(!is.na(transaction_valuation_status) & transaction_valuation_status == input$transaction_valuation) 
    # Apply opportunity flag filter.
    if (input$opportunity != "All" && "opportunity_flag" %in% names(d_filtered)) d_filtered <- d_filtered %>% filter(!is.na(opportunity_flag) & opportunity_flag == input$opportunity) 
    return(d_filtered)
  })
  
  # Reactive value to store the ID of the currently selected player.
  selected_player_id <- reactiveVal(NULL)
  
  # Observer for row selection in the player table.
  observeEvent(input$playerTable_rows_selected, {
    req(input$playerTable_rows_selected); current_filtered_data <- filtered_data(); req(nrow(current_filtered_data) > 0) 
    row_num <- input$playerTable_rows_selected
    if (row_num <= nrow(current_filtered_data) && "player_id" %in% names(current_filtered_data)) {
      player_id_val <- current_filtered_data %>% slice(row_num) %>% pull(player_id)
      if(length(player_id_val) == 1) selected_player_id(as.character(player_id_val))
    }
  })
  
  # Observer for click events on the bubble plot.
  observeEvent(event_data("plotly_click", source = "bubblePlotSource"), {
    d_plotly <- event_data("plotly_click", source = "bubblePlotSource")
    req(d_plotly, !is.null(d_plotly$customdata)); selected_player_id(as.character(d_plotly$customdata[[1]])) # customdata holds player_id
  })
  
  # Reactive expression to get details of the selected player.
  player_details <- reactive({
    req(selected_player_id(), master_data, "player_id" %in% names(master_data))
    master_data %>% filter(player_id == as.character(selected_player_id())) %>% slice_head(n=1) # Get the first match
  })
  
  # Render UI for the player detail card header.
  output$playerDetailCardHeader <- renderUI({
    pd <- player_details()
    if(nrow(pd) == 1) HTML(paste0("<h5 class='card-title mb-0'>", pd$name, " <small class='text-muted'>(", round(pd$age,1), " Yrs, ", pd$position, ")</small></h5>"))
    else HTML("<h5 class='card-title mb-0'>Player Details (please select)</h5>")
  })
  
  # Helper function to render bslib value boxes for player details.
  render_player_value_box <- function(title, value_expr_str, icon_name, theme_color) {
    renderUI({
      pd <- player_details(); req(nrow(pd) == 1)
      # Evaluate the expression string in the context of player_details data.
      value_to_display <- tryCatch(eval(rlang::parse_expr(value_expr_str), envir=pd), error = function(e) "N/A")
      # Format numeric values related to money.
      if (is.numeric(value_to_display) && !is.na(value_to_display) && grepl("market_value|fee", value_expr_str, ignore.case = TRUE) ) value_to_display <- scales::dollar(value_to_display, prefix="€", big.mark=".", decimal.mark=",", accuracy=1)
      else if (is.na(value_to_display) || (is.character(value_to_display) && value_to_display == "") ) value_to_display <- "N/A"
      bslib::value_box(
        title = HTML(paste0("<small>", title, "</small>")), 
        value = as.character(value_to_display), 
        showcase = bsicons::bs_icon(icon_name, size = "1.75em"), 
        theme_color = theme_color, 
        class="mb-0"
      )
    })
  }
  # Render value boxes for Fair MV, Current MV.
  output$vbPlayerFairMV <- render_player_value_box("Fair MV (Model)", "fair_market_value_model", "graph-up-arrow", "primary")
  output$vbPlayerCurrentMV <- render_player_value_box("Current Market Value", "market_value", "tag-fill", "info")
  # Render value box for Opportunity Score.
  output$vbPlayerOpportunityScore <- renderUI({ 
    pd <- player_details(); req(nrow(pd) == 1)
    score_val <- if(!is.na(pd$opportunity_score)) pd$opportunity_score else "N/A"
    # Dynamically set icon and theme based on opportunity flag categories.
    icon <- case_when(
      pd$opportunity_flag == "High Priority Target" ~ "award-fill",
      pd$opportunity_flag == "Notable Prospect" ~ "binoculars-fill",
      TRUE ~ "person-lines-fill" 
    )
    theme <- case_when(
      pd$opportunity_flag == "High Priority Target" ~ "success", 
      pd$opportunity_flag == "Notable Prospect" ~ "info",    
      TRUE ~ "secondary"                                       
    )
    bslib::value_box(
      title = HTML("<small>Opportunity Score</small>"), 
      value = as.character(score_val), 
      showcase = bsicons::bs_icon(icon, size = "1.75em"), 
      theme_color = theme, 
      class="mb-0" 
    )
  })
  
  # Render UI for additional player detail information.
  output$playerDetailExtraInfo <- renderUI({
    pd <- player_details(); req(nrow(pd) == 1)
    # Helper to format values, handling NAs.
    format_na <- function(value, type = "dollar", suffix = "", digits = 1) {
      if (is.na(value)) return("N/A")
      if (type == "dollar") return(scales::dollar(value, prefix="€", big.mark=".", decimal.mark=",", accuracy=0))
      if (type == "round") return(paste0(round(value, digits), suffix))
      return(as.character(value)) 
    }
    tagList(hr(style="margin-top: 5px; margin-bottom: 10px;"),
            div(style="font-size: 0.85em;",
                p(strong("Last Fee: "), format_na(pd$transfer_fee, type="dollar"), 
                  " (Deal: ", ifelse(!is.na(pd$transaction_valuation_status), pd$transaction_valuation_status, "N/A"), ")"), 
                p(strong("Remaining Contract: "), format_na(pd$contract_remaining_years, type="round", suffix=" Yrs", digits=1)),
                p(strong("Prev. Season: "), format_na(pd$prev_season_minutes, type="none"), " Mins, ", format_na(pd$prev_season_goals, type="none"), " G, ", format_na(pd$prev_season_assists, type="none"), " A (", format_na(pd$prev_season_games, type="none"), " Gms.)")))
  })
  
  # Render the interactive bubble plot.
  output$bubblePlot <- renderPlotly({
    data_to_plot <- filtered_data()
    validate(need(ui_ready && !is.null(data_to_plot) && nrow(data_to_plot) > 0, "No data for Bubble Plot. Check CSVs or adjust filters."))
    make_bubble_plot(data_to_plot) %>% plotly::event_register('plotly_click') %>% # Register click event
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d", "sendDataToCloud", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian")) # Customize mode bar
  })
  
  # Render the interactive player data table using DT.
  output$playerTable <- renderDT({
    display_data_reactive <- filtered_data() 
    validate(need(ui_ready && !is.null(display_data_reactive) && nrow(display_data_reactive) > 0, "No players for table. Check CSVs or adjust filters."))
    
    cols_to_display <- c("name", "age", "position", "club_name", "market_value", "fair_market_value_model", "mv_to_fair_value_ratio", "transfer_fee", "fee_to_fair_value_ratio", "transaction_valuation_status", "opportunity_score", "opportunity_flag", "contract_remaining_years", "prev_season_minutes", "prev_season_goals", "prev_season_assists", "player_id")
    cols_to_display_final <- intersect(cols_to_display, names(display_data_reactive)) 
    
    numeric_cols_for_dt <- c("market_value", "fair_market_value_model", "transfer_fee", "prev_season_minutes", "prev_season_goals", "prev_season_assists")
    ratio_cols_for_dt <- c("mv_to_fair_value_ratio", "fee_to_fair_value_ratio")
    score_cols_for_dt <- c("opportunity_score") 
    
    selected_display_data <- display_data_reactive %>% 
      select(all_of(cols_to_display_final)) %>% 
      mutate(player_id = as.character(player_id)) %>%
      mutate(across(any_of(c(numeric_cols_for_dt, ratio_cols_for_dt, score_cols_for_dt, "contract_remaining_years", "age")), 
                    ~as.numeric(as.character(.)))) %>%
      arrange(desc(opportunity_score)) 
    
    format_dt_num <- function(x) scales::comma(x, accuracy = 0, big.mark = ".", decimal.mark = ",", na_str = "–")
    format_dt_ratio <- function(x) scales::percent(x, accuracy = 0.1, na_str = "–")
    format_dt_years <- function(x) ifelse(is.na(x), "–", paste0(round(x, 1), " Yrs"))
    format_dt_score <- function(x) ifelse(is.na(x), "–", round(x, 2))
    format_dt_age <- function(x) ifelse(is.na(x), "–", round(x,1))
    
    selected_display_data_formatted <- selected_display_data %>%
      mutate(
        across(all_of(intersect(names(.), numeric_cols_for_dt)), format_dt_num),
        across(all_of(intersect(names(.), ratio_cols_for_dt)), format_dt_ratio),
        across(all_of(intersect(names(.), score_cols_for_dt)), format_dt_score)
      )
    
    if ("contract_remaining_years" %in% names(selected_display_data_formatted)) { 
      selected_display_data_formatted$contract_remaining_years <- format_dt_years(selected_display_data_formatted$contract_remaining_years) 
    }
    if ("age" %in% names(selected_display_data_formatted)) {
      selected_display_data_formatted$age <- format_dt_age(selected_display_data_formatted$age)
    }
    
    colnames_map <- c(name="Name", age="Age", position="Pos.", club_name="Current Club", market_value="Market Value (€)", 
                      fair_market_value_model="Fair MV (€ Model)", mv_to_fair_value_ratio="MV/Fair MV (%)", 
                      transfer_fee="Last Fee (€)", fee_to_fair_value_ratio="Fee/Fair MV (%)", 
                      transaction_valuation_status="Deal (vs Fair MV)", 
                      opportunity_score="Opp. Score", opportunity_flag="Scouting Flag", 
                      contract_remaining_years="Rem. Contract", prev_season_minutes="Mins Prev.", 
                      prev_season_goals="Goals Prev.", prev_season_assists="Ass. Prev.", 
                      player_id = "Internal Player ID")
    
    original_dt_names <- names(selected_display_data_formatted) 
    display_names_for_dt <- sapply(original_dt_names, function(name) ifelse(name %in% names(colnames_map), colnames_map[[name]], name), USE.NAMES = FALSE)
    
    dt_obj <- DT::datatable(selected_display_data_formatted, 
                            options = list(pageLength = 10, searching = TRUE, scrollX = TRUE, autoWidth=FALSE,
                                           columnDefs = list(list(targets = which(original_dt_names == "player_id")-1, visible = FALSE)),
                                           language = list(search = "_INPUT_", searchPlaceholder = "Search...", lengthMenu = "Show _MENU_ entries", info = "Showing _START_ to _END_ of _TOTAL_ entries", infoEmpty= "Showing 0 to 0 of 0 entries", infoFiltered = "(filtered from _MAX_ total entries)", zeroRecords = "No matching records found", paginate = list(previous = '‹', `next` = '›')) ), 
                            rownames = FALSE, filter = 'none', 
                            escape = TRUE, 
                            selection = 'single', colnames = display_names_for_dt)
    
    if("opportunity_flag" %in% original_dt_names){
      dt_obj <- dt_obj %>% formatStyle(
        columns = 'opportunity_flag', 
        target = 'row', 
        backgroundColor = styleEqual(
          c("High Priority Target", "Notable Prospect"), 
          c('rgba(40, 167, 69, 0.2)', 
            'rgba(23, 162, 184, 0.2)')
        ), 
        fontWeight = styleEqual(
          c("High Priority Target"), 
          c("bold")
        )
      )
    }
    if("transaction_valuation_status" %in% original_dt_names){
      dt_obj <- dt_obj %>% formatStyle(
        columns = 'transaction_valuation_status', 
        color = styleEqual( 
          c("Undervalued (vs Fair MV)", "Overvalued (vs Fair MV)"), 
          c('darkgreen', 'darkred')
        )
      )
    }
    return(dt_obj)
  }, server = TRUE) 
}

# -------------------- Run the Application --------------------
shinyApp(ui, server)
