# -------------------- Pakete --------------------
#' @import shiny
#' @import tidyverse
#' @import plotly
#' @import DT
#' @import lubridate
#' @import scales
#' @import bslib
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(scales)
library(bslib)

# -------------------- Hilfsfunktionen --------------------

#' Datenaufbereitung (prep_data)
#' @return Ein Dataframe `data_out` mit allen aufbereiteten Informationen.
prep_data <- function(players_path = "players.csv",
                      transfers_path = "transfers.csv",
                      valuations_path = "player_valuations.csv",
                      clubs_path = "clubs.csv",
                      appearances_path = "appearances.csv") {
  
  # --- 1. Daten laden ---
  read_robust_csv <- function(path, name) {
    tryCatch(
      read.csv(path, stringsAsFactors = FALSE),
      error = function(e) {
        warning(paste("Fehler beim Laden von", name, "aus", path, ":", e$message), call. = FALSE)
        tibble(placeholder_id = integer()) 
      }
    )
  }
  players_raw <- read_robust_csv(players_path, "players.csv")
  transfers_raw_input <- read_robust_csv(transfers_path, "transfers.csv")
  player_valuations_raw <- read_robust_csv(valuations_path, "player_valuations.csv")
  clubs_raw <- read_robust_csv(clubs_path, "clubs.csv")
  appearances_raw <- read_robust_csv(appearances_path, "appearances.csv")
  
  # --- Verarbeitung der Transfergebühr ---
  transfers <- transfers_raw_input
  if ("transfer_fee" %in% names(transfers)) {
    transfers <- transfers %>% mutate(transfer_fee = as.numeric(as.character(transfer_fee)))
  } else {
    warning("Spalte 'transfer_fee' in 'transfers_raw_input' nicht gefunden. Initialisiere mit NA.", call. = FALSE)
    transfers <- transfers %>% mutate(transfer_fee = NA_real_)
  }
  
  # --- Datumskonvertierungen ---
  safe_as_date <- function(date_vector, col_name) {
    if(is.null(date_vector) || all(is.na(date_vector)) || length(date_vector) == 0) return(as.Date(rep(NA, length(date_vector)))) # Sicherer Return
    parsed_dates <- tryCatch(as.Date(date_vector), error = function(e) { NULL })
    if(is.null(parsed_dates)) {
      warning(paste("Konnte '", col_name, "' nicht als Datum parsen. NAs werden erzeugt."), call. = FALSE)
      return(as.Date(rep(NA, length(date_vector))))
    }
    return(parsed_dates)
  }
  
  if("date_of_birth" %in% names(players_raw)) players <- players_raw %>% mutate(date_of_birth = safe_as_date(date_of_birth, "date_of_birth")) else players <- players_raw %>% mutate(date_of_birth = as.Date(NA))
  if("contract_expiration_date" %in% names(players_raw)) players <- players %>% mutate(contract_expiration_date = safe_as_date(contract_expiration_date, "contract_expiration_date")) else players <- players_raw %>% mutate(contract_expiration_date = as.Date(NA))
  if("date" %in% names(player_valuations_raw)) player_valuations <- player_valuations_raw %>% mutate(date = safe_as_date(date, "valuation_date")) else player_valuations <- player_valuations_raw %>% mutate(date = as.Date(NA))
  if("transfer_date" %in% names(transfers)) transfers <- transfers %>% mutate(transfer_date = safe_as_date(transfer_date, "transfer_date")) else transfers <- transfers %>% mutate(transfer_date = as.Date(NA))
  if("date" %in% names(appearances_raw)) appearances <- appearances_raw %>% mutate(appearance_date = safe_as_date(date, "appearance_date")) else appearances <- appearances_raw %>% mutate(appearance_date = as.Date(NA))
  
  # --- 2. Feature Engineering ---
  # Basis Spieler-Infos
  req_cols_player_df <- c("player_id", "name", "date_of_birth", "position", "country_of_citizenship")
  player_df <- if(all(req_cols_player_df %in% names(players))) {
    players %>% mutate(age = if (all(is.na(date_of_birth))) NA_real_ else as.numeric(difftime(Sys.Date(), date_of_birth, units = "days")) / 365.25) %>%
      select(player_id, name, age, position, nationality = country_of_citizenship)
  } else { warning("Basis-Spielerinfos unvollständig.", call.=F); tibble(player_id = integer()) }
  
  # Letzter Marktwert
  req_cols_last_val <- c("player_id", "date", "market_value_in_eur")
  last_valuation <- if(all(req_cols_last_val %in% names(player_valuations))) {
    player_valuations %>% 
      filter(!is.na(player_id) & !is.na(date)) %>% 
      group_by(player_id) %>%
      filter(date == max(date, na.rm = TRUE)) %>% # Nimm das aktuellste Datum
      slice_head(n = 1) %>% # Falls mehrere Einträge am selben Tag, nimm den ersten
      ungroup() %>%
      select(player_id, market_value = market_value_in_eur, valuation_date = date)
  } else { 
    warning("Marktwert-Daten unvollständig.", call.=F)
    tibble(player_id = integer(), market_value=numeric(), valuation_date=as.Date(character())) 
  }
  
  # Aggregation von Leistungsdaten (Saison vor Transfer)
  get_season <- function(date_col) { # Nimmt einen Datumsvektor
    req(date_col) # Stellt sicher, dass date_col nicht NULL ist
    year_val <- year(date_col)
    month_val <- month(date_col)
    # Saison geht typischerweise von Juli/August bis Mai/Juni des Folgejahres
    if_else(month_val >= 7, paste0(year_val, "/", year_val + 1), paste0(year_val - 1, "/", year_val))
  }
  
  player_performance_prev_season <- tibble() # Initialisiere leer
  req_cols_appearances <- c("player_id", "appearance_date", "goals", "assists", "minutes_played")
  # game_id ist optional für n_distinct, wenn nicht da, wird es ignoriert
  if (!"game_id" %in% names(appearances)) appearances$game_id <- NA # Füge game_id hinzu falls nicht vorhanden, für summarize
  
  if (nrow(appearances) > 0 && all(req_cols_appearances %in% names(appearances)) &&
      nrow(transfers) > 0 && "transfer_date" %in% names(transfers) && "player_id" %in% names(transfers)) {
    
    appearances_with_season <- appearances %>%
      filter(!is.na(appearance_date)) %>%
      mutate(appearance_season = get_season(appearance_date))
    
    transfers_with_prev_season_id <- transfers %>%
      filter(!is.na(transfer_date) & !is.na(player_id)) %>%
      mutate(transfer_actual_season = get_season(transfer_date)) %>%
      mutate(transfer_season_start_year = as.numeric(str_extract(transfer_actual_season, "^\\d{4}"))) %>%
      mutate(prev_season_id = paste0(transfer_season_start_year - 1, "/", transfer_season_start_year)) %>%
      distinct(player_id, transfer_date, prev_season_id) 
    
    performance_by_season <- appearances_with_season %>%
      group_by(player_id, appearance_season) %>%
      summarise(
        prev_season_goals = sum(as.numeric(goals), na.rm = TRUE),
        prev_season_assists = sum(as.numeric(assists), na.rm = TRUE),
        prev_season_minutes = sum(as.numeric(minutes_played), na.rm = TRUE),
        prev_season_games = n_distinct(game_id, na.rm=TRUE), 
        .groups = "drop"
      )
    
    player_performance_prev_season <- transfers_with_prev_season_id %>%
      left_join(performance_by_season, by = c("player_id", "prev_season_id" = "appearance_season")) %>%
      mutate(across(c(prev_season_goals, prev_season_assists, prev_season_minutes, prev_season_games), ~ifelse(is.na(.), 0, .))) %>%
      select(player_id, transfer_date, prev_season_goals, prev_season_assists, prev_season_minutes, prev_season_games)
    
    if(nrow(player_performance_prev_season) > 0) message("Leistungsdaten der Vorsaison aggregiert.")
  } else { warning("Leistungsdaten-Aggregation nicht möglich (fehlende Spalten oder Daten).", call. = FALSE) }
  
  # Letzter Transfer mit Vertrags- und Leistungsdaten
  req_cols_last_transfer <- c("player_id", "transfer_date", "transfer_fee", "from_club_id", "to_club_id")
  last_transfer <- if(all(req_cols_last_transfer %in% names(transfers))) {
    player_contracts <- if("player_id" %in% names(players) && "contract_expiration_date" %in% names(players)) {
      players %>% select(player_id, contract_expiration_date) %>% filter(!is.na(contract_expiration_date)) %>% distinct(player_id, .keep_all = TRUE)
    } else { tibble(player_id = integer(), contract_expiration_date = as.Date(NA)) }
    
    last_transfer_base <- transfers %>% filter(!is.na(player_id) & !is.na(transfer_date)) %>% group_by(player_id) %>%
      filter(transfer_date == max(transfer_date, na.rm = TRUE)) %>% slice_head(n = 1) %>% ungroup() # Nimm nur den letzten Transfer pro Spieler
    
    lt <- last_transfer_base %>% left_join(player_contracts, by = "player_id") %>%
      mutate( contract_remaining_days = if_else(!is.na(contract_expiration_date) & !is.na(transfer_date), as.numeric(difftime(contract_expiration_date, transfer_date, units = "days")), NA_real_),
              contract_remaining_years = case_when(is.na(contract_remaining_days) ~ NA_real_, contract_remaining_days < 0 ~ 0, TRUE ~ pmin(contract_remaining_days / 365.25, 5)))
    
    # Joine Leistungsdaten der Vorsaison
    if(nrow(player_performance_prev_season) > 0 && "player_id" %in% names(player_performance_prev_season) && "transfer_date" %in% names(player_performance_prev_season)){
      lt <- lt %>% left_join(player_performance_prev_season, by = c("player_id", "transfer_date"))
    } else { 
      # Füge Spalten hinzu, falls player_performance_prev_season leer ist oder Join fehlschlägt
      lt$prev_season_goals <- NA_real_
      lt$prev_season_assists <- NA_real_
      lt$prev_season_minutes <- NA_real_
      lt$prev_season_games <- NA_real_
    }
    lt %>% select(player_id, transfer_fee, transfer_date, from_club_id, to_club_id, contract_remaining_years, 
                  prev_season_goals, prev_season_assists, prev_season_minutes, prev_season_games)
  } else { 
    warning("Letzte Transferdaten unvollständig.", call.=F)
    tibble(player_id=integer(), transfer_fee=NA_real_, transfer_date=as.Date(NA), from_club_id=NA_integer_, to_club_id=NA_integer_, contract_remaining_years=NA_real_, prev_season_goals=NA_real_, prev_season_assists=NA_real_, prev_season_minutes=NA_real_, prev_season_games=NA_real_)
  }
  
  clubs <- if("club_id" %in% names(clubs_raw) && "name" %in% names(clubs_raw)){ clubs_raw %>% select(club_id, club_name = name) } else { tibble(club_id=integer(), club_name=character())}
  
  # Joins
  data_intermediate <- player_df %>% 
    left_join(last_valuation, by = "player_id") %>%
    left_join(last_transfer,  by = "player_id") %>%
    filter(!is.na(age)) # Mindestanforderung: Alter muss vorhanden sein
  
  # Finale Aufbereitung data_out
  if (nrow(data_intermediate) > 0) {
    data_out <- data_intermediate %>% left_join(clubs, by = c("to_club_id" = "club_id")) %>%
      mutate( 
        position = ifelse(is.na(position) | position == "", "Unbekannt", as.character(position)),
        market_value_plot = ifelse(is.na(market_value) | market_value <= 0, 1, market_value),
        log_market_value = ifelse(!is.na(market_value) & market_value > 0, log1p(market_value), NA_real_),
        # Fehlende Leistungsdaten hier mit 0 füllen, wenn sie nicht schon durch den Join gefüllt wurden
        across(any_of(c("prev_season_goals", "prev_season_assists", "prev_season_minutes", "prev_season_games")), ~replace_na(as.numeric(.), 0)) 
      )
  } else { 
    # Definition des leeren Dataframes mit allen Spalten (gekürzt, aber stellen Sie sicher, dass alle Spalten hier definiert sind)
    data_out <- tibble(player_id=integer(), name=character(), age=numeric(), position=character(), nationality=character(), market_value=numeric(), valuation_date=as.Date(NA), transfer_fee=numeric(), transfer_date=as.Date(NA), from_club_id=integer(), to_club_id=integer(), club_name=character(), contract_remaining_years=numeric(), prev_season_goals=numeric(), prev_season_assists=numeric(), prev_season_minutes=numeric(), prev_season_games=numeric(), log_market_value=numeric(), market_value_plot=numeric(), predicted_potential_market_value=numeric(), predicted_transaction_fee=numeric(), mv_to_potential_ratio=numeric(), fee_to_potential_ratio=numeric(), potential_score_age=numeric(), performance_score_minutes=numeric(), productivity_per_90=numeric(), performance_score_productivity=numeric(), opportunity_score=numeric(), opportunity_flag=character(), transaction_valuation_status=character())
    warning("Keine verwertbaren Zwischendaten nach Joins.", call.=F)
  }
  
  # --- 3a. Regression für "Predicted Potential Market Value" (Ziel: log_market_value) ---
  model_potential_mv <- NULL
  predictors_potential_mv <- c("age", "position", "contract_remaining_years", "prev_season_minutes", "prev_season_goals", "prev_season_assists")
  
  # Sicherstellen, dass alle Prädiktoren und Zielvariable vorhanden sind
  actual_predictors_potential_mv <- intersect(predictors_potential_mv, names(data_out))
  
  if("log_market_value" %in% names(data_out) && length(actual_predictors_potential_mv) > 1 && nrow(data_out) > 10){ # Mind. 2 Prädiktoren
    data_for_potential_mv <- data_out %>% 
      filter(!is.na(log_market_value), 
             complete.cases(across(all_of(actual_predictors_potential_mv))),
             position != "Unbekannt") %>%
      mutate(position = as.factor(droplevels(position)))
    
    if(nrow(data_for_potential_mv) > (length(actual_predictors_potential_mv) + 10) && n_distinct(data_for_potential_mv$position) > 1){
      formula_potential_mv_str <- paste("log_market_value ~", paste(actual_predictors_potential_mv, collapse=" + "))
      model_potential_mv <- tryCatch(lm(as.formula(formula_potential_mv_str), data = data_for_potential_mv),
                                     error = function(e){ warning(paste("Modell für Potential Market Value fehlgeschlagen:",e$message), call.=F); NULL})
      if(!is.null(model_potential_mv)) message("Modell für Predicted Potential Market Value trainiert.")
    } else { warning("Nicht genug Daten/Varianz für Potential-MW-Modell nach Filterung.", call.=F)}
  } else { warning("Fehlende Spalten oder zu wenig Daten für Potential-MW-Modell.", call.=F)}
  
  if (!is.null(model_potential_mv)) {
    predicted_log_pmv <- predict(model_potential_mv, newdata = data_out, na.action = na.pass)
    data_out$predicted_potential_market_value <- ifelse(is.na(predicted_log_pmv), NA_real_, expm1(predicted_log_pmv))
    data_out <- data_out %>% mutate(predicted_potential_market_value = ifelse(!is.na(predicted_potential_market_value) & predicted_potential_market_value < 0, 0, predicted_potential_market_value))
  } else { data_out$predicted_potential_market_value <- NA_real_ }
  
  # --- 3b. Regression für "Predicted Transaction Fee" (Ziel: transfer_fee mit GLM) ---
  model_transaction_fee <- NULL
  predictors_transaction_fee <- c("age", "market_value", "position", "contract_remaining_years", "prev_season_minutes", "prev_season_goals", "prev_season_assists")
  actual_predictors_transaction_fee <- intersect(predictors_transaction_fee, names(data_out))
  
  if("transfer_fee" %in% names(data_out) && length(actual_predictors_transaction_fee) > 1 && nrow(data_out) > 10){
    data_for_transaction_fee <- data_out %>%
      filter(transfer_fee > 0, # Wichtig für Gamma GLM
             !is.na(transfer_fee),
             complete.cases(across(all_of(actual_predictors_transaction_fee))),
             position != "Unbekannt") %>%
      mutate(position = as.factor(droplevels(position)))
    
    if(nrow(data_for_transaction_fee) > (length(actual_predictors_transaction_fee) + 10) && n_distinct(data_for_transaction_fee$position) > 1){
      formula_transaction_fee_str <- paste("transfer_fee ~", paste(actual_predictors_transaction_fee, collapse=" + "))
      model_transaction_fee <- tryCatch(
        glm(as.formula(formula_transaction_fee_str), data = data_for_transaction_fee, family = Gamma(link = "log"),
            control = glm.control(epsilon = 1e-7, maxit = 100, trace = FALSE)), # Angepasste Konvergenz
        error = function(e){ warning(paste("GLM für Transaction Fee fehlgeschlagen:", e$message), call.=F); NULL }
      )
      if(!is.null(model_transaction_fee)) message("GLM für Predicted Transaction Fee trainiert.")
    } else {warning("Nicht genug Daten/Varianz für Transaktionsgebühr-GLM nach Filterung.", call.=F)}
  } else {warning("Fehlende Spalten oder zu wenig Daten für Transaktionsgebühr-GLM.", call.=F)}
  
  if (!is.null(model_transaction_fee)) {
    data_out$predicted_transaction_fee <- predict(model_transaction_fee, newdata = data_out, type = "response", na.action = na.pass)
    data_out <- data_out %>% mutate(predicted_transaction_fee = ifelse(!is.na(predicted_transaction_fee) & predicted_transaction_fee < 0, 0, predicted_transaction_fee))
  } else { data_out$predicted_transaction_fee <- NA_real_ }
  
  # --- 4. Scouting-Metriken ---
  if ("predicted_potential_market_value" %in% names(data_out)) {
    data_out <- data_out %>%
      mutate(
        mv_to_potential_ratio = ifelse(predicted_potential_market_value > 0 & !is.na(market_value), market_value / predicted_potential_market_value, NA_real_),
        fee_to_potential_ratio = ifelse(predicted_potential_market_value > 0 & !is.na(transfer_fee), transfer_fee / predicted_potential_market_value, NA_real_),
        potential_score_age = case_when(age <= 20 ~ 4, age <= 22 ~ 3, age <= 24 ~ 2, age <= 26 ~ 1, TRUE ~ 0),
        performance_score_minutes = case_when(is.na(prev_season_minutes) ~ 0, prev_season_minutes >= 2500 ~ 4, prev_season_minutes >= 1800 ~ 3, prev_season_minutes >= 1000 ~ 2, prev_season_minutes > 200 ~ 1, TRUE ~ 0),
        productivity_per_90 = ifelse(!is.na(prev_season_minutes) & prev_season_minutes > 0, (replace_na(prev_season_goals,0) + replace_na(prev_season_assists,0)) / (prev_season_minutes / 90), 0),
        performance_score_productivity = case_when(is.na(productivity_per_90) ~ 0, productivity_per_90 >= 0.6 ~ 3, productivity_per_90 >= 0.4 ~ 2, productivity_per_90 >= 0.2 ~ 1, TRUE ~ 0),
        opportunity_score = round( (potential_score_age * 0.35) + (performance_score_minutes * 0.15) + (performance_score_productivity * 0.15) + (ifelse(!is.na(mv_to_potential_ratio) & mv_to_potential_ratio <= 0.8, 2, ifelse(!is.na(mv_to_potential_ratio) & mv_to_potential_ratio <= 1, 1, 0)) * 0.2) + (ifelse(!is.na(fee_to_potential_ratio) & fee_to_potential_ratio <= 0.7, 2, ifelse(!is.na(fee_to_potential_ratio) & fee_to_potential_ratio <= 1, 1, 0)) * 0.15), 1),
        opportunity_flag = case_when( opportunity_score >= 3.0 ~ "💎 Top-Juwel", opportunity_score >= 2.0 ~ "🔥 Heißer Kandidat", opportunity_score >= 1.0 ~ "👀 Beobachten", TRUE ~ "Standard" ),
        transaction_valuation_status = case_when( !is.na(predicted_transaction_fee) & !is.na(transfer_fee) & transfer_fee < 0.9 * predicted_transaction_fee ~ "Günstiger Deal", !is.na(predicted_transaction_fee) & !is.na(transfer_fee) & transfer_fee > 1.1 * predicted_transaction_fee ~ "Teurer Deal", !is.na(predicted_transaction_fee) & !is.na(transfer_fee) ~ "Fairer Deal",  TRUE ~ NA_character_ )
      )
  } else { # Fallback für Scouting-Metriken
    cols_to_add_na <- c("mv_to_potential_ratio", "fee_to_potential_ratio", "potential_score_age", "performance_score_minutes", "productivity_per_90", "performance_score_productivity", "opportunity_score")
    for(col_add in cols_to_add_na) if(!col_add %in% names(data_out)) data_out[[col_add]] <- NA_real_
    if(!"opportunity_flag" %in% names(data_out)) data_out$opportunity_flag <- NA_character_
    if(!"transaction_valuation_status" %in% names(data_out)) data_out$transaction_valuation_status <- NA_character_
  }
  
  # Sicherstellen, dass alle finalen Ausgabespalten existieren
  final_cols_check <- c("predicted_potential_market_value", "predicted_transaction_fee", 
                        "mv_to_potential_ratio", "fee_to_potential_ratio", 
                        "potential_score_age", "performance_score_minutes", "productivity_per_90", 
                        "performance_score_productivity", "opportunity_score", "opportunity_flag", 
                        "transaction_valuation_status", "market_value_plot", "contract_remaining_years",
                        "prev_season_goals", "prev_season_assists", "prev_season_minutes", "prev_season_games")
  for(col_fc in final_cols_check) {
    if(!col_fc %in% names(data_out)) {
      data_out[[col_fc]] <- if(col_fc %in% c("opportunity_flag", "transaction_valuation_status")) NA_character_ else NA_real_
    }
  }
  
  return(data_out)
}

# (make_bubble_plot Funktion - angepasst für neue Hover-Daten und bslib-Theme-Freundlichkeit)
make_bubble_plot <- function(plot_data) {
  cols_for_hover_and_plot <- c("name", "club_name", "market_value", "transfer_fee", 
                               "predicted_potential_market_value", "transaction_valuation_status", "age", "position", 
                               "market_value_plot", "contract_remaining_years", 
                               "prev_season_goals", "prev_season_assists", "prev_season_minutes", 
                               "opportunity_score", "opportunity_flag", "player_id") # player_id für customdata
  for(col_p in cols_for_hover_and_plot){
    if(!col_p %in% names(plot_data)){
      plot_data[[col_p]] <- if(col_p %in% c("market_value", "transfer_fee", "predicted_potential_market_value", "age", "market_value_plot", "contract_remaining_years", "prev_season_goals", "prev_season_assists", "prev_season_minutes", "opportunity_score", "player_id")) NA_real_ else NA_character_
    }
  }
  
  plot_data_final <- plot_data %>%
    mutate(
      hover_text = paste(
        "<b>Name:</b>", ifelse(is.na(name), "N/A", name),
        "<br><b>Club:</b>", ifelse(is.na(club_name), "N/A", club_name),
        "<br><b>Position:</b>", ifelse(is.na(position), "N/A", position),
        "<br><b>Alter:</b>", ifelse(is.na(age), "N/A", round(age,1)),
        "<br><b>Akt. Marktwert:</b>", ifelse(is.na(market_value), "N/A", comma(market_value, accuracy = 0, big.mark=".", decimal.mark=",")),
        "<br><b>Potenzial-MW (Modell):</b>", ifelse(is.na(predicted_potential_market_value), "N/A", comma(round(predicted_potential_market_value), accuracy = 0, big.mark=".", decimal.mark=",")),
        "<br><b>Transfer Fee:</b>", ifelse(is.na(transfer_fee), "N/A", comma(transfer_fee, accuracy = 0, big.mark=".", decimal.mark=",")),
        "<br><b>Deal Bewertung:</b>", ifelse(is.na(transaction_valuation_status), "N/A", transaction_valuation_status),
        "<br><b>Opportunity Score:</b>", ifelse(is.na(opportunity_score), "N/A", opportunity_score),
        "<br><b>Scouting Flag:</b>", ifelse(is.na(opportunity_flag), "N/A", opportunity_flag),
        "<br><b>Restvertrag:</b>", ifelse(is.na(contract_remaining_years), "N/A", paste(round(contract_remaining_years,1), "J.")),
        "<br><b>Vorsaison Min:</b>", ifelse(is.na(prev_season_minutes), "N/A", prev_season_minutes),
        "<br><b>Vorsaison G+A:</b>", ifelse(is.na(prev_season_goals) | is.na(prev_season_assists), "N/A", prev_season_goals + prev_season_assists)
      )
    )
  
  p <- plot_ly(
    data  = plot_data_final, x = ~age, y = ~transfer_fee, type  = "scatter", mode  = "markers",
    color = ~position, size  = ~market_value_plot, sizes = c(6, 45), 
    text  = ~hover_text, hoverinfo = "text", customdata = ~player_id, # customdata für Klick-Events
    marker = list(sizemin = 6, line = list(width = 0.5, color = 'rgba(0,0,0,0.5)'), opacity = 0.75)
  ) 
  p %>% layout(
    title = NULL, 
    xaxis = list(title = "Alter (Jahre)", gridcolor = "rgba(0,0,0,0.05)"),
    yaxis = list(title = "Transfer Fee (€)", tickformat = ",.0f", gridcolor = "rgba(0,0,0,0.05)"),
    legend = list(title = list(text = '<b>Position</b>'), orientation = "h", y = -0.2, x = 0.5, xanchor="center", font = list(size=10)),
    paper_bgcolor = "transparent", plot_bgcolor = "transparent",
    clickmode = "event+select" 
  )
}

# Globale Daten und UI Vorbereitung
master_data <- prep_data() 
ui_ready <- !is.null(master_data) && nrow(master_data) > 0 && ncol(master_data) > 0

default_min_age <- 16; default_max_age <- 40
default_min_fee <- if(ui_ready && "transfer_fee" %in% names(master_data) && any(!is.na(master_data$transfer_fee))) floor(min(master_data$transfer_fee, na.rm=T)) else 0
default_max_fee <- if(ui_ready && "transfer_fee" %in% names(master_data) && any(!is.na(master_data$transfer_fee))) ceiling(max(master_data$transfer_fee, na.rm=T)) else 100000000
if(default_max_fee <= default_min_fee) default_max_fee <- default_min_fee + 1000000 # Sicherstellen, dass max > min

min_age_val <- if (ui_ready && "age" %in% names(master_data) && any(!is.na(master_data$age))) floor(min(master_data$age, na.rm = TRUE)) else default_min_age
max_age_val <- if (ui_ready && "age" %in% names(master_data) && any(!is.na(master_data$age))) ceiling(max(master_data$age, na.rm = TRUE)) else default_max_age
if (!is.finite(min_age_val) || !is.finite(max_age_val) || min_age_val >= max_age_val) { min_age_val <- default_min_age; max_age_val <- default_max_age }

min_fee_val <- default_min_fee
max_fee_val <- default_max_fee

position_choices <- if (ui_ready && "position" %in% names(master_data) && n_distinct(master_data$position, na.rm = TRUE) > 0) { c("All", sort(unique(master_data$position))) } else { c("All") }
transaction_valuation_choices <- if (ui_ready && "transaction_valuation_status" %in% names(master_data) && n_distinct(master_data$transaction_valuation_status, na.rm = TRUE) > 0) {c("All", sort(unique(na.omit(master_data$transaction_valuation_status))))} else {c("All", "Günstiger Deal", "Fairer Deal", "Teurer Deal")}
opportunity_choices <- if (ui_ready && "opportunity_flag" %in% names(master_data) && n_distinct(master_data$opportunity_flag, na.rm = TRUE) > 0) {c("All", "💎 Top-Juwel", "🔥 Heißer Kandidat", "👀 Beobachten", "Standard")} else {c("All")}


# --- UI mit bslib ---
ui <- page_sidebar(
  title = "Football Player Scouting Dashboard",
  theme = bslib::bs_theme(version = 5, bootswatch = "pulse", 
                          primary = "#00529B", secondary = "#FFC107",
                          base_font = bslib::font_google("Open Sans"),
                          heading_font = bslib::font_google("Montserrat"),
                          font_scale = 0.9) %>% 
    bslib::bs_add_rules(sass::sass_file("www/custom_styles.scss")), # www-Ordner mit custom_styles.scss erstellen
  
  sidebar = sidebar(
    position = "left", open = "always", width = 300, bg = "#f8f9fa",
    accordion(
      id = "filters_accordion", open = TRUE, # Alle Panels standardmäßig offen
      accordion_panel("Basisfilter", icon = bsicons::bs_icon("sliders"),
                      sliderInput("age", "Alter", min = min_age_val, max = max_age_val, value = c(min_age_val, max_age_val), step = 1, ticks = FALSE),
                      sliderInput("transfer_fee", "Bezahlte Transfer Fee (€)", min = min_fee_val, max = max_fee_val, value = c(min_fee_val, max_fee_val), step = 100000, pre = "€", sep = ".", ticks=FALSE),
                      selectInput("position", "Position", choices = position_choices, selected = "All")
      ),
      accordion_panel("Bewertungs- & Scoutingfilter", icon = bsicons::bs_icon("search-heart"),
                      selectInput("transaction_valuation", "Deal Bewertung (Transaktionsmodell)", choices = transaction_valuation_choices, selected = "All"),
                      selectInput("opportunity", "Scouting Flag (Potenzial-MW)", choices = opportunity_choices, selected = "All")
      )
    )
  ), 
  
  layout_columns(
    col_widths = c(12, 12, 12), 
    row_heights = c("auto", 2.5, 2), # Relative Höhen angepasst
    
    card( id = "player_detail_card", style="min-height: 180px;", # Höhe reduziert
          card_header(uiOutput("playerDetailCardHeader")),
          card_body(padding = "10px", # Weniger Padding
                    layout_columns(col_widths = c(4,4,4), gap="10px",
                                   uiOutput("vbPlayerPotentialMV"),
                                   uiOutput("vbPlayerCurrentMV"),
                                   uiOutput("vbPlayerOpportunityScore")
                    ),
                    uiOutput("playerDetailExtraInfo")
          )
    ), 
    
    card( full_screen = TRUE,
          card_header("Spieler-Übersicht: Transfers & Marktwerte"),
          card_body(padding=0, plotlyOutput("bubblePlot", height = "480px")) # Padding 0 für Plotly
    ),
    
    card( full_screen = TRUE,
          card_header("Detail-Tabelle & Spieler-Analyse"),
          card_body(DTOutput("playerTable"))
    )
  ), 
  
  tags$footer( class = "text-center text-muted mt-3 mb-2", "Dashboard v2.1 | Data-driven Scouting Insights")
)


# --- Server mit Interaktivität ---
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    d <- master_data 
    req(ui_ready) 
    
    d_filtered <- d %>%
      filter( if(!is.na(input$age[1])) age >= input$age[1] else TRUE,
              if(!is.na(input$age[2])) age <= input$age[2] else TRUE )
    
    # Filter für Transfer Fee nur anwenden, wenn die Spalte transfer_fee existiert und nicht alle Werte NA sind
    if("transfer_fee" %in% names(d_filtered) && any(!is.na(d_filtered$transfer_fee))){
      d_filtered <- d_filtered %>% filter(
        if(!is.na(input$transfer_fee[1])) { if(is.na(transfer_fee)) FALSE else transfer_fee >= input$transfer_fee[1] } else TRUE,
        if(!is.na(input$transfer_fee[2])) { if(is.na(transfer_fee)) FALSE else transfer_fee <= input$transfer_fee[2] } else TRUE
      )
    }
    
    if (input$position != "All") { d_filtered <- d_filtered %>% filter(position == input$position) } # Fehler behoben: input$position
    if (input$transaction_valuation != "All" && "transaction_valuation_status" %in% names(d_filtered)) { 
      d_filtered <- d_filtered %>% filter(!is.na(transaction_valuation_status) & transaction_valuation_status == input$transaction_valuation) 
    }
    if (input$opportunity != "All" && "opportunity_flag" %in% names(d_filtered)) { 
      d_filtered <- d_filtered %>% filter(opportunity_flag == input$opportunity) 
    }
    return(d_filtered)
  })
  
  selected_player_id <- reactiveVal(NULL)
  
  observeEvent(input$playerTable_rows_selected, {
    req(input$playerTable_rows_selected)
    row_num <- input$playerTable_rows_selected
    # player_id muss in den Daten sein, die DT anzeigt UND in filtered_data() existieren
    player_id_val <- (filtered_data() %>% slice(row_num) %>% pull(player_id))
    if(length(player_id_val) == 1) selected_player_id(player_id_val)
  })
  
  observeEvent(event_data("plotly_click"), {
    d_plotly <- event_data("plotly_click")
    req(d_plotly, !is.null(d_plotly$customdata))
    selected_player_id(d_plotly$customdata)
  })
  
  player_details <- reactive({
    req(selected_player_id())
    master_data %>% filter(player_id == selected_player_id()) %>% slice_head(n=1) # Nimm nur die erste Zeile, falls Duplikate
  })
  
  output$playerDetailCardHeader <- renderUI({
    pd <- player_details()
    if(nrow(pd) == 1) {
      HTML(paste0("<h5 class='card-title mb-0'>", pd$name, 
                  " <small class='text-muted'>(", round(pd$age,1), " J, ", pd$position, ")</small></h5>"))
    } else {
      HTML("<h5 class='card-title mb-0'>Spielerdetails</h5>")
    }
  })
  
  output$vbPlayerPotentialMV <- renderUI({
    pd <- player_details(); req(nrow(pd) == 1)
    bslib::value_box( title = HTML("<small>Potenzial-MW (Modell)</small>"),
                      value = if(!is.na(pd$predicted_potential_market_value)) scales::dollar(pd$predicted_potential_market_value, prefix="€", big.mark=".", decimal.mark=",", accuracy=1) else "N/A",
                      showcase = bsicons::bs_icon("graph-up-arrow"), theme_color = "primary", height="100px", class="mb-0" )
  })
  output$vbPlayerCurrentMV <- renderUI({
    pd <- player_details(); req(nrow(pd) == 1)
    bslib::value_box( title = HTML("<small>Aktueller Marktwert</small>"),
                      value = if(!is.na(pd$market_value)) scales::dollar(pd$market_value, prefix="€", big.mark=".", decimal.mark=",", accuracy=1) else "N/A",
                      showcase = bsicons::bs_icon("tag-fill"), theme_color = "info", height="100px", class="mb-0" )
  })
  output$vbPlayerOpportunityScore <- renderUI({
    pd <- player_details(); req(nrow(pd) == 1)
    bslib::value_box( title = HTML("<small>Opportunity Score</small>"),
                      value = if(!is.na(pd$opportunity_score)) pd$opportunity_score else "N/A",
                      showcase = bsicons::bs_icon(case_when(pd$opportunity_flag == "💎 Top-Juwel" ~ "gem", pd$opportunity_flag == "🔥 Heißer Kandidat" ~ "fire", TRUE ~ "search")),
                      theme_color = case_when(pd$opportunity_flag == "💎 Top-Juwel" ~ "success", pd$opportunity_flag == "🔥 Heißer Kandidat" ~ "warning", TRUE ~ "secondary"), 
                      height="100px", class="mb-0" )
  })
  
  output$playerDetailExtraInfo <- renderUI({
    pd <- player_details(); req(nrow(pd) == 1)
    tagList(
      hr(style="margin-top: 5px; margin-bottom: 10px;"),
      div(style="font-size: 0.85em;",
          p(strong("Letzte Fee: "), if(!is.na(pd$transfer_fee)) scales::dollar(pd$transfer_fee, prefix="€", big.mark=".", decimal.mark=",", accuracy=1) else "N/A",
            " (Deal: ", if(!is.na(pd$transaction_valuation_status)) pd$transaction_valuation_status else "N/A", ")"),
          p(strong("Restvertrag: "), if(!is.na(pd$contract_remaining_years)) paste(round(pd$contract_remaining_years,1), "J.") else "N/A"),
          p(strong("Vorsaison: "), pd$prev_season_minutes, " Min, ", pd$prev_season_goals, " G, ", pd$prev_season_assists, " A (", pd$prev_season_games, " Sp.)")
      )
    )
  })
  
  output$bubblePlot <- renderPlotly({
    data_to_plot <- filtered_data()
    validate(need(nrow(data_to_plot) > 0, "Keine Daten für die aktuelle Auswahl verfügbar."))
    make_bubble_plot(data_to_plot)
  })
  
  output$playerTable <- renderDT({
    display_data <- filtered_data()
    validate(need(nrow(display_data) > 0, "Keine Spieler für die aktuelle Auswahl in der Tabelle."))
    
    cols_to_display <- c("name", "age", "position", "club_name", "market_value", "predicted_potential_market_value", "mv_to_potential_ratio", "transfer_fee", "fee_to_potential_ratio", "transaction_valuation_status", "opportunity_score", "opportunity_flag", "contract_remaining_years", "prev_season_minutes", "prev_season_goals", "prev_season_assists", "player_id") # player_id für Selektion, kann versteckt werden
    cols_to_display_final <- intersect(cols_to_display, names(display_data)) 
    
    selected_display_data <- display_data %>% select(all_of(cols_to_display_final)) %>% arrange(desc(opportunity_score)) 
    
    num_cols_to_format_dt <- c("market_value", "predicted_potential_market_value", "transfer_fee", "prev_season_minutes", "prev_season_goals", "prev_season_assists")
    for(col in num_cols_to_format_dt){
      if(col %in% names(selected_display_data) && is.numeric(selected_display_data[[col]])){
        selected_display_data[[col]] <- scales::comma(selected_display_data[[col]], accuracy = 0, big.mark = ".", decimal.mark = ",", na_str = "–") # N/A als Strich
      }
    }
    ratio_cols <- c("mv_to_potential_ratio", "fee_to_potential_ratio")
    for(col in ratio_cols){
      if(col %in% names(selected_display_data) && is.numeric(selected_display_data[[col]])){
        selected_display_data[[col]] <- scales::percent(selected_display_data[[col]], accuracy = 0.1, na_str = "–")
      }
    }
    if("contract_remaining_years" %in% names(selected_display_data) && is.numeric(selected_display_data$contract_remaining_years)){
      selected_display_data$contract_remaining_years <- ifelse(is.na(selected_display_data$contract_remaining_years), "–", paste0(round(selected_display_data$contract_remaining_years, 1), " J."))
    }
    score_cols <- c("opportunity_score", "productivity_per_90") # productivity_per_90 fehlt noch in cols_to_display
    for(col in score_cols){
      if(col %in% names(selected_display_data) && is.numeric(selected_display_data[[col]])){
        selected_display_data[[col]] <- ifelse(is.na(selected_display_data[[col]]), "–", round(selected_display_data[[col]], 2))
      }
    }
    
    # Spalten umbenennen für bessere Lesbarkeit in der Tabelle
    colnames_map <- c(name="Name", age="Alter", position="Pos.", club_name="Akt. Club", market_value="Marktwert", 
                      predicted_potential_market_value="Potenzial-MW", mv_to_potential_ratio="MW/Potenzial",
                      transfer_fee="Letzte Fee", fee_to_potential_ratio="Fee/Potenzial", 
                      transaction_valuation_status="Deal", opportunity_score="Opp. Score", 
                      opportunity_flag="Scouting Flag", contract_remaining_years="Restvertrag",
                      prev_season_minutes="Min. Vors.", prev_season_goals="Tore Vors.", prev_season_assists="Ass. Vors.")
    
    current_colnames <- names(selected_display_data)
    new_colnames <- ifelse(current_colnames %in% names(colnames_map), colnames_map[current_colnames], current_colnames)
    # names(selected_display_data) <- new_colnames # Umbenennung erfolgt in DT options
    
    dt_obj <- DT::datatable(selected_display_data, 
                            options = list(pageLength = 10, searching = TRUE, scrollX = TRUE, autoWidth=FALSE,
                                           columnDefs = list(list(targets = which(names(selected_display_data) == "player_id")-1, visible = FALSE)), # Verstecke player_id Spalte (Index ist 0-basiert)
                                           language = list(search = "_INPUT_", searchPlaceholder = "Suche...",
                                                           lengthMenu = "Zeige _MENU_", info = "_START_-_END_ von _TOTAL_",
                                                           infoEmpty= "0 Einträge", infoFiltered = "(von _MAX_)",
                                                           zeroRecords = "Nichts gefunden",
                                                           paginate = list(previous = '‹', `next` = '›')) ), 
                            rownames = FALSE, filter = 'top', escape = FALSE, selection = 'single',
                            colnames = new_colnames[current_colnames %in% names(selected_display_data)] # Wende neue Spaltennamen an
    )
    
    if("opportunity_flag" %in% names(selected_display_data)){ # Benutze den originalen Spaltennamen für formatStyle
      dt_obj <- dt_obj %>% formatStyle( 'opportunity_flag', target = 'row',
                                        backgroundColor = styleEqual( c("💎 Top-Juwel", "🔥 Heißer Kandidat", "👀 Beobachten"), 
                                                                      c('rgba(40, 167, 69, 0.2)', 'rgba(255, 193, 7, 0.2)', 'rgba(220, 53, 69, 0.1)')),
                                        fontWeight = styleEqual(c("💎 Top-Juwel"), c("bold"))
      )
    }
    if("transaction_valuation_status" %in% names(selected_display_data)){
      dt_obj <- dt_obj %>% formatStyle( 'transaction_valuation_status',
                                        color = styleEqual( c("Günstiger Deal", "Teurer Deal"), c('darkgreen', 'darkred')))
    }
    return(dt_obj)
    
  }, server = TRUE)
}

# App starten
shinyApp(ui, server)

# Erstellen Sie einen Ordner namens 'www' im selben Verzeichnis wie app.R
# und fügen Sie eine Datei 'custom_styles.scss' (oder .css) hinzu mit z.B.:
# /* www/custom_styles.scss */
# .card-header { font-weight: 600; background-color: rgba(0,0,0,0.03) !important; }
# .value-box .value-box-title { font-size: 0.8rem !important; text-transform: uppercase; color: #555; }
# .value-box .value-box-value { font-size: 1.5rem !important; font-weight: 700; }
# table.dataTable th { font-size: 0.85em !important; }
# table.dataTable td { font-size: 0.85em !important; }