scrape_chl_game <- function(game_id, league = c("WHL","OHL","QMJHL")){
  
  # handle qmjhl french abbreviation
  league_code <- ifelse(tolower(league) == "qmjhl","lhjmq",tolower(league))
  
  # get league key
  key <- get_league_key(league)
  
  if(is.null(key)){
    stop(paste0(league," is not a valid league. Please enter one of c('WHL','OHL','QMJHL')"))
  }
  
  url <- paste0("https://cluster.leaguestat.com/feed/index.php?feed=gc&key=",key,"&client_code=",league_code,"&game_id=",game_id,"&lang_code=en&fmt=json&tab=pxpverbose")
  
  site <- tryCatch(
    jsonlite::read_json(url),
    warning = function(cond){
      message(paste0("There was an error connecting to the ",toupper(league)," server \n\n",cond))
      return(NULL)
    },
    error = function(cond){
      message(paste0("There was an error connecting to the ",toupper(league)," server \n\n",cond))
      return(NULL)
    }
  )
  
  if(is.null(site)){
    stop("Could not get game data for game ID ",game_id,". Check your internet connection and try again later")
  }
  
  pbp <- site$GC$Pxpverbose |>
    dplyr::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::select(-player_team_id) |>
    tidyr::unnest_wider(player, names_sep = "_") |>
    tidyr::unnest_wider(goal_scorer, names_sep = "_") |>
    utils::type.convert(as.is = TRUE) |>
    dplyr::mutate(
      period_id = ifelse(is.na(period_id), period, period_id),
      game_seconds = s + 1200 * (as.numeric(period_id) - 1),
      period_time = hms::hms(s),
      game_time = hms::hms(game_seconds),
      period_ordinal = dplyr::case_when(
        period_id == 1 ~ "1st",
        period_id == 2 ~ "2nd",
        period_id == 3 ~ "3rd",
        TRUE ~ paste0(period_id,"th")
      ),
      x = x_location / 3 - 100,
      y = (y_location-150) / 3
    ) |>
    dplyr::select(-period) |>
    dplyr::rename(
      period_seconds = s,
      period = period_id
      )
  
  return(pbp)
}