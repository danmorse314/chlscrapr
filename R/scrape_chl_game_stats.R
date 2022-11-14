scrape_chl_game_stats <- function(game_id, league = c("WHL","OHL","QMJHL")){
  
  # handle qmjhl french abbreviation
  league_code <- ifelse(tolower(league) == "qmjhl","lhjmq",tolower(league))
  
  # get league key
  key <- get_league_key(league)
  
  if(is.null(key)){
    stop(paste0(league," is not a valid league. Please enter one of c('WHL','OHL','QMJHL')"))
  }
  
  url <- paste0("https://cluster.leaguestat.com/feed/index.php?feed=gc&key=",key,"&client_code=",league_code,"&game_id=",game_id,"&lang_code=en&fmt=json&tab=gamesummary")
  
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
  
  home_team <- site$GC$Gamesummary$home |>
    stack() |>
    dplyr::tibble() |>
    tidyr::pivot_wider(names_from = ind, values_from = values) |>
    dplyr::select(team_id, team_code, team_name = name, team_city = city, team_nickname = nickname)
  
  home_skaters <- site$GC$Gamesummary$home_team_lineup |>
    dplyr::tibble() |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::mutate(player_name = paste(first_name, last_name)) |>
    dplyr::bind_cols(home_team)
  
  away_team <- site$GC$Gamesummary$visitor |>
    stack() |>
    dplyr::tibble() |>
    tidyr::pivot_wider(names_from = ind, values_from = values) |>
    dplyr::select(team_id, team_code, team_name = name, team_city = city, team_nickname = nickname)
  
  away_skaters <- site$GC$Gamesummary$visitor_team_lineup |>
    dplyr::tibble() |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1) |>
    dplyr::mutate(player_name = paste(first_name, last_name)) |>
    dplyr::bind_cols(away_team)
  
  game_stats <- dplyr::bind_rows(home_skaters, away_skaters) |>
    dplyr::select(player_name, player_id, team_id:team_nickname, dplyr::everything()) |>
    utils::type.convert(as.is = TRUE)
  
  return(game_stats)
}