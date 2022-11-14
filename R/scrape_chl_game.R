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
    utils::type.convert(as.is = TRUE) |>
    dplyr::mutate(
      period_id = suppressWarnings(ifelse(is.na(period_id), as.integer(period), as.integer(period_id))),
      game_seconds = s + 1200 * (as.numeric(period_id) - 1),
      period_time = hms::hms(s),
      game_time = hms::hms(game_seconds),
      period_ordinal = dplyr::case_when(
        period_id == 1 ~ "1st",
        period_id == 2 ~ "2nd",
        period_id == 3 ~ "3rd",
        TRUE ~ paste0(period_id,"th")
      ),
      x_fixed = round(x_location / 3 - 100),
      y_fixed = round((y_location-150) / 3),
      event_id = dplyr::row_number()
    ) |>
    dplyr::select(-period) |>
    dplyr::rename(
      period_seconds = s,
      period = period_id
      )
  
  # split it up
  
  #faceoffs
  fo <- dplyr::filter(pbp, event == "faceoff") |>
    dplyr::mutate(
      event_player_1_id = ifelse(home_win == 1, home_player_id, visitor_player_id),
      event_player_2_id = ifelse(home_win == 0, home_player_id, visitor_player_id),
      event_player_1_type = "Winner",
      event_player_2_type = "Loser"
    )
  
  # shots and goals
  
  # goals are counted in two rows -- one for a shot and one for a goal immediately after
  
  shots <- dplyr::filter(pbp, event %in% c("goal","shot")) |>
    # fill in missing values in goal with prior shot event
    dplyr::mutate(
      shot_type = ifelse(event == "goal", dplyr::lag(shot_type), shot_type),
      shot_type_description = ifelse(event == "goal", dplyr::lag(shot_type_description), shot_type_description),
      shot_quality_description = ifelse(event == "goal", dplyr::lag(shot_quality_description), shot_quality_description),
      shot_quality = ifelse(event == "goal", dplyr::lag(quality), quality),
      game_goal_id = ifelse(event == "goal", dplyr::lag(game_goal_id), game_goal_id),
      goalie = ifelse(event == "goal", dplyr::lag(goalie), goalie)
    ) |>
    # remove shot event immediately prior to goal because they're the same event
    dplyr::filter(dplyr::lead(event) != "goal") |>
    # get rid of goalie columns; will fill with unnest(goalie)
    dplyr::select(-goalie_id, -goalie_team_id) |>
    tidyr::unnest_wider(goal_scorer, names_sep = "_") |>
    tidyr::unnest_wider(assist1_player, names_sep = "_") |>
    tidyr::unnest_wider(assist2_player, names_sep = "_") |>
    tidyr::unnest_wider(goalie, names_sep = "_") |>
    dplyr::mutate(
      player_id = ifelse(event == "goal", goal_scorer_player_id, player_id)
    )
  
  # penalties
  
  pens <- dplyr::filter(pbp, event == "penalty") |>
    dplyr::rename(
      penalty = player_penalized_info,
      served = player_served_info
      ) |>
    tidyr::unnest_wider(penalty, names_sep = "_") |>
    tidyr::unnest_wider(served, names_sep = "_")
  
  # goalie changes
  goalies <- dplyr::filter(pbp, event == "goalie_change") |>
    dplyr::mutate(
      goalie_out_id = ifelse(!is.na(dplyr::lead(goalie_out_id)), dplyr::lead(goalie_out_id), goalie_out_id)
    ) |>
    dplyr::filter(!is.na(goalie_in_id))
  
  pbp <- dplyr::bind_rows(shots, pens, fo, goalies) |>
    dplyr::arrange(event_id) |>
    dplyr::mutate(
      game_id = game_id,
      event_idx = stringr::str_pad(dplyr::row_number(), width = 4, side = "left", pad = 0),
      event_id = as.numeric(paste0(game_id,event_idx))
    )
  
  # add event player columns
  pbp <- pbp |>
    dplyr::mutate(
      event_player_1_id = dplyr::case_when(
        event == "goal" ~ goal_scorer_player_id,
        event == "faceoff" ~ event_player_1_id,
        event == "goalie_change" ~ goalie_in_id,
        TRUE ~ player_id
      ),
      event_player_1_type = dplyr::case_when(
        event == "goal" ~ "Scorer",
        event == "faceoff" ~ event_player_1_type,
        event == "penalty" ~ "Penalty On",
        event == "goalie_change" ~ "Goalie In",
        event == "shot" ~ "Shooter"
      ),
      event_player_2_id = dplyr::case_when(
        event == "goal" ~ assist1_player_player_id,
        event == "faceoff" ~ event_player_2_id,
        event == "goalie_change" ~ goalie_out_id,
        event == "penalty" ~ served_player_id,
        event == "shot" ~ NA_integer_
      ),
      event_player_2_type = dplyr::case_when(
        event == "goal" ~ "Primary Assist",
        event == "faceoff" ~ event_player_2_type,
        event == "penalty" ~ "Served By",
        event == "goalie_change" ~ "Goalie Out",
        TRUE ~ NA_character_
      ),
      event_player_3_id = ifelse(event == "goal", assist2_player_player_id, NA_integer_),
      event_player_3_type = dplyr::case_when(
        event == "goal" ~ "Secondary Assist",
        TRUE ~ NA_character_
      ),
      event_goalie_id = goalie_player_id,
      secondary_type = dplyr::case_when(
        event %in% c("goal","shot") ~ shot_type_description,
        event == "penalty" ~ lang_penalty_description,
        TRUE ~ NA_character_
      )
    )
  
  # get player ids and team info
  source("R/scrape_chl_game_stats.R")
  rosters <- scrape_chl_game_stats(game_id, league) |>
    dplyr::select(player_name:team_name)
  
  pbp <- pbp |>
    # event_player_1
    dplyr::left_join(
      rosters |>
        dplyr::select(
          event_player_1_id = player_id,
          event_player_1_name = player_name,
          event_team = team_name,
          event_team_abbr = team_code
        ),
      by = "event_player_1_id"
    ) |>
    # event_player_2
    dplyr::left_join(
      rosters |>
        dplyr::select(
          event_player_2_id = player_id,
          event_player_2_name = player_name
        ),
      by = "event_player_2_id"
    ) |>
    # event_player_3
    dplyr::left_join(
      rosters |>
        dplyr::select(
          event_player_3_id = player_id,
          event_player_3_name = player_name
        ),
      by = "event_player_3_id"
    ) |>
    # goalie_id
    dplyr::left_join(
      rosters |>
        dplyr::select(
          event_goalie_id = player_id,
          event_goalie_name = player_name
        ),
      by = "event_goalie_id"
    )
  
  # reorder columns
  pbp <- pbp |>
    #dplyr::mutate(
    #  # add shot distance/angle
    #  shot_distance = dplyr::case_when(
    #    event_team == home_name & event_type %in% fenwick_events ~
    #      round(abs(sqrt((x_fixed - 89)^2 + (y_fixed)^2)),1),
    #    event_team == away_name & event_type %in% fenwick_events ~
    #      round(abs(sqrt((x_fixed - (-89))^2 + (y_fixed)^2)),1)
    #  ),
    #  shot_angle = dplyr::case_when(
    #    event_team == home_name & event_type %in% fenwick_events ~
    #      round(abs(atan((0-y_fixed) / (89-x_fixed)) * (180 / pi)),1),
    #    event_team == away_name & event_type %in% fenwick_events ~
    #      round(abs(atan((0-y_fixed) / (-89-x_fixed)) * (180 / pi)),1)
    #  ),
    #  # fix behind the net angles
    #  shot_angle = ifelse(
    #    (event_team == home_name & x_fixed > 89) |
    #      (event_team == away_name & x_fixed < -89),
    #    180 - shot_angle,
    #    shot_angle
    #  ),
    #  event_team_type =  dplyr::case_when(
    #    event_team == home_name ~ "home",
    #    event_team == away_name ~ "away"
    #  )
    #) |>
    dplyr::select(
      event, secondary_type, event_team, event_team_abbr,
      period, period_seconds, game_seconds,
      event_player_1_name, event_player_1_type,
      event_player_2_name, event_player_2_type,
      event_player_3_name, event_player_3_type,
      event_goalie_name,
      penalty_minutes = minutes, penalty_severity = penalty_class,
      x_fixed, y_fixed, #shot_distance, shot_angle,
      plus, minus,
      dplyr::everything()
    )
  
  return(pbp)
}