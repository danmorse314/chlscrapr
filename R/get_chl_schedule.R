get_chl_schedule <- function(season, league = c("WHL","OHL","QMJHL"), season_type = c("Reg","Playoffs")){
  
  # getting season IDs
  
  league_code <- ifelse(tolower(league) == "qmjhl","lhjmq",tolower(league))
  
  if(tolower(league) == "ohl"){
    key <- "2976319eb44abe94&="
  } else if(tolower(league) == "whl"){
    key <- "41b145a848f4bd67"
  } else if(tolower(league) == "qmjhl"){
    key <- "f322673b6bcae299"
  }
  
  url <- paste0("https://lscluster.hockeytech.com/feed/?feed=modulekit&view=seasons&key=",key,"&fmt=json&client_code=",league_code,"&lang=en&league_code=&fmt=json")
  
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
    stop("Could not get league schedule. Check your internet connection and try again later")
  }
  
  # create season ID dataframe
  df <- site$SiteKit$Seasons |>
    dplyr::tibble() |>
    tidyr::unnest_wider(1) |>
    dplyr::filter(stringr::str_detect(tolower(season_name), "pre", negate = TRUE))
  
  # cleaning up season ID dataframe
  if(tolower(league) == "ohl"){
    
    suppressWarnings({
      df <- df |>
        tidyr::separate(
          season_name, into = c("season","season_type"),
          sep = "\\ ", extra = "merge", remove = FALSE
        ) |>
        dplyr::mutate(
          season = ifelse(
            season_type == "Playoffs",
            paste0(as.numeric(season)-1,"-",substr(season,3,4)),
            season
          ),
          season_short = as.numeric(substr(season,1,4))+1
        ) |>
        utils::type.convert(as.is = TRUE)
    })
  } else if(tolower(league) == "qmjhl"){
    
    df <- df |>
      tidyr::separate(
        season_name, into = c("season","season_type"),
        sep = "\\ \\|\\ ", extra = "merge", remove = FALSE
      ) |>
      dplyr::mutate(
        season_short = ifelse(
          season_type != "Playoffs",
          as.numeric(substr(season,1,4))+1,
          as.numeric(substr(season,1,4))
        )
      ) |>
      utils::type.convert(as.is = TRUE)
  } else if(tolower(league == "whl")){
    
    suppressWarnings({
      df <- df |>
        dplyr::mutate(
          season_name = stringr::str_replace(season_name, "\\ \\-\\ ","-"),
          season_name = stringr::str_remove_all(season_name, "\\ WHL")
        ) |>
        tidyr::separate(
          season_name, into = c("season","season_type"),
          sep = "\\ ", extra = "merge", remove = FALSE
        ) |>
        dplyr::mutate(
          season = ifelse(
            season_type == "Playoffs",
            paste0(as.numeric(season)-1,"-",substr(season,3,4)),
            season
          ),
          season_short = as.numeric(substr(season,1,4))+1
        ) |>
        utils::type.convert(as.is = TRUE)
    })
  }
  
  # define season to pull from function argument
  season_to_pull <- season
  type <- dplyr::case_when(
    season_type == "Reg" ~ "Regular Season",
    season_type == "Playoffs" ~ "Playoffs"
  )
  
  season_id <- dplyr::filter(
    df, (season == season_to_pull | season_short == season_to_pull) &
      season_type == type
  )$season_id
  
  # check to see if season exists in season ID dataframe
  if(length(season_id) == 0){
    stop(paste0("Could not get ",toupper(league)," schedule for ",season,
                "\nPlease enter a season between ",min(df$season_short)," and ",max(df$season_short)))
  }
  
  # scrape season schedule
  url <- paste0("https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&key=",key,"&fmt=json&client_code=",league_code,"&lang=en&season_id=",season_id,"&team_id=&league_code=&fmt=json")
  
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
    stop("Could not get league schedule. Check your internet connection and try again later")
  }
  
  # get schedule
  sched <- site$SiteKit$Schedule |>
    dplyr::tibble() |>
    tidyr::unnest_wider(1) |>
    utils::type.convert(as.is = TRUE) |>
    # add season name/type info
    dplyr::left_join(
      dplyr::select(df, dplyr::starts_with("season")),
      by = "season_id"
    ) |>
    # add league info
    dplyr::mutate(league = toupper(league))
  
  return(sched)
}