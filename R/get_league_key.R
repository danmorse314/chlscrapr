get_league_key <- function(league){
  if(tolower(league) == "ohl"){
    key <- "2976319eb44abe94="
  } else if(tolower(league) == "whl"){
    key <- "41b145a848f4bd67"
  } else if(tolower(league) == "qmjhl" | tolower(league) == "lhjmq"){
    key <- "f322673b6bcae299"
  } else {
    key <- NULL
  }
}
