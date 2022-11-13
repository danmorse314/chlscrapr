scrape_game_whl <- function(game_id){
  url <- paste0("https://cluster.leaguestat.com/feed/index.php?feed=gc&key=41b145a848f4bd67&client_code=whl&game_id=",game_id,"lang_code=en&fmt=json&tab=pxpverbose")
  
  site <- jsonlite::read_json(url)
  
  pbp <- site$GC |>
    dplyr::tibble() |>
    dplyr::slice(2) |>
    tidyr::unnest_longer(1) |>
    tidyr::unnest_wider(1)
}