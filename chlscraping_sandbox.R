source("R/get_chl_schedule.R")
source("R/get_league_key.R")
source("R/scrape_chl_game.R")
library(ggplot2)

# get team information/logos
url <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=teamsbyseason&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&season_id=73&league_code=&fmt=json"

site <- jsonlite::read_json(url)

teams <- site$SiteKit$Teamsbyseason |>
  dplyr::tibble() |>
  tidyr::unnest_wider(1)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}

schedule <- get_chl_schedule(2023, "ohl", "Reg")

game_id <- dplyr::filter(schedule, date_played == Sys.Date())$game_id[7]

pbp <- scrape_chl_game(game_id, "OHL")

shots <- dplyr::filter(pbp, event == "shot")

team_logos <- teams |>
  dplyr::filter(code %in% unique(pbp$team_code)) |>
  # add in dummy variables to put logos on the ice
  dplyr::mutate(x = ifelse(code == "NB", 50, -50),
                y = 0)

sportyR::geom_hockey("NHL") +
  ggimage::geom_image(
    data = team_logos,
    ggplot2::aes(x = x, y = y, image = team_logo_url),
    image_fun = transparent, size = 0.22, asp = 2.2
  ) +
  geom_point(
    data = shots,
    aes(x,y, color = player_team_code),
    size = 6, alpha = .6, show.legend = FALSE
  ) +
  geom_text(
    data = dplyr::filter(pbp, event == "goal"),
    aes(x,y, label = goal_scorer_jersey_number),
    size = 3, color = "white"
  ) +
  scale_color_manual(values = c("#002F6C","#787121"))
