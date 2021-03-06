library(dplyr)
library(lubridate)
library(jsonlite)
library(tidyr)
library(purrr)
library(readr)

key <- Sys.getenv("api_key")

get_summoner_data <- function(name) {
  
  json_url <- paste0("https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-name/", name, "?api_key=", key)
  
  tibble(
    key = names(fromJSON(json_url, simplifyVector = FALSE)),
    json = fromJSON(json_url, simplifyVector = FALSE)
  ) %>% 
    mutate(json = map(json, as.character)) %>% 
    unnest(json) %>% 
    pivot_wider(
      names_from = key,
      values_from = json
    ) %>% 
    type_convert() %>% 
    mutate(revisionDate = as.POSIXct(revisionDate/1000, origin="1970-01-01"))
}
# get_summoner_data("Jocar")

jocar_puuid <- "PUVIXbSAf37rmqeF90dnVLSy_nKUCCWVjv1i4a2h_DciV25Wjr5vgiRVLDd4Wi-rQ_w-7cSsQhje6A"
paintrain_puuid <- "ns6A_0BsSV7nKvhtK8bmEKK4gVy_2wUWPN2Joe2MQuzJnnA0zTU-nijiSqVWgRw-o2f-TOZ-eGoNNA"
bigfish_puuid <- "KDpgUXKFafwS5CLMzit77xE-Zlin6Y6pLOUM_d8ZjFretTfwMDLa1OeissL9YhjxQySLQGuDbd_-4Q"
locked_puuid <- "BZvNWALIu_0S1Ic69c9tEe2Sur7U_wxCasc8IfOZrTjFjH5rxohd8V3iGvNWuzuGeOCJBerTd9FWkg"
blua_puuid <- "RfESDk2R3LJSBYsOIJVv4KCdwe6DV65QcQ9qt3jRzH8sROhs4-T8xtFEraFZgR4F5r30OedwOFbAMA"

get_game_ids <- function(puuid) {
  all_games <- NULL
  for (i in 1:10) {
    print((i-1)*100)
    all_games[[i]] <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/", puuid, "/ids?start=", (i-1)*100, "&count=100&api_key=", key) %>% 
      fromJSON() %>% 
      as_tibble()
  }  
  game_ids <- bind_rows(all_games) %>% pull(value)
  
  return(game_ids)
}

game_ids <- tibble(
  name = c("Jocar", "Paintrain100", "BigFish84", "locked", "blua"),
  puuids = c(jocar_puuid, paintrain_puuid, bigfish_puuid, locked_puuid, blua_puuid)
) %>% 
  rowwise() %>% 
  mutate(game_ids = map(puuids, get_game_ids))

game_file <-  read_rds("data/all_games.rds") %>% 
  mutate(game_id = paste0(platformId, "_", gameId))

new_games <- game_ids %>% 
  ungroup() %>% 
  unnest(game_ids) %>% 
  distinct(game_ids) %>% 
  select(game_id = 1) %>% 
  anti_join(
    game_file %>% 
      distinct(game_id)
  ) %>% 
  pull(game_id)

get_game_data <- function(game_id, api_key) {
  
  print(paste0("Fetching data from: ", game_id))
  
  game_data <- paste0("https://europe.api.riotgames.com/lol/match/v5/matches/", game_id, "?api_key=", api_key) %>% 
    fromJSON(simplifyVector = FALSE)
  
  data <- tibble(
    key = names(game_data),
    json_raw = game_data
  ) %>% 
    filter(key == "info") %>% 
    unnest_wider(json_raw) %>% 
    unnest(participants) %>% 
    unnest_wider(participants) %>%
    mutate(gameStartTimestamp = as.POSIXct(gameStartTimestamp/1000, origin="1970-01-01")) %>% 
    select(gameMode, gameDuration, gameStartTimestamp, queueId, championName, kills, deaths, assists, lane, summonerName, summonerLevel, win, everything())
  
  return(data)
  
}

steps <- 100
print(paste0("Process will take: ", ((seq(1, length(new_games), steps) %>% length() - 1) * 135 / 60), " minutes."))
all_game_data <- NULL
for (i in seq(1, length(new_games), steps)) {
  Sys.sleep(135)
  
  while ((i %% steps) != 0) {
    all_game_data[[i]] <- get_game_data(new_games[i], key)
    i <- i + 1
  }
  
  print("Schlafe 2min")
  Sys.sleep(135) # in truth 2min 15sec
}

file_name <- paste0("data/all_games.rds")
all_game_data %>% 
  bind_rows() %>% 
  bind_rows(game_file) %>% 
  write_rds(., file = file_name)

