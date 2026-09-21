# Run from the assignment-3b RStudio project to rebuild the daily input CSV.
# Source: https://data.mendeley.com/datasets/ycy3sy3vj2/1
# Wannigamage, Barlow, Lakshika, and Kasmarik (2020), version 1.
# DOI: 10.17632/ycy3sy3vj2.1; license: CC BY 4.0.
# Transformation: average available five-minute player counts by UTC date.
library(dplyr)

games <- read.csv("applicationInformation.csv") %>%
  filter(appid %in% c(570, 730)) %>%
  select(appid, game = name)
stopifnot(nrow(games) == 2L)

daily_players <- bind_rows(lapply(games$appid, function(id) {
  connection <- unz("PlayerCountHistoryPart1.zip",
                    paste0("PlayerCountHistoryPart1/", id, ".csv"))
  readings <- read.csv(connection) %>%
    mutate(date = as.Date(substr(Time, 1, 10))) %>%
    filter(date >= as.Date("2019-01-01"), date <= as.Date("2019-12-31"))

  stopifnot(!anyDuplicated(readings$Time),
            all(readings$Playercount >= 0, na.rm = TRUE))

  readings %>%
    group_by(date) %>%
    summarise(
      daily_avg_players = mean(Playercount, na.rm = TRUE),
      recorded_slots = n(),
      available_readings = sum(!is.na(Playercount)),
      missing_readings = sum(is.na(Playercount)),
      zero_readings = sum(Playercount == 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(appid = id)
})) %>%
  left_join(games, by = "appid") %>%
  select(appid, game, everything()) %>%
  arrange(game, date)

stopifnot(nrow(daily_players) == 730L,
          all(is.finite(daily_players$daily_avg_players)),
          all(daily_players$recorded_slots == 288L))
write.csv(daily_players, "steam_daily_2019.csv", row.names = FALSE)
