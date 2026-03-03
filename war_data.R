library(rvest)
library(dplyr)
library(janitor)
library(readr)

# 2025
url <- "https://www.espn.com/mlb/war/leaders/_/year/2025/type/seasonal/alltime/false/count/1"

tables <- read_html(url) |>
  html_elements("table") |>
  html_table(fill = TRUE)

war <- tables[[ which.max(sapply(tables, nrow)) ]] |>
  janitor::clean_names()

war
write_csv(war, "2025_war.csv")

# 2024
url1 <- "https://www.espn.com/mlb/war/leaders/_/type/seasonal/year/2024"

tables <- read_html(url1) |>
  html_elements("table") |>
  html_table(fill = TRUE)

war1 <- tables[[ which.max(sapply(tables, nrow)) ]] |>
  janitor::clean_names()

war1
write_csv(war1, "2024_war.csv")

# 2023
url2 <- "https://www.espn.com/mlb/war/leaders/_/type/seasonal/year/2023"

tables <- read_html(url2) |>
  html_elements("table") |>
  html_table(fill = TRUE)

war2 <- tables[[ which.max(sapply(tables, nrow)) ]] |>
  janitor::clean_names()

war2
write_csv(war2, "2023_war.csv")