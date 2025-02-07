# Script for making tables
library(tidyverse)
library(gt)
library(targets)
library(here)
library(igraph)

tar_load(flightGraphs)
tar_load(feedingGraphs)
tar_load(roostingGraphs)
tar_load(season_names)

all <- data.frame(seasonUnique = season_names,
                  flight = map_dbl(flightGraphs, ~length(igraph::V(.x))),
                  feeding = map_dbl(feedingGraphs, ~length(igraph::V(.x))),
                  roosting = map_dbl(roostingGraphs, ~length(igraph::V(.x)))) %>%
  mutate(year = str_extract(seasonUnique, "[0-9]+"),
         season = str_extract(seasonUnique, "[a-z]+")) %>%
  select(-seasonUnique) %>%
  mutate(type = "social network")# there are some differences--and I think that's because of how I factored out the individuals that were never observed participating in that behavior, before calculating interactions.

tar_load(linked)
focal <- linked %>%
  group_by(seasonUnique, type) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "type", values_from = "n") %>%
  mutate(year = str_extract(seasonUnique, "[0-9]+"),
         season = str_extract(seasonUnique, "[a-z]+")) %>%
  ungroup() %>%
  select(-seasonUnique) %>%
  mutate(type = "space use")

indivs <- bind_rows(all, focal) %>%
  relocate(year, season) %>%
  arrange(year, season) %>%
  pivot_longer(c("flight", "feeding", "roosting"), names_to = "situ", values_to = "n") %>%
  pivot_wider(names_from = "type", values_from = "n")

# For comparison: how many tagged birds in the entire uncleaned da --------
tar_load(joined0)# to get total numbers of tagged birds during the same time periods
with_seasons <- joined0 %>% mutate(month = lubridate::month(timestamp),
                                   day = lubridate::day(timestamp),
                                   year = lubridate::year(timestamp)) %>%
  mutate(season = case_when(((month == 12 & day >= 15) | 
                               (month %in% 1:4) | 
                               (month == 5 & day < 15)) ~ "breeding",
                            ((month == 5 & day >= 15) | 
                               (month %in% 6:8) | 
                               (month == 9 & day < 15)) ~ "summer",
                            .default = "fall")) %>%
  filter(dateOnly != "2023-09-15") %>% # remove September 15 2023, because it turns out the season boundaries are supposed to be non-inclusive and this is the latest one.
  mutate(seasonUnique = case_when(season == "breeding" & month == 12 ~
                                    paste(as.character(year + 1), season, sep = "_"),
                                  .default = paste(as.character(year), season, sep = "_"))) %>%
  mutate(seasonUnique = factor(seasonUnique, levels = c("2020_summer", "2020_fall", "2021_breeding", "2021_summer", "2021_fall", "2022_breeding", "2022_summer", "2022_fall", "2023_breeding", "2023_summer")),
         season = factor(season, levels = c("breeding", "summer", "fall")))
all_tagged <- with_seasons %>%
  select(local_identifier, month, day, year, season, seasonUnique) %>%
  group_by(year, season) %>%
  summarize(`all tagged` = length(unique(local_identifier)))
a_t <- bind_rows(all_tagged %>% mutate(situ = "flight"),
                 all_tagged %>% mutate(situ = "feeding"),
                 all_tagged %>% mutate(situ = "roosting")) %>%
  mutate(year = as.character(year))

indivs <- left_join(indivs, a_t)


# Make a nice table -------------------------------------------------------
min <- min(c(indivs$`space use`, indivs$`social network`, indivs$`all tagged`))
max <- max(c(indivs$`space use`, indivs$`social network`, indivs$`all tagged`))
tar_load(season_names)

tab <- indivs %>%
  mutate(yrsz = paste(year, season, sep = " ")) %>%
  mutate(yrsz = factor(yrsz, levels = str_replace(season_names, "_", " "))) %>%
  arrange(yrsz) %>%
  select(-c("year", "season")) %>%
  pivot_longer(cols = c("social network", "space use", "all tagged"), names_to = "type", values_to = "count") %>%
  pivot_wider(names_from = "yrsz", values_from = "count") %>%
  arrange(situ, type) %>%
  mutate(situ = str_to_title(situ)) %>%
  gt(groupname_col = "situ", rowname_col = "type") %>%
  tab_header(
    title = md("**Number of vultures**"),
  ) %>%
  data_color(columns = starts_with("20"),
             palette = "Oranges",
             domain = c(min, max)) %>%
  tab_style(
    style = "padding-left:20px;",
    locations = cells_stub()
  )
tab
gtsave(tab, filename = here("fig/tab1.png"))
# XXX can do more with levels with years and columns I think

# Degree and strength per season and situation ----------------------------
head(linked)
summ <- linked %>%
  ungroup() %>%
  select(seasonUnique, year, season, type, degree, strength) %>%
  pivot_longer(cols = c("degree", "strength"), 
               names_to = "measure", values_to = "value") %>%
  group_by(seasonUnique, year, season, type, measure) %>%
  summarize(mn = round(mean(value), 2),
            sd = round(sd(value), 2),
            min = round(min(value), 2),
            max = round(max(value), 2),
            words = paste0(mn, " (", sd, ")<br>", 
                           min, "-", max))

tab2 <- summ %>%
  ungroup() %>%
  select(-c("year", "season")) %>%
  select(seasonUnique, type, measure, words) %>%
  mutate(seasonUnique = str_replace(seasonUnique, "_", " ")) %>%
  pivot_wider(id_cols = c("measure", "type"), names_from = "seasonUnique", values_from = "words") %>%
  mutate(measure = str_to_title(measure),
         type = str_to_title(type)) %>%
  group_by(measure) %>%
  gt(rowname_col = "type") %>%
  tab_style(
    style = "padding-left:20px;",
    locations = cells_stub()
  ) %>%
  fmt_markdown() %>%
  tab_header(
    title = md("**Social network measures**"),
    subtitle = md("mean (sd)<br>min-max")
  )
tab2
gtsave(tab2, filename = here("fig/tab2.png"))
