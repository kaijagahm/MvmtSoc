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
  mutate(type = "all")# there are some differences--and I think that's because of how I factored out the individuals that were never observed participating in that behavior, before calculating interactions.

tar_load(linked)
focal <- linked %>%
  group_by(seasonUnique, type) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = "type", values_from = "n") %>%
  mutate(year = str_extract(seasonUnique, "[0-9]+"),
         season = str_extract(seasonUnique, "[a-z]+")) %>%
  ungroup() %>%
  select(-seasonUnique) %>%
  mutate(type = "focal")

indivs <- bind_rows(all, focal) %>%
  relocate(year, season) %>%
  arrange(year, season) %>%
  pivot_longer(c("flight", "feeding", "roosting"), names_to = "situ", values_to = "n") %>%
  pivot_wider(names_from = "type", values_from = "n") %>%
  mutate(proportion = round(focal/all, 2))

indivs %>%
  summarize(mn_all = mean(all),
            sd_all = sd(all),
            mn_focal = mean(focal),
            sd_focal = sd(focal),
            mn_prop = mean(proportion),
            sd_prop = sd(proportion))


# Make a nice table -------------------------------------------------------
min <- min(c(indivs$focal, indivs$all))
max <- max(c(indivs$focal, indivs$all))
tar_load(season_names)

tab <- indivs %>%
  mutate(yrsz = paste(year, season, sep = " ")) %>%
  mutate(yrsz = factor(yrsz, levels = str_replace(season_names, "_", " "))) %>%
  arrange(yrsz) %>%
  select(-c("year", "season")) %>%
  pivot_longer(cols = c("all", "focal"), names_to = "type", values_to = "count") %>%
  pivot_wider(names_from = "yrsz", values_from = "count") %>%
  arrange(situ, type) %>%
  rename("Situation" = situ,
         "# vultures" = type) %>%
  gt(groupname_col = "Situation") %>%
  tab_header(
    title = "Number of individuals",
    subtitle = "Space use measured for focal vultures only"
  ) %>%
  data_color(columns = starts_with("20"),
             palette = "Oranges",
             domain = c(min, max))
gtsave(tab, filename = here("fig/tab1.png"))
# XXX can do more with levels with years and columns I think
