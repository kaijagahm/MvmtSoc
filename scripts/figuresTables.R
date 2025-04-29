library(tidyverse)
library(here)
library(targets)
library(patchwork)
library(ggeffects)
library(gtsummary)
library(extrafont)
library(ggthemes)
library(lemon)
library(see)
library(sf)
library(scales)
library(ggmap)
library(glmmTMB) # have to have this loaded because emmeans cannot actually handle models of class glmmTMB, but the glmmTMB package has implemented its own methods for this.
library(gt)
library(ggfortify)

tar_config_set(store = here::here('_targets'))
tar_load(cc)
tar_load(linked)
tar_load(situcolors)
tar_load(seasoncolors)
linked2 <- readRDS(here("data/created/linked2.RDS"))
tar_load(flightGraphs)
tar_load(feedingGraphs)
tar_load(roostingGraphs)
tar_load(season_names)

# FIGURE 2: map -----------------------------------------------------------
load(here("credentials/stadia_key.Rda"))
ggmap::register_stadiamaps(key = stadia_key)
rm(stadia_key)
# Get roost data (just one season, say summer 2023)
tar_load(season_names)
whch <- which(season_names == "2023_summer")
tar_load(roosts)
r <- roosts[[whch]] %>% filter(lubridate::month(roost_date) == 7 & lubridate::day(roost_date) <= 5) # first five days of july in 2023

# Get feeding/flight interaction data (just one season, say summer 2023)
## and then let's limit it to the first five days of July so that it's a little less chaotic to plot.
tar_load(flightEdges)
fle <- flightEdges[[whch]] %>% filter(lubridate::month(minTimestamp) == 7 & lubridate::day(minTimestamp) <= 5)
tar_load(feedingEdges)
fee <- feedingEdges[[whch]] %>% filter(lubridate::month(minTimestamp) == 7 & lubridate::day(minTimestamp) <= 5)

bbox <- c(34.516, 30.408, 35.429, 31.582)
# going to manually layer the points so the most concentrated ones (feeding events) are on top and the most dispersed ones (flight interactions) are on the bottom so it's easier to see.
# mp <- ggmap(get_stadiamap(bbox = bbox, maptype = "stamen_terrain_background", color = "bw"))
# save(mp, file = here("mp.Rda"))
load(here("data/created/mp.Rda"))
dat <- st_drop_geometry(r) %>% select("long" = location_long, "lat" = location_lat) %>%
  mutate(type = "Roost\nlocations\n") %>%
  bind_rows(st_drop_geometry(fle) %>% select("long" = interactionLong, "lat" = interactionLat) %>%
              mutate(type = "Co-flight\ninteractions\n")) %>%
  bind_rows(st_drop_geometry(fee) %>% select("long" = interactionLong, "lat" = interactionLat) %>%
              mutate(type = "Co-feeding\ninteractions\n"))
mymap <- mp+
  geom_point(data = dat, aes(x = long, y = lat, col = type, shape = type),
             alpha = 0.9, size = 2)+
  scale_color_manual(values = situcolors)+
  scale_shape_manual(values = c(4, 1, 2))+
  guides(shape = guide_legend(override.aes = list(alpha = 1, size = 5)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 18),
        legend.position = "left")
mymap
ggsave(mymap, filename = here("fig/fig2_map.png"), width = 7, height = 6)

# FIGURE 3: Model results -------------------------------------------------
# Load models
load(here("data/created/sp_deg_obs_mod.Rda"))
load(here("data/created/sp_deg_int_mod.Rda"))
load(here("data/created/sp_str_obs_mod.Rda"))
load(here("data/created/sp_str_int_mod.Rda"))

# List models so we can operate on them
models <- list(sp_deg_obs_mod, sp_deg_int_mod, sp_str_obs_mod, sp_str_int_mod)

effs <- effs <- map(models, ~as.data.frame(emmeans::emtrends(.x, specs = "situ", var = "space_use")))
preds <- map(models, ~as.data.frame(ggeffect(.x, terms = c("space_use", "situ"))))
responses <- rep(c("degree", "strength"), each = 2)
mods <- rep(c("Observed", "Intentional"), 2)

effs <- pmap(.l = list(x = effs, y = responses, z = mods), 
             .f = function(x, y, z){x %>% mutate(response = y,
                                                 mod = z)}) %>%
  purrr::list_rbind() %>%
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting"),
         mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  mutate(sig = case_when(sign(lower.CL) == sign(upper.CL) ~ T,
                         .default = F))

preds <- pmap(.l = list(x = preds, y = responses, z = mods), 
              .f = function(x, y, z){x %>% mutate(response = y,
                                                  mod = z)}) %>%
  purrr::list_rbind() %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding",
                           group == "Fl" ~ "Flight",
                           group == "Ro" ~ "Roosting"),
         mod = factor(mod, levels = c("Observed", "Intentional")))

preds <- preds %>%
  left_join(effs %>%
              dplyr::select(response, mod, sig, situ) %>%
              distinct(), by = c("group" = "situ", "response", "mod"))

# Prepare data for forest plots (insets)
prepdata_forforestplots <- function(variable, mod, z_mod){
  dat <- as.data.frame(ggeffect(mod, terms = c(variable, "situ"))) %>%
    mutate(mod = "mod")
  z_dat <- as.data.frame(ggeffect(z_mod, terms = c(variable, "situ"))) %>%
    mutate(mod = "z_mod")
  eff1 <- as.data.frame(emmeans::emtrends(mod, specs = "situ", 
                                          var = variable)) %>%
    mutate(mod = "Observed")
  names(eff1)[5:6] <- c("lower", "upper")
  eff2 <- as.data.frame(emmeans::emtrends(z_mod, specs = "situ", 
                                          var = variable)) %>% 
    mutate(mod = "Intentional")
  names(eff2)[5:6] <- c("lower", "upper")
  eff <- bind_rows(eff1, eff2) %>%
    mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
    mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                            situ == "Fl" ~ "Flight",
                            situ == "Ro" ~ "Roosting"),
           var = variable)
  
  annot <- eff %>%
    mutate(sig = case_when(lower < 0 & upper < 0 ~ T,
                           lower > 0 & upper > 0 ~ T,
                           .default = F))
  return(annot)
}

deg_space <- prepdata_forforestplots(variable = "space_use", mod = sp_deg_obs_mod,
                                     z_mod = sp_deg_int_mod) %>%
  mutate(measure = "degree") %>%
  group_by(mod) %>%
  group_split()
str_space <- prepdata_forforestplots(variable = "space_use", mod = sp_str_obs_mod,
                                     z_mod = sp_str_int_mod) %>%
  mutate(measure = "strength") %>%
  group_by(mod) %>%
  group_split()

forforest <- c(deg_space, str_space)
length(forforest)
forforest <- map(forforest, ~.x %>% rename("trend" = 2))

forestplots <- map(forforest, ~{
  p <- .x %>%
    ggplot(aes(x = trend, y = situ, col = situ))+
    geom_vline(aes(xintercept = 0), linetype = 2)+
    geom_errorbar(aes(xmin = lower, xmax = upper),
                  width = 0, linewidth = 0.75)+
    geom_point(size = 3)+
    scale_color_manual(values = situcolors, guide = "none")+
    scale_y_discrete(limits = rev, position = "right")+
    labs(x = "Effect")+
    theme(text = element_text(size = 10),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
  return(p)
})

## Line plots
preds_list <- preds %>%
  group_by(mod, response) %>%
  group_split() %>%
  map(., ~.x %>% mutate(sig = factor(sig, levels = c(F, T))))

labs <- c("Degree", "Strength", "Degree z-score", "Strength z-score")
titles <- c("Observed", "Observed", "Intentional", "Intentional")

lineplots <- pmap(list(a = preds_list, b = labs), .f = function(a, b){
  p <- a %>%
    ggplot(aes(x = x, y = predicted, col = group))+
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL),
                alpha = 0.2)+
    geom_line(aes(linetype = sig), show.legend = T, linewidth = 1.5)+
    scale_linetype_manual(drop = FALSE, values = c(2, 1))+
    scale_color_manual(name = "Situation", values = situcolors)+
    scale_fill_manual(name = "Situation", values = situcolors)+
    labs(y = b, x = NULL)+
    #geom_point(data = e, pch = 1, alpha = 0.5)+
    theme_classic()+
    theme(legend.position = "none",
          text = element_text(size = 14))
  return(p)
})

a <- lineplots[[1]] + ggtitle("Observed") + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
b <- lineplots[[3]] + ggtitle("Intentional") + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
c <- lineplots[[2]] + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
d <- lineplots[[4]] + theme(plot.margin = unit(c(1.4, 1.1, 0.1, 0.1), "cm"))
results <- (a + b)/(c + d)
results

ggsave(results, width =9, height = 6.5, file = here("fig/fig3_line.png"))

ggsave(forestplots[[1]], filename = here("fig/fig3_forest1.png"), width = 1.5, height = 1)
ggsave(forestplots[[2]], filename = here("fig/fig3_forest2.png"), width = 1.5, height = 1)
ggsave(forestplots[[3]], filename = here("fig/fig3_forest3.png"), width = 1.5, height = 1)
ggsave(forestplots[[4]], filename = here("fig/fig3_forest4.png"), width = 1.5, height = 1)
# FIGURE S1 ------------------------------------------------------
# just pulling some code out of remove_northern() from functions.R to make a plot of the centroids
tar_load(seasons_list)
tar_load(season_names)
seasons_sf <- map(seasons_list, ~.x %>%
                    sf::st_as_sf(coords = c("location_long", "location_lat"), 
                                 remove = F) %>%
                    sf::st_set_crs("WGS84") %>%
                    sf::st_transform(32636))

## Get centroids, so we can see who's "southern" for that season.
centroids <- map(seasons_sf, ~.x %>%
                   group_by(Nili_id) %>%
                   summarize(geometry = sf::st_union(geometry)) %>%
                   sf::st_centroid())

centroids_df <- map2(centroids, season_names, ~.x %>% mutate(seasonUnique = .y) %>% bind_cols(sf::st_coordinates(.))) %>% purrr::list_rbind()

centroids_hist <- centroids_df %>%
  ggplot(aes(x = Y))+
  geom_histogram(aes(fill = seasonUnique))+
  theme_classic()+
  facet_wrap(~seasonUnique)+
  theme(legend.position = "none",
        text = element_text(size = 14))+
  labs(y = "Frequency",
       x = "UTM northing")+
  geom_vline(col = "black", linetype = 2, aes(xintercept = 3550000))+
  annotate("rect", xmin = 3550000, xmax = max(centroids_df$Y), ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "black") 

ggsave(centroids_hist, file = here("fig/figS1.png"), width = 8, height = 6)

# FIGURE S2 ---------------------------------------------------------------
bordercolors <- c("gold3", "dodgerblue3", "darkolivegreen")
linked3 <- linked2 %>%
  mutate(szn = str_replace(seasonUnique, "_", " "),
         szn = factor(szn, levels = c("2020 fall", "2021 breeding", "2021 summer", "2021 fall", "2022 breeding", "2022 summer", "2022 fall", "2023 breeding", "2023 summer")),
         szn = paste0(substr(szn, 3, 4), "_", toupper(substr(szn, 6, 6))))

obs_deg_box <- linked3 %>%
  ggplot(aes(x = szn, y = degree, fill = type, col = type, group = interaction(szn, type)))+
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5)+
  theme_minimal()+
  scale_fill_manual(name = "Behavioral situation", values = situcolors)+
  scale_color_manual(name = "Behavioral situation", values = bordercolors)+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")+
  labs(y = "Degree (observed)", x = "Season")
obs_deg_box

obs_str_box <- linked3 %>%
  ggplot(aes(x = szn, y = strength, fill = type, col = type, group = interaction(szn, type)))+
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5)+
  theme_minimal()+
  scale_fill_manual(name = "Behavioral situation", values = situcolors)+
  scale_color_manual(name = "Behavioral situation", values = bordercolors)+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")+
  labs(y = "Strength (observed)", x = "Season")
obs_str_box

int_deg_box <- linked3 %>%
  ggplot(aes(x = szn, y = z_deg, fill = type, col = type, group = interaction(szn, type)))+
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5)+
  theme_minimal()+
  scale_fill_manual(name = "Behavioral situation", values = situcolors)+
  scale_color_manual(name = "Behavioral situation", values = bordercolors)+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")+
  labs(y = "Degree (intentional)", x = "Season")
int_deg_box

int_str_box <- linked3 %>%
  ggplot(aes(x = szn, y = z_str, fill = type, col = type, group = interaction(szn, type)))+
  geom_boxplot(outlier.shape = 1, outlier.size = 0.5)+
  theme_minimal()+
  scale_fill_manual(name = "Behavioral situation", values = situcolors)+
  scale_color_manual(name = "Behavioral situation", values = bordercolors)+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")+
  labs(y = "Strength (intentional)", x = "Season")
int_str_box

boxplots <- ((obs_deg_box/obs_str_box) + plot_layout(axes = "collect") | (int_deg_box/int_str_box) + plot_layout(axes = "collect")) + plot_layout(guides = "collect") & theme(legend.position = 'bottom', panel.grid.minor.x = element_blank())
boxplots
ggsave(boxplots, file = here("fig/figS2.png"), width = 9, height = 5)

# FIGURE S4 ---------------------------------------------------------------
season_effects <- pmap(list(x = models, y = responses, z = mods), function(x, y, z){
  as.data.frame(ggeffect(x, terms = "season")) %>%
    mutate(response = y,
           mod = z)
}) %>%
  purrr::list_rbind()

# figure out if any of them are significantly different
summary(sp_deg_int_mod)
summary(sp_str_int_mod) 
summary(sp_deg_obs_mod) 
summary(sp_str_obs_mod) 

season_plot <- season_effects %>%
  ggplot(aes(x = x, y = predicted, col = x))+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                size = 0.7, width = 0)+
  geom_errorbar(aes(ymin = predicted-std.error, ymax = predicted+std.error),
                size = 1.5, width = 0)+
  geom_point(size = 4, pch = 21, fill = "white")+
  facet_wrap(~interaction(mod, response, sep = " "), scales = "free_y")+
  scale_color_manual(values = seasoncolors)+
  theme_minimal()+
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        strip.text = element_text(size = 12))+
  labs(caption = "Error bars represent standard error (thick) and 95% confidence intervals (thin)")+
  NULL
season_plot

ggsave(season_plot, file = here("fig/figS4.png"), width = 5.5, height = 7)

# TABLE 1 -----------------------------------------------------------------
effs_modified <- effs %>%
  mutate(`Social network` = paste0(str_to_title(response), " (", str_to_lower(mod), ")")) %>%
  rename("Situation" = "situ",
         "Estimate" = "space_use.trend") %>%
  mutate("95% CI" = paste(round(`lower.CL`, 3), round(`upper.CL`, 3), 
                          sep = ", "),
         "Estimate±SE" = paste(round(Estimate, 3), "±", round(SE, 3))) %>%
  select(`Social network`, Situation, `Estimate±SE`, `95% CI`) %>%
  mutate(`Predictor variable` = paste0("Space use * Situation (", str_to_lower(Situation), ")")) %>%
  select(-Situation) %>%
  relocate(`Predictor variable`, .after = `Social network`)

em_obs <- effs_modified %>%
  filter(grepl("observed", `Social network`))
em_int <- effs_modified %>%
  filter(grepl("intentional", `Social network`))

sig_obs <- stringr::str_count(em_obs$`95% CI`, "-") %in% c(0, 2)
sig_int <- stringr::str_count(em_int$`95% CI`, "-") %in% c(0, 2)
conditional_effects_tab_obs <- em_obs %>%
  group_by(`Social network`) %>%
  gt(row_group_as_column = T) %>%
  tab_style(style = list(
    cell_text(weight = "bold")
  ),
  locations = list(cells_column_labels(), cells_row_groups())) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = "95% CI",
      rows = sig_obs
    )
  )

conditional_effects_tab_int <- em_int %>%
  group_by(`Social network`) %>%
  gt(row_group_as_column = T) %>%
  tab_style(style = list(
    cell_text(weight = "bold")
  ),
  locations = list(cells_column_labels(), cells_row_groups())) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = "95% CI",
      rows = sig_int
    )
  )
gtsave(conditional_effects_tab_obs, filename = here("fig/tab1_obs.rtf"))
gtsave(conditional_effects_tab_obs, filename = here("fig/tab1_obs.png"))
gtsave(conditional_effects_tab_int, filename = here("fig/tab1_int.rtf"))
gtsave(conditional_effects_tab_int, filename = here("fig/tab1_int.png"))

# TABLE S1 ----------------------------------------------------------------
all <- data.frame(seasonUnique = season_names,
                  flight = map_dbl(flightGraphs, ~length(igraph::V(.x))),
                  feeding = map_dbl(feedingGraphs, ~length(igraph::V(.x))),
                  roosting = map_dbl(roostingGraphs, ~length(igraph::V(.x)))) %>%
  mutate(year = str_extract(seasonUnique, "[0-9]+"),
         season = str_extract(seasonUnique, "[a-z]+")) %>%
  select(-seasonUnique) %>%
  mutate(type = "social network")
# XXX START HERE--COMPARE TO DATA FOR SOCIAL ANALYSIS

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

tab <- indivs %>%
  mutate(yrsz = paste(year, season, sep = " ")) %>%
  mutate(yrsz = factor(yrsz, levels = str_replace(season_names, "_", " "))) %>%
  arrange(yrsz) %>%
  select(-c("year", "season")) %>%
  pivot_longer(cols = c("social network", "space use"), names_to = "type", values_to = "count") %>%
  pivot_wider(names_from = "yrsz", values_from = "count") %>%
  arrange(situ, type) %>%
  mutate(situ = str_to_title(situ)) %>%
  gt(groupname_col = "situ", rowname_col = "type") %>%
  tab_header(
    title = md("**Number of vultures**"),
  )
tab
gtsave(tab, filename = here("fig/tabS1.rtf"))
gtsave(tab, filename = here("fig/tabS1.png"))

# TABLE S2 ----------------------------------------------------------------
summ <- linked %>%
  ungroup() %>%
  select(seasonUnique, year, season, type, degree, strength, z_deg, z_str) %>%
  pivot_longer(cols = c("degree", "strength", "z_deg", "z_str"), 
               names_to = "measure", values_to = "value") %>%
  group_by(seasonUnique, year, season, type, measure) %>%
  summarize(mn = round(mean(value), 2),
            sd = round(sd(value), 2),
            min = round(min(value), 2),
            max = round(max(value), 2),
            words = paste0(mn, " (", sd, ")<br>", 
                           min, "-", max)) %>%
  mutate(measure = case_when(measure == "degree" ~ "Degree (observed)",
                             measure == "strength" ~ "Strength (observed)",
                             measure == "z_deg" ~ "Degree (intentional)",
                             measure == "z_str" ~ "Strength (intentional)"))

tab2 <- summ %>%
  ungroup() %>%
  select(-c("year", "season")) %>%
  select(seasonUnique, type, measure, words) %>%
  mutate(seasonUnique = str_replace(seasonUnique, "_", " ")) %>%
  pivot_wider(id_cols = c("measure", "type"), names_from = "seasonUnique", values_from = "words") %>%
  mutate(type = str_to_title(type)) %>%
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
gtsave(tab2, filename = here("fig/tabS2.rtf"))
gtsave(tab2, filename = here("fig/tabS2.png"))

# TABLE S3 ----------------------------------------------------------------
do <- broom.mixed::tidy(sp_deg_obs_mod) %>% mutate(model = "Degree (observed)")
di <- broom.mixed::tidy(sp_deg_int_mod) %>% mutate(model = "Degree (intentional)")
so <- broom.mixed::tidy(sp_str_obs_mod) %>% mutate(model = "Strength (observed)")
si <- broom.mixed::tidy(sp_str_int_mod) %>% mutate(model = "Strength (intentional)")

confint_fun <- function(mod, mod_name){
  mat <- confint(mod)
  df <- as.data.frame(mat) %>%
    mutate(term = row.names(.)) %>%
    mutate(model = mod_name) %>%
    relocate(term)
  row.names(df) <- NULL
  return(df)
}

cis <- map2(models, c("Degree (observed)", "Degree (intentional)", "Strength (observed)", "Strength (intentional)"),  ~confint_fun(.x, .y)) %>% purrr::list_rbind()

all <- bind_rows(do, di, so, si)
fixed <- all %>%
  filter(effect == "fixed") %>%
  select(-c("effect", "component", "group")) %>%
  relocate("model") %>%
  left_join(cis, by = c("term", "model")) %>%
  select(-estimate) %>% # I actually like the formatting of the ones in the "Estimate" column from the confint function better
  mutate(term = case_when(term == "space_use" ~ "Space use",
                          term == "situFl" ~ "Situation (flight)",
                          term == "situRo" ~ "Situation (roosting)",
                          term == "seasonsummer" ~ "Season (summer)",
                          term == "seasonfall" ~ "Season (fall)",
                          term == "space_use:situFl" ~ "Space use * Situation (flight)",
                          term == "space_use:situRo" ~ "Space use * Situation (roosting)",
                          .default = term)) %>%
  rename("Social network" = model,
         "Predictor variable" = term,
         "Statistic" = statistic,
         "P" = "p.value") %>%
  mutate("95% CI" = paste(round(`2.5 %`, 3), round(`97.5 %`, 3), sep = ", "),
         "Estimate±SE" = paste(round(Estimate, 3), "±", round(std.error, 3)),
         P = round(P, 3)) %>%
  select(`Social network`, `Predictor variable`, `Estimate±SE`, `95% CI`, P) %>%
  mutate(P = as.character(P),
         P = case_when(P == "0" ~ "<0.001",
                       .default = P))

fixed_tab <- fixed %>%
  group_by(`Social network`) %>%
  gt(row_group_as_column = T) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = list(cells_column_labels(), cells_row_groups())
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(P, `95% CI`),
      rows = P < 0.05
    )
  )
fixed_tab # this is too long... let me try doing it in two separate ones

fixed_obs <- fixed %>% filter(grepl("observed", `Social network`))
fixed_int <- fixed %>% filter(grepl("intentional", `Social network`))

fixed_tab_obs <- fixed_obs %>%
  group_by(`Social network`) %>%
  gt(row_group_as_column = T) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = list(cells_column_labels(), cells_row_groups())
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(P, `95% CI`),
      rows = P < 0.05
    )
  )
fixed_tab_obs

fixed_tab_int <- fixed_int %>%
  group_by(`Social network`) %>%
  gt(row_group_as_column = T) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = list(cells_column_labels(), cells_row_groups())
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = c(P, `95% CI`),
      rows = P < 0.05
    )
  )
fixed_tab_int

gtsave(fixed_tab_obs, filename = here("fig/tabS3_obs.rtf"))
gtsave(fixed_tab_obs, filename = here("fig/tabS3_obs.png"))
gtsave(fixed_tab_int, filename = here("fig/tabS3_int.rtf"))
gtsave(fixed_tab_int, filename = here("fig/tabS3_int.png"))

tar_load(space_use_pca)
summ <- summary(space_use_pca)$importance
figS3 <- autoplot(space_use_pca, loadings = TRUE, loadings.colour = "blue", loadings.label = TRUE, loadings.label.size = 5, loadings.label.color = "blue")+
  theme_classic()+
  labs(x = paste0("PC1 (", round(summ[2,1]*100, 2), "%)"),
       y = paste0("PC2 (", round(summ[2,2]*100, 2), "%)"))
ggsave(figS3, file = here("fig/figS3.png"))
