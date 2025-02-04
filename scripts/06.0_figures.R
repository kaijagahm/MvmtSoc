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
tar_config_set(store = here::here('_targets'))
tar_load(cleaned_r)
tar_load(joined0_r)
tar_load(downsampled_10min_forSocial_r)
tar_load(downsampled_10min_r)
tar_load(cc)
tar_load(linked)
tar_load(situcolors)
tar_load(seasoncolors)
tar_load(movement_corr)
tar_load(space_corr)
linked2 <- readRDS(here("data/linked2.RDS"))

# New models with only space use
load(here("data/sp_deg_obs_mod.Rda"))
load(here("data/sp_deg_int_mod.Rda"))
load(here("data/sp_str_obs_mod.Rda"))
load(here("data/sp_str_int_mod.Rda"))

models <- list(sp_deg_obs_mod, sp_deg_int_mod, sp_str_obs_mod, sp_str_int_mod)

effs <- map(models, ~as.data.frame(emmeans::emtrends(.x, specs = "situ", var = "space_use")))
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
              select(response, mod, sig, situ) %>%
              distinct(), by = c("group" = "situ", "response", "mod"))

# Figure 1 ----------------------------------------------------------------
# Show two individuals, both of which have approximately the same observed degree value, but which have very different distributions of expected degree values
targets::tar_load(allMetrics)
obs <- allMetrics %>% select(season, situ, Nili_id, degree, strength) %>%
  distinct()
exp <- allMetrics %>% select(season, situ, Nili_id, "degree" = wrapped_degree, "strength" = wrapped_strength)
set.seed(6)
szn <- sample(unique(allMetrics$season), 1)
#indivs <- sample(unique(allMetrics$Nili_id), 3)
indivs <- c("dingle", "kedros") # chose two with same degree but different expected distributions

vals <- linked %>% ungroup() %>% filter(seasonUnique == szn, situ == "Fl", Nili_id %in% indivs) %>%
  select(Nili_id, z_deg)

dat <- exp %>%
  filter(Nili_id %in% indivs, season == szn, situ == "flight") %>%
  mutate(Nili_id = fct_recode(Nili_id, "A" = "dingle",
                              "B" = "kedros"))
mns <- dat %>%
  group_by(Nili_id) %>%
  summarize(mn = mean(degree))

set.seed(6)
fig1 <- dat %>%
  ggplot(aes(x = degree, col = Nili_id))+
  geom_density(linewidth = 0.75, aes(fill = Nili_id), alpha = 0.1)+
  geom_vline(data = obs %>% filter(Nili_id %in% indivs, 
                                   season == szn, situ == "flight") %>%
               mutate(Nili_id = fct_recode(Nili_id, "A" = "dingle",
                                           "B" = "kedros")), 
             aes(xintercept = jitter(degree), col = Nili_id), linetype = 2,
             linewidth = 1)+
  labs(y = "Density",
       x = "Degree (co-flight)",
       col = "Individual",
       fill = "Individual")+
  scale_color_manual(values = c("orange", "purple"))+
  scale_fill_manual(values = c("orange", "purple"))+
  annotate("errorbar", y = 0.1, xmin = mns$mn[1], xmax = 39, col = "orange", width = 0.01)+
  annotate("errorbar", y = 0.05, xmin = mns$mn[2], xmax = 39, col = "purple", width = 0.01)+
  annotate("text", y = 0.11, x = (39-mns$mn[1])/2, col = "orange", 
           label = paste0("z-score = ", round(vals$z_deg[1], 2)))+
  annotate("text", y = 0.06, x = (39-mns$mn[2])/2, col = "purple",
           label = paste0("z-score = ", round(vals$z_deg[2], 2)))+
  annotate("text", y = 0.013, x = 34.5, col = "black", label = "observed")+
  annotate("text", y = 0.013, x = 15, col = "black", label = "incidental")
ggsave(fig1, file = here("fig/1.png"), width = 5, height = 3.5)


# re-do this figure with a sample dataset
exp <- data.frame(exp_deg = c(rpois(1000, lambda = 12),
                              rpois(1000, lambda = 5)),
                  ID = rep(c("A", "B"), each = 1000))

ggplot(exp, aes(x = ID, y = exp_deg)) + 
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .75, 
    ## adjust height
    width = .75, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .15, 
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` or `outlier.alpha = 0` works as well
  )+
  coord_cartesian(xlim = c(1.2, 2.2))+
  geom_point(data = data.frame(ID = c("A", "B"),
                               exp_deg = c(21, 18)),
             col = "red",
             size = 4)+
  # geom_linerange(data = data.frame(ID = c("A", "B"),
  #                                min = (c(median(exp$exp_deg[exp$ID == "A"]),
  #                                         median(exp$exp_deg[exp$ID == "B"]))),
  #                                max = c(21, 18)),
  #              aes(ymin = min, ymax = max, x = ID),
  #              inherit.aes = F,
  #              col = "red",
  #              linetype = 2,
  #              position = position_dodge(width = 0.5)
  # )+
  theme(text = element_text(size = 14))+
  labs(y = "")+
  NULL


# Figure 2 ----------------------------------------------------------------
## A
fig_2a <- linked %>%
  filter(situ == "Fl", seasonUnique == "2022_fall") %>%
  ungroup() %>%
  ggplot(aes(x = degree))+
  geom_histogram(fill = situcolors[2], col = "white")+
  theme_classic()+
  theme(legend.position = "bottom",
        text = element_text(family = "Verdana", size = 12))+
  xlab("Degree (observed)")+
  ylab("Density")
fig_2a

## B
fig_2b <- linked2 %>%
  filter(situ == "Fl", seasonUnique == "2022_fall") %>%
  ungroup() %>%
  ggplot(aes(x = z_deg))+
  geom_histogram(fill = situcolors[2], col = "white")+
  theme_classic()+
  theme(legend.position = "bottom",
        text = element_text(family = "Verdana", size = 12))+
  xlab("Degree z-score (intentional)")+
  ylab("Density")
fig_2b

## C. Correlation between measured and intentional sociality
fig_2c <- linked2 %>%
  ungroup() %>%
  filter(situ == "Fl", seasonUnique == "2022_fall") %>%
  ggplot(aes(x = degree, y = z_deg))+
  geom_point(pch = 1, size = 2, alpha = 0.8, col = situcolors[2])+
  theme_classic()+
  theme(text = element_text(family = "Verdana", size = 12))+
  geom_smooth(alpha = 0.2, size = 0, method = "lm", col = situcolors[2], fill = situcolors[2])+ # creates se error clouds without the lines 
  stat_smooth(geom = "line", size = 1, method = "lm", col = situcolors[2]) +
  ylab("Degree (intentional)") + xlab("Degree (observed)")
fig_2c # correlated, but not perfectly

fig_2 <- ((fig_2a | fig_2b & theme(axis.title.y = element_blank()))/fig_2c) + plot_layout(nrow = 2, heights = c(3, 5))
ggsave(fig_2, filename = here("fig/fig_2.png"), height = 5, width = 6)

# Figure 3 ----------------------------------------------------------------
# Main results
# Degree and strength, space use only.
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

# Line plots --------------------------------------------------------------
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

ggsave(results, width =9, height = 6.5, file = here("fig/results.png"))

forestplots[[1]]
ggsave(forestplots[[1]], filename = here("fig/forest1.png"), width = 1.5, height = 1)
ggsave(forestplots[[2]], filename = here("fig/forest2.png"), width = 1.5, height = 1)
ggsave(forestplots[[3]], filename = here("fig/forest3.png"), width = 1.5, height = 1)
ggsave(forestplots[[4]], filename = here("fig/forest4.png"), width = 1.5, height = 1)

# Season plots ------------------------------------------------------------
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
  theme(legend.position = "none",
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        strip.text = element_text(size = 12))+
  labs(caption = "Error bars represent standard error (thick) and 95% confidence intervals (thin)")+
  NULL
season_plot

ggsave(season_plot, file = here("fig/season_plot.png"), width = 5.5, height = 7)


# Social measures plots ---------------------------------------------------
degree_density <- linked2 %>%
  mutate(szn = str_replace(seasonUnique, "_", " "),
         szn = factor(szn, levels = c("2020 fall", "2021 breeding", "2021 summer", "2021 fall", "2022 breeding", "2022 summer", "2022 fall", "2023 breeding", "2023 summer"))) %>%
  ggplot(aes(x = degree, group = szn, color = szn))+
  stat_density(geom = "line", alpha = 0.5, position = "identity", linewidth = 1)+
  facet_wrap(~str_to_title(type), scales = "free_x")+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")+
  labs(y = "Density", color = "Season", title = "Degree")
degree_density  

strength_density <- linked2 %>%
  mutate(szn = str_replace(seasonUnique, "_", " "),
         szn = factor(szn, levels = c("2020 fall", "2021 breeding", "2021 summer", "2021 fall", "2022 breeding", "2022 summer", "2022 fall", "2023 breeding", "2023 summer"))) %>%
  ggplot(aes(x = strength, group = szn, color = szn))+
  stat_density(geom = "line", alpha = 0.5, position = "identity", linewidth = 1)+
  facet_wrap(~str_to_title(type), scales = "free")+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")+
  labs(y = "Density", color = "Season", title = "Strength")
strength_density  

densities <- degree_density/strength_density  + plot_layout(guides = "collect")
densities
ggsave(densities, file = here("fig/densities.png"), width = 6, height = 5)

degree_density_z <- linked2 %>%
  mutate(szn = str_replace(seasonUnique, "_", " "),
         szn = factor(szn, levels = c("2020 fall", "2021 breeding", "2021 summer", "2021 fall", "2022 breeding", "2022 summer", "2022 fall", "2023 breeding", "2023 summer"))) %>%
  ggplot(aes(x = z_deg, group = szn, color = szn))+
  stat_density(geom = "line", alpha = 0.5, position = "identity", linewidth = 1)+
  facet_wrap(~str_to_title(type), scales = "free_y")+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")+
  labs(y = "Density", color = "Season", title = "Degree z-score")
degree_density_z

strength_density_z <- linked2 %>%
  mutate(szn = str_replace(seasonUnique, "_", " "),
         szn = factor(szn, levels = c("2020 fall", "2021 breeding", "2021 summer", "2021 fall", "2022 breeding", "2022 summer", "2022 fall", "2023 breeding", "2023 summer"))) %>%
  ggplot(aes(x = z_str, group = szn, color = szn))+
  stat_density(geom = "line", alpha = 0.5, position = "identity", linewidth = 1)+
  facet_wrap(~str_to_title(type), scales = "free_y")+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        legend.position = "bottom")+
  labs(y = "Density", color = "Season", title = "Strength z-score")
strength_density_z 

densities_all <- (((degree_density / degree_density_z) + plot_layout(axis_titles = "collect")) | ((strength_density / strength_density_z)+ plot_layout(axis_titles = "collect"))) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

densities_all
ggsave(densities_all, file = here("fig/densities_all.png"), width = 10, height = 6)

# Map with interaction and roost locations --------------------------------
load(here("stadia_key.Rda"))
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
load(here("mp.Rda"))
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
ggsave(mymap, filename = here("fig/mymap.png"), width = 7, height = 6)
