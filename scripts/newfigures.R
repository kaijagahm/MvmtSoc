# Making the three figures we designed with Orr

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
loadfonts(device = "win")
theme_set(theme_classic() + theme(text = element_text(family="Verdana")))
tar_config_set(store = here::here('_targets'))
tar_load(cleaned_r)
tar_load(joined0_r)
tar_load(downsampled_10min_forSocial_r)
tar_load(downsampled_10min_r)
tar_load(cc)
tar_load(linked)
tar_load(deg_mod)
tar_load(deg_z_mod)
tar_load(str_mod)
tar_load(str_z_mod)
tar_load(situcolors)
tar_load(seasoncolors)
tar_load(movement_corr)
tar_load(space_corr)

# Figure 1 ----------------------------------------------------------------

# Figure 2 ----------------------------------------------------------------
## A
fig_2a <- linked %>%
  filter(situ == "Fe") %>%
  ungroup() %>%
  ggplot(aes(x = normDegree))+
  geom_density(linewidth = 1.5, col = situcolors[1])+
  theme_classic()+
  theme(legend.position = "bottom",
        text = element_text(family = "Verdana", size = 14),
        axis.title.y = element_blank())+
  xlab("Observed degree\n(normalized)")+
  ylab(" ")
fig_2a
ggsave(filename = here("fig/2a.png"), plot = fig_2a, width = 2.75, height = 2.75)

## B
fig_2b <- linked %>%
  filter(situ == "Fe") %>%
  ungroup() %>%
  ggplot(aes(x = z_deg))+
  geom_density(linewidth = 1.5, col = situcolors[1])+
  theme_classic()+
  theme(legend.position = "bottom",
        text = element_text(family = "Verdana", size = 14),
        axis.title.y = element_blank())+
  xlab("Intentional degree\n(z-score)")+
  ylab(" ")
fig_2b
ggsave(filename = here("fig/2b.png"), plot = fig_2b, width = 2.75, height = 2.75)

## C. Correlation between measured and intentional sociality
fig_2c <- linked %>%
  ungroup() %>%
  filter(situ == "Fe") %>%
  ggplot(aes(x = normDegree, y = z_deg))+
  geom_point(pch = 1, size = 2, alpha = 0.6, col = situcolors[1])+
  theme_classic()+
  theme(text = element_text(family = "Verdana", size = 14))+
  geom_smooth(method = "lm", col = situcolors[1])+
  ylab("Intentional") + xlab("Observed")
fig_2c # we see that these are correlated, but not perfectly. In particular, individuals with low normalized degree
ggsave(filename = here("fig/2c.png"), fig_2c, width = 2.75, height = 2.75)

## D. Example scatter and forest plots
deg_movement_situ <- as.data.frame(ggeffect(deg_mod, 
                                            terms = c("movement", "situ"))) %>%
  mutate(mod = "deg")
deg_z_movement_situ <- as.data.frame(ggeffect(deg_z_mod, 
                                              terms = c("movement", "situ"))) %>%
  mutate(mod = "deg_z")

eff1 <- as.data.frame(emmeans::emtrends(deg_mod, specs = "situ", var = "movement")) %>% 
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting")) %>%
  mutate(mod = "Observed")
eff2 <- as.data.frame(emmeans::emtrends(deg_z_mod, specs = "situ", var = "movement")) %>% 
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting")) %>%
  mutate(mod = "Intentional")

eff <- bind_rows(eff1, eff2) %>%
  mutate(mod = factor(mod, levels = c("Observed", "Intentional")))


annot <- eff %>%
  mutate(sig = case_when(lower.CL < 0 & upper.CL < 0 ~ T,
                         lower.CL > 0 & upper.CL > 0 ~ T,
                         .default = F)) %>%
  select(situ, mod, sig)

# combine the data and scale it
dat <- bind_rows(deg_movement_situ, deg_z_movement_situ)
mn_moddeg <- (deg_movement_situ %>% select(predicted, conf.low, conf.high) %>% rowSums() %>% sum())/(3*nrow(deg_movement_situ))
mn_moddeg_z <- (deg_z_movement_situ %>% select(predicted, conf.low, conf.high) %>% rowSums() %>% sum())/(3*nrow(deg_z_movement_situ))

dat <- dat %>%
  mutate(mod = case_when(mod == "deg" ~ "Observed",
                         mod == "deg_z" ~ "Intentional")) %>%
  mutate(mn = ifelse(mod == "Observed", mn_moddeg, mn_moddeg_z)) %>%
  mutate(predicted_scl = predicted-mn,
         conf.low_scl = conf.low-mn,
         conf.high_scl = conf.high-mn) %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding",
                           group == "Fl" ~ "Flight",
                           group == "Ro" ~ "Roosting")) %>%
  left_join(annot, by = c("group" = "situ",
                          "mod")) %>%
  mutate(mod = factor(mod, levels = c("Observed", "Intentional")))

fig_2d <- dat %>%
  ggplot(aes(x = x, y = predicted, group = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL), alpha = 0.1)+
  geom_line(linewidth = 1.5, aes(col = group, linetype = sig))+
  scale_linetype_manual(values = c(2, 1), guide = "none")+
  facet_wrap(~mod, scale = "free_y")+
  scale_color_manual(name = "Situation",
                     values = situcolors)+
  scale_fill_manual(name = "Situation", 
                    values = situcolors)+
  xlab("Movement")+
  ylab("Model prediction")+
  theme(text = element_text(family = "Verdana", size = 14), 
        legend.position = "right",
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA))
fig_2d
ggsave(filename = here("fig/2d.png"), fig_2d, width = 6, height = 4)

## E. Example forest plot with both types of lines
fig_2e <- eff %>%
  ggplot(aes(x = movement.trend, y = situ, col = situ,
             group = interaction(mod, situ)))+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0, linewidth = 0.5,
                position = position_dodge(width = 0.2))+
  geom_point(size = 3, 
             position = position_dodge(width = 0.2),
             fill = "white", aes(shape = mod))+
  scale_color_manual(values = situcolors, guide = "none")+
  scale_y_discrete(limits = rev, position = "right")+
  scale_shape_manual(name = "", values = c(16, 21))+
  facet_wrap(~mod, scales = "free_x",
             ncol = 2)+
  ylab("Movement effect")+ xlab("Effect size")+
  theme(text = element_text(family = "Verdana", size = 14),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11),
        legend.position = "none")
fig_2e
ggsave(filename = here("fig/2e.png"), fig_2e, width = 4, height = 4)

# Figure 3 ----------------------------------------------------------------
# Main results
# Degree and strength, movement and space use only.
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

deg_movement <- prepdata_forforestplots(variable = "movement", mod = deg_mod,
                                        z_mod = deg_z_mod) %>%
  mutate(measure = "degree")
deg_space <- prepdata_forforestplots(variable = "space_use", mod = deg_mod,
                                     z_mod = deg_z_mod) %>%
  mutate(measure = "degree")
str_movement <- prepdata_forforestplots(variable = "movement", mod = str_mod,
                                        z_mod = str_z_mod) %>%
  mutate(measure = "strength")
str_space <- prepdata_forforestplots(variable = "space_use", mod = str_mod,
                                     z_mod = str_z_mod) %>%
  mutate(measure = "strength")
forforest <- list(deg_movement, deg_space, str_movement, str_space)
forforest <- map(forforest, ~.x %>% rename("trend" = 2))
forforest_df <- purrr::list_rbind(forforest) %>%
  mutate(var = case_when(var == "movement" ~ "Movement",
                         var == "space_use" ~ "Space use"))

plots <- map(forforest, ~{
  var <- str_to_sentence(str_replace(.x$var[1], "_", " "))
  measure <- str_to_sentence(.x$measure[1])
  title <- paste0(var, " effect")
  p <- .x %>%
    ggplot(aes(x = trend, y = situ, col = situ,
               group = interaction(mod, situ)))+
    geom_vline(aes(xintercept = 0), linetype = 2)+
    geom_errorbar(aes(xmin = lower, xmax = upper),
                  width = 0, linewidth = 0.5,
                  position = position_dodge(width = 0.2))+
    geom_point(size = 3, 
               position = position_dodge(width = 0.2),
               fill = "white")+
    scale_color_manual(values = situcolors, guide = "none")+
    scale_y_discrete(limits = rev, position = "right")+
    scale_shape_manual(name = "", values = c(16, 21))+
    facet_wrap(~mod, scales = "free_x",
               nrow = 1)+
    xlab(title)+
    theme(text = element_text(size = 14),
          axis.title.y = element_blank())
  return(p)
})

patchwork <- ((plots[[1]]+
                 ggtitle("Degree")+
                 theme(axis.text.y = element_blank(),
                       axis.ticks.y = element_blank(),
                       plot.title = element_text(hjust = 0.5),
                       axis.title.x = element_blank())) + 
                (plots[[2]] + ggtitle("Strength")+
                   theme(plot.title = element_text(hjust = 0.5),
                         axis.title.x = element_blank()))) / 
  ((plots[[3]] + 
      ggtitle("")+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(size = 14))) + 
     (plots[[4]] +
        theme(axis.title.x = element_text(size = 14))))
patchwork
ggsave(patchwork, filename = here("fig/3a.png"), width = 7, height = 6)

# But I don't like this because it allows each of the axes to vary. We need to have one set of axes for each model. We have a total of four models--degree and strength, observed and intentional. That means we need four different axes. But we can, and (maybe) should, but the effect sizes on the same axes for movement and space, since those are covariates in the same model. People are used to seeing that for forest plots, and it makes sense.

# Let's give that a shot. There will be two plots, with two panels each, for a total of 4 panels.
deg <- forforest_df %>%
  filter(measure == "degree") %>%
  ggplot(aes(x = trend, y = situ, col = situ,
             group = interaction(mod, situ, var)))+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  geom_errorbar(aes(xmin = lower, xmax = upper),
                linewidth = 1,
                width = 0, position = position_dodge(width = 0.3))+
  geom_point(aes(pch = var), size = 5,
             position = position_dodge(width = 0.3),
             fill = "white")+
  facet_wrap(~mod, scales = "free_x")+
  scale_color_manual(values = situcolors, guide = F)+
  scale_shape_manual(name = "Predictor", values = c(19, 21))+
  ylab(NULL)+xlab("Effect size")+
  ggtitle("Degree")+
  scale_y_discrete(limits = rev)+
  theme(text = element_text(family = "Verdana", size = 14),
        legend.position = "none")

str <- forforest_df %>%
  filter(measure == "strength") %>%
  ggplot(aes(x = trend, y = situ, col = situ,
             group = interaction(mod, situ, var)))+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  geom_errorbar(aes(xmin = lower, xmax = upper),
                linewidth = 1,
                width = 0, position = position_dodge(width = 0.3))+
  geom_point(aes(pch = var), size = 5,
             position = position_dodge(width = 0.3),
             fill = "white")+
  facet_wrap(~mod, scales = "free_x")+
  scale_color_manual(values = situcolors, guide = F)+
  scale_shape_manual(name = "Predictor", values = c(19, 21))+
  ylab(NULL)+xlab("Effect size")+
  ggtitle("Strength")+
  scale_y_discrete(limits = rev)+
  theme(text = element_text(family = "Verdana", size = 14),
        legend.position = "right",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 11))

patchwork_new <- deg + str
patchwork_new
ggsave(patchwork_new, filename = here("fig/3a.png"), width = 12, height = 6)
# Ok yeah this version is much much more readable and way simpler and also more correct! Four sets of axes for four different models, no need to repeat them.

# Same thing but separating out movement and space ------------------------
a <- forforest_df %>%
  filter(measure == "degree", mod == "Observed") %>%
  ggplot(aes(x = trend, y = situ, col = situ,
             group = interaction(var, situ)))+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  geom_errorbar(aes(xmin = lower, xmax = upper),
                linewidth = 1, width = 0)+
  geom_point(size = 5, pch = 19)+
  facet_rep_wrap(~var, nrow = 2, strip.position = "left", repeat.tick.labels = TRUE)+
  scale_color_manual(values = situcolors, guide = "none")+
  ylab(NULL)+ xlab(NULL)+
  scale_y_discrete(position = "right", limits = rev)+
  theme(text = element_text(family = "Verdana", size = 14),
        strip.text = element_text(size = 14),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = unit(c(5.5, 1.5, 5.5, 5.5), "pt"))+
  ggtitle("Observed")+
  theme(plot.title = element_text(size = 14, hjust = 0.5))
a

b <- forforest_df %>%
  filter(measure == "degree", mod == "Intentional") %>%
  ggplot(aes(x = trend, y = situ, col = situ,
             group = interaction(var, situ)))+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  geom_errorbar(aes(xmin = lower, xmax = upper),
                linewidth = 1, width = 0)+
  geom_point(size = 5, fill = "white", pch = 21)+
  facet_rep_wrap(~var, nrow = 2, strip.position = "left",
                 repeat.tick.labels = TRUE)+
  scale_color_manual(values = situcolors, guide = "none")+
  ylab(NULL)+ xlab(NULL)+
  scale_y_discrete(position = "right", limits = rev)+
  theme(text = element_text(family = "Verdana", size = 14),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(5.5, 25, 5.5, 5.5), "pt"))+
  ggtitle("Intentional")+
  theme(plot.title = element_text(size = 14, hjust = 0.5))
b

c <- forforest_df %>%
  filter(measure == "strength", mod == "Observed") %>%
  ggplot(aes(x = trend, y = situ, col = situ,
             group = interaction(var, situ)))+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  geom_errorbar(aes(xmin = lower, xmax = upper),
                linewidth = 1, width = 0)+
  geom_point(size = 5, pch = 19)+
  facet_rep_wrap(~var, nrow = 2, strip.position = "left",
                 repeat.tick.labels = TRUE)+
  scale_color_manual(values = situcolors, guide = "none")+
  ylab(NULL)+ xlab(NULL)+
  scale_y_discrete(position = "right", limits = rev)+
  theme(text = element_text(family = "Verdana", size = 14),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.margin = unit(c(5.5, 1.5, 5.5, 5.5), "pt"))+
  ggtitle("Observed")+
  theme(plot.title = element_text(size = 14, hjust = 0.5))
c

d <- forforest_df %>%
  filter(measure == "strength", mod == "Intentional") %>%
  ggplot(aes(x = trend, y = situ, col = situ,
             group = interaction(var, situ)))+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  geom_errorbar(aes(xmin = lower, xmax = upper),
                linewidth = 1, width = 0)+
  geom_point(size = 5, fill = "white", pch = 21)+
  facet_rep_wrap(~var, nrow = 2, strip.position = "left",
                 repeat.tick.labels = TRUE)+
  scale_color_manual(values = situcolors, guide = "none")+
  ylab(NULL)+ xlab(NULL)+
  scale_y_discrete(position = "right", limits = rev)+
  theme(text = element_text(family = "Verdana", size = 14),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "pt"))+
  ggtitle("Intentional")+
  theme(plot.title = element_text(size = 14, hjust = 0.5))
d

patchwork_3 <- (a+b+c+d) + plot_layout(ncol = 4)
patchwork_3
ggsave(patchwork_3, filename = here("fig/3a_rev.png"), width = 10, height = 6)

# Map with interaction and roost locations --------------------------------
load(here("stadia_key.Rda"))
ggmap::register_stadiamaps(key = stadia_key)
rm(stadia_key)
# Get roost data (just one season, say summer 2023)
tar_load(season_names)
whch <- which(season_names == "2023_summer")
tar_load(roosts)
r <- roosts[[whch]] %>% filter(lubridate::month(roost_date) == 7)

# Get feeding/flight interaction data (just one season, say summer 2023)
## and then let's limit it to the month of July so that it's a little less chaotic to plot.
tar_load(flightEdges)
fle <- flightEdges[[whch]] %>% filter(lubridate::month(minTimestamp) == 7)
tar_load(feedingEdges)
fee <- feedingEdges[[whch]] %>% filter(lubridate::month(minTimestamp) == 7)

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
  geom_point(data = dat, aes(x = long, y = lat, col = type, shape = type), alpha = 0.4)+
  scale_color_manual(values = situcolors)+
  scale_shape_manual(values = c(4, 1, 2))+
  guides(shape = guide_legend(override.aes = list(alpha = 1, size = 3)),
         color = guide_legend(override.aes = list(alpha = 1, size = 3)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        text = element_text(family = "Verdana", size = 14),
        legend.position = "left")
ggsave(mymap, filename = here("fig/mymap.png"), width = 7, height = 6)



# Supplement: Scatterplots with trendlines for all models -----------------
## Degree
### Observed--movement
deg_obs_movement <- as.data.frame(ggeffect(deg_mod, terms = c("movement", "situ"))) %>% mutate(mod = "Observed")
### Intentional--movement
deg_int_movement <- as.data.frame(ggeffect(deg_z_mod, terms = c("movement", "situ"))) %>% mutate(mod = "Intentional")

#### See which trends are significant
deg_eff_movement <- as.data.frame(emmeans::emtrends(deg_mod, specs = "situ", var = "movement")) %>% mutate(mod = "Observed") %>% bind_rows(as.data.frame(emmeans::emtrends(deg_z_mod, specs = "situ", var = "movement")) %>% mutate(mod = "Intentional")) %>% mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  mutate(sig = case_when(lower.CL < 0 & upper.CL < 0 ~ T,
                         lower.CL > 0 & upper.CL > 0 ~ T,
                         .default = F))
deg_movement <- bind_rows(deg_obs_movement, deg_int_movement) %>%
  mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  left_join(deg_eff_movement %>% select("group" = situ, mod, sig))

fig_deg_movement <- deg_movement %>%
  ggplot(aes(x = x, y = predicted, group = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL),
              alpha = 0.1)+
  geom_line(linewidth = 1.5, aes(col = group, linetype = sig))+
  scale_linetype_manual(values = c(2, 1), guide = "none", drop = FALSE)+
  facet_wrap(~mod, scale = "free_y")+
  scale_color_manual(name = "Situation",
                     values = situcolors)+
  scale_fill_manual(name = "Situation", 
                    values = situcolors)+
  xlab("Movement")+
  ylab("Model prediction (degree)")+
  theme(text = element_text(family = "Verdana", size = 14), 
        legend.position = "right")
fig_deg_movement

### Observed--space
deg_obs_space <- as.data.frame(ggeffect(deg_mod, terms = c("space_use", "situ"))) %>% mutate(mod = "Observed")
### Intentional--space
deg_int_space <- as.data.frame(ggeffect(deg_z_mod, terms = c("space_use", "situ"))) %>% mutate(mod = "Intentional")

deg_eff_space <- as.data.frame(emmeans::emtrends(deg_mod, specs = "situ", var = "space_use")) %>% mutate(mod = "Observed") %>% bind_rows(as.data.frame(emmeans::emtrends(deg_z_mod, specs = "situ", var = "space_use")) %>% mutate(mod = "Intentional")) %>% mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  mutate(sig = case_when(lower.CL < 0 & upper.CL < 0 ~ T,
                         lower.CL > 0 & upper.CL > 0 ~ T,
                         .default = F))

deg_space <- bind_rows(deg_obs_space, deg_int_space) %>%
  mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  left_join(deg_eff_space %>% select("group" = situ, mod, sig))

fig_deg_space <- deg_space %>%
  ggplot(aes(x = x, y = predicted, group = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL),
              alpha = 0.1)+
  geom_line(linewidth = 1.5, aes(col = group, linetype = sig))+
  scale_linetype_manual(values = c(2, 1), guide = "none", drop = FALSE)+
  facet_wrap(~mod, scale = "free_y")+
  scale_color_manual(name = "Situation",
                     values = situcolors)+
  scale_fill_manual(name = "Situation", 
                    values = situcolors)+
  xlab("Space use")+
  ylab("Model prediction (degree)")+
  theme(text = element_text(family = "Verdana", size = 14), 
        legend.position = "right")
fig_deg_space
  
## Strength
### Observed--movement
str_obs_movement <- as.data.frame(ggeffect(str_mod, terms = c("movement", "situ"))) %>% mutate(mod = "Observed")
### Intentional--movement
str_int_movement <- as.data.frame(ggeffect(str_z_mod, terms = c("movement", "situ"))) %>% mutate(mod = "Intentional")

#### See which trends are significant
str_eff_movement <- as.data.frame(emmeans::emtrends(str_mod, specs = "situ", var = "movement")) %>% mutate(mod = "Observed") %>% bind_rows(as.data.frame(emmeans::emtrends(str_z_mod, specs = "situ", var = "movement")) %>% mutate(mod = "Intentional")) %>% mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  mutate(sig = case_when(lower.CL < 0 & upper.CL < 0 ~ T,
                         lower.CL > 0 & upper.CL > 0 ~ T,
                         .default = F))
str_movement <- bind_rows(str_obs_movement, str_int_movement) %>%
  mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  left_join(str_eff_movement %>% select("group" = situ, mod, sig))

fig_str_movement <- str_movement %>%
  ggplot(aes(x = x, y = predicted, group = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL),
              alpha = 0.1)+
  geom_line(linewidth = 1.5, aes(col = group, linetype = sig))+
  scale_linetype_manual(values = c(2, 1), guide = "none", drop = FALSE)+
  facet_wrap(~mod, scale = "free_y")+
  scale_color_manual(name = "Situation",
                     values = situcolors)+
  scale_fill_manual(name = "Situation", 
                    values = situcolors)+
  xlab("Movement")+
  ylab("Model prediction (strength)")+
  theme(text = element_text(family = "Verdana", size = 14), 
        legend.position = "right")
fig_str_movement

### Observed--space
str_obs_space <- as.data.frame(ggeffect(str_mod, terms = c("space_use", "situ"))) %>% mutate(mod = "Observed")
### Intentional--space
str_int_space <- as.data.frame(ggeffect(str_z_mod, terms = c("space_use", "situ"))) %>% mutate(mod = "Intentional")

str_eff_space <- as.data.frame(emmeans::emtrends(str_mod, specs = "situ", var = "space_use")) %>% mutate(mod = "Observed") %>% bind_rows(as.data.frame(emmeans::emtrends(str_z_mod, specs = "situ", var = "space_use")) %>% mutate(mod = "Intentional")) %>% mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  mutate(sig = case_when(lower.CL < 0 & upper.CL < 0 ~ T,
                         lower.CL > 0 & upper.CL > 0 ~ T,
                         .default = F))

str_space <- bind_rows(str_obs_space, str_int_space) %>%
  mutate(mod = factor(mod, levels = c("Observed", "Intentional"))) %>%
  left_join(str_eff_space %>% select("group" = situ, mod, sig))

fig_str_space <- str_space %>%
  ggplot(aes(x = x, y = predicted, group = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL),
              alpha = 0.1)+
  geom_line(linewidth = 1.5, aes(col = group, linetype = sig))+
  scale_linetype_manual(values = c(2, 1), guide = "none", drop = FALSE)+
  facet_wrap(~mod, scale = "free_y")+
  scale_color_manual(name = "Situation",
                     values = situcolors)+
  scale_fill_manual(name = "Situation", 
                    values = situcolors)+
  xlab("Space use")+
  ylab("Model prediction (strength)")+
  theme(text = element_text(family = "Verdana", size = 14), 
        legend.position = "right")
fig_str_space

ggsave(fig_deg_movement, filename = here("fig/figs_for_Noa/fig_deg_movement.png"), width = 9, height = 6)
ggsave(fig_deg_space, filename = here("fig/figs_for_Noa/fig_deg_space.png"), width = 9, height = 6)
ggsave(fig_str_movement, filename = here("fig/figs_for_Noa/fig_str_movement.png"), width = 9, height = 6)
ggsave(fig_str_space, filename = here("fig/figs_for_Noa/fig_str_space.png"), width = 9, height = 6)


# Some network graphs -----------------------------------------------------


