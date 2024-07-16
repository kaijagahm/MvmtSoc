# Making the three figures we designed with Orr

library(tidyverse)
library(here)
library(targets)
library(patchwork)
library(ggeffects)
library(gtsummary)
library(extrafont)
library(ggthemes)
library(see)
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
        strip.text = element_text(size = 14),
        text = element_text(family = "Verdana"),
        axis.title.y = element_blank())+
  xlab("Observed degree\n(normalized)")
fig_2a
ggsave(filename = here("fig/2a.png"), plot = fig_2a, width = 2.5, height = 2.5)

## B
fig_2b <- linked %>%
  filter(situ == "Fe") %>%
  ungroup() %>%
  ggplot(aes(x = z_deg))+
  geom_density(linewidth = 1.5, col = situcolors[1])+
  theme_classic()+
  theme(legend.position = "bottom",
        strip.text = element_text(size = 14),
        text = element_text(family = "Verdana"),
        axis.title.y = element_blank())+
  xlab("Intentional degree\n(z-score)")
fig_2b
ggsave(filename = here("fig/2b.png"), plot = fig_2b, width = 2.5, height = 2.5)

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
ggsave(filename = here("fig/2c.png"), fig_2c, width = 3, height = 2.5)

## D. Example scatter and forest plots
deg_movement_situ <- as.data.frame(ggeffect(deg_mod, 
                                            terms = c("movement", "situ"))) %>%
  mutate(mod = "deg")
deg_z_movement_situ <- as.data.frame(ggeffect(deg_z_mod, 
                                              terms = c("movement", "situ"))) %>%
  mutate(mod = "deg_z")

# combine the data and scale it
dat <- bind_rows(deg_movement_situ, deg_z_movement_situ)
mn_moddeg <- (deg_movement_situ %>% select(predicted, conf.low, conf.high) %>% rowSums() %>% sum())/(3*nrow(deg_movement_situ))
mn_moddeg_z <- (deg_z_movement_situ %>% select(predicted, conf.low, conf.high) %>% rowSums() %>% sum())/(3*nrow(deg_z_movement_situ))

dat <- dat %>%
  mutate(mn = ifelse(mod == "deg", mn_moddeg, mn_moddeg_z)) %>%
  mutate(predicted_scl = predicted-mn,
         conf.low_scl = conf.low-mn,
         conf.high_scl = conf.high-mn) %>%
  mutate(group = case_when(group == "Fe" ~ "Feeding",
                           group == "Fl" ~ "Flight",
                           group == "Ro" ~ "Roosting")) %>%
  mutate(mod = case_when(mod == "deg" ~ "Observed",
                         mod == "deg_z" ~ "Intentional"))

fig_2d <- dat %>%
  ggplot(aes(x = x, y = predicted, group = group))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, col = NULL), alpha = 0.1)+
  geom_line(linewidth = 1.5, aes(col = group))+
  facet_wrap(~mod, scale = "free_y")+
  scale_color_manual(name = "Situation",
                     values = situcolors)+
  scale_fill_manual(name = "Situation", 
                    values = situcolors)+
  xlab("Movement")+
  ylab("Model prediction")+
  theme(text = element_text(family = "Verdana", size = 14))
fig_2d

## D. Example forest plot with both types of lines

dat1 <- as.data.frame(emmeans::emtrends(deg_mod, specs = "situ", var = "movement")) %>% 
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting")) %>%
  mutate(mod = "Normalized")
dat2 <- as.data.frame(emmeans::emtrends(deg_z_mod, specs = "situ", var = "movement")) %>% 
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting")) %>%
  mutate(mod = "Intentional")
dat <- bind_rows(dat1, dat2)

fig_2d <- dat %>%
  ggplot(aes(x = situ, y = movement.trend, col = situ,
             group = interaction(mod, situ)))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0, linewidth = 0.5,
                position = position_dodge(width = 0.2))+
  geom_point(size = 3, aes(pch = mod),
             position = position_dodge(width = 0.2),
             fill = "white")+
  scale_color_manual(values = situcolors, guide = "none")+
  scale_x_discrete(limits = rev, position = "top")+
  scale_shape_manual(name = "", values = c(16, 21))+
  coord_flip() + ylab("Movement effect")+
  guides(pch = guide_legend(position = "inside"))+
  theme(text = element_text(size = 14),
        legend.position.inside = c(0.2, 0.15),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = NA))
fig_2d
ggsave(filename = here("fig/2d.png"), fig_2d, width = 3.5, height = 4)


# Figure 3 ----------------------------------------------------------------
deg_mov <- as.data.frame(emmeans::emtrends(deg_mod, specs = "situ", var = "movement")) %>% 
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting")) %>%
  mutate(mod = "Normalized")

deg_z_mov <- as.data.frame(emmeans::emtrends(deg_z_mod, specs = "situ", var = "movement")) %>% 
  mutate(situ = case_when(situ == "Fe" ~ "Feeding",
                          situ == "Fl" ~ "Flight",
                          situ == "Ro" ~ "Roosting")) %>%
  mutate(mod = "Intentional")
deg_all_mov <- bind_rows(deg_mov, deg_z_mov)

deg_all_mov %>%
  ggplot(aes(x = situ, y = movement.trend, col = situ),
         group = interaction(mod, situ)) +
  geom_hline(aes(yintercept = 0), linetype = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax =))

dat %>%
  ggplot(aes(x = situ, y = movement.trend, col = situ,
             group = interaction(mod, situ)))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0, linewidth = 0.5,
                position = position_dodge(width = 0.2))+
  geom_point(size = 3, aes(pch = mod),
             position = position_dodge(width = 0.2),
             fill = "white")+
  scale_color_manual(values = situcolors, guide = "none")+
  scale_x_discrete(limits = rev, position = "top")+
  scale_shape_manual(name = "", values = c(16, 21))+
  coord_flip() + ylab("Movement effect")+
  guides(pch = guide_legend(position = "inside"))+
  theme(text = element_text(size = 14),
        legend.position.inside = c(0.2, 0.15),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = NA))


