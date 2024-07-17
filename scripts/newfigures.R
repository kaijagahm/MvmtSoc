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
        legend.position = "bottom")
fig_2d
ggsave(filename = here("fig/2d.png"), fig_2d, width = 6, height = 4)

## E. Example forest plot with both types of lines
fig_2e <- eff %>%
  ggplot(aes(x = situ, y = movement.trend, col = situ,
             group = interaction(mod, situ)))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0, linewidth = 0.5,
                position = position_dodge(width = 0.2))+
  geom_point(size = 3, 
             position = position_dodge(width = 0.2),
             fill = "white")+
  scale_color_manual(values = situcolors, guide = "none")+
  scale_x_discrete(limits = rev, position = "top")+
  scale_shape_manual(name = "", values = c(16, 21))+
  coord_flip() + 
  facet_wrap(~mod, scales = "free",
             ncol = 1)+
  ylab("Movement effect")+
  theme(text = element_text(size = 14),
        axis.title.y = element_blank())
fig_2e
ggsave(filename = here("fig/2e.png"), fig_2e, width = 3, height = 4)

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
                            situ == "Ro" ~ "Roosting"))
  
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

plots <- map(forforest, ~{
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
    xlab("Effect")+
    theme(text = element_text(size = 14),
          axis.title.y = element_blank())
  return(p)
})

patchwork <- (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]])
patchwork + plot_annotation(
  title = 'Forest plots for model results',
  subtitle = 'test subtitle',
  caption = 'test caption'
)
