load("data/derived/contrib.Rda")
source("scripts/ggplot_themes.R")
# Variable contribution plots

# plots -------------------------------------------------------------------
# Let's look at the PC contributions
contrib <- contrib %>%
  mutate(var = row.names(.),
         varName = case_when(var == "meanDMD" ~ "Mn. daily max displacement",
                             var == "propSwitch" ~"Prop. nights roost-switching",
                             var == "shannon" ~"Roost diversity",
                             var == "coreArea" ~"Core area",
                             var == "coreAreaFidelity" ~"50%/95% KDE",
                             var == "homeRange"~"Home range",
                             var == "meanDDT" ~ "Mn. daily distance traveled",
                             var == "uniqueRoosts"~"# unique roosts",
                             var == "meanDFD" ~"Mn. daily flight time",
                             var == "mnDailyMaxAlt" ~"Mn. daily max. altitude",
                             var == "mnDailyMedAlt" ~"Mn. daily median altitude",
                             var == "mnTort" ~ "Mn. daily tortuosity")
  )

# PC1
PC1contrib <- contrib %>%
  ggplot(aes(x = reorder(varName, PC1), y = PC1))+
  geom_col(fill = "#7A695A")+
  coord_flip()+
  ylab("PC1 % contribution")+
  xlab("")+
  theme_abs_2023()
PC1contrib

PC1contrib_quals <- contrib %>%
  ggplot(aes(x = reorder(varName, PC1), y = PC1))+
  geom_col(fill = "gray40")+
  coord_flip()+
  ylab("PC1 % contribution")+
  xlab("")+
  theme_quals()
ggsave(PC1contrib_quals, file = "fig/PC1contrib_quals.png", width = 7, height = 5)

PC2contrib <- contrib %>%
  ggplot(aes(x = reorder(varName, PC2), y = PC2))+
  geom_col(fill = "#7A695A")+
  coord_flip()+
  ylab("PC2 % contribution")+
  xlab("")+theme_abs_2023()
PC2contrib

PC2contrib_quals <- contrib %>%
  ggplot(aes(x = reorder(varName, PC2), y = PC2))+
  geom_col(fill = "gray40")+
  coord_flip()+
  ylab("PC2 % contribution")+
  xlab("")+
  theme_quals()
ggsave(PC2contrib_quals, file = "fig/PC2contrib_quals.png", width = 7, height = 5)

