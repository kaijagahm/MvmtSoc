load("data/contrib.Rda")
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
  theme(panel.background = element_rect(fill = "#FFFCF6"),
        plot.background = element_rect(fill = "#FFFCF6"),
        text = element_text(color = "#7A695A", size = 18),
        axis.text = element_text(color = "#7A695A"),
        axis.ticks = element_line(color = "#7A695A"),
        axis.line = element_line(color = "#7A695A"))+
  NULL
PC1contrib

PC2contrib <- contrib %>%
  ggplot(aes(x = reorder(varName, PC2), y = PC2))+
  geom_col(fill = "#7A695A")+
  coord_flip()+
  ylab("PC2 % contribution")+
  xlab("")+
  theme(panel.background = element_rect(fill = "#FFFCF6"),
        plot.background = element_rect(fill = "#FFFCF6"),
        text = element_text(color = "#7A695A", size = 18),
        axis.text = element_text(color = "#7A695A"),
        axis.ticks = element_line(color = "#7A695A"),
        axis.line = element_line(color = "#7A695A"))+
  theme(text = element_text(size = 18))
PC2contrib

