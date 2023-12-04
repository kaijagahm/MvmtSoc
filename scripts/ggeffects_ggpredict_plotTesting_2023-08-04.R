library(tidyverse)
library(sjPlot)
library(ggeffects)
load("d.Rda")

# Using plot_model
# Conditional (predict) plot with plot_model
plot_model(d, type = "pred", terms = c("PC2", "situ"), show.data = T)+
  theme_classic()

# Marginal effects plot with plot_model
plot_model(d, type = "eff", terms = c("PC2", "situ"), show.data = T)+
  theme_classic()

# Using ggeffects and then ggplot
pred <- as.data.frame(ggpredict(d, terms = c("PC2", "situ")))
eff <- as.data.frame(ggeffect(d, terms = c("PC2", "situ")))
ggplot(pred, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(aes(col = group), linewidth = 1.5)

ggplot(eff, aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
              alpha = 0.2, linewidth = 0.6, show.legend = F)+
  geom_line(aes(col = group), linewidth = 1.5)+
  geom_point(data = forModeling, aes(x = PC2, y = degree_scl, col = situ))
