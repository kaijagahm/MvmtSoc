# Frequentist models parallel to Bayesian models
# 1. degree ~ PC1
# 2. degree ~ situ + PC1
# 3. degree ~ situ + PC1 + PC2
# 4. degree ~ situ + PC1 + PC2 + age_group

library(tidyverse)
library(lme4)
load("data/derived/forModeling.Rda")

# 1.
mod1 <- lm(degree_scl ~ PC1, data = forModeling)
summary(mod1)

# 2.
mod2 <- lm(degree_scl ~ situ + PC1, data = forModeling)
summary(mod2)

# 3.
mod3 <- lm(degree_scl ~ situ + PC1 + PC2, data = forModeling)
summary(mod3)

# 4.
mod4 <- lm(degree_scl ~ situ + PC1 + PC2 + age_group, data = forModeling)
summary(mod4)
