library(ctmm)
library(tidyverse)
data(buffalo)  

#summarize buffalo data
summary(buffalo)


# Plot telemetry ----------------------------------------------------------
plot(buffalo)
COL <- rainbow(length(buffalo))
plot(buffalo, col = COL)

# plot buffalo by spatially-separated rainbow
COL <- color(buffalo, by = "individual")
plot(buffalo, col = COL)
?color # many other built-in operations

projection(buffalo)
compass() # add compass to map. Note that compass isn't pointing up.
projection(buffalo) <- median(buffalo) # set projection to the geometric median
plot(buffalo)
compass()


# Outlier detection/error modeling ----------------------------------------


# Variogram ---------------------------------------------------------------
names(buffalo)

# select buffalo Cilla
DATA <- buffalo$Cilla
plot(DATA)

# color by time
COL <- color(DATA, by ="time")
plot(DATA, col = COL) # good idea when you're doing home range estimation, to see if the animal is range-resident.

# calculate a variogram object (named SVF)
svf <- variogram(DATA)
plot(svf) # at 0 lags, all data 100% correlated, and then less and less correlated (acf). Variogram is kinda like that upside down and not normalized. Starts at 0 and goes to an asymptote = variance of the process (roughly the scale of the home range)
# Take two data points e.g. one month apart, how far do they tend to be, squared, on average (instad of taking two data points some distance apart, how close in time do they tend to be)
# Where the asymptote is is a rough estimate of the gaussian home range.

# There are options for e.g. irregular sampling
svf <- variogram(DATA, CI ="Gauss") # more accurate confidence intervals (don't use this on a large dataset). By default only showing the first half of the data because at the largest lag, there are only two points, so estimates too far out are garbage.
# generally, if you want to use e.g. KDE/MCP, you would have to thin your data to e.g. 2 weeks (point at time when you reach the asymptote)
zoom(svf)
# Things to look for
# * the asymptote (if any)
# * how long does it take to asymptote
# * initial curvature or initial linear? (linear--regular diffusion, brownian motion. But curve in the beginning will get you in trouble if you didn't thin the data down. Curve means autocorrelation in velocity.)


# Model selection ---------------------------------------------------------
# model guesstimate
ctmm.guess(DATA) # red is the guess. We are about to do optimization. Whenever you do that, you need good starting points. This is intractably nonlinear so we need good starting points.
guess <- ctmm.guess(DATA, interactive = FALSE) # get an automated guess

# automated model selection
?ctmm.select

FITS <- ctmm.select(DATA, guess, trace = 3, verbose = TRUE, cores=-1) # takes a few minutes because it's fitting several different models.
save(FITS, file = "cilla.rda")
load("cilla.rda")
