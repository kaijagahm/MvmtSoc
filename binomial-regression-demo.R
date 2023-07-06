# Script to explore a simple binomial regression

# Setup ------------------------------------------------------------------------

par(mfrow = c(1, 1))

# Define logit function
logit <- function(p) {
  out <- log(p/(1 - p))
  return(out)
}

# Define inverse logit (i.e. logistic) function
inv_logit <- function(x) {
  out <- 1/(1 + exp(-x))
  return(out)
}

# Check that these functions work as expected
yseq <- seq(-4, 4, by = 0.1)
inv_logit(yseq)
logit(inv_logit(yseq))
plot(yseq, inv_logit(yseq))
# This last plot illustrates that applying inv_logit to a linear model
# constraints its arbitrary predictions to between 0 and 1 (probability scale)

# Simualte data ----------------------------------------------------------------

# Simulate some data
# The predictors (A-C) can be thought of as being centered and scaled
n <- 100
A <- rnorm(n)
B <- rnorm(n)
C <- rnorm(n)
bA <- 0.1 # coefficient for A
bB <- 0.5 # coefficient for B
bC <- 0.9 # coefficient for C
m <- bA*A + bB*B + bC*C # linear model
N <- 10 # let each count be from 10 trials, for simplicity
y <- rbinom(n, N, inv_logit(m)) # simulate data

# Construct data frame
d <- data.frame(A = A, B = B, C = C, N = N, y = y)

# Fit model --------------------------------------------------------------------

# Fit model
# Just using glm here since there are not random effects in the simulated data
fit <- glm(cbind(y, N - y) ~ A + B + C, 
           data = d, 
           family = binomial(link = "logit"))

# Inspect model summary
summary(fit)

# Inspect model ----------------------------------------------------------------

# Did we recover the 'true' parameter values?
# Note that parameter values are all on the logit scale
plot(NULL, xlim = c(-0.2, 1.2), ylim = c(0, 4), yaxt = "n", 
     xlab = "value (logit scale)", ylab = "coefficient")
axis(2, 1:3, c("bA", "bB", "bC"))
abline(h = 1:3, col = "gray")
abline(v = 0, lty = 2)
points(x = coef(fit)[2:4], y = 1:3, pch = 16, cex = 1.5)
for (i in 1:3) {
  lines(x = confint(fit)[(2:4)[i], ], y = rep(i, 2), lwd = 2)
}
points(x = c(bA, bB, bC), 1:3, lwd = 3, cex = 1.5, col = "red")

# Experiment with number of samples (n) to see how it influences uncertainty

# Next, inspect parameter values on the probability scale
plot(NULL, xlim = c(inv_logit(-0.2), inv_logit(1.2)), ylim = c(0, 4), yaxt = "n", 
     xlab = "value (probability scale)", ylab = "coefficient")
axis(2, 1:3, c("bA", "bB", "bC"))
abline(h = 1:3, col = "gray")
abline(v = 0.5, lty = 2)
points(x = inv_logit(coef(fit)[2:4]), y = 1:3, pch = 16, cex = 1.5)
for (i in 1:3) {
  lines(x = inv_logit(confint(fit)[(2:4)[i], ]), y = rep(i, 2), lwd = 2)
}
points(x = inv_logit(c(bA, bB, bC)), 1:3, lwd = 3, cex = 1.5, col = "red")

# This also means that when predictors A-C are all 0 (their average here), the
# expected probability of the outcome is:
inv_logit(coef(fit)[1])
# which is expected, since the intercept was implicitly coded as inv_logit(0)
# when simulating the data

# Generate predicted (expected) values for the data
# Make sure to predict on the probability (response) scale
pred <- predict(fit, type = "response")

# Plot predicted probs against observed probs
plot(x = pred, y = d$y/d$N, xlab = "expected", ylab = "observed")
abline(a = 0, b = 1, lty = 2)

# Plot residuals against predicted probs
plot(x = pred, y = pred - d$y/d$N, xlab = "expected", ylab = "residuals")
abline(h = 0, lty = 2)

# These predictions/residuals look good
# One thing I can say from this is that I don't think it makes sense for
# residuals to deviate more than 1 from the expected value, because
# probabilities are constrained to be between 0 and 1

# Predictions ------------------------------------------------------------------

# Let's try generating some counterfactual predictions

##### Scenario 1 #####

# Let A be 0 (its average) since it has little effect on the outcome
# Let B be large, intermediate, or small
# Let C vary continuously

# Specify new data
Bseq <- c(-2, 0, 2)
Cseq <- seq(-2, 2, by = 0.1)
d2 <- data.frame(A = 0,
                 B = rep(Bseq, each = length(Cseq)),
                 C = rep(Cseq, 3))

# Predict
# Assuming normal error around the estimate, as is common
pred2 <- predict(fit, newdata = d2, type = "response", se.fit = TRUE)
d2$ypred <- pred2$fit
d2$lower <- pred2$fit - pred2$se.fit*1.96
d2$upper <- pred2$fit + pred2$se.fit*1.96

# Plot predictions
par(mfrow = c(1, 3))
for (i in 1:length(Bseq)) {
  myd <- d2[d2$B == Bseq[i], ]
  plot(NULL, xlim = c(-2, 2), ylim = c(0, 1),
       xlab = "C", ylab = "prob success", main = paste0("B = ", Bseq[i]))
  lines(x = Cseq, y = myd$ypred)
  lines(x = Cseq, y = myd$lower, lty = 2)
  lines(x = Cseq, y = myd$upper, lty = 2)
}

##### Scenario 2 #####

# Let A be 0 (its average) since it has little effect on the outcome
# Let B vary continuously
# Let C be large, intermediate, or small

# Specify new data
Bseq <- seq(-2, 2, by = 0.1)
Cseq <- c(-2, 0, 2)
d3 <- data.frame(A = 0,
                 B = rep(Bseq, 3),
                 C = rep(Cseq, each = length(Bseq)))

# Predict
# Assuming normal error around the estimate, as is common
pred3 <- predict(fit, newdata = d3, type = "response", se.fit = TRUE)
d3$ypred <- pred3$fit
d3$lower <- pred3$fit - pred3$se.fit*1.96
d3$upper <- pred3$fit + pred3$se.fit*1.96

# Plot predictions
par(mfrow = c(1, 3))
for (i in 1:length(Cseq)) {
  myd <- d3[d3$C == Cseq[i], ]
  plot(NULL, xlim = c(-2, 2), ylim = c(0, 1),
       xlab = "B", ylab = "prob success", main = paste0("C = ", Cseq[i]))
  lines(x = Bseq, y = myd$ypred)
  lines(x = Bseq, y = myd$lower, lty = 2)
  lines(x = Bseq, y = myd$upper, lty = 2)
}

# Extra ------------------------------------------------------------------------

# Finally, let's see what happens if one predictor has a disproportionately
# large influence on the outcome

# Simulate another set of data:
n <- 100
A <- rnorm(n)
B <- rnorm(n)
C <- rnorm(n)
bA <- 0.1
bB <- 0.5
bC <- 2 # set this coefficient to a very large value (try inv_logit(2))
m <- bA*A + bB*B + bC*C
N <- 10
y <- rbinom(n, N, inv_logit(m))

# Construct data frame
dextra <- data.frame(A = A, B = B, C = C, N = N, y = y)

# Fit model
fitextra <- glm(cbind(y, N - y) ~ A + B + C, 
                data = dextra, 
                family = binomial(link = "logit"))

# Inspect model summary
summary(fitextra)

# Specify new data
Bseq <- seq(-2, 2, by = 0.1)
Cseq <- c(-2, 0, 2)
d4 <- data.frame(A = 0,
                 B = rep(Bseq, 3),
                 C = rep(Cseq, each = length(Bseq)))

# Predict
# Assuming normal error around the estimate, as is common
pred4 <- predict(fitextra, newdata = d4, type = "response", se.fit = TRUE)
d4$ypred <- pred4$fit
d4$lower <- pred4$fit - pred4$se.fit*1.96
d4$upper <- pred4$fit + pred4$se.fit*1.96

# Plot predictions
par(mfrow = c(1, 3))
for (i in 1:length(Cseq)) {
  myd <- d4[d4$C == Cseq[i], ]
  plot(NULL, xlim = c(-2, 2), ylim = c(0, 1),
       xlab = "B", ylab = "prob success", main = paste0("C = ", Cseq[i]))
  lines(x = Bseq, y = myd$ypred)
  lines(x = Bseq, y = myd$lower, lty = 2)
  lines(x = Bseq, y = myd$upper, lty = 2)
}

# Since C has such a large effect on the outcome, once C is very large or very
# small the other predictors will exert much less influence on the outcome.

