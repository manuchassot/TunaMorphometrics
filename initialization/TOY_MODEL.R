
# https://www.r-bloggers.com/2013/05/bayesian-type-ii-regression/
# https://stats.stackexchange.com/questions/215034/bayesian-errors-in-variables-model-definition-in-jags-and-symbolically
# see also http://biometry.github.io/APES//LectureNotes/StatsCafe/Linear_models_jags.html for linear models

# Libraries
library(rjags)
library(ggmcmc)
library(data.table)

# simulate covariate data
n <- 50
sdx <- 6
sdobs <- 5
taux <- 1 / (sdobs * sdobs)
truex <- rnorm(n, 0, sdx)
errorx <- rnorm(n, 0, sdobs)
obsx <- truex + errorx

# simulate response data
alpha <- 0
beta <- 10
sdy <- 20
errory <- rnorm(n, 0, sdy)
obsy <- alpha + beta*truex + errory
parms <- data.frame(alpha, beta)

obs = data.frame(obsx, obsy)

# simulate response data
alpha <- 0
beta <- 10
sdy <- 20
errory <- rnorm(n, 0, sdy)
obsy <- alpha + beta * truex + errory
parms <- data.frame(alpha, beta)

# bundle data
jags_d <- list(x = obsx, y = obsy, n = length(obsx))

# NO ERROR IN THE COVARIATE ####

# write model
cat("
    model{
## Priors
alpha ~ dnorm(0, .001)
beta ~ dnorm(0, .001)
sdy ~ dunif(0, 100)
tauy <- 1 / (sdy * sdy)

## Likelihood
  for (i in 1:n){
    mu[i] <- alpha + beta * x[i]
    y[i] ~ dnorm(mu[i], tauy)
  }
}
",
fill = TRUE, file = "yerror.txt")

# initiate model
mod1 <- jags.model("yerror.txt", data = jags_d, n.chains = 3, n.adapt = 1000)

# R2jags
#params = c("alpha", "beta", "sdy")

#mod1 = R2jags::jags(data = jags_d, parameters.to.save = params, model.file = "yerror.txt", n.chains = 3, n.iter = 12000, n.burnin = 2000, n.thin = 10, DIC = T)

# simulate posterior
out <- coda.samples(mod1, n.iter = 1000, thin = 1, variable.names = c("alpha", "beta", "sdy"))

# store parameter estimates
ggd <- ggs(out)
a <- ggd$value[which(ggd$Parameter == "alpha")]
b <- ggd$value[which(ggd$Parameter == "beta")]
sdy <- ggd$value[which(ggd$Parameter == "sdy")]
d <- data.table(a, b, sdy)[, Model := "yerror"]

# ERROR IN THE COVARIATE ####

# specify model
cat("
    model {
## Priors
alpha ~ dnorm(0, .001)
beta ~ dnorm(0, .001)
sdy ~ dunif(0, 100)
tauy <- 1 / (sdy * sdy)
taux ~ dunif(.03, .05)
sdx = pow(taux, -2)  #sdx = pow(1/taux, -2)

## Likelihood
  for (i in 1:n){
    truex[i] ~ dnorm(0, .04)
    x[i] ~ dnorm(truex[i], taux)
    y[i] ~ dnorm(mu[i], tauy)
    mu[i] <- alpha + beta * truex[i]
  }
}
    ", fill = T, file = "xyerror.txt")

# bundle data
jags_d <- list(x = obsx, y = obsy, n = length(obsx))

# initiate model
mod2 <- jags.model("xyerror.txt", data = jags_d, n.chains = 3, n.adapt = 1000)

# R2jags
#params = c("alpha", "beta", "sdy", "sdx")

#mod2 = R2jags::jags(data = jags_d, parameters.to.save = params, model.file = "xyerror.txt", n.chains = 3, n.iter = 12000, n.burnin = 2000, n.thin = 10, DIC = T)

# simulate posterior
out <- coda.samples(mod2, n.iter  = 30000, thin  = 30, variable.names = c("alpha", "beta", "sdy", "sdx", "truex"))

# store parameter estimates
ggd  <- ggs(out)
a2   <- ggd$value[which(ggd$Parameter == "alpha")]
b2   <- ggd$value[which(ggd$Parameter == "beta")]
sdx2 <- ggd$value[which(ggd$Parameter == "sdx")]
sdy2 <- ggd$value[which(ggd$Parameter == "sdy")]
d2 <- data.table(a2, b2, sdx2, sdy2)[, Model := "xyerror"]

# Build table results
MODELs_RESULTS = rbindlist(list(
  melt.data.table(d, id.vars = "Model", value.name = "Parameter", variable.name = "Value"),
  melt.data.table(d2, id.vars = "Model", value.name = "Parameter", variable.name = "Value"))
)

# Mean linear models
ggplot(obs, aes(x = obsx, obsy)) +
  geom_point(shape = 1, size = 3) +
  theme_bw() +
  geom_abline(intercept = mean(d$a), slope = mean(d$b), color = "red") +
  geom_abline(intercept = mean(d2$a2), slope = mean(d2$b2), color="blue") +
  geom_abline(intercept = alpha, slope = beta, color = "green", size = 1.5, linetype = "dashed") +
  xlab("X values") + ylab("Observed Y values") +
  ggtitle("Model results with and without modeling error in X")

# Posterior distributions of a based on model 1 (yerror) and model s2 (xyerror)
ggplot(d, aes(x = a)) +
  geom_density() +
  theme_bw() +
  labs(x = "value", y = "Density")







