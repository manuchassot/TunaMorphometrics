#https://www.r-bloggers.com/2013/05/bayesian-type-ii-regression/

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
fill=TRUE, file="yerror.txt")

require(rjags)

# initiate model
mod1 <- jags.model("yerror.txt", data = jags_d, n.chains = 3, n.adapt = 1000)

# simulate posterior
out <- coda.samples(mod1, n.iter = 1000, thin = 1, variable.names = c("alpha", "beta", "sdy"))

# store parameter estimates
require(ggmcmc)
ggd <- ggs(out)
a <- ggd$value[which(ggd$Parameter == "alpha")]
b <- ggd$value[which(ggd$Parameter == "beta")]
d <- data.frame(a, b)

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

## Likelihood
  for (i in 1:n){
    truex[i] ~ dnorm(0, .04)
    x[i] ~ dnorm(truex[i], taux)
    y[i] ~ dnorm(mu[i], tauy)
    mu[i] <- alpha + beta * truex[i]
  }
}
    ", fill=T, file="xyerror.txt")

# bundle data
jags_d <- list(x = obsx, y = obsy, n = length(obsx))

# initiate model
mod2 <- jags.model("xyerror.txt", data=jags_d,
                   n.chains=3, n.adapt=1000)

# simulate posterior
out <- coda.samples(mod2, n.iter=30000, thin=30,
                    variable.names=c("alpha", "beta", "tauy", "taux"))
# store parameter estimates
ggd <- ggs(out)
a2 <- ggd$value[which(ggd$Parameter == "alpha")]
b2 <- ggd$value[which(ggd$Parameter == "beta")]
d2 <- data.frame(a2, b2)


