
# BAYESIAN MODEL ####
nls2_jags <- function(){
  
  # Priors
  alpha_prior ~ dnorm(0, 0.01) # intercept prior | mean = 0; var = 100; sd = 10
  alpha       ~ dnorm(0, 0.01) # intercept
  beta_prior  ~ dunif(1,6)     # slope
  beta        ~ dunif(1,6)     # slope
  sigma_prior ~ dunif(0, 100)  # standard deviation prior
  sigma       ~ dunif(0, 100)  # standard deviation (both measurement and residual combined) - Uniform between 0 and 100
  
  # Likelihood
  for (i in 1:N){
    logwt[i] ~ dnorm(logmuwt[i], tau)
    logmuwt[i] = alpha + beta * logfl[i]
  }
  
  # Derived quantities
  tau = pow(sigma, -2) # tau is precision (1 / variance)
  a   = exp(alpha) * exp(pow(sigma, 2))/2
  b   = beta
  
  # Assess model using a sums-of-squares-type discrepancy
  for (i in 1:N){
    residual[i]  = logwt[i] - logmuwt[i]    # Residuals for observed data
    predicted[i] = logmuwt[i]               # Predicted values
    sq[i]        = pow(residual[i], 2)      # Square residuals for observed data
    
  # Generate replicate data and compute fit stats for them
    logwt.new[i] ~ dnorm(logmuwt[i], tau)              # One new data set at each MCMC iteration
    sq.new[i]    = pow(logwt.new[i] - predicted[i], 2) # Squared residuals for new data
  }
  
  # Quantities of interest
  fit     = sum(sq[])           # Sum of square residuals for actual data set
  fit.new = sum(sq.new[])       # Sum of squared residuals for new data set
  test    = step(fit.new - fit) # Test whether new data set more extreme
  bpvalue = mean(test)          # Bayesian p-value
}

### Data set
FORK_LENGTH_ROUND_WEIGHT_DATASET = FULL_DATASET[ocean_code == "IO" & species_code_fao == "BET" & !is.na(fork_length) & !is.na(whole_fish_weight) & !is.na(sex)]

JAGS_DATA = with(FORK_LENGTH_ROUND_WEIGHT_DATASET, list(logfl = log(fork_length), logwt = log(whole_fish_weight), N = FORK_LENGTH_ROUND_WEIGHT_DATASET[, .N]))

### Parameters to estimate
params = c("alpha_prior", "alpha", "beta_prior", "beta", "sigma", "sigma_prior", "a", "fit", "fit.new", "bpvalue", "residual", "predicted")

### Inits function
init_values = function(){
  list(alpha = rnorm(1, mean = 0, sd = 1), beta = runif(1, 1, 6), sigma = runif(1, min = 0, max = 1))
}

### Statistical inference
### 3 chains of 12,000 samples each, remove 2,000 first and extract every 10 sample = 1,000 samples per chain x 3 = 3,0000 samples
fit_nls2 <- jags(data = JAGS_DATA, inits = init_values, parameters.to.save = params, model.file = nls2_jags, n.chains = 3, n.iter = 50000, n.burnin = 2000, n.thin = 10, DIC = T)

### POSTERIORS ####
### ALPHA parameter (intercept)
hist(fit_nls2$BUGSoutput$sims.list$alpha,freq=FALSE,main='',col='lightgrey',xlab=expression(paste(alpha)),xlim=c(-30,30), yaxs = 'i', xaxs='i',las=1)
lines(density(fit_nls2$BUGSoutput$sims.list$alpha),col="black",lwd=2,lty=1)
lines(density(fit_nls2$BUGSoutput$sims.list$alpha_prior),col="black",lwd=2,lty=2)
legend('topright',lty=c(2,1),legend=c("Prior","Posterior"),bty='n',cex=1.2)
box()

### a parameter
hist(fit_nls2$BUGSoutput$sims.list$a,freq=FALSE,main='',col='lightgrey',xlab=expression(paste(alpha)),xlim=c(-30,30), yaxs = 'i', xaxs='i',las=1)
lines(density(fit_nls2$BUGSoutput$sims.list$a),col="black",lwd=2,lty=1)
lines(density(exp(fit_nls2$BUGSoutput$sims.list$alpha_prior)),col="black",lwd=2,lty=2)
legend('topright',lty=c(2,1),legend=c("Prior","Posterior"),bty='n',cex=1.2)
box()

### BETA parameter (slope)
hist(fit_nls2$BUGSoutput$sims.list$beta,freq=FALSE,main='',col='lightgrey',xlab=expression(paste(beta)),xlim=c(-30,30), las = 1, ylim=c(0,0.4),xaxs='i',yaxs='i')
lines(density(fit_nls2$BUGSoutput$sims.list$beta),col="black",lwd=2,lty=1)
lines(density(fit_nls2$BUGSoutput$sims.list$beta_prior),col="black",lwd=2,lty=2)
legend('topleft',lty=c(2,1),legend=c("Prior","Posterior"),bty='n',cex=1.2)
box()

### SIGMA parameter (standard deviation of the model error)
hist(fit_nls2$BUGSoutput$sims.list$sigma,freq=FALSE,main='',col='lightgrey',xlab=expression(paste(sigma)),xlim=c(0,100), las = 1, ylim=c(0,0.4),xaxs='i',yaxs='i')
lines(density(fit_nls2$BUGSoutput$sims.list$sigma),col="black",lwd=2,lty=1)
lines(density(fit_nls2$BUGSoutput$sims.list$sigma_prior),col="black",lwd=2,lty=2)
legend('topright',lty=c(2,1),legend=c("Prior","Posterior"),bty='n',cex=1.2)
box()

### PREDICTIONS ####

### Extract mean values of alpha & beta
mean_predictions <- exp(as.numeric(fit_nls2$BUGSoutput$mean$alpha) + as.numeric(fit_nls2$BUGSoutput$mean$beta) * jagsdata_yft_io$logfl)

### Array of predictions
predictions <- array(dim = c(jagsdata_yft_io$N,fit_nls2$BUGSoutput$n.sims))

for (i in 1:jagsdata_yft_io$N){
  predictions[i,] <-  exp(fit_nls2$BUGSoutput$sims.list$alpha + fit_nls2$BUGSoutput$sims.list$beta * jagsdata_yft_io$logfl[i])
}

# Lower bound
LPB <- apply(predictions,1,quantile,0.025,na.rm=T)   #2.5% quantile for each observation logFL
UPB <- apply(predictions,1,quantile,0.975,na.rm=T)  #97.5% quantile for each observation logFL

### Plot the observations
plot(exp(jagsdata_yft_io$logfl),exp(jagsdata_yft_io$logwt),cex.axis=1.3,cex.lab=1.3,las=1,xlab=expression(paste("Fork length ",italic(L[F])," (cm)",sep="")),ylab=expression(paste("Round weight ", italic(W[T])," (kg)",sep="")),pch=19,col="darkgrey",cex=.3)

### Add the mean predictions
lines(sort(exp(jagsdata_yft_io$logfl)),sort(mean_predictions),col="black",lwd=2)

### Add the credible interval (uncertainty interval from the actual data)
lines(sort(exp(jagsdata_yft_io$logfl)),sort(LPB),col='red',lty=2)
lines(sort(exp(jagsdata_yft_io$logfl)),sort(UPB),col='red',lty=2)