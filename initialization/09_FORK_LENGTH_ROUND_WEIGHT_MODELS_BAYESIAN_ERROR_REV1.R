
# BAYESIAN MODEL ####
nls2_jags <- function(){
  
  # Priors
  log10a_prior        ~ dnorm(-4.589, 21)   # intercept prior not updated
  log10a              ~ dnorm(-4.589, 21)   # intercept to update
  b_prior             ~ dnorm(2.955, 91)    # slope prior not updated
  b                   ~ dnorm(2.955, 91)    # slope to update
  sigma_prior         ~ dunif(0, 100)       # standard deviation prior: model error on weights
  sigma               ~ dunif(0, 100)       # standard deviation (both measurement and residual combined)
  sigma_log10fl_prior ~ dunif(0.5, 1.5)     # standard deviation of errors measurements in log(fork length) - not updated
  sigma_log10fl       ~ dunif(0.5, 1.5)     # standard deviation of errors measurements in log(fork length)

  # Prior on mean_log10fl
  for (i in 1:N){
  mean_log10fl[i] ~ dnorm(1.8, 40)  # prior on mean_log10fl  # dnorm(1.8, 40)
                }
  
  # Likelihood
  for (i in 1:N){
    log10fl[i] ~ dnorm(mean_log10fl[i], tau_fl)
    log10wt[i] ~ dnorm(mean_log10wt[i], tau)
    mean_log10wt[i] = log10a + b * mean_log10fl[i]
    
  }
  
  # Derived quantities
  tau        = 1 / (sigma * sigma)                  # tau is precision (1 / variance)
  tau_fl     = 1 / (sigma_log10fl * sigma_log10fl)
  a          = 10^(log10a)
  a_prior    = 10^(log10a_prior)
  a1e5_prior = 10^(log10a_prior) * 1e5
  a1e5       = a*1e5
  
  # Assess model using a sums-of-squares-type discrepancy
  for (i in 1:N){
    residual[i]  = log10wt[i] - mean_log10wt[i]    # Residuals for observed data
    predicted[i] = mean_log10wt[i]                 # Predicted values
    sq[i]        = pow(residual[i], 2)             # Square residuals for observed data
    
    # Generate replicate data and compute fit stats for them
    log10wt.new[i] ~ dnorm(mean_log10wt[i], tau)              # One new data set at each MCMC iteration
    sq.new[i]    = pow(log10wt.new[i] - predicted[i], 2)      # Squared residuals for new data
  }
  
  # Quantities of interest
  fit     = sum(sq[])           # Sum of square residuals for actual data set
  fit.new = sum(sq.new[])       # Sum of squared residuals for new data set
  test    = step(fit.new - fit) # Test whether new data set more extreme
  bpvalue = mean(test)          # Bayesian p-value
}

## DATA ####
FORK_LENGTH_ROUND_WEIGHT_DATASET_IO_BET = FULL_DATASET[ocean_code == "IO" & species_code_fao == "BET" & !is.na(fork_length) & !is.na(whole_fish_weight)]
FORK_LENGTH_ROUND_WEIGHT_DATASET_IO_BET[, log10fl := log(fork_length) / log(10)]
FORK_LENGTH_ROUND_WEIGHT_DATASET_IO_BET[, log10wt := log(whole_fish_weight) / log(10)]
FORK_LENGTH_ROUND_WEIGHT_DATASET_IO_BET[, log10flstd := (log10fl - mean(log10fl))/ sd(log10fl)]

JAGS_DATA = with(FORK_LENGTH_ROUND_WEIGHT_DATASET_IO_BET, list(log10fl = log10fl, log10wt = log10wt, N = FORK_LENGTH_ROUND_WEIGHT_DATASET_IO_BET[, .N]))

## PARAMETERS TO ESTIMATE ####
params = c("log10a_prior", "log10a", "b_prior", "b", "sigma", "sigma_prior", "a", "a_prior", "a1e5", "a1e5_prior", "fit", "fit.new", "bpvalue", "residual", "predicted", "mean_log10fl", "sigma_log10fl_prior", "sigma_log10fl")

## INITIALIZATION FUNCTION ####
init_values <- function(){
  list()
                  }
# STATISTICAL INFERENCE ####
### 3 chains of 12,000 samples each, remove 2,000 first and extract every 10 sample = 1,000 samples per chain x 3 = 3,000 samples
nc = 3
ni = 100000
nb = 2000
nt = 10

FL_RW_MODEL = jags(data = JAGS_DATA, inits = init_values, parameters.to.save = params, model.file = nls2_jags, n.chains = nc, n.iter = ni, n.burnin = nb, n.thin = nt, DIC = T)

# OUTPUTS ####

PARAMETERS_SUMMARY = as.data.table(FL_RW_MODEL$BUGSoutput$summary, keep.rownames = TRUE)

ITERATIONS = FL_RW_MODEL$BUGSoutput$n.sims

PARAMETERS = c("a1e5_prior", "a1e5", "b_prior", "b", "sigma_prior", "sigma", "sigma_log10fl_prior", "sigma_log10fl")

PARAMETERS_POSTERIORS = rbindlist(lapply(FL_RW_MODEL$BUGSoutput$sims.list[PARAMETERS], as.data.frame))

names(PARAMETERS_POSTERIORS)[1] = "VALUE"

PARAMETERS_POSTERIORS[, PARAM := rep(PARAMETERS, each = ITERATIONS)]

PARAMETERS_POSTERIORS[, PARAM := factor(PARAM, levels = c("a1e5_prior", "b_prior", "sigma_prior", "sigma_log10fl_prior", "a1e5", "b", "sigma", "sigma_log10fl"))]

setcolorder(PARAMETERS_POSTERIORS, c("PARAM", "VALUE"))

## PRIOR & POSTERIOR PARAMETERS ####

PRIORS_POSTERIORS_CHART = 
ggplot(data = PARAMETERS_POSTERIORS, aes(x = VALUE)) +
  geom_histogram(aes(y = stat(count) / sum(count)), bins = 30, fill = "grey", color = "black", alpha = 0.6) +
  theme_bw() +
  labs(x = "Value", y = "Relative frequency") +
  facet_wrap(~PARAM, scale = "free", nrow = 2, ncol = 4) +
  theme(strip.background = element_rect(fill = "white"))

ggsave("../outputs/charts/BAYESIAN/PRIORS_POSTERIORS_CHART.png", PRIORS_POSTERIORS_CHART, width = 8, height = 4.5)

a_PRIOR = data.table(a = 10^(rnorm(1000, mean = -4.589, sd = sqrt(1/21))) * 1e5)
a_PRIOR[, REL := a/sum(a)]

ggplot(PARAMETERS_POSTERIORS[PARAM == "a1e5"], aes(x = VALUE)) +
  geom_density() +
  geom_density(data = a_PRIOR, aes(x = a), size = 1.2, color = "red") +
  theme_bw() +
  labs(x = "value", y = "Density")

b_PRIOR = data.table(b = rnorm(1000, mean = 2.955, sd = sqrt(1/91)))
b_PRIOR[, REL := b/sum(b)]

ggplot(PARAMETERS_POSTERIORS[PARAM == "b"], aes(x = VALUE)) +
  geom_density() +
  geom_density(data = b_PRIOR, aes(x = b), size = 1.2, color = "red") +
  theme_bw() +
  labs(x = "value", y = "Density")

## PREDICTIONS ####

### Extract mean values of a & b
MEAN_PREDICTIONS = as.numeric(FL_RW_MODEL$BUGSoutput$mean$a) * (10^JAGS_DATA$log10fl) ^ as.numeric(FL_RW_MODEL$BUGSoutput$mean$b)

### Array of predictions (rows = observations, columns = samples)
PREDICTIONS = array(dim = c(JAGS_DATA$N, FL_RW_MODEL$BUGSoutput$n.sims))

for (i in 1:JAGS_DATA$N){
  
  PREDICTIONS[i,] = FL_RW_MODEL$BUGSoutput$sims.list$a * (10^JAGS_DATA$log10fl[i]) ^ FL_RW_MODEL$BUGSoutput$sims.list$b
}

# Lower bound
LPB <- apply(PREDICTIONS, 1, quantile, 0.025, na.rm = TRUE)   #2.5% quantile for each observation FL
UPB <- apply(PREDICTIONS, 1, quantile, 0.975, na.rm = TRUE)  #97.5% quantile for each observation FL

# NEW DATAFRAME FOR PREDICTIONS
fork_length = seq(floor(min(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), ceiling(max(FORK_LENGTH_ROUND_WEIGHT_DATASET$fork_length)), 0.1)

WEIGHT_PREDICTIONS = matrix(NA, nrow = ITERATIONS, ncol = length(fork_length))
  
for (i in 1:length(fork_length)){

  WEIGHT_PREDICTIONS[, i] = FL_RW_MODEL$BUGSoutput$sims.list$a * fork_length[i] ^  FL_RW_MODEL$BUGSoutput$sims.list$b
                       }

# Credible intervals
MEAN_WEIGHT_PREDICTIONS  = as.data.frame(cbind(fork_length, whole_fish_weight_predicted_mean = apply(WEIGHT_PREDICTIONS, 2, mean, na.rm = TRUE)))
UPPER_WEIGHT_PREDICTIONS = as.data.frame(cbind(fork_length, whole_fish_weight_predicted_upper = apply(WEIGHT_PREDICTIONS, 2, quantile, 0.975, na.rm = TRUE)))
LOWER_WEIGHT_PREDICTIONS = as.data.frame(cbind(fork_length, whole_fish_weight_predicted_lower = apply(WEIGHT_PREDICTIONS, 2, quantile, 0.025, na.rm = TRUE)))

## Plot the observations and predictions ####

LW_RW_FIT_CHART =
ggplot(FORK_LENGTH_ROUND_WEIGHT_DATASET_IO_BET, aes(x = fork_length, y = whole_fish_weight, color = "black")) +
  geom_point(shape = 3, size= 0.8) +
  theme_bw() +
  scale_color_manual(values = "darkgrey") +
  labs(x = "Fork length (cm)", y = "Whole weight (kg)") +
  theme(legend.position = "none") +
  geom_line(data = MEAN_WEIGHT_PREDICTIONS, aes(x = fork_length, y = whole_fish_weight_predicted_mean)) +
  geom_line(data = UPPER_WEIGHT_PREDICTIONS, aes(x = fork_length, y = whole_fish_weight_predicted_upper), linetype = 2) +
  geom_line(data = LOWER_WEIGHT_PREDICTIONS, aes(x = fork_length, y = whole_fish_weight_predicted_lower), linetype = 2)

ggsave("../outputs/charts/BAYESIAN/LW_RW_FIT_CHART_IO.png", LW_RW_FIT_CHART_IO_BET, width = 8, height = 4.5)
