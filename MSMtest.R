# Set parameters
n <- 1000  # Number of samples
t_max <- 100  # Maximum time points
overall_death_rate <- 0.3  # Desired overall death rate (30%)
rr_intervention <- 0.5  # Relative risk of death when intervention goes from 0 to 1

# Initialize the data frame
set.seed(123)
data <- data.table(id = 1:n)

# 1. Generate covariates from a uniform distribution (representing severity)
data[, severity := runif(n, min = 0, max = 1)]  # Covariate (severity)

# 2. Generate time-dependent intervention
for (t in 1:t_max) {
  data[, paste0("intervention_", t) := ifelse(t < (t_max / 2 * (1 - severity)), 0, (t - t_max / 2 * (1 - severity)) * severity)]
}

# 3. Calculate the probability of death using logistic regression at each time point
severity_effect <- 3  # Coefficient for severity
intervention_effect <- log(rr_intervention)  # Effect of intervention at each time point on logit scale

# Initialize survival probability (starts at 1 for all subjects)
data[, cumulative_survival := 1]

# Loop over each time point to calculate the death probability at each time point
for (t in 1:t_max) {
  intervention_col <- paste0("intervention_", t)
  
  # Calculate logit for death probability at each time point
  data[, logit_p_death := severity_effect * severity - intervention_effect * get(intervention_col)]
  
  # Convert logit to probability for this time point
  data[, p_death_t := exp(logit_p_death) / (1 + exp(logit_p_death))]
  
  # Update cumulative survival probability
  data[, cumulative_survival := cumulative_survival * (1 - p_death_t)]  # Multiply by survival at this time point
}

# Calculate final probability of death over all time points (1 - cumulative survival)
data[, p_death := 1 - cumulative_survival]

# Scale the probabilities to achieve the overall death rate of 30%
scaling_factor <- overall_death_rate / mean(data$p_death)
data[, p_death := p_death * scaling_factor]

# 4. Simulate death based on the calculated probability of death
data[, death_time := sapply(p_death, function(p) {
  if (runif(1) < p) {
    return(sample(1:t_max, 1))  # Random time for death if it occurs
  } else {
    return(t_max + 1)  # No death (survives past t_max)
  }
})]

# Generate a binary death outcome
data[, death := ifelse(death_time <= t_max, 1, 0)]

# Check the distribution of p_death
summary(data$p_death)
