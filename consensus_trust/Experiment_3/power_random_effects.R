# Power simulation for interaction model (a)
## including random effects for participants

#############
# Attention: with the effect coding: competence scores are generated using
# effect (-0.5, 05) and not dummy (0,1) coding for the factor variables. 
# This is to specify meaningful random effects for the within factor 
# 'convergence'.
#
# Among the two 'get_coefficients' functions, use the effect coding one to
# generate coefficients from assumptions about condition means. 
#############

library(tidyverse); library(purrr); library(broom); library(lmerTest)
set.seed(1000)

# Generate data
create_data <- function(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, beta_3,
                        tau_0, tau_1, rho, sigma) { 
  
  items <- data.frame(
    item_id = seq_len(n_div + n_conv),
    convergence = rep(c("div", "conv"), c(n_div, n_conv)),
    # make the same as a numeric variable to generate competence
    convergence_num = rep(c(-0.5, 0.5), c(n_div, n_conv)))
  
  # variance-covariance matrix
  cov_mx  <- matrix(
    c(tau_0^2,             rho * tau_0 * tau_1,
      rho * tau_0 * tau_1, tau_1^2            ),
    nrow = 2, byrow = TRUE)
  
  subjects <- data.frame(subj_id = seq_len(n_subj),
                         MASS::mvrnorm(n = n_subj,
                                       mu = c(T_0s = 0, T_1s = 0),
                                       Sigma = cov_mx))
  
  crossing(subjects, items)  %>%
    mutate(independence = rep(c("confl", "indep"), c(nrow(.)/2, nrow(.)/2)),
           # make the same as a numeric variable to generate competence
           independence_num = rep(c(-0.5, 0.5), c(nrow(.)/2, nrow(.)/2)),
           # turn independence and convergence into factor variables
           # for later analysis. Make sure the levels correspond to the 
           # numeric variables we generate competence with
           convergence = as.factor(convergence),
           convergence = fct_relevel(convergence, "div", "conv"),
           independence = as.factor(independence),
           independence = fct_relevel(independence, "confl", "indep"),
           # residual error
           e_si = rnorm(nrow(.), mean = 0, sd = sigma),
           # generate competence
           competence = beta_0 + T_0s + (beta_1 + T_1s) * convergence_num + 
             beta_2 * independence_num + 
             beta_3 * convergence_num * independence_num +
             e_si) %>%
    select(subj_id, item_id, convergence, competence, independence, 
           convergence_num, independence_num)
}

# Our estimation function
est_model <- function(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, beta_3,
                      tau_0, tau_1, rho, sigma) { # residual (standard deviation)
  
  data_sim <- create_data(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, beta_3,
                          tau_0, tau_1, rho, sigma)
  model_sim <- lmer(competence ~ convergence + independence + 
                      convergence*independence + (1 + convergence | subj_id),
                    data_sim)
  
  tidied <- broom.mixed::tidy(model_sim)
  sig <- tidied$p.value[4] < .05
  
  return(sig)
}


# Iteration function
iterate <- function(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, beta_3,
                    tau_0, tau_1, rho, sigma, iterations) 
{ # number of iterations
  
  results <-  1:iterations %>%
    map_dbl(function(x) {
      # To keep track of progress
      if (x %% 100 == 0) {print(paste("iteration number ", x))}
      
      # Run our model and return the result
      return(est_model(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, beta_3,
                       tau_0, tau_1, rho, sigma))
    })
  
  # We want to know statistical power, 
  # i.e., the proportion of significant results
  return(mean(results))
}

# get fixed effect coefficients from assumptions about means in dummy coding
get_dummy_coefficients <- function(mean_convergence_dependence, 
                                   mean_divergence_dependence, 
                                   mean_convergence_independence, 
                                   mean_divergence_independence) {
  
  beta_0 = mean_divergence_dependence
  beta_1 = mean_convergence_dependence - mean_divergence_dependence
  beta_2 = mean_divergence_independence - mean_divergence_dependence
  beta_3 = (mean_convergence_independence - mean_divergence_independence) - 
    (mean_convergence_dependence - mean_divergence_dependence)
  
  coefficients_dummy <- tibble(beta_0, beta_1, beta_2, beta_3)
  coefficients_dummy
}

# get fixed effect coefficients from assumptions about means in effect coding
get_effect_coefficients <- function(mean_convergence_dependence, 
                                   mean_divergence_dependence, 
                                   mean_convergence_independence, 
                                   mean_divergence_independence) {
  
  # overall mean of DV (i.e. competence)
  beta_0 = (mean_convergence_dependence + mean_convergence_independence + 
              mean_divergence_dependence + mean_divergence_independence)/4
  # mean difference for within conditions: convergence - divergence
  beta_1 = (mean_convergence_dependence + mean_convergence_independence)/2 - 
    (mean_divergence_dependence + mean_divergence_independence)/2
  # mean difference for between conditions: independence - dependence
  beta_2 = (mean_divergence_independence + mean_convergence_independence)/2 - 
    (mean_divergence_dependence + mean_convergence_dependence)/2
  # interaction between convergence and independence
  beta_3 = (mean_convergence_independence - mean_divergence_independence) - 
    (mean_convergence_dependence - mean_divergence_dependence)
  
  coefficients_effect <- tibble(beta_0, beta_1, beta_2, beta_3)
  coefficients_effect
}

###########################

# IMPORTANT: use the effect coefficients (not the dummy ones!) 
# for the simulations below 
get_dummy_coefficients(mean_convergence_dependence = 4, 
                       mean_divergence_dependence = 4, 
                       mean_convergence_independence = 4.5, 
                       mean_divergence_independence = 3.5)

get_effect_coefficients(mean_convergence_dependence = 4, 
                       mean_divergence_dependence = 4, 
                       mean_convergence_independence = 4.5, 
                       mean_divergence_independence = 3.5)

# quick data frame check
data <- create_data(
  n_subj = 100,
  n_div  =  2,   # number of low convergence stimuli
  n_conv =  2,   # number of high convergence stimuli
  beta_0 = 4,
  beta_1 = 0.5,  
  beta_2 = 0, 
  beta_3 = 1, 
  tau_0      = 0.7,   # by-subject random intercept sd
  tau_1      =  0.9,   # by-subject random slope sd
  rho        = 0.1,   # correlation between intercept and slope by-subject
  sigma      = 0.99) # residual (standard deviation)

data

# quick model check
check_model <- function(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, beta_3,
                      tau_0, tau_1, rho, sigma)
{ # residual (standard deviation)
  
  data_sim <- create_data(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, beta_3,
                          tau_0, tau_1, rho, sigma)
  model_sim <- lmer(competence ~ convergence + independence + 
                      convergence*independence + (1 + convergence | subj_id),
                    data_sim)
  
  tidied <- broom.mixed::tidy(model_sim)
  
  return(tidied)
}
estimates <- check_model(
  n_subj = 100,
  n_div  =  2,   # number of low convergence stimuli
  n_conv =  2,   # number of high convergence stimuli
  beta_0 = 4,
  beta_1 = 0.5,  
  beta_2 = 0, 
  beta_3 = 1, 
  tau_0      = 0.7,   # by-subject random intercept sd
  tau_1      =  0.9,   # by-subject random slope sd
  rho        = 0.1,   # correlation between intercept and slope by-subject
  sigma      = 0.99) # residual (standard deviation)

estimates

interaction <- estimates$p.value[4] 
interaction

# quick power tryout
power <- iterate(
  n_subj = 100,
  n_div  =  2,   # number of low convergence stimuli
  n_conv =  2,   # number of high convergence stimuli
  beta_0 = 4, 
  beta_1 = 0.5,  
  beta_2 = 0, 
  beta_3 = 1, 
  tau_0      = 0.7,   # by-subject random intercept sd
  tau_1      =  1,   # by-subject random slope sd
  rho        = 0.1,   # correlation between intercept and slope by-subject
  sigma      = 1,  # residual (standard deviation)
  iterations = 500)

power
#############################

# Let's find the minimum sample size
mss <- tibble(n_subj = c(30, 50, 70, 80, 90, 100, 150, 200, 250, 300))
mss$power <- c(30, 50, 70, 80, 90, 100, 150, 200, 250, 300) %>%
  # the argument we're passing is the first argument of iterate(), i.e. "n_subj"
  map_dbl(iterate,
          n_div  =  2,   # number of low convergence stimuli
          n_conv =  2,   # number of high convergence stimuli
          beta_0 = 4, # grand mean (mean of competence across all conditions)
          beta_1 = 0.5, # mean difference for within conditions: convergence - divergence
          beta_2 = 0, # mean difference for between conditions: independence - dependence
          beta_3 = 1, # interaction (difference in effect of convergence between indep)
          tau_0      = 0.7,   # by-subject random intercept sd
          tau_1      =  1,   # by-subject random slope sd
          rho        = 0.1,   # correlation between intercept and slope by-subject
          sigma      = 1,  # residual (standard deviation)
          iterations = 1000)
# Look for the first N with power above 90%
mss

# plot results
ggplot(mss, 
       aes(x = n_subj, y = power)) +
  geom_line(color = 'red', size = 1.5) + 
  # add a horizontal line at 90%
  geom_hline(aes(yintercept = .9), linetype = 'dashed') + 
  # Prettify!
  theme_minimal() + 
  scale_y_continuous(labels = scales::percent) + 
  labs(x = 'Sample Size', y = 'Power') +  
  ggtitle("Power analysis",
          "(accounting for *random* participant effects)")









