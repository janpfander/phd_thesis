# Power simulation for interaction model (b)
## NOT including random effects 

library(tidyverse); library(purrr); library(broom)
set.seed(1000)

# data generation function
create_data <- function(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, 
                        beta_3, residual_sd) { 
  
  items <- data.frame(
    item_id = seq_len(n_div + n_conv),
    convergence = rep(c("div", "conv"), c(n_div, n_conv)),
    # make the same as a numeric variable to generate competence
    convergence_num = rep(c(0, 1), c(n_div, n_conv)))
  
subjects <- data.frame(subj_id = seq_len(n_subj))
  
  crossing(subjects, items)  %>%
    mutate(independence = rep(c("confl", "indep"), c(nrow(.)/2, nrow(.)/2)),
           # make the same as a numeric variable to generate competence
           independence_num = rep(c(0, 1), c(nrow(.)/2, nrow(.)/2)),
           # turn independence and convergence into factor variables
           # for later analysis. Make sure the levels correspond to the 
           # numeric variables we generate competence with
           convergence = as.factor(convergence),
           convergence = fct_relevel(convergence, "div", "conv"),
           independence = as.factor(independence),
           independence = fct_relevel(independence, "confl", "indep"),
           # residual error
           e = rnorm(nrow(.), mean = 0, sd = residual_sd),
           # generate competence
           competence = beta_0 + beta_1  * convergence_num + 
             beta_2 * independence_num + 
             beta_3 * convergence_num * independence_num +
             e) %>%
    select(subj_id, item_id, convergence, competence, independence, 
           convergence_num, independence_num)
}


# estimation function
est_model <- function(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, 
                      beta_3, residual_sd) {
  d <- create_data(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, 
                   beta_3, residual_sd)
  
  # Our model
  m <- lm(competence~convergence*independence, data = d)
  tidied <- tidy(m)
  
  # By looking we can spot that the interaction
  # term is in the 5th row
  sig <- tidied$p.value[4] < .05
  
  return(sig)
}

# iteration function
iterate <- function(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, 
                     beta_3, residual_sd, iters) {
  results <-  1:iters %>%
    map_dbl(function(x) {
      # To keep track of progress
      if (x %% 100 == 0) {print(x)}
      
      # Run our model and return the result
      return(est_model(n_subj, n_div, n_conv, beta_0, beta_1, beta_2, 
                       beta_3, residual_sd))
    })
  
  # We want to know statistical power, 
  # i.e., the proportion of significant results
  return(mean(results))
}

# get coefficients from assumptions about means assuming dummy coding
# of conditions
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

###########################
# quick power tryout
get_dummy_coefficients(mean_convergence_dependence = 4, 
                       mean_divergence_dependence = 4, 
                       mean_convergence_independence = 4.5, 
                       mean_divergence_independence = 3.5)

power <- iterate(
  n_subj = 100,
  n_div  =  2,   # number of low convergence stimuli
  n_conv =  2,   # number of high convergence stimuli
  beta_0 = 4, # intercept, (convergence = 0) condition
  beta_1 = 0,  # effect of convergence given dependence (independence = 0)
  beta_2 = -0.5, # effect of independence given divergence (convergence = 0)
  beta_3 = 1, # interaction (difference in effect of convergence between indep)
  residual_sd = 1.3, # residual standard error of the regression model
  iters = 500) # number of iterations

power
#############################

# Let's find the minimum sample size

# set assumptions about means and get coefficients
get_dummy_coefficients(mean_convergence_dependence = 4, 
                 mean_divergence_dependence = 4, 
                 mean_convergence_independence = 4.5, 
                 mean_divergence_independence = 3.5)

# enter coefficients from the output below into the iterate function
mss <- tibble(n_subj = c(30, 50, 70, 80, 90, 100, 150, 200, 250, 300))
mss$power <- c(30, 50, 70, 80, 90, 100, 150, 200, 250, 300) %>%
  # now the argument we're 
  # passing is the first argument of iterate()
  map_dbl(iterate,
    n_div  =  2,   # number of low convergence stimuli
    n_conv =  2,   # number of high convergence stimuli
    beta_0 = 4, # intercept, (convergence = 0) condition
    beta_1 = 0,  # effect of convergence given dependence (independence = 0)
    beta_2 = -0.5, # effect of independence given divergence (convergence = 0)
    beta_3 = 1, # interaction (difference in effect of convergence between indep)
    residual_sd = 1.3, # residual standard error of the regression model
    iters = 1000) # number of iterations
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
          "(*not* accounting for random participant effects)")


