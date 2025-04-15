# round numbers from models
rounded_numbers <- function(x) mutate_if(x, is.numeric, round, 3)

# get model outputs ready for inline text reporting
text_ready <- function(model_output) {
  
  result <- tidy(model_output, conf.int = TRUE)
  
  if ("effect" %in% colnames(result)) {
    # Filter for effect == "fixed" if the variable exists
    result <- result %>% 
      filter(effect == "fixed")
  } 
  
  result <- result %>% 
    # report p.value according to apa standards
    mutate(p.value = case_when(p.value < 0.001 ~ "< .001",
                               TRUE ~ paste0("= ", sprintf("%.3f", p.value))
    )
    ) %>% 
    # all other terms
    rounded_numbers() %>% 
    mutate(ci = glue::glue("[{conf.low}, {conf.high}]")) 
  
  if ("term" %in% colnames(result)) {
    # Filter for effect == "fixed" if the variable exists
    result <- result %>% 
      mutate(
        term = ifelse(term == "(Intercept)", "intercept", term),
        # if there is an interaction (recognized by ":"), name it just interaction
        term = str_replace(term, ".*:.*", "interaction")
      ) %>% 
      split(.$term)
  } 
  
  return(result)
}

# clean data frame results for paired t-test
clean_t_test <- function(t.test) {
  
  t.test %>% 
    tidy() %>% 
    select(-c(method, alternative)) %>% 
    # report p.value according to apa standards
    mutate(p.value = case_when(p.value < 0.001 ~ "< .001",
                               TRUE ~ sprintf("p = %.3f", p.value)
    )
    ) %>% 
    # all other terms
    rounded_numbers() %>%
    mutate_all(~ {attributes(.) <- NULL; .})
  
}

# Function for splitting data along several variables (useful for inline reporting)
# taken from here: https://www.tjmahr.com/lists-knitr-secret-weapon/
super_split <- function(.data, ...) {
  dots <- rlang::enquos(...)
  for (var in seq_along(dots)) {
    var_name <- rlang::as_name(dots[[var]])
    .data <- purrr::map_depth(
      .x = .data,
      .depth = var - 1,
      .f = function(xs) split(xs, xs[var_name])
    )
  }
  .data
}

# Function to run regression and tidy results
run_regression <- function(data, dependent_var, independent_var, return = "data") {
  
  model <- lm(as.formula(paste(dependent_var, "~", independent_var)), data = data) 
  
  if(return == "data") {
    
    data_frame <- model %>%
      tidy(conf.int = TRUE) %>%
      mutate(outcome = {{dependent_var}})  %>% 
    # report p.value according to apa standards
    mutate(p.value = case_when(p.value < 0.001 ~ "< .001",
                               TRUE ~ paste0("= ", sprintf("%.3f", p.value))
    )
    ) %>% 
      # all other terms
      rounded_numbers() %>% 
      mutate(
        term = ifelse(term == "(Intercept)", "intercept", term),
        ci = glue::glue("[{conf.low}, {conf.high}]"), 
      ) 
    
    return(data_frame)
  }
  
  if(return == "model") {
    
    return(model)
  }
}







