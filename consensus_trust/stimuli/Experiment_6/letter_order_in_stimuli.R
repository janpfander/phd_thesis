# Generate random order of alphabet letters for stimuli

library(tidyverse)
# Set the seed for reproducibility
set.seed(123456)

# Generate a vector with the alphabet (A to Z)
alphabet_vector <- LETTERS

# Randomly sample ten letters from the alphabet for each stimulus
stimulus = 1:8

stimuli_10 <- stimulus %>% map_df(function(x){
  
  sampled_letters <- sample(alphabet_vector, 10, replace = FALSE)
  
  tibble(sampled_letters, stimulus = x) 
}) 

stimuli_10 %>% filter(stimulus == 8)
