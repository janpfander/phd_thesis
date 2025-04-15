---
title: "Data cleaning experiment 6"
author: "Jan Pfänder, Hugo Mercier"
date: "2023-03-09"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)     # create plots with ggplot, manipulate data, etc.
library(broom.mixed)   # convert regression models into nice tables
library(modelsummary)  # combine multiple regression models into a single table
library(lme4)          # model specification / estimation 
library(lmerTest)      # provides p-values in the output
library(ggpubr)        # stile feature of ggplot
library(gghalves)      # do special plots in ggplot
library(kableExtra)    # for tables
```

## Import data

```r
d <- read_csv("./data/qualtrics.csv")
names(d)
```

```
##  [1] "StartDate"             "EndDate"               "Status"               
##  [4] "IPAddress"             "Progress"              "Duration (in seconds)"
##  [7] "Finished"              "RecordedDate"          "ResponseId"           
## [10] "RecipientLastName"     "RecipientFirstName"    "RecipientEmail"       
## [13] "ExternalReference"     "LocationLatitude"      "LocationLongitude"    
## [16] "DistributionChannel"   "UserLanguage"          "consent"              
## [19] "attention"             "0_acc_a_1"             "0_com_a"              
## [22] "0_acc_b_1"             "0_com_b"               "1_acc_a_1"            
## [25] "1_com_a"               "1_acc_b_1"             "1_com_b"              
## [28] "2_acc_a_1"             "2_com_a"               "2_acc_b_1"            
## [31] "2_com_b"               "3_acc_a_1"             "3_com_a"              
## [34] "3_acc_b_1"             "3_com_b"               "Q1"                   
## [37] "gender"                "age"                   "education"            
## [40] "PROLIFIC_PID"          "number"                "counter"
```


```r
# delete first two rows
d <- d %>% 
  slice(3: nrow(.)) 
```

## Attention check

```r
# attention check
# to see different answers given (i.e.levels), transform into factor
d$attention <- as.factor(d$attention)
# check levels to see different answer types
levels(d$attention) 
```

```
## [1] "60dc963e81e24696fb377f61" "I  pay attention"        
## [3] "i pay attention"          "I pay attention"         
## [5] "I pay attention."         "in the box"
```

```r
# We filter out two participants 
table(d$attention)
```

```
## 
## 60dc963e81e24696fb377f61         I  pay attention          i pay attention 
##                        1                        1                       11 
##          I pay attention         I pay attention.               in the box 
##                      285                        1                        1
```

There are two clearly failed attention check that we remove. 


```r
# filter to only valid attention check responses
d <- d %>% filter(str_detect(attention, "attention"))
```

## Re-shape data

```r
# clean and re-shape
d <- d %>% 
  # add an easy to read participant identifier
  mutate(id = 1:nrow(.)) %>% 
  # build two variables for the number options factor
  mutate(number_options = ifelse(str_detect(number, "ten"), "ten", 
                          number), 
         stimuli_10_version = ifelse(str_detect(number, "three"), NA, 
                                     number)
         ) %>% 
  # bring to long format
  pivot_longer(cols = c("0_acc_a_1" : "3_com_b"), 
               names_to = "condition", values_to = "score") %>% 
  # For some weird reason, qualtrics added `_1` at the end of each accuracy
  # stimulus name. We remove that
  mutate(across(condition, ~str_remove(., "_1"))) %>% 
  # separate conditions into CONVERGENCE_OUTCOME_STIMULUS
  separate_wider_delim(condition, "_", names = c("convergence", "outcome", 
                                                 "stimulus_variant")
                       ) %>%
  pivot_wider(names_from = outcome, values_from = score) %>% 
  # create better variable names
  rename(competence = com, 
         accuracy = acc) %>% 
  # all variables are coded as `character` - make key variables numeric
  mutate(across(c(convergence, competence, accuracy), as.numeric)
         )
```

## Recode demographics


```r
prolific_demographics <- read_csv("./data/prolific_demographics.csv")

d <- left_join(d, prolific_demographics, by = c("PROLIFIC_PID" = "Participant id"))
```



```r
d <- d %>% 
  mutate(gender = case_when(Sex == "Male" ~ "male", 
                            Sex == "Female" ~  "female", 
                            .default = NA)
         )
```

## Export data


```r
write_csv(d, "data/cleaned.csv")
```
