---
title: "Data cleaning Experiment 3"
output: 
  html_document: 
    keep_md: yes
---


``` r
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

``` r
d <- read_csv("./data/qualtrics.csv")
names(d)
```

```
##   [1] "StartDate"             "EndDate"               "Status"               
##   [4] "IPAddress"             "Progress"              "Duration (in seconds)"
##   [7] "Finished"              "RecordedDate"          "ResponseId"           
##  [10] "RecipientLastName"     "RecipientFirstName"    "RecipientEmail"       
##  [13] "ExternalReference"     "LocationLatitude"      "LocationLongitude"    
##  [16] "DistributionChannel"   "UserLanguage"          "consent"              
##  [19] "attention"             "archeo_clear"          "archeo_surprise"      
##  [22] "archeo_know"           "archeo_learn"          "archeo_knowledge1"    
##  [25] "archeo_knowledge2"     "archeo_knowledge3"     "archeo_knowledge4"    
##  [28] "archeo_knowledge5"     "archeo_imp"            "archeo_comp"          
##  [31] "archeo_trust"          "entom_clear"           "entom_surprise"       
##  [34] "entom_know"            "entom_learn"           "entom_knowledge1"     
##  [37] "entom_knowledge2"      "entom_knowledge3"      "entom_knowledge4"     
##  [40] "entom_knowledge5"      "entom_imp"             "entom_comp"           
##  [43] "entom_trust"           "text1_int_rep_q1"      "text1_int_rep_q2_1"   
##  [46] "text1_int_rep_q2_2"    "text1_int_rep_q2_3"    "text1_int_rep_q2_4"   
##  [49] "text1_int_rep_q2_5"    "text1_int_rep_q2_6"    "text1_int_rep_q2_7"   
##  [52] "text1_int_rep_q2_8"    "text2_unint_mat_q1"    "text2_unint_mat_q2_1" 
##  [55] "text2_unint_mat_q2_2"  "text2_unint_mat_q2_3"  "text2_unint_mat_q2_4" 
##  [58] "text2_unint_mat_q2_5"  "text2_unint_mat_q2_6"  "text2_unint_mat_q2_7" 
##  [61] "text2_unint_mat_q2_8"  "text1_int_mat_q1"      "text1_int_mat_q2_1"   
##  [64] "text1_int_mat_q2_2"    "text1_int_mat_q2_3"    "text1_int_mat_q2_4"   
##  [67] "text1_int_mat_q2_5"    "text1_int_mat_q2_6"    "text1_int_mat_q2_7"   
##  [70] "text1_int_mat_q2_8"    "text2_unint_rep_q1"    "text2_unint_rep_q2_1" 
##  [73] "text2_unint_rep_q2_2"  "text2_unint_rep_q2_3"  "text2_unint_rep_q2_4" 
##  [76] "text2_unint_rep_q2_5"  "text2_unint_rep_q2_6"  "text2_unint_rep_q2_7" 
##  [79] "text2_unint_rep_q2_8"  "text1_unint_rep_q1"    "text1_unint_rep_q2_1" 
##  [82] "text1_unint_rep_q2_2"  "text1_unint_rep_q2_3"  "text1_unint_rep_q2_4" 
##  [85] "text1_unint_rep_q2_5"  "text1_unint_rep_q2_6"  "text1_unint_rep_q2_7" 
##  [88] "text1_unint_rep_q2_8"  "text2_int_mat_q1"      "text2_int_mat_q2_1"   
##  [91] "text2_int_mat_q2_2"    "text2_int_mat_q2_3"    "text2_int_mat_q2_4"   
##  [94] "text2_int_mat_q2_5"    "text2_int_mat_q2_6"    "text2_int_mat_q2_7"   
##  [97] "text2_int_mat_q2_8"    "text1_unint_mat_q1"    "text1_unint_mat_q2_1" 
## [100] "text1_unint_mat_q2_2"  "text1_unint_mat_q2_3"  "text1_unint_mat_q2_4" 
## [103] "text1_unint_mat_q2_5"  "text1_unint_mat_q2_6"  "text1_unint_mat_q2_7" 
## [106] "text1_unint_mat_q2_8"  "text2_int_rep_q1"      "text2_int_rep_q2_1"   
## [109] "text2_int_rep_q2_2"    "text2_int_rep_q2_3"    "text2_int_rep_q2_4"   
## [112] "text2_int_rep_q2_5"    "text2_int_rep_q2_6"    "text2_int_rep_q2_7"   
## [115] "text2_int_rep_q2_8"    "Q1"                    "education"            
## [118] "PROLIFIC_PID"          "condition"             "benefit_band"         
## [121] "benefit_bookclub"      "intention_band"        "intention_bookclub"
```


``` r
# inspect
head(d) # you can also use View(d)
```

```
## # A tibble: 6 × 123
##   StartDate    EndDate Status IPAddress Progress Duration (in seconds…¹ Finished
##   <chr>        <chr>   <chr>  <chr>     <chr>    <chr>                  <chr>   
## 1 "Start Date" "End D… "Resp… "IP Addr… "Progre… "Duration (in seconds… "Finish…
## 2 "{\"ImportI… "{\"Im… "{\"I… "{\"Impo… "{\"Imp… "{\"ImportId\":\"dura… "{\"Imp…
## 3 "2024-06-07… "2024-… "0"    "86.166.… "100"    "251"                  "1"     
## 4 "2024-06-07… "2024-… "0"    "167.98.… "100"    "121"                  "1"     
## 5 "2024-06-07… "2024-… "0"    "81.79.1… "100"    "181"                  "1"     
## 6 "2024-06-07… "2024-… "0"    "86.12.2… "100"    "179"                  "1"     
## # ℹ abbreviated name: ¹​`Duration (in seconds)`
## # ℹ 116 more variables: RecordedDate <chr>, ResponseId <chr>,
## #   RecipientLastName <chr>, RecipientFirstName <chr>, RecipientEmail <chr>,
## #   ExternalReference <chr>, LocationLatitude <chr>, LocationLongitude <chr>,
## #   DistributionChannel <chr>, UserLanguage <chr>, consent <chr>,
## #   attention <chr>, archeo_clear <chr>, archeo_surprise <chr>,
## #   archeo_know <chr>, archeo_learn <chr>, archeo_knowledge1 <chr>, …
```

``` r
# delete first two rows
d <- d %>% 
  slice(3: nrow(.)) 
```

## Attention check

``` r
# attention check
# to see different answers given (i.e.levels), transform into factor
d$attention <- as.factor(d$attention)
# check levels to see different answer types
levels(d$attention) 
```

```
## [1] "\"I pay attention\"" "I pay attension"     "i pay attention"    
## [4] "I pay attention"     "I PAY ATTENTION"     "I pay attention."
```

``` r
table(d$attention)
```

```
## 
## "I pay attention"   I pay attension   i pay attention   I pay attention 
##                 2                 1                12               185 
##   I PAY ATTENTION  I pay attention. 
##                 1                 1
```

There is no failed attention check. 


``` r
# filter to only valid attention check responses
d <- d %>% filter(str_detect(attention, regex("atten", ignore_case = TRUE)))
```

## Re-shape data


``` r
# check all names and their order
names(d)
```

```
##   [1] "StartDate"             "EndDate"               "Status"               
##   [4] "IPAddress"             "Progress"              "Duration (in seconds)"
##   [7] "Finished"              "RecordedDate"          "ResponseId"           
##  [10] "RecipientLastName"     "RecipientFirstName"    "RecipientEmail"       
##  [13] "ExternalReference"     "LocationLatitude"      "LocationLongitude"    
##  [16] "DistributionChannel"   "UserLanguage"          "consent"              
##  [19] "attention"             "archeo_clear"          "archeo_surprise"      
##  [22] "archeo_know"           "archeo_learn"          "archeo_knowledge1"    
##  [25] "archeo_knowledge2"     "archeo_knowledge3"     "archeo_knowledge4"    
##  [28] "archeo_knowledge5"     "archeo_imp"            "archeo_comp"          
##  [31] "archeo_trust"          "entom_clear"           "entom_surprise"       
##  [34] "entom_know"            "entom_learn"           "entom_knowledge1"     
##  [37] "entom_knowledge2"      "entom_knowledge3"      "entom_knowledge4"     
##  [40] "entom_knowledge5"      "entom_imp"             "entom_comp"           
##  [43] "entom_trust"           "text1_int_rep_q1"      "text1_int_rep_q2_1"   
##  [46] "text1_int_rep_q2_2"    "text1_int_rep_q2_3"    "text1_int_rep_q2_4"   
##  [49] "text1_int_rep_q2_5"    "text1_int_rep_q2_6"    "text1_int_rep_q2_7"   
##  [52] "text1_int_rep_q2_8"    "text2_unint_mat_q1"    "text2_unint_mat_q2_1" 
##  [55] "text2_unint_mat_q2_2"  "text2_unint_mat_q2_3"  "text2_unint_mat_q2_4" 
##  [58] "text2_unint_mat_q2_5"  "text2_unint_mat_q2_6"  "text2_unint_mat_q2_7" 
##  [61] "text2_unint_mat_q2_8"  "text1_int_mat_q1"      "text1_int_mat_q2_1"   
##  [64] "text1_int_mat_q2_2"    "text1_int_mat_q2_3"    "text1_int_mat_q2_4"   
##  [67] "text1_int_mat_q2_5"    "text1_int_mat_q2_6"    "text1_int_mat_q2_7"   
##  [70] "text1_int_mat_q2_8"    "text2_unint_rep_q1"    "text2_unint_rep_q2_1" 
##  [73] "text2_unint_rep_q2_2"  "text2_unint_rep_q2_3"  "text2_unint_rep_q2_4" 
##  [76] "text2_unint_rep_q2_5"  "text2_unint_rep_q2_6"  "text2_unint_rep_q2_7" 
##  [79] "text2_unint_rep_q2_8"  "text1_unint_rep_q1"    "text1_unint_rep_q2_1" 
##  [82] "text1_unint_rep_q2_2"  "text1_unint_rep_q2_3"  "text1_unint_rep_q2_4" 
##  [85] "text1_unint_rep_q2_5"  "text1_unint_rep_q2_6"  "text1_unint_rep_q2_7" 
##  [88] "text1_unint_rep_q2_8"  "text2_int_mat_q1"      "text2_int_mat_q2_1"   
##  [91] "text2_int_mat_q2_2"    "text2_int_mat_q2_3"    "text2_int_mat_q2_4"   
##  [94] "text2_int_mat_q2_5"    "text2_int_mat_q2_6"    "text2_int_mat_q2_7"   
##  [97] "text2_int_mat_q2_8"    "text1_unint_mat_q1"    "text1_unint_mat_q2_1" 
## [100] "text1_unint_mat_q2_2"  "text1_unint_mat_q2_3"  "text1_unint_mat_q2_4" 
## [103] "text1_unint_mat_q2_5"  "text1_unint_mat_q2_6"  "text1_unint_mat_q2_7" 
## [106] "text1_unint_mat_q2_8"  "text2_int_rep_q1"      "text2_int_rep_q2_1"   
## [109] "text2_int_rep_q2_2"    "text2_int_rep_q2_3"    "text2_int_rep_q2_4"   
## [112] "text2_int_rep_q2_5"    "text2_int_rep_q2_6"    "text2_int_rep_q2_7"   
## [115] "text2_int_rep_q2_8"    "Q1"                    "education"            
## [118] "PROLIFIC_PID"          "condition"             "benefit_band"         
## [121] "benefit_bookclub"      "intention_band"        "intention_bookclub"
```



``` r
# clean and re-shape
d <- d %>% 
  # add an easy to read participant identifier
  mutate(id = 1:nrow(.)) %>% 
  # rename variable indicating which question has been used
  rename(treatment = condition) %>% 
  # bring to long format
  pivot_longer(cols = c(starts_with("archeo"), starts_with("entom")), 
               names_to = "condition", values_to = "score") %>% 
  # separate conditions into CONVERGENCE_OUTCOME_STIMULUS
  separate_wider_delim(condition, "_", names = c("discipline", "outcome")
                       ) %>%
  # necessary because of the way we labeled
  drop_na(score) %>% 
  pivot_wider(names_from = outcome, values_from = score) %>% 
  # create better variable names
  rename(competence = comp, 
         impressed = imp) %>% 
  # all variables are coded as `character` - make key variables numeric
  mutate(across(c(learn, competence, impressed, trust, starts_with("knowledge")), as.numeric),
         # recode knowledge questions as TRUE and FALSE, such that 1 corresponds to TRUE and 0 to FALSE
         across(starts_with("knowledge"), ~ifelse(.x == 1, TRUE, FALSE))
         )
```

Calculate an average knowledge score and number of correctly answered questions per participant. 


``` r
d <- d %>% 
  # add average knowledge score
  rowwise() %>% 
  mutate(
    n_correct = sum(c_across(starts_with("knowledge")),na.rm = TRUE),
    knowledge_avg = mean(c_across(starts_with("knowledge")),na.rm = TRUE),
         ) %>% 
  ungroup() # Turn off rowwise()
```

## Remove distraction task variable


``` r
d <- d %>% 
  select(-c(starts_with("text"), contains("intention"), contains("benefit")))
```

## Add scaled variables


``` r
# Scale all variables except id and add a suffix "std" to the new variables
d <- d %>%
  mutate(across(c(n_correct, trust, impressed, competence), 
                ~as.numeric(scale(.x)),
                .names = "{.col}_std"))
```

## Add demographics

## Recode demographics


``` r
prolific_demographics <- read_csv("./data/prolific.csv")

d <- left_join(d, prolific_demographics, by = c("PROLIFIC_PID" = "Participant id"))
```



``` r
d <- d %>% 
  mutate(gender = case_when(Sex == "Male" ~ "male", 
                            Sex == "Female" ~  "female", 
                            .default = NA), 
         age = as.numeric(Age)
         ) 
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `age = as.numeric(Age)`.
## Caused by warning:
## ! NAs introduced by coercion
```

## Make long format version

We want to add a binary variable that identifies whether the outcome is knowledge (`n_correct_std`) or one of the other outcomes of interest.


``` r
# Convert to long format
data_long <- d %>%
  pivot_longer(cols = c(n_correct_std, trust_std, impressed_std, competence_std), 
               names_to = "outcome", values_to = "value") %>% 
  # use nicer names for outcomes
  mutate(outcome = recode(outcome,
                          "competence_std" = "competence",
                          "impressed_std" = "impressiveness",
                          "n_correct_std" = "knowledge",
                          "trust_std" = "trust")
         )
```


## Export data


``` r
# wide format
write_csv(d, "data/cleaned_wide.csv")

# long format
write_csv(data_long, "data/cleaned_long.csv")
```
