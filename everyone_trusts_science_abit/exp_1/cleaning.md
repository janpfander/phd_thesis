---
title: "Data cleaning Study 1"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)     # create plots with ggplot, manipulate data, etc.
```

```
## Warning: package 'stringr' was built under R version 4.2.3
```

```r
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
## [19] "attention_check"       "electrons_know"        "electrons_accept"     
## [22] "antibiotics_know"      "antibiotics_accept"    "continents_know"      
## [25] "continents_accept"     "sex_know"              "sex_accept"           
## [28] "lasers_know"           "lasers_accept"         "orbit_know"           
## [31] "orbit_accept"          "diamonds_know"         "diamonds_accept"      
## [34] "speed_know"            "speed_accept"          "salt_know"            
## [37] "salt_accept"           "trees_know"            "trees_accept"         
## [40] "water_know"            "water_accept"          "Q1...42"              
## [43] "CMQ_1"                 "CMQ_2"                 "CMQ_3"                
## [46] "CMQ_4"                 "CMQ_5"                 "SICBS"                
## [49] "BCTI_intro"            "BCTI_apollo"           "BCTI_cancer"          
## [52] "BCTI_viruses"          "BCTI_climatechange"    "BCTI_flatearth"       
## [55] "BCTI_autism"           "BCTI_polio"            "BCTI_alien"           
## [58] "BCTI_hydorxychlor"     "BCTI_evolution"        "wgm_scientists"       
## [61] "wgm_sciencegeneral"    "pew"                   "Q1...63"              
## [64] "education"             "comments"              "PROLIFIC_PID"
```


```r
# inspect
head(d) # you can also use View(d)
```

```
## # A tibble: 6 × 66
##   StartDate    EndDate Status IPAddress Progress Duration (in seconds…¹ Finished
##   <chr>        <chr>   <chr>  <chr>     <chr>    <chr>                  <chr>   
## 1 "Start Date" "End D… "Resp… "IP Addr… "Progre… "Duration (in seconds… "Finish…
## 2 "{\"ImportI… "{\"Im… "{\"I… "{\"Impo… "{\"Imp… "{\"ImportId\":\"dura… "{\"Imp…
## 3 "2024-03-05… "2024-… "IP A… "75.82.1… "100"    "139"                  "True"  
## 4 "2024-03-05… "2024-… "IP A… "68.252.… "100"    "152"                  "True"  
## 5 "2024-03-05… "2024-… "IP A… "204.15.… "100"    "172"                  "True"  
## 6 "2024-03-05… "2024-… "IP A… "98.233.… "100"    "141"                  "True"  
## # ℹ abbreviated name: ¹​`Duration (in seconds)`
## # ℹ 59 more variables: RecordedDate <chr>, ResponseId <chr>,
## #   RecipientLastName <chr>, RecipientFirstName <chr>, RecipientEmail <chr>,
## #   ExternalReference <chr>, LocationLatitude <chr>, LocationLongitude <chr>,
## #   DistributionChannel <chr>, UserLanguage <chr>, consent <chr>,
## #   attention_check <chr>, electrons_know <chr>, electrons_accept <chr>,
## #   antibiotics_know <chr>, antibiotics_accept <chr>, continents_know <chr>, …
```

```r
# delete first two rows
d <- d %>% 
  slice(3: nrow(.)) 
```

## Attention check

```r
# attention check
table(d$attention_check)
```

```
## 
##       3       4       5 Never 1 Often 6 
##       1       1       3     194       1
```

There are 6 failed attention checks. 


```r
# filter to only valid attention check responses
d <- d %>% filter(attention_check == "Never 1")
```

## Add demographics

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
         ) %>% 
  rename(age = Age)
```

## Add survey duration


```r
d <- d %>%
  mutate(
    # Parse the datetime
    StartDate = ymd_hms(StartDate, tz = "UTC"), 
    EndDate = ymd_hms(EndDate, tz = "UTC"),
    # Duration in minutes
    duration_mins = as.numeric(difftime(EndDate, StartDate, units = "mins"))
    )
```


## Clean and re-shape data


```r
# check all names and their order
names(d)
```

```
##  [1] "StartDate"                     "EndDate"                      
##  [3] "Status.x"                      "IPAddress"                    
##  [5] "Progress"                      "Duration (in seconds)"        
##  [7] "Finished"                      "RecordedDate"                 
##  [9] "ResponseId"                    "RecipientLastName"            
## [11] "RecipientFirstName"            "RecipientEmail"               
## [13] "ExternalReference"             "LocationLatitude"             
## [15] "LocationLongitude"             "DistributionChannel"          
## [17] "UserLanguage"                  "consent"                      
## [19] "attention_check"               "electrons_know"               
## [21] "electrons_accept"              "antibiotics_know"             
## [23] "antibiotics_accept"            "continents_know"              
## [25] "continents_accept"             "sex_know"                     
## [27] "sex_accept"                    "lasers_know"                  
## [29] "lasers_accept"                 "orbit_know"                   
## [31] "orbit_accept"                  "diamonds_know"                
## [33] "diamonds_accept"               "speed_know"                   
## [35] "speed_accept"                  "salt_know"                    
## [37] "salt_accept"                   "trees_know"                   
## [39] "trees_accept"                  "water_know"                   
## [41] "water_accept"                  "Q1...42"                      
## [43] "CMQ_1"                         "CMQ_2"                        
## [45] "CMQ_3"                         "CMQ_4"                        
## [47] "CMQ_5"                         "SICBS"                        
## [49] "BCTI_intro"                    "BCTI_apollo"                  
## [51] "BCTI_cancer"                   "BCTI_viruses"                 
## [53] "BCTI_climatechange"            "BCTI_flatearth"               
## [55] "BCTI_autism"                   "BCTI_polio"                   
## [57] "BCTI_alien"                    "BCTI_hydorxychlor"            
## [59] "BCTI_evolution"                "wgm_scientists"               
## [61] "wgm_sciencegeneral"            "pew"                          
## [63] "Q1...63"                       "education"                    
## [65] "comments"                      "PROLIFIC_PID"                 
## [67] "Submission id"                 "Status.y"                     
## [69] "Custom study tncs accepted at" "Started at"                   
## [71] "Completed at"                  "Reviewed at"                  
## [73] "Archived at"                   "Time taken"                   
## [75] "Completion code"               "Total approvals"              
## [77] "age"                           "Sex"                          
## [79] "Ethnicity simplified"          "Country of birth"             
## [81] "Country of residence"          "Nationality"                  
## [83] "Language"                      "Student status"               
## [85] "Employment status"             "gender"                       
## [87] "duration_mins"
```

Clean wide format data.


```r
# Function to extract numbers from a string for BCTI and SICBS
extract_numbers <- function(string_var) {
  as.numeric(gsub("\\D", "", string_var))
}

# test the function
# d %>% 
#   mutate(across(starts_with("BCTI"), ~extract_numbers(.), .names = "{.col}_num")) %>% 
#   select(BCTI_autism, BCTI_autism_num)

# clean and re-shape
d_wide <- d %>% 
  # add an easy to read participant identifier
  mutate(id = 1:nrow(.)) %>%
  # make some character variables numeric
  mutate(across(c(starts_with("BCTI"), starts_with("SICBS"), starts_with("wgm"), 
                  starts_with("pew")), ~extract_numbers(.)
  )
  ) %>% 
  mutate(across(starts_with("CMQ"), as.numeric)
  ) %>% 
  # add average conspiracy measures 
  rowwise() %>% 
  mutate(BCTI_avg = mean(c_across(starts_with("BCTI")
                                  ),
                         na.rm = TRUE
                         ),
         CMQ_avg = mean(c_across(starts_with("CMQ")
                                  ),
                         na.rm = TRUE
                         )
         ) %>% 
  ungroup() 
```

Make long format data.


```r
d_long <- d_wide %>% 
  # bring to long format
  pivot_longer(cols = electrons_know:water_accept, 
               names_to = "subject_question", values_to = "correct_accept") %>% 
  # separate conditions into CONVERGENCE_OUTCOME_STIMULUS
  separate_wider_delim(subject_question, "_", names = c("subject", "question")
                       ) %>%
  # necessary because each participant only saw ten out of 11 questions
  drop_na(correct_accept) %>% 
  pivot_wider(names_from = question, values_from = correct_accept) %>% 
# create better variable names
  rename(knowledge = know, 
         acceptance = accept) 
```

Add knowledge and acceptance by-participant averages to wide format data.


```r
# make a version with averages per participant
d_avg <- d_long %>% 
  # make numeric versions
  mutate(acceptance_num = ifelse(acceptance == "Yes", 1, 0), 
         knowledge_num = ifelse(knowledge == TRUE, 1, 0)
  ) %>% 
  group_by(id) %>% 
  # calculate by-participant averages
  summarize(avg_acceptance  = sum(acceptance_num)/n(), 
            avg_knowledge = sum(knowledge_num)/n()
  )

# make data frame with average particpant data and wide format
d_wide <- left_join(d_wide %>% 
                 select(-c(ends_with("know"), ends_with("accept"))), 
               d_avg, 
               by = "id")
```


## Export data


```r
# wide format
write_csv(d_wide, "data/cleaned_wide.csv")

# long format
write_csv(d_long, "data/cleaned_long.csv")
```
