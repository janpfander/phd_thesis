---
title: "Data cleaning Study 3"
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
d <- read_csv("data/qualtrics.csv")
names(d)
```

```
##  [1] "StartDate"               "EndDate"                
##  [3] "Status"                  "IPAddress"              
##  [5] "Progress"                "Duration (in seconds)"  
##  [7] "Finished"                "RecordedDate"           
##  [9] "ResponseId"              "RecipientLastName"      
## [11] "RecipientFirstName"      "RecipientEmail"         
## [13] "ExternalReference"       "LocationLatitude"       
## [15] "LocationLongitude"       "DistributionChannel"    
## [17] "UserLanguage"            "consent"                
## [19] "attention_check"         "electrons_know"         
## [21] "electrons_accept"        "antibiotics_know"       
## [23] "antibiotics_accept"      "continents_know"        
## [25] "continents_accept"       "sex_know"               
## [27] "sex_accept"              "lasers_know"            
## [29] "lasers_accept"           "orbit_know"             
## [31] "orbit_accept"            "diamonds_know"          
## [33] "diamonds_accept"         "speed_know"             
## [35] "speed_accept"            "salt_know"              
## [37] "salt_accept"             "water_know"             
## [39] "water_accept"            "electrons"              
## [41] "electrons_confirm"       "antibiotics"            
## [43] "antibiotics_confirm"     "continents"             
## [45] "continents_confirm"      "sex"                    
## [47] "sex_confirm"             "lasers"                 
## [49] "lasers_confirm"          "orbit"                  
## [51] "orbit_confirm"           "diamonds"               
## [53] "diamonds_confirm"        "speed"                  
## [55] "speed_confirm"           "salt"                   
## [57] "salt_confirm"            "water"                  
## [59] "water_confirm"           "Q1...60"                
## [61] "CMQ_1"                   "CMQ_2"                  
## [63] "CMQ_3"                   "CMQ_4"                  
## [65] "CMQ_5"                   "SICBS"                  
## [67] "BCTI_intro"              "BCTI_apollo"            
## [69] "BCTI_cancer"             "BCTI_viruses"           
## [71] "BCTI_climatechange"      "BCTI_flatearth"         
## [73] "BCTI_autism"             "BCTI_polio"             
## [75] "BCTI_alien"              "BCTI_hydorxychlor"      
## [77] "BCTI_evolution"          "wgm_scientists"         
## [79] "wgm_sciencegeneral"      "pew"                    
## [81] "reason_agreement"        "reason_agreement_3_TEXT"
## [83] "reason_followup"         "Q1...84"                
## [85] "education"               "comments"               
## [87] "PROLIFIC_PID"
```


```r
# inspect
head(d) # you can also use View(d)
```

```
## # A tibble: 6 × 87
##   StartDate    EndDate Status IPAddress Progress Duration (in seconds…¹ Finished
##   <chr>        <chr>   <chr>  <chr>     <chr>    <chr>                  <chr>   
## 1 "Start Date" "End D… "Resp… "IP Addr… "Progre… "Duration (in seconds… "Finish…
## 2 "{\"ImportI… "{\"Im… "{\"I… "{\"Impo… "{\"Imp… "{\"ImportId\":\"dura… "{\"Imp…
## 3 "2024-04-22… "2024-… "IP A… "71.47.1… "100"    "132"                  "True"  
## 4 "2024-04-22… "2024-… "IP A… "173.216… "100"    "400"                  "True"  
## 5 "2024-04-22… "2024-… "IP A… "50.247.… "100"    "207"                  "True"  
## 6 "2024-04-22… "2024-… "IP A… "98.244.… "100"    "206"                  "True"  
## # ℹ abbreviated name: ¹​`Duration (in seconds)`
## # ℹ 80 more variables: RecordedDate <chr>, ResponseId <chr>,
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
## Never 1 
##     200
```

There are % failed attention checks. 


```r
# filter to only valid attention check responses
d <- d %>% filter(attention_check == "Never 1")
```

## Recode demographics


```r
prolific_demographics <- read_csv("data/prolific_demographics.csv")

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
##   [1] "StartDate"                     "EndDate"                      
##   [3] "Status.x"                      "IPAddress"                    
##   [5] "Progress"                      "Duration (in seconds)"        
##   [7] "Finished"                      "RecordedDate"                 
##   [9] "ResponseId"                    "RecipientLastName"            
##  [11] "RecipientFirstName"            "RecipientEmail"               
##  [13] "ExternalReference"             "LocationLatitude"             
##  [15] "LocationLongitude"             "DistributionChannel"          
##  [17] "UserLanguage"                  "consent"                      
##  [19] "attention_check"               "electrons_know"               
##  [21] "electrons_accept"              "antibiotics_know"             
##  [23] "antibiotics_accept"            "continents_know"              
##  [25] "continents_accept"             "sex_know"                     
##  [27] "sex_accept"                    "lasers_know"                  
##  [29] "lasers_accept"                 "orbit_know"                   
##  [31] "orbit_accept"                  "diamonds_know"                
##  [33] "diamonds_accept"               "speed_know"                   
##  [35] "speed_accept"                  "salt_know"                    
##  [37] "salt_accept"                   "water_know"                   
##  [39] "water_accept"                  "electrons"                    
##  [41] "electrons_confirm"             "antibiotics"                  
##  [43] "antibiotics_confirm"           "continents"                   
##  [45] "continents_confirm"            "sex"                          
##  [47] "sex_confirm"                   "lasers"                       
##  [49] "lasers_confirm"                "orbit"                        
##  [51] "orbit_confirm"                 "diamonds"                     
##  [53] "diamonds_confirm"              "speed"                        
##  [55] "speed_confirm"                 "salt"                         
##  [57] "salt_confirm"                  "water"                        
##  [59] "water_confirm"                 "Q1...60"                      
##  [61] "CMQ_1"                         "CMQ_2"                        
##  [63] "CMQ_3"                         "CMQ_4"                        
##  [65] "CMQ_5"                         "SICBS"                        
##  [67] "BCTI_intro"                    "BCTI_apollo"                  
##  [69] "BCTI_cancer"                   "BCTI_viruses"                 
##  [71] "BCTI_climatechange"            "BCTI_flatearth"               
##  [73] "BCTI_autism"                   "BCTI_polio"                   
##  [75] "BCTI_alien"                    "BCTI_hydorxychlor"            
##  [77] "BCTI_evolution"                "wgm_scientists"               
##  [79] "wgm_sciencegeneral"            "pew"                          
##  [81] "reason_agreement"              "reason_agreement_3_TEXT"      
##  [83] "reason_followup"               "Q1...84"                      
##  [85] "education"                     "comments"                     
##  [87] "PROLIFIC_PID"                  "Submission id"                
##  [89] "Status.y"                      "Custom study tncs accepted at"
##  [91] "Started at"                    "Completed at"                 
##  [93] "Reviewed at"                   "Archived at"                  
##  [95] "Time taken"                    "Completion code"              
##  [97] "Total approvals"               "Covid-19 vaccination"         
##  [99] "Covid-19 vaccine opinions"     "Vaccine opinions 2"           
## [101] "age"                           "Sex"                          
## [103] "Ethnicity simplified"          "Country of birth"             
## [105] "Country of residence"          "Nationality"                  
## [107] "Language"                      "Student status"               
## [109] "Employment status"             "gender"                       
## [111] "duration_mins"
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

transform_text <- function(x) {
  ifelse(str_detect(x, "^I agree"), "Yes", "No")
}

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
  # clean the text for correction 
  mutate_at(vars(ends_with("_confirm")), ~ transform_text(.))%>%
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

Code the reasons for why people accept the scientific consensus:


```r
d_wide <- d_wide %>%
  mutate(reason_agreement = case_when(
    grepl("Other:", reason_agreement) ~ "other",
    grepl("independently", reason_agreement) ~ "independent verification",
    grepl("trust scientists", reason_agreement) ~ "trust in scientists",
    TRUE ~ NA_character_  # This handles any NAs
  )) %>% 
  rename(other_reason_agreement = reason_agreement_3_TEXT)
```

Make long format data.

We use the initial acceptance question and the confirmation question (only participants who did not accept the consensus for the intial question answered the confirmation question) to make a final acceptance question. This final acceptance will take on the initial answer in case of initial acceptance, and the answer of the confirmation question in case of initial non-acceptance. The final question will simply be named `acceptance`. 


```r
d_long <- d_wide %>% 
  # bring to long format
  pivot_longer(cols = c(ends_with("know"), ends_with("accept"), ends_with("confirm")), 
               names_to = "subject_question", values_to = "answer") %>% 
  # separate subject (e.g. water) and question (e.g. acceptance)
  separate_wider_delim(subject_question, "_", names = c("subject", "question")
                       ) %>%
  pivot_wider(names_from = question, values_from = answer) %>% 
# create better variable names
  rename(knowledge = know, 
         acceptance_initial = accept, 
         acceptance_confirmation = confirm) %>% 
  # make a final acceptance variable
  mutate(acceptance = ifelse(is.na(acceptance_confirmation), acceptance_initial, acceptance_confirmation))
```

Add knowledge and acceptance by-participant averages to wide format data.


```r
# make a version with averages per participant
d_avg <- d_long %>% 
  # make numeric versions
  mutate(acceptance_num = ifelse(acceptance == "Yes", 1, 0), 
         knowledge_num = ifelse(knowledge == TRUE, 1, 0), 
         acceptance_initial_num = ifelse(acceptance_initial == "Yes", 1, 0)
  ) %>% 
  group_by(id) %>% 
  # calculate by-participant averages
  summarize(avg_acceptance  = sum(acceptance_num)/n(), 
            avg_knowledge = sum(knowledge_num)/n(),
            avg_acceptance_initial  = sum(acceptance_initial_num)/n()
  )

# make data frame with average participant data and wide format
d_wide <- left_join(d_wide, d_avg, 
               by = "id")
```

## Check NAs


```r
d_wide %>% 
  summarize(across(c("wgm_scientists", "wgm_sciencegeneral", "pew", "reason_agreement", "BCTI_avg", "CMQ_avg", "SICBS", "avg_acceptance", "avg_knowledge"), 
                   ~sum(is.na(.x))
                   )
            )
```

```
## # A tibble: 1 × 9
##   wgm_scientists wgm_sciencegeneral   pew reason_agreement BCTI_avg CMQ_avg
##            <int>              <int> <int>            <int>    <int>   <int>
## 1             60                 60    60               78       62      62
## # ℹ 3 more variables: SICBS <int>, avg_acceptance <int>, avg_knowledge <int>
```

It's technically not possible to have NA's for trust in science and conspiracy questions, because we used forced responses on them. Checking the qualtrics, we know that we accidentally randomized people into two of three outcome measures: trust in science, conspiracy or reasons for accepting consensus. This leaves us with reduced sample sizes for all analyses concerning these outcomes (XX for trust in science question, XX for conspiracy measures, XX for accepting consensus). 


```r
d_wide %>% 
  summarize(across(c("wgm_scientists", "wgm_sciencegeneral", "pew", "reason_agreement", "BCTI_avg", "CMQ_avg", "SICBS"), 
                   ~sum(!is.na(.x))
                   )
            ) %>% 
  pivot_longer(everything(), 
               names_to = "outcome", 
               values_to = "n_valid")
```

```
## # A tibble: 7 × 2
##   outcome            n_valid
##   <chr>                <int>
## 1 wgm_scientists         140
## 2 wgm_sciencegeneral     140
## 3 pew                    140
## 4 reason_agreement       122
## 5 BCTI_avg               138
## 6 CMQ_avg                138
## 7 SICBS                  138
```

## Clean justifications



```r
justifications <- read_csv("data/justifications_coded.csv")
```

```
## Rows: 74 Columns: 6
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Subject, Answer
## dbl (4): id, Type, blind_coding, final_coding
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# Research questions
justifications <- justifications %>%
  mutate(category = case_when(final_coding == 1 ~ "No justification",
                              final_coding == 2 ~  "Mistake",
                              final_coding == 3 ~ "Not convinced",
                              final_coding == 4 ~ "Personal convictions",
                              final_coding == 5 ~ "Religious Beliefs",
                              TRUE ~ NA_character_
                              )
         )

write_csv(justifications, "data/justifications_clean.csv")
```


## Export data


```r
# wide format
write_csv(d_wide, "data/cleaned_wide.csv")

# long format
write_csv(d_long, "data/cleaned_long.csv")
```

