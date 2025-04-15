---
title: "Data cleaning Study 2"
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
## [37] "salt_accept"           "water_know"            "water_accept"         
## [40] "electrons"             "antibiotics"           "continents"           
## [43] "sex"                   "lasers"                "orbit"                
## [46] "diamonds"              "speed"                 "salt"                 
## [49] "water"                 "Q1...50"               "CMQ_1"                
## [52] "CMQ_2"                 "CMQ_3"                 "CMQ_4"                
## [55] "CMQ_5"                 "SICBS"                 "BCTI_intro"           
## [58] "BCTI_apollo"           "BCTI_cancer"           "BCTI_viruses"         
## [61] "BCTI_climatechange"    "BCTI_flatearth"        "BCTI_autism"          
## [64] "BCTI_polio"            "BCTI_alien"            "BCTI_hydorxychlor"    
## [67] "BCTI_evolution"        "wgm_scientists"        "wgm_sciencegeneral"   
## [70] "pew"                   "Q1...71"               "education"            
## [73] "comments"              "PROLIFIC_PID"
```


```r
# inspect
head(d) # you can also use View(d)
```

```
## # A tibble: 6 × 74
##   StartDate    EndDate Status IPAddress Progress Duration (in seconds…¹ Finished
##   <chr>        <chr>   <chr>  <chr>     <chr>    <chr>                  <chr>   
## 1 "Start Date" "End D… "Resp… "IP Addr… "Progre… "Duration (in seconds… "Finish…
## 2 "{\"ImportI… "{\"Im… "{\"I… "{\"Impo… "{\"Imp… "{\"ImportId\":\"dura… "{\"Imp…
## 3 "2024-04-03… "2024-… "IP A… "38.178.… "100"    "229"                  "True"  
## 4 "2024-04-03… "2024-… "IP A… "173.91.… "100"    "163"                  "True"  
## 5 "2024-04-03… "2024-… "IP A… "168.220… "100"    "330"                  "True"  
## 6 "2024-04-03… "2024-… "IP A… "24.127.… "100"    "267"                  "True"  
## # ℹ abbreviated name: ¹​`Duration (in seconds)`
## # ℹ 67 more variables: RecordedDate <chr>, ResponseId <chr>,
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
##       2       3       4       5 Never 1 
##       5       2       1       3     190
```

There are 11 failed attention checks. 


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
## [37] "salt_accept"                   "water_know"                   
## [39] "water_accept"                  "electrons"                    
## [41] "antibiotics"                   "continents"                   
## [43] "sex"                           "lasers"                       
## [45] "orbit"                         "diamonds"                     
## [47] "speed"                         "salt"                         
## [49] "water"                         "Q1...50"                      
## [51] "CMQ_1"                         "CMQ_2"                        
## [53] "CMQ_3"                         "CMQ_4"                        
## [55] "CMQ_5"                         "SICBS"                        
## [57] "BCTI_intro"                    "BCTI_apollo"                  
## [59] "BCTI_cancer"                   "BCTI_viruses"                 
## [61] "BCTI_climatechange"            "BCTI_flatearth"               
## [63] "BCTI_autism"                   "BCTI_polio"                   
## [65] "BCTI_alien"                    "BCTI_hydorxychlor"            
## [67] "BCTI_evolution"                "wgm_scientists"               
## [69] "wgm_sciencegeneral"            "pew"                          
## [71] "Q1...71"                       "education"                    
## [73] "comments"                      "PROLIFIC_PID"                 
## [75] "Submission id"                 "Status.y"                     
## [77] "Custom study tncs accepted at" "Started at"                   
## [79] "Completed at"                  "Reviewed at"                  
## [81] "Archived at"                   "Time taken"                   
## [83] "Completion code"               "Total approvals"              
## [85] "age"                           "Sex"                          
## [87] "Ethnicity simplified"          "Country of birth"             
## [89] "Country of residence"          "Nationality"                  
## [91] "Language"                      "Student status"               
## [93] "Employment status"             "gender"                       
## [95] "duration_mins"
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

## Justifications

### Extract justifications


```r
# questions <- c("electrons", "antibiotics", "continents", "sex", "lasers", "orbit", "diamonds", "speed", "salt", "water")
# 
# justifications <- exp2_wide %>% 
#   select(id, all_of(questions)) %>% 
#   pivot_longer(all_of(questions), 
#                names_to = "question", 
#                values_to = "answer") %>% 
#   drop_na(answer) %>% 
#   arrange(id)
# 
# write_csv(justifications, "data/justifications.csv")
```

### Clean justifications

Note that we had used the following coding scheme (where 4 would be the top-category for 5 and 6, i.e. 4 has never been coded):

1	- no justification provided / no clear justification	
2	- mistake, they actually accept the consensus	
3	- not convinced by the explanation provided	
4	- motivated rejection of the belief	
5	-- do to personal convictions	
6	-- for religious reasons	


```r
justifications <- read_csv("data/justifications_coded.csv")
```

```
## Rows: 35 Columns: 4
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): question, answer
## dbl (2): id, category
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# Research questions
justifications <- justifications %>%
  mutate(category = case_when(category == 1 ~ "No justification",
                              category  == 2 ~  "Mistake",
                              category  == 3 ~ "Not convinced",
                              category  == 5 ~ "Personal convictions",
                              category  == 6 ~ "Religious Beliefs",
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
