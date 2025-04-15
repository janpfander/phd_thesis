---
title: "Data cleaning experiment 5"
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
## [40] "PROLIFIC_PID"          "condition"             "counter"
```



## Attention check

```
## [1] "63468b3bd07050877a6beb11" "i pay attention"         
## [3] "I pay attention"          "I pay attention."
```

There is one clearly failed attention check that we remove. 



## Re-shape data


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
