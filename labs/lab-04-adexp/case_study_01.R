#' case_study_01.R
#' 
#' What this file does:
#' - Complete the code to answer questions related to Case Study 1
#' 
#' Need to install a new package?
#' 
#' install.package("PACKAGE_NAME")
#' 

# --- Libraries --- #
library(readr)
library(dplyr)
library(infer)

# --- Exercises --- #

## Exercise 1
## HINT: how to read in a csv file?
## 
ad_experiment <- 
    YOUR_CODE("data/display_ads_student.csv") %>%
    # the second line formats the "converted" variable for 
    # later analysis 
    mutate(converted= as.numeric(converted)) 

## Exercise 2
## HINT: you want to select the variable which
##       has the user identifiers stored in it
ad_experiment %>%
    YOUR_CODE(YOUR_VARIABLE) %>%
    n_distinct()

## Exercise 3
## HINT: which variable contains information on which 
##       treatment condition the user is in?
ad_experiment %>% 
    group_by(YOUR_VARIABLE) %>%
    count()

## Exercise 4 A
## HINTS:
##    - you want to group by the variable that indicates 
##      treatment or control group
ad_experiment %>%
    group_by(YOUR_VARIABLE) %>%
    count()

## Exercise 4 B
## HINTS:
##    - you want to group by the variable that indicates 
##      treatment or control group
##    - you want to count the rows of the entire dataset
ad_experiment %>%
    group_by(YOUR_VARIABLE) %>%
    count() %>%
    ungroup() %>%
    mutate(n_users = nrow(YOUR_DATASET),
           pct_condition = 100 * (n / n_users))

## Exercise 5
## HINTS:
##    - you want to compute statistics at the treatment group level
##    - you want to sum up the number of conversions
ad_experiment %>%
    YOUR_CODE(YOUR_VARIABLE1) %>%
    summarise(converted = sum(YOUR_VARIABLE2))

## Exercise 6
## HINTS:
##    - you want to compute statistics at the treatment group level
##    - you want to sum up the number of conversions
##    - you want to compute the percentage of users that converted
##    - you only want to keep the treatment group and the percentage variable
ad_experiment %>%
    YOUR_CODE(YOUR_VARIABLE1) %>%
    summarise(converted = sum(YOUR_VARIABLE2),
              n_users = n(),
              pct_converted = 100 * (YOUR_VARIABLE3 / YOUR_VARIABLE4)
              )  %>%
    select(YOUR_VARIABLE5, YOUR_VARIABLE6)

## Exercise 7
## 
t_test(YOUR_DATASET, 
       YOUR_OUTCOME_VARIABLE ~ YOUR_TREATMENT,
       alternative = "greater",
       order = c("ad", "psa")
    )     

## Exercise 8
# Step 1: run the regression
mod <- 
    lm(YOUR_OUTCOME_VARIABLE ~ YOUR_TREATMENT, 
       data = YOUR_DATASET
       )
# Step 2: Print the results to screen
summary(mod)
