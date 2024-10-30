#' case_study_02.R
#' 
#' What this file does:
#' - Complete the code to answer questions related to Case Study 2
#' 
#' Need to install a new package?
#' 
#' install.package("PACKAGE_NAME")
#' 

# --- Libraries --- #
library(readr)
library(dplyr)
library(ggplot2)
library(infer)
library(lubridate)
library(tidyr)

# --- Exercises --- #

## Exercise 3
## HINT: how to read in a csv file?
##
paid_search <- 
    YOUR_CODE("data/paid_search_student.csv") 

## Exercise 4 
# Run this code before continuing: We compute weekly revenue for each DMA
weekly_revenue <-
    paid_search %>%
    mutate(cal_week = week(date)) %>%
    group_by(cal_week, dma) %>%
    summarise(weekly_revenue = sum(revenue),
              # we want to keep track of two extra variables
              # in our summarised data - this two lines make this happen
              treatment = max(treatment),
              treatment_period = max(treatment_period)
    )  %>%
    # so we don't have any nasty group structures hidden in the data
    ungroup()

## Exercise 4A
## HINT:
##   - you want to compute average weekly revenue (3) by treatment(1) and week(2) 
avg_weekly_revenue <-
    weekly_revenue %>%
    group_by(YOUR_VARIABLE1, YOURVARIABLE2) %>%
    summarise(avg_revenue = mean(YOURVARIABLE3))

## Exercise 4B
## HINT:
##    - the x axis should record which week of the year
##    - the y axis should record revenue
#     - there should be a line for both treatment and control that have different colors
avg_weekly_revenue %>%
    ggplot(aes(x = YOUR_VARIABLE1,
               y = YOUR_VARIABLE2,
               color = factor(YOUR_VARIABLE3)
    )
    ) +
    geom_line() +
    # the line below ads a black line when some DMAS paid search is turned off
    geom_vline(xintercept = week(as_date("2012-05-22"))) +
    # Decorate your plot with meaningful labels:
    ggtitle("PLOT TITLE") +
    xlab("NAME OF THE X AXIS") +
    ylab("NAME OF THE Y AXIS") +
    theme_bw()

## Exercise 5A
## HINT: What variable stores when the experiment of turning off
#        advertising begins?
post_treatment <- 
    weekly_revenue %>%
    filter(YOUR_VARIABLE == 1)

## Exercise 5B
YOUR_CODE(YOUR_DATASET,
    YOUR_OUTCOME_VARIABLE ~ YOUR_TREATMENT,
       alternative = "greater",
       order = c("0", "1")
    )

## Exercise 5C
# Estimate Regression
reg_mod <- 
    lm(YOUR_OUTCOME_VARIABLE ~ YOUR_TREATMENT, 
        data = YOUR_DATASET
        )
# Print results to screen
summary(reg_mod)

## Exercise 6A
treat_table <-
    weekly_revenue %>%
    group_by(treatment_period, treatment) %>%
    summarise(revenue = mean(YOUR_OUTCOME_VARIABLE)) %>% 
    ungroup() %>%
    pivot_wider(names_from = treatment_period,
                values_from = revenue
    ) 

## Exercise 6B
## Run this code 
first_diff <-
    treat_table %>%
    mutate(diff = `1` - `0`)

## Exercise 6C
# Run this code 
second_diff <-
    first_diff %>%
    mutate(did = diff - lag(diff))

## Exercise 7 
## HINT:
## - if i want to run the regression
##      y = a + b + a*b
##   the code to do this in R is
## 
##  lm(y ~ a*b, 
##     data = YOUR_DATASET
#      )
did_est <- 
    lm(YOUR_EQUATION, 
       data = weekly_revenue
       )

# Print result to screen
summary(did_est)

## Exercise 10
## HINT:
## - if i want to run the regression
##      log(y) = a + b + a*b
##   the code to do this in R is
## 
##  lm(log(y) ~ a*b, 
##     data = YOUR_DATASET
#      )
did_est2 <- 
    lm(YOUR_EQUATION, 
       data = weekly_revenue
       )

# Print result to screen
summary(did_est2)
