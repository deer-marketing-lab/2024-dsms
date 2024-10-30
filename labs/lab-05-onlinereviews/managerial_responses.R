#' managerial_responses.R
#' 
#' What this file does:
#' - Complete the code to answer questions related to the Hands On
#'   exercise from Lab 5 of Digital and Social Media Strategies
#' 
#' Need to install a new package?
#' 
#' install.package("PACKAGE_NAME")
#' 
#' 

# --- Libraries --- #
library(haven) # read .dta data
library(dplyr) # manipulate data
library(tidyr) # reshape datasets 

# --- Task 3 --- #
# A
hotels_orm <-
    read_stata("data/responses.dta") %>%
    filter(xplatform_dd_obs == 1) %>%
    select(hotel_id, year, stars, after, ta_dummy, 
           first_response, cum_avg_stars_lag, 
           log_count_reviews_lag, t, ash_interval
           )

# B
# HINT: Replace TREATMENT_DUMMY, BEFORE_AFTER_DUMMY and OUTCOME_VARIBLE
# with the correct variables from the data set
did_table <- 
    hotels_orm %>%
    group_by(TREATMENT_DUMMY, BEFORE_AFTER_DUMMY) %>%
    summarize(stars = mean(OUTCOME_VARIBLE)) %>%
    pivot_wider(names_from = ta_dummy, values_from = stars) %>%
    rename(expedia = `0`, trip_advisor = `1`) %>%
    ungroup()

# C 
# HINT: Replace YOUR_CODE_HERE with code to estimate the first differences
#       for each platform
#
# HINT 2: The lag(YOUR_VARIABLE) will get the value of the previous row
first_diff <-
    did_table %>%
    mutate(expedia_diff = YOUR_CODE_HERE,
           ta_diff = YOUR_CODE_HERE
           ) %>%
    select(expedia_diff, ta_diff) %>%
    na.omit()

# D
# HINT: Compute the differences between the columns you constructed in C
second_diff <-
    first_diff %>%
    mutate(did_estimate = YOUR_CODE_HERE) %>%
    select(did_estimate)

# E
# HINT: Replace YOUR_DATASET with the name of the dataset 
# you want to see the values from
print(YOUR_DATASET)

# --- Task 4 --- #
# A
# HINT: Write the formula for the regression you want to run
model_1 <- 
    lm(YOUR_REGRESSION_FORMULA,
                 data = hotels_orm)

summary(model_1)

# B
# HINT 1: Replace YOUR_CONDITION with the expression to drop observations 
#        where ash_dip takes the value '0'
# HINT 2: Use the same regression formula as above

ash_dip <- 
    hotels_orm %>% 
    filter(YOUR_CONDITION)

model_2 <- 
    lm(YOUR_REGRESSION_FORMULA,
       data = ash_dip)

summary(model_2)
