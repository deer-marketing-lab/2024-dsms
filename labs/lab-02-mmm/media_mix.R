# media_mix.R
# 
# What this file does:
#  - Script to work on solutions to Lab 2: Media Mix Modelling
#
# Note: Data Loading step on Line 36 
#       assumes you are working in an R Studio
#       project with the folder "lab-02-mmm"
#       as the base directory. Instructors will
#       show you how to set this up during the 
#       Computer Lab
#
#       You need to install additional packages to 
#       your system to complete this tutorial. You can
#       install these by running the following lines of code
#       (uncomment, i.e. remove the '#'). 
#       
#        You only need to complete this installation step once.
#  
#        Uncomment the lines below: 
# 
# to_install <- c("readr", "dplyr", "marginaleffects", 
#                 "ggplot2", "skimr"
#                 )    
#
# install.packages(to_install)

library(readr)
library(dplyr)
library(marginaleffects)
library(ggplot2)
library(skimr)

# --- Task 2 --- #
# A
df <- 
    read_csv("data/mediamix_data.csv")

# B 
YOUR_CODE

# C
skim(YOUR_NAME_OF_DATA)

# --- Task 3 --- #
# B
simple_mmm <- 
    lm(sales ~ tv_spend, data = df)

summary(simple_mmm)

# D
simple_mmm_seas <- 
    lm(sales ~ tv_spend + as.factor(month), data = df) 
summary(simple_mmm_seas)

# --- Task 4 --- #
# C
source("create_ad_stocks.R")
df2 <- compute_ad_stocks(df)

# D
stock_mmm <-
    lm(sales ~ tv_stock + as.factor(month), data = df2) 

# E
summary(stock_mmm)

# --- Task 5 --- #
# B
stock_mmm_sat <-
    lm(sales ~ tv_stock + I(tv_stock^2) + as.factor(month), data = df2) 

# C
summary(stock_mmm_sat)

# D
plot_predictions(stock_mmm_sat, condition = "tv_stock")
