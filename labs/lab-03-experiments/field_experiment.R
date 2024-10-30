# field_experiment.R
# 
# What this file does:
#  - Script to work on solutions to Lab 3: Field Experiments
#
# Note: Data Loading step
#       assumes you are working in an R Studio
#       project with the folder "lab-03-experiments"
#       as the base directory.
#
#       You need to install additional packages to 
#       your system to complete this tutorial. You can
#       install these by running the following lines of code
#       (uncomment, i.e. remove the '#'). 
#       
#        You only need to complete this installation step once.
#        If you have already installed these packages in previous labs,
#        you do not need to complete this step.
#  
#        Uncomment the lines below: 
# 
# to_install <- c("readr", "dplyr", "tidyr", 
#                 "ggplot2", "infer"
#                 )    
#
# install.packages(to_install)

# ---- Libraries ---- # 
# You will need these libraries to answer the questions of the assignment.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(infer)

# ---- Read in Data --- #

cookie_cats <- read_csv("data/cookie_cats.csv")

# --- Question 4 --- #
# Put your code to answer Q4 below.
# HINTS: your code needs to use the following elements 
# - A function to find the unique number of entries in a column, n_distinct()
# - A column of data that you select from a larger dataset using the select() command
# - The pipe operator %>%, potentially more than once
#
# Your job is to assemble these Hints into the correct sequence

# --- Question 5 --- #
# Put your code to answer Q5 below.
# First: How many  did not complete any level?
# Your code will use the following snippets:
# - The function filter() to select rows
# - The expression sum_gamerounds == 0 to find rows where sum_gamerounds is equal to zero
# - A step that passes the data to the filter command
# - The pipe operator %>%, potentially more than once
# - The count() function to count the number of observations in the dataset
# 
# Your job is to assemble these snippets into the correct sequence


# Second: Drop these consumers
# You can drop rows of data by keeping rows where sum_gamerounds is not equal to zero.
# Your code will use the following snippets:
# - The function filter() to select rows
# - The expression sum_gamerounds != 0 to find rows where sum_gamerounds is not equal to zero
# - A step that passes the data to the filter command
# - The pipe operator %>%, potentially more than once
# - An assignment operator, <- , that stores the output of your work to a new data set called "cookie_cats_no_zero"
# 
# Your job is to assemble these snippets into the correct sequence


# --- Question 6 --- #
# Put your code to answer Q6 below.
# First, find the mean number of rounds complete
# Use the following snippets:
# - The pipe operator %>%, potentially more than once
# - The summarise() command to tell R you want to compute a summary statistic
# - The mean(YOUR_VARIABLE) function to compute the mean of a variable called YOUR_VARIABLE
# - A step that passes the data to the summarise command
# 
# Your job is to assemble these snippets into the correct sequence

# Second, remove any players playing more than 10,000 levels
# Your code will use the following snippets:
# - The function filter() to select rows
# - The expression sum_gamerounds > 10,000 to find rows where sum_gamerounds is greater than 10,000
# - A step that passes the data to the filter command
# - The pipe operator %>%, potentially more than once
# - The count() function to count the number of observations in the dataset
# - An assignment operator, <- , that stores the output of your work to a new data set called "cookie_cats_analysis"
# 
# Your job is to assemble these snippets into the correct sequence


# --- Question 7 --- #
# Put your code to answer Q7 below.
# First: 1 day retention 
# Run the code below:
cookie_cats_analysis %>%
    group_by(version) %>%
    summarise(
        # the number of users in each treatment
        n_users = n(),
        # number of retained users
        n_retained = sum(retention_1),
        # number of not_retained users
        n_not_retained = n_users - n_retained,
        # the proportion
        prop = sum(retention_1) / n_users
        )

# Second: 7 day retention
# Hint: Adapt the code above

# --- Question 8 --- #
# Put your code to answer Q8 below.
# Use the following snippets:
# - The pipe operator %>%, potentially more than once
# - The group_by(YOUR_TREATMENT) command that groups the data based on a column that 
#   specifies which treatment a user is in
# - The mean(YOUR_VARIABLE) function that computes means for the column YOUR_VARIABLE
# - A step that passes the data to the group_by() command
# 
# Your job is to assemble these snippets into the correct sequence


# --- Question 9 --- #
# Put your code to answer Q9 below.
# Hint 1: YOUR_VARIABLE1 should be the outcome variable
# Hint 2: YOUR_VARIABLE2 should be the variable that identifies which
#         condition a user is in

cookie_cats_analysis %>%
    ggplot() +
    geom_boxplot(aes(x = YOUR_VARIABLE1, 
                     fill = as.factor(YOUR_VARIABLE2)
                     )
        ) +
    xlab("YOUR AXIS LABEL") +
    # everything below makes the plot look like the 
    # suggested plot in the assignment questions
    # ignore the details 
    coord_flip() +
    scale_x_continuous(limits = quantile(cookie_cats$sum_gamerounds, c(0.01, 0.95))) +
    scale_fill_discrete(name = "Treatment Group", labels = c("Control", "Treatment")) +
    theme_bw() +
    theme(text = element_text(size = 15),
          axis.text.x=element_blank()
    ) +
    theme(legend.position = "bottom")


# --- Question 11 --- #
# Put your code to answer Q11 below.
prop_test(YOUR_DATA, 
          YOUR_OUTCOME ~ YOUR_TREATMENT,
          order = c("gate_30", "gate_40")
          )

# --- Question 12 --- #
# Put your code to answer Q12 below.
prop_test(YOUR_DATA, 
          YOUR_OUTCOME ~ YOUR_TREATMENT,
          alternative  = YOUR_CHOICE, # choose between "two-sided", "greater", "less".
                                     # keep the quotations!                 
          order = c("gate_30", "gate_40")
          )

# --- Question 13 --- #
# Put your code to answer Q13 below.
t_test(YOUR_DATA, 
          YOUR_OUTCOME ~ YOUR_TREATMENT,
          order = c("gate_30", "gate_40")
          )

# --- Question 14 --- #
# Put your code to answer Q14 below.
reg_output <- lm(log(YOUR_OUTCOME) ~ YOUR_TREATMENT,
                 data = YOUR_DATA
                    )
summary(reg_output)