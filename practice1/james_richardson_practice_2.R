# Date: 22/01/2019 
# Name: James Richardson
# Student Number: 18057447
# Data: wildebeest.csv

# Data Structure
# --------------
# park – location of national park
# sex – male or female
# horn – horn length recorded in centimeters
# feeding – time spent feeding over survey period (minutes)

# Investigate if there is a difference in horn length between sexes of wildebeest.

# Marking guidance
# ------------------
# 1 mark for the hypothesis; 
# 2 marks for preliminary data exploration and visualisation, 
# 2 marks for testing the assumptions and reporting, 
# 2 marks for selection and carrying out correct test, 
# 3 marks reporting your results and presenting the data.

# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/practice1")
# list files in working directory
list.files() 

# Load required libaries
library(tidyverse)
library(ggplot2)

# -----------------
# Null Hypothesis: 
# -----------------
# There is no significant difference in horn length between male and female wildebeest

# 
# Load the wildebeest data
wildebeest <- read_csv("wildebeest2.csv")
# Parsed with column specification:
#   cols(
#     park = col_character(),
#     sex = col_character(),
#     horn = col_double(),
#     feeding = col_double()
#   )

# Examine the data
names(wildebeest) 
str(wildebeest)

# check the mean of horn length for females and males
tapply(wildebeest$horn, wildebeest$sex, FUN=mean)
# female     male 
# 39.84217 59.52892 
tapply(wildebeest$horn, wildebeest$sex, FUN=sd)
# female     male 
# 5.221464 4.480868 

# Look at box plot of male vs female
ggplot(wildebeest, aes(x=sex, y=horn, group=sex)) + 
  geom_boxplot() +
  xlab("Male vs Female") + ylab("Horn Length (cm)")

# Examine distribution of horn length
ggplot(wildebeest, aes(x = horn, fill=sex)) + 
  geom_bar(binwidth = 2)+ 
  facet_wrap(~sex, ncol=1) +
  xlab("Horn Length (cm)") + ylab("Number of Wildebeest")

# Choose which test to do:
# -------------------------

# Have to one variable with two categories (male + female) and one continuous variable (horn length)

# Test for normality 
shapiro.test(wildebeest$horn)
# W = 0.94774, p-value = 8.077e-06 <- Significantly different from normal

# Test for homogeneity of variances
bartlett.test(wildebeest$horn, wildebeest$sex)
# Bartlett's K-squared = 1.8995, df = 1, p-value = 0.1681 <- # NO significant difference in variance

# Not normal, so use Wilcoxon Rank test
# -------------------------------------
wilcox.test(wildebeest$horn~wildebeest$sex)
# W = 5, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

# ------------------
# RESULTS STATEMENT: 
# ------------------
# There is a significant difference in the horn length between
# male (mean±SD, 59.52892±4.480868) and female (39.84217±5.22146) wildebeest (p-value < 0.001).


# Create PDF of boxplot of males vs females
pdf("Wildebeest_horn_length.pdf", width = 8, height = 6) 
ggplot(wildebeest, aes(x=sex, y=horn, group=sex)) + 
  geom_boxplot() +
  ylab("Horn Length (cm)") + xlab("") +
  labs(title = "Comparing Horn Length of Wildebeest", subtitle = "Males vs Females") +
  stat_summary(aes(label=..y..), fun.y=mean, geom="text", size=4, vjust=-0.9)
dev.off() # close plotting element

# --------------------------------
# James Richardson	                              Score	  Notes
# Hypothesis 	                                    1	      good clear hypothesis
# Preliminary data exploration and visualisation 	2	      boxplot, histogram, mean and sd, very good
# Testing the assumptions and reporting 	        2	 
# Selection and carrying out correct test 	      1.5	    would also expect to see transformations attempted
# Reporting your results and presenting the data 	2	      Report to sensible number of decimal places, use units in results statement, no test statistic given
# Very nice script, well annotated and organised
# Total 	8.5	 
