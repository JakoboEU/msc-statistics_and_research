# Date: 30/01/2019 
# Name: James Richardson
# Student Number: 18057447
# Data:  enrich.csv

# Script Set Up
# -------------
# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/practice3")
# list files in working directory
list.files() 

# Load required libaries
library(tidyverse)
library(ggplot2)
library(multcomp)

# Analysis Pathway
# ------------------
# 1. Write clear hypotheses
# 2. Explore descriptive statistics – means, standard deviation, distribution etc
# 3. Create appropriate plots to visualise/explore data
# 4. Decide on test(s) to be done (see flow chart)
# 5. Examine assumptions of tests: normality, variances, independence
# 6. Carry out tests
# 7. Report your results: results statements / plots

# Marking guidance
# -----------------
# 1 mark for the hypothesis; 
# 2 marks for preliminary data exploration and visualisation, 
# 2 marks for testing the assumptions and reporting, 
# 2 marks for selection and carrying out correct tests, 
# 3 marks reporting your results and presenting the data.


# The researchers tested three different types of environmental enrichment within lion enclosures to examine the effect 
# each treatment had on the amount of pacing (a proxy measure of animal anxiety).
# The dataset contains two variables,
# * pacing – amount of pacing (recorded in minutes) recorded over the observation period (2 hours)
# * type – three enclosure enrichment treatments
# Examine the dataset to answer the following research question; 
# is there a difference in lion anxiety (pacing behaviour) between the three enrichment treatments?

# -----------------
# Null Hypothesis: 
# -----------------
# There is no significant difference in the pacing behaviour of lions between the three enrichment treatments.

# 
# Load the enrichment data
enrichments_data <- read_csv("enrich.csv")
# Parsed with column specification:
#   cols(
#     pacing = col_double(),
#     type = col_character()
#   )

# -----------------
# Examine the data
# -----------------
names(enrichments_data) # [1] "pacing" "type"  
str(enrichments_data)

# How are the encrichments described?
as.factor(enrichments_data$type) # Levels: enrich1 enrich2 enrich3
# Convert type to factor
enrichments_data$type <- as.factor(enrichments_data$type)

# check the mean and standard deviation pacing time (minutes) between enrichment types
tapply(enrichments_data$pacing, enrichments_data$type, FUN=mean)
# enrich1 enrich2 enrich3 
# 21.6125 30.4000 27.3250 
tapply(enrichments_data$pacing, enrichments_data$type, FUN=sd)
#  enrich1  enrich2  enrich3 
# 3.479440 5.573150 5.420793 

# Look at box plot comparison between enrichment types
ggplot(enrichments_data, aes(x=type, y=pacing, group=type)) + 
  geom_boxplot() +
  xlab("Enrichment Type") + ylab("Pacing Time (minutes)")

# Examine distribution of pacing time
ggplot(enrichments_data, aes(x = pacing, fill=type)) + 
  geom_bar(binwidth = 5)+ 
  facet_wrap(~type, ncol=1) +
  xlab("Pacing Time (minutes)") + ylab("Number of Lions")

# -------------------------
# Choose which test to do:
# -------------------------

# Difference between one continuous variable (pacing time) and one categorical variable (enrichment type).
# Three groups of categorical data (enrich1 enrich2 enrich3)
# Is the data normal?
## 
# Test for normality 
shapiro.test(enrichments_data$pacing)
# data:  enrichments_data$pacing
# W = 0.95781, p-value = 0.2098 <-# NOT significantly different from normal

# Are the variances of the groups homogenous?
##
# Test for homogeneity of variances
bartlett.test(enrichments_data$pacing, enrichments_data$type)
# data:  enrichments_data$pacing and enrichments_data$type
# Bartlett's K-squared = 2.9886, df = 2, p-value = 0.2244 <- NO significant difference in variance

# So can use a One-way ANOVA test
###

# -------------------------
# Do the tests:
# -------------------------
anova1<-aov(pacing~type, data=enrichments_data)
summary(anova1) 
#              Df Sum Sq Mean Sq F value   Pr(>F)    
# type          2  508.5  254.24   11.82 0.000153 ***
#  Residuals   31  666.8   21.51  

# So there is a difference, but we need to find out which group is different
# Use Tukey post hoc test
cld(glht(anova1, linfct=mcp(type="Tukey")))
# enrich1 enrich2 enrich3 
#   "a"     "b"     "b" 

# ------------------
# RESULTS STATEMENT: 
# ------------------
# There is a significant difference in the pacing behaviour of lions between the enrich1 treatment (group a)
# and the other two treatments (enrich2 and enrich3) (group b)
# enrich1 (mean±SD, 21.61±3.48 minutes)
# enrich2 (mean±SD, 30.40±5.57 minutes)
# enrich3 (mean±SD, 27.33±5.42 minutes)
# p < 0.001, df = 2, F = 11.82

####
# Draw results plot
####

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# Create summary of data
enrich.summary <- summarySE(enrichments_data, measurevar="pacing", groupvars=c("type")) 
enrich.summary$group <- c('a','b','b')
enrich.summary

# Plot Result
ggplot(enrich.summary, aes(x=type, y=pacing)) +
  geom_errorbar(aes(ymin=pacing-ci, ymax=pacing+ci, color=group), width=.1) + 
  geom_point(aes(color=group)) +
  scale_y_continuous(name="Pacing Time (minutes)") +
  scale_x_discrete(name = "Encrichment Type") +
  guides(color=guide_legend(title="Treatment Group")) +
  labs(title = "Pacing Behaviour of Lions based on Enrichment Treatment", 
       subtitle = "Treatment group (a) signicantly different to treatment group (b)")

#-----------------------------------------------
# James Richardson	                                Score	Notes
# Hypothesis 	                                      1	    good clear hypothesis
# Preliminary data exploration and visualisation 	  2	    excellent
# Testing the assumptions and reporting 	          2	    nicely reported
# Selection and carrying out correct test 	        2	 
# Reporting your results and presenting the data 	  2.5	  Very good but not clear the purpose of the plot legend. 
#                                                         Also, results statement needs to be written as a flowing statement. 

# Total 	                                          9.5	 