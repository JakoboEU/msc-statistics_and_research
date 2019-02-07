# Date: 06/02/2019 
# Name: James Richardson
# Student Number: 18057447
# Data:  lichen.csv

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
library(reshape2)

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
# 2 marks for testing correlations; 
# 3 marks for correctly reporting the correlations; 
# 2 marks for testing the relationship between growth rate and pollution index; 
# 2 marks for reporting the results statement; 
# 1 mark for plotting the relationship, including a fitted/modeled line.

# Different species of lichen are often identified as tolerant or intolerant to atmospheric pollution, 
# and hence the occurrence of different lichen species has been used as a guide to pollution levels. 
# This dataset contains the results of a study looking at the lichen growth rate of a single species 
# at various locations across the UK. At each location several measures of pollution were recorded 
# as well as the number of bacteria on each sampled lichen. 
# The researchers are interested to know if the growth rate of an individual species can be related 
# to the amount of pollution and if the growth rate is related to the bacterial community inhabiting the lichen. 
# The variables in the dataset are;
# * plot – individual number of each sampled location.
# * growth_rate – lichen growth rate (cm2/yr).
# * pollution_index – composite measure of pollution (no units), higher values indicate greater level of pollution.
# * NOx – pollution measurement, oxides of nitrogen (g/ha/yr).
# * bacteria – number of bacteria (measured in colony forming units) from sampled lichen branch (cfu/g).
# * NH3 – pollution measurement, atmospheric ammonia (ppb).

lichen <- read_csv("lichen.csv")
# Parsed with column specification:
#   cols(
#     Plot = col_character(),
#     growth.rate = col_double(),
#     polution.index = col_double(),
#     NH3 = col_double(),
#     bacteria = col_double(),
#     NOx = col_double()
#   )

################################################################################################################
# Examine the dataset to answer the following research questions;
# 1) Is there correlation between any of the measured variables? 
#     If so, statistically report the three strongest correlations.
################################################################################################################

summary(lichen)

# Grab just the continuous variables
lichen_continuous<-na.omit(lichen[,2:6])

# Check how many rows we have
nrow(lichen_continuous) # [1] 177

# Let's look at the scatter plot of our continuous variables
########
pairs(lichen_continuous)

# Let's check which of the continuous varibles follow a normal distribution
########
# What do the distributions look like?
barplot(lichen$growth.rate)
barplot(lichen$polution.index)
barplot(lichen$NH3)
barplot(lichen$bacteria)
barplot(lichen$NOx)

# Do shapiro test on all continuous variables:
lichen.shapiro.test <- sapply(lichen_continuous, FUN=shapiro.test)
lichen.shapiro.test[2,]
# $growth.rate
# [1] 0.001704148
# $polution.index
# [1] 7.105127e-05
# $NH3
# [1] 8.684515e-05
# $bacteria
# [1] 7.656569e-05
# $NOx
# [1] 4.371544e-06

# So all of the variables are significantly different to the normal distribution

# Let's check their correlation using spearman (as none of the variables are normal)
########
cor(lichen_continuous, method="spearman")
#                 growth.rate   polution.index  NH3    bacteria         NOx
# growth.rate     1.00000000    -0.76495001 -0.7870545 -0.08988591 -0.62018674
# polution.index -0.76495001     1.00000000  0.8020893  0.06696776  0.73324132
# NH3            -0.78705451     0.80208926  1.0000000  0.10912329  0.86071757
# bacteria       -0.08988591     0.06696776  0.1091233  1.00000000  0.03681355
# NOx            -0.62018674     0.73324132  0.8607176  0.03681355  1.00000000

# Highest Correlations (above/below (-)0.7):
# 1) [0.86071757] NOx + NH3 - scatter plot seems to show a strong linear relationship
# 2) [0.8020893] NH3 + polution.index - scatter plot seems to show a linear relationship
# 3) [-0.7870545] NH3 + growth.rate - scatter plot seems to show a linear relationship
# 4) [-0.76495001] polution.index + growth.rate - scatter plot seems to show  linear relationship
# 5) [0.73324132] NOx + polution.index - scatter plot seems to show linear relationship

# Let's check the p-values of the top 3 to see if these are significantly correlated
# We will use the spearman test again in each case.

# Null Hypothesis: There is no correlation between NOx and NH3 in the lichen data.
# ----------------
cor.test(lichen_continuous$NOx, lichen_continuous$NH3, method="spearman")
# Spearman's rank correlation rho
# 
# data:  lichen_continuous$NOx and lichen_continuous$NH3
# S = 128720, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.8607176 

# Result:
# There was a significant positive correlation between the NOx and NH3 (Spearmans; n=177, p<0.001, rho=0.86)

# Null Hypothesis: There is no correlation between polution.index and NH3 in the lichen data.
# ----------------
cor.test(lichen_continuous$polution.index, lichen_continuous$NH3, method="spearman")
# Spearman's rank correlation rho
# 
# data:  lichen_continuous$polution.index and lichen_continuous$NH3
# S = 182900, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.8020893 

# Result:
# There was a significant positive correlation between the polution.index and NH3 (Spearmans; n=177, p<0.001, rho=0.80)

# Null Hypothesis: There is no correlation between growth.rate and NH3 in the lichen data.
# ----------------
cor.test(lichen_continuous$growth.rate, lichen_continuous$NH3, method="spearman")
# Spearman's rank correlation rho
# 
# data:  lichen_continuous$growth.rate and lichen_continuous$NH3
# S = 1651600, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# -0.7870545 

# Result:
# There was a significant negative correlation between the growth.rate and NH3 (Spearmans; n=177, p<0.001, rho=-0.78)

# ---------------------------------
# RESULTS STATEMENT FOR Question 1:
# ---------------------------------
# The three strongest significant correlations in the lichen data are a positive correlation 
# between NOx and NH3 (Spearmans; n=177, p<0.001, rho=0.86), a positive correlation between 
# polution.index and NH3 (Spearmans; n=177, p<0.001, rho=0.80) and a negative correlation between
# growth.rate and NH3 (Spearmans; n=177, p<0.001, rho=-0.78)

# Try drawing a scatter plot of the 3 variables (polution.index, growth.rate, NOx) that are correlated with NH3
####
lichen.result.graph <- melt(lichen_continuous[,c(1,2,3,5)], measure.vars=c("polution.index", "growth.rate", "NOx"))

variable.names <- list(
  'polution.index'="Measure of pollution (no units)",
  'growth.rate'="Lichen growth rate (cm2/yr)",
  'NOx'="Oxides of nitrogen (g/ha/yr)"
)

ggplot(lichen.result.graph, aes(x=NH3, y=value, color=variable)) + 
  labs(title="Significant Correlations with Atmospheric Ammonia") +
  facet_grid(variable~., labeller=function(variable,value){ return(variable.names[value]);}) +
  geom_point() +
  ylab("") +
  xlab("atmospheric ammonia (ppb)") +
  guides(color=FALSE)


################################################################################################################
# 2) Is there a relationship between lichen growth rate and the pollution index? 
#     [hint: think carefully about this relationship, does one variable directly impact the other?]
# P.S. there will be no hints in the unit assessment.
################################################################################################################

# We expect high pollution levels to inhibit the growth rate

# Null Hypothesis: The pollution index does not significantly predict the growth rate of lichen.
# ----------------

# Do the linear regression
linear_regression <- lm(growth.rate ~ polution.index, data=lichen_continuous)

summary(linear_regression)

# Call:
#   lm(formula = growth.rate ~ polution.index, data = lichen_continuous)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -3.10326 -0.58369  0.09974  0.59651  2.52376 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     6.95640    0.08523   81.62   <2e-16 ***
#   polution.index -0.76729    0.05021  -15.28   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.063 on 175 degrees of freedom
# Multiple R-squared:  0.5716,	Adjusted R-squared:  0.5692 
# F-statistic: 233.5 on 1 and 175 DF,  p-value: < 2.2e-16

# ---------------------------------
# RESULTS STATEMENT FOR Question 2:
# ---------------------------------
# We found that the polution index was a significant predictor of the growth rate of lichen (R2=0.5692, P<0.001).

ggplot(lichen_continuous, aes(x=polution.index, y=growth.rate)) + 
  xlab("Composite measure of pollution (no units), higher values indicate greater level of pollution.") +
  ylab("Lichen growth rate (cm2/yr).") + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")
