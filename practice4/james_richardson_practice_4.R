# Date: 13/02/2019 
# Name: James Richardson
# Student Number: 18057447
# Data:  SheepFitness.csv

# Script Set Up
# -------------
# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/practice4")
# list files in working directory
list.files() 

# Load required libaries
library(tidyverse)
library(ggplot2)

# Analysis Pathway 
# ----------------
# 1. Write clear hypotheses
# 2. Explore descriptive statistics – means, standard deviation, distribution etc
# 3. Create appropriate plots to visualise/explore data
# 4. Decide on test(s) to be done (see flow chart)
# 5. Examine assumptions of tests: normality, variances, independence
# 6. Carry out tests
# 7. Report your results: results statements / plots

# Marking Guidance
# ----------------
# 1 mark for the hypothesis; 
# 2 marks for preliminary data exploration and visualisation, 
# 4 marks for model building and validation, 
# 3 marks for reporting your results and presenting plot with modelled line.


# The dataset ‘SheepFitness.csv’ should be downloaded from moodle. 
# The dataset contains two variables, bodysize (mothers body size) and fitness (number of offspring). 
# Researchers would like to know if a relationship exists between the female body condition 
# (as measured by sheep body weight (kg)) and the number of offspring produced.
# Construct a model to investigate the relationship between the mother’s body mass and the number of offspring she produced.


# Null Hypothesis
# ----------------
# The body weight of a sheep does not significantly predict the number of offspring she will have.

# Data Investigation
# ------------------

# Read in data
sheep_fitness <- read_csv("SheepFitness.csv")
# Parsed with column specification:
#  cols(
#    fitness = col_double(),
#    body.size = col_double()
#  )

str(sheep_fitness)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	50 obs. of  2 variables:
# $ fitness  : num  4 3 2 14 5 2 2 5 8 4 ...
# $ body.size: num  63.7 71.8 61.6 86 73.3 ...

hist(sheep_fitness$fitness)
hist(sheep_fitness$body.size)

ggplot(sheep_fitness, aes(x=body.size, y=fitness)) +  
  geom_point() 

# Test for Normality
# ------------------
shapiro.test(sheep_fitness$fitness) # W = 0.86617, p-value = 4.371e-05
shapiro.test(sheep_fitness$body.size) # W = 0.96397, p-value = 0.1304
# So fitness is significantly different to the normal distribution

# Check for correlation
# ---------------------
# Choose spearman as we don't have normally distributed variables

cor.test(sheep_fitness$fitness, sheep_fitness$body.size, method="spearman")

# data:  sheep_fitness$fitness and sheep_fitness$body.size
# S = 8447.2, p-value = 5.34e-06
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.5943709 
nrow(sheep_fitness)
# [1] 50

# So the fitness and body.size are significantly correlated (rho = 0.59, n = 50, p < 0.001)

# Check for prediction
# --------------------
# Explanatory variable: body.size
# Response variable: fitness

# Test with Linear Model
###
sheep.model.lm <- lm(fitness~body.size, data=sheep_fitness)

# Are residuals normally distributed?
hist(sheep.model.lm$residuals)
shapiro.test(sheep.model.lm$residuals) # W = 0.95188, p-value = 0.04079 
# So residuals NOT significantly different to normal - so Linear model okay.

par(mfrow=c(2,2)) 
plot(sheep.model.lm)
# Q-Q Plot is a bit wacky - dots falling well off line at start and end.
# Got outliers (4) approaching cookes distance
# Slight bulge in both the predicted values

summary(sheep.model.lm)
# Call:
# lm(formula = fitness ~ body.size, data = sheep_fitness)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.2111 -1.1558  0.0023  0.9030  6.3077 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -10.55224    2.86886  -3.678 0.000593 ***
#   body.size     0.21227    0.04013   5.289 2.99e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.336 on 48 degrees of freedom
# Multiple R-squared:  0.3682,	Adjusted R-squared:  0.355 
# F-statistic: 27.97 on 1 and 48 DF,  p-value: 2.994e-06

# Test with Generalised Linear Model
###
# Poisson is usually a good fit for count data (e.g. number of offspring)
sheep.model.glm <- glm(fitness~body.size, data=sheep_fitness, family = poisson)
par(mfrow=c(2,2)) 
plot(sheep.model.glm)
# Q-Q plot has points tied closer to the line
# Outliers further from cooks distance
# Less of a bulge in the predicted values.

# Will choose GLM model using poison distribution as preferred model
summary(sheep.model.glm)

# Call:
#   glm(formula = fitness ~ body.size, family = poisson, data = sheep_fitness)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.7633  -0.6273   0.1140   0.5369   1.9579  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -2.422136   0.694361  -3.488 0.000486 ***
#   body.size    0.054088   0.009317   5.806 6.42e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 85.081  on 49  degrees of freedom
# Residual deviance: 48.042  on 48  degrees of freedom
# AIC: 210.85
# 
# Number of Fisher Scoring iterations: 4

# Pseudo-R2
48.042 / 85.081
# [1] 0.5646619
(85.081 - 48.042) / 85.081
# [1] 0.4353381

# Results Statement
# -----------------
# The body weight of a sheep is a significant predictor of the number of offspring a she will have 
# (generalised linear model: Pseudo-R2 = 0.56, df = 48, P < 0.001)

ggplot(sheep_fitness, aes(x=body.size, y=fitness)) + 
  xlab("Sheep Body Weight (Kg)") +
  ylab("Number of Offspring") + 
  geom_point() + 
  stat_smooth(method = "glm", col = "red", method.args = list(family = "poisson"))


############################################################################################################################
############################################################################################################################

# Written Practice: Part 1
###############################

# a) For each figure write a sentence, suitable for a results section, that describes the contents of the figure. [2 marks]

# Fig 1. Crop yield against canopy index (a measure of both diversity of shade trees and the amount of shade). 
# Model statistics R2 = 0.58, p < 0.001, n = 164; intercept = 6.96399 slope = 0.76280.

# Results statement:  The canopy index is a significant postive predictor of crop yield (R2 = 0.58, n = 164, p < 0.001)

# Fig 2. Crop yield against the diversity of predatory arthropods sampled in sun and shade grown coffee plantations. 
# Model statistics R2 = 0.07, p < 0.001, n = 164; intercept = 6.1383 slope = 0.4509.

# Results statewment: The number of predatory arthropods is a significant postive predictor of crop yield (R2 = 0.07, p < 0.001, n = 164)

# b) Using the model statistics in Fig 1, 
# calculate what the crop yield would be when the canopy index is 10.57, give your answer to two decimal places. [1 mark]

(10.57 * 0.76280) + 6.96399

# [1] 15.02679 -> 15.03 tons

# c) Briefly state the confidence you would place in the relationships in the figures below 
# and why you think that this is the case. [2 marks

# Fig 1: Reasonably positive:  Points generally close to line (R2 = 0.58)
# Fig 2. Not positive: points very scattered from line (R2 = 0.07)

# Written Practice: Part 2
###############################

# a) Write a sentence suitable for a results section that describes the findings of this model. [3 marks]

# The canopy index was a signficant positive predictor (p < 0.001), the  number of arthropods was a positive predictor(p < 0.001) 
# and the aridity was a significant negative predictor (p < 0.001) of the crop yield whereas the number of 
# microbes was not found to be a significant predictor (p = 0.97) (df = 159, R2 = 0.73)

# b) What would you do next if you got this output? Describe the procedures you would carry out next [2 marks]

# Remove the number of microbes from the linear model and run again.
