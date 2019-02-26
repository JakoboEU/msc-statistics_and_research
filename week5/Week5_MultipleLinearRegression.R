# Date: 12/02/2019 
# Name: James Richardson
# Analysis: Testing for similarities: Multiple linear regression + Generalised Linear Models
# Test: Multiple Linear regression
# Data: sleep.csv


#########
# Set up
#########

# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/week5")
# list files in working directory
list.files() 

# Load required libaries
library(tidyverse)
library(ggplot2)

# Load the data
sleep <- read_csv("sleep.csv")
# Parsed with column specification:
#   cols(
#     Species = col_character(),
#     BodyWt = col_double(),
#     BrainWt = col_double(),
#     TotalSleep = col_double(),
#     LifeSpan = col_double(),
#     Gestation = col_double(),
#     Danger = col_double()
#   )

# What if you have more than one explanatory variable! 
# Don’t panic, its time to use multiple linear regression! 
# This data set consists of sleep times of various mammal species.
# We want to know if sleep time (hrs/day) is related to body size and any other covariates. 
# Variables in the dataset are;
# * BodyWt = body weight (kg)
# * BrainWt = brain weight (g)
# * TotalSleep = sleep time per day (hrs)
# * LifeSpan = species mean life span (yrs)
# * Gestation = gestation period (days)
# * Danger = overall danger index (1-5) 1 = least danger (from other animals); 5 = most danger (from other animals)
# * First, what is your response and what are your explanatory variables?
######
######
# * 1. Clearly state your null hypothesis
######
# The body size of an animal does not significantly predict the number of hours it sleeps

######
# * 2. Plot the data. Remember, scatters for continuous data and boxplots for factors.
######
str(sleep)
plot(sleep[,2:7])

ggplot(sleep, aes(x=BodyWt, y=TotalSleep)) +
  geom_point()

ggplot(sleep, aes(x=BrainWt, y=TotalSleep)) +
  geom_point()

ggplot(sleep, aes(x=LifeSpan, y=TotalSleep)) +
  geom_point()

ggplot(sleep, aes(x=Gestation, y=TotalSleep)) +
  geom_point()

ggplot(sleep, aes(x=Danger, y=TotalSleep)) +
  geom_point()

# Do the variables look sensible, are there any outliers (hint: look at body weight and brain weight).
# We should also look to see if any of the explanatory variables are correlated. 
# Strong correlation indicates collinearity in explanatory variables and we will need to deal with this before modelling.
# The code below will help assess collinearity and also check for normality in our covariates.

sleep<-na.omit(sleep) # Remove all NA’s from dataset
# Check for normality of variables (boxplots centred) and linearity of relationships 
library(car)
scatterplotMatrix(~BodyWt+BrainWt+LifeSpan+Gestation, data=sleep, diagonal=list(method="boxplot"))
# Ok, so the covariates are not normal, we could see outliers from the scatter plots above. 
# One solution is to transform (log10) the non-normal variables and try again. 
scatterplotMatrix(~log10(BodyWt)+log10(BrainWt)+LifeSpan+Gestation, data=sleep,diagonal=list(method="boxplot"))
# This looks much better!
# Lets look at collinearity now

pairs(~log10(BodyWt)+log10(BrainWt)+LifeSpan+Gestation, data=sleep, upper.panel = NULL)

# We have some strong correlations, lets see how strong using corrplot
library(corrplot)
M<-cor(sleep[2:6], method="pearson") # create matrix of pearson correlations
corrplot(M, method="number", type="lower") # plot matrix showing correlation scores


# So we have some strong correlations in our covariates and hence collinearity in our data, 
# we will need to deal with this before we can build our model. 
# BrainWt is strongly correlated with our main explanatory variable BodyWt and also Gestation, 
# so we should not include BrainWt in the model.
# Let’s build the full model and test the significance of each covariate.
######
# 3. Fit the model
######
Mult.lm1 <- lm(TotalSleep~log10(BodyWt)+LifeSpan+Gestation+Danger, data=sleep) 
summary(Mult.lm1)
# if all the terms (covariates) are not significant try removing the least significant and rerun
# the model. 
# Repeat this until all terms are significant – but don’t forget to keep in body weight 
# as this is the thing we are interested in.
Mult.lm2 <- lm(TotalSleep~log10(BodyWt)+Gestation+Danger, data=sleep) 
summary(Mult.lm2)

Mult.lm3 <- lm(TotalSleep~log10(BodyWt)+Danger, data=sleep) 
summary(Mult.lm3)

# What is your final model?
######
# 4. Now lets check the model validation plots as above in the linear model example. 
# Do these you sensible? 
# Do your residuals look normally distributed?
  
plot(Mult.lm3)

hist(Mult.lm3$residuals)

#######
# 5. If you are happy with the model validation you can then extract the results using summary() and report these in a sentence.
#######
summary(Mult.lm3)

# Call:
#   lm(formula = TotalSleep ~ log10(BodyWt) + Danger, data = sleep)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.3705 -2.1740  0.1379  1.9363  6.9130 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)    15.2777     0.9102  16.786  < 2e-16 ***
#   log10(BodyWt)  -1.4018     0.3375  -4.153 0.000115 ***
#   Danger         -1.5981     0.3259  -4.903 8.73e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.31 on 55 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.5017,	Adjusted R-squared:  0.4836 
# F-statistic: 27.69 on 2 and 55 DF,  p-value: 4.791e-09

# Results Statement
# ------------------
# Body weight and the overall danger posed from other animals where found to be significant
# predictors of the total amount of time spent asleep (multiple linear regression: R2 = 0.48, df = 55, p < 0.001).

# To plot the data we need to first create some predicted data using the predict() function.
# Make new X data, first finding out the min and max of the min(sleep$BodyWt)
max(sleep$BodyWt)
newX<-expand.grid(BodyWt = seq(from=0.005, to=6654, length=50),
                  Danger =seq(from=1, to=5, by=1)) 
newX
# Predictions of TotalSleep using model. 
# Note: Mult.lm3 should be the name of your final model ]
newY<-predict(Mult.lm3, newdata=newX, interval="confidence")
# Housekeeping 
sleep.plot.metrics<-cbind(newX, newY) 
head(sleep.plot.metrics)

# We have modeled data for each of the separate danger index values, 
# and have subset only the middle danger index of 3 to use in our plot (you can do all of them if you have time).
danger3<-subset(sleep.plot.metrics, sleep.plot.metrics$Danger == 3)
plot(TotalSleep~log10(BodyWt), data=sleep) # plot data 
lines(fit ~ log10(BodyWt), data=danger3) # add modelled line 
lines(upr ~ log10(BodyWt), data=danger3, lty=2) # add upper CI
lines(lwr ~ log10(BodyWt), data=danger3, lty=2) # add lower CI

danger5<-subset(sleep.plot.metrics, sleep.plot.metrics$Danger == 5)
plot(TotalSleep~log10(BodyWt), data=sleep) # plot data 
lines(fit ~ log10(BodyWt), data=danger5) # add modelled line 
lines(upr ~ log10(BodyWt), data=danger5, lty=2) # add upper CI
lines(lwr ~ log10(BodyWt), data=danger5, lty=2) # add lower CI


ggplot(sleep, aes(x=log10(BodyWt), y=TotalSleep)) + 
  xlab("Log10 of Body Weight") +
  ylab("Total amount of time asleep") + 
  geom_point(aes(color=as.factor(Danger))) + 
  stat_smooth(aes(color=as.factor(Danger)), method = "lm", fullrange=TRUE)

