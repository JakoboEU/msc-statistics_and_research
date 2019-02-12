# Date: 12/02/2019 
# Name: James Richardson
# Analysis: Testing for similarities: Multiple linear regression + Generalised Linear Models
# Test: Linear regression
# Data: iris dataset

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

# 
# Below is the equation for the linear regression.
#     Y=a+bX OR Y=mx+c either way Y=intercept+(slope*X)
# Confusingly, textbooks will call the X and Y components of the model different things so here are the most 
# commonly used names. The ones in bold are the ones we will use for this course.
#     Y = response variable = dependent variable
#     X = explanatory variable = predictor = independent variable
# Remember, when you plot your model variables you need to know which ones go on which axis. 
# The response should be on the Y-axis and the explanatory should be on the X-axis.
#


data(iris)
myiris <- iris[1:50, ] # use first 50 rows

names(myiris)
str(myiris)

# For this example we will use Sepal.Length to predict Sepal.Width – sepal width is the response (Y) variable and sepal length is the explanatory (X) variable. We want to first know if there is a significant
# Test: Linear regression
# Data: iris dataset
data(iris)
myiris <- iris[1:50, ] # use first 50 rows

# For this example we will use Sepal.Length to predict Sepal.Width 
# – sepal width is the response (Y) variable and sepal length is the explanatory (X) variable. 
# We want to first know if there is a significant relationship and, if so, then we want to be able to use 
# the explanatory variable to predict values for sepal width using the linear regression equation.
#
# Ok, state your null hypothesis for this data and plot the data using a simple scatter plot. 
# Now the fun part, lets model the data using the lm function.


# 1. Null Hypothesis - there is no relationship between SW and SL
# 2. Plot the data
plot(Sepal.Width~Sepal.Length, data=myiris) # do you think sepal length is a strong explanatory variable of sepal width?
# 3. model the data
iris.mod <- lm(Sepal.Width~Sepal.Length, data=myiris)

# Before we look at the results of the model we need to first validate the fit of the model. 
# To do this we first conduct a histogram of the residuals and then use the plot function on the model object.

#4. model validation hist( resid(iris.mod) )
class(iris.mod) # tells you that the iris.mod object is of the class ‘lm’ (linear model) par(mfrow=c(2,2))
plot(iris.mod)


# Do the validation plots look ok? 
# Do the residuals look normally distributed? 
# Are there any strong patterns in the residuals? 
# Are any outliers having a strong effect in the model?
# ONLY AFTER CHECKING THE VALIDATION PLOTS DO WE THEN CHECK THE MODEL RESULTS 
# Using the summary() function on your model object will give you the results below

summary(iris.mod)

# Call:
#   lm(formula = Sepal.Width ~ Sepal.Length, data = myiris)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.72394 -0.18273 -0.00306  0.15738  0.51709 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -0.5694     0.5217  -1.091    0.281    
# Sepal.Length   0.7985     0.1040   7.681 6.71e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.2565 on 48 degrees of freedom
# Multiple R-squared:  0.5514,	Adjusted R-squared:  0.542 
# F-statistic: 58.99 on 1 and 48 DF,  p-value: 6.71e-10

# The test output gives you the value of the F-statistic, the degrees of freedom 
# and finally the significance value (p-value) for the model. 
# The output also gives you the model intercept and slope, which you can use with the linear regression 
# equation to predict values of Y (sepal width). 
# The model output also tells you which of these values is significantly different from zero. 
# Generally, we are not interested in whether the intercept is different from zero 
# but we are more concerned with knowing if the slope is different from zero, 
# which in this case the slope of 0.7985 is significantly different from a slope of zero (P<0.001). 
# This tells us there is a positive relationship between our explanatory variable (sepal length) 
# and our response variable (sepal width).
# 
# We can also check the confidence interval around the slope and intercept using the following code 
confint(iris.mod, level = 0.95)
#                   2.5 %    97.5 %
# (Intercept)  -1.6184048 0.4795395
# Sepal.Length  0.5894925 1.0075641

# This tells us that we can be 95% certain the true intercept falls  between -1.62 and 0.48 
# (and as this range contains zero the above test can not report a significant difference from zero). 
# Also, the true slope falls within the range 0.59-1.01 which doesn’t contain zero
# (so must be significantly different from zero). Isn’t science fun!
#
#
##########################
#  Reporting the results
##########################
# Now report the results in a nice concise sentence, 
# e.g. we found sepal length is a significant linear predictor of sepal width (linear regression: R2 = 0.55, df = 48, P < 0.001).
# We also need to plot the data. 
# To do this we could simply use the plot() function followed by the abline() function 
# as we have done in previous practicals. 
# But as we will use the predict() function later in this practical lets use it now on this familiar dataset.



# 5. Report results
# Make new X data, which we will use to make predictions 
newX<-data.frame(Sepal.Length=seq(from=4.3, to=5.8, length.out=50)) # 50 new X data values newX
# Use predict to make new Y's (our predictions) with confidence intervals 
newY<-predict(iris.mod, newdata=newX, interval="confidence")
# Housekeeping - bring newX and newY together 
add.these2plot<-cbind(newX, newY)
head(add.these2plot)
#take a look at add.these2plot object. Now lets make the plot
plot(Sepal.Width~Sepal.Length, data=myiris, xlab="Sepal length (cm)", ylab="Sepal width (cm)") 
lines(fit ~ Sepal.Length, data=add.these2plot, col="blue", lwd=3, lty=3)
# Using the add.these2plot object, see if you can add the upper and lower confidence intervals
lines(lwr ~ Sepal.Length, data=add.these2plot, col="red", lwd=3, lty=3)
lines(upr ~ Sepal.Length, data=add.these2plot, col="red", lwd=3, lty=3)

# Using the model to predict values
##################################
# Finally, we said our goal was to not only test the relationship 
# but also to use the linear regression equation to predict values of sepal width.
# Lets look the equation again: 
# Y= ab+c OR Y=intercept+(slope*X)
# Calculate Y values (sepal width values) for the X values (sepal length values) of 2 and 5 using
# the linear regression equation (type the equation into R-Studio). 
# Extend you plot axes and see if the Y values you calculated look correct.
plot(Sepal.Width~Sepal.Length, data=myiris, ylim=c(-1,6), xlim=c(-1,6)) 
abline(iris.mod)
