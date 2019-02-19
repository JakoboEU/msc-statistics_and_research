# Date: 12/02/2019 
# Name: James Richardson
# Analysis: Testing for similarities: Multiple linear regression + Generalised Linear Models
# Test: Generalised Linear Model
# Data: herbivore.csv

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
library(corrplot)
library(car)

# A GLM allows you to fit a model when the error structure (residuals) is not normally distributed. 
# It is a flexible tool that allows you to change the model error structure to accommodate tricky real-life data.
# In ecology we often count things, abundance of animals, species richness of plants, 
# this count data is often suited to a poisson error distribution. 
# A good rule of thumb is if you have count data for your response variable you will probably need to use a poisson 
# GLM when looking at relationships.

# The data set we use now contains the number of herbivores (count data), the annual net primary productivity (ANPP) 
# of grassland and the winter rainfall. 
# Specifically, we want to know if there is a relationship between the number of herbivores and productivity. 
# Go ahead and load the ‘herbivore.csv’ file from moodle. 
# Have a look at the variable names and data types.
herbivore <- read_csv("herbivore.csv")
# Parsed with column specification:
#  cols(
#    ANPP = col_double(),
#    winter.precip = col_double(),
#    herb.abund = col_double()
#  )
str(herbivore)

# Which is the response variable? Which are the explanatory variables?
# Explanatory: ANPP + winter.precip
# Response: herb.abund
# 
######
#  1. Write a null hypothesis to test.
######
# Annual net primary productivity and winter rainfall do not significantly predict the number of herbivores.

######
#  2. Now plot your data using either base R (or ggplot2).
######
ggplot(herbivore, aes(x=ANPP, y=herb.abund)) +
  geom_point()

ggplot(herbivore, aes(x=winter.precip, y=herb.abund)) +
  geom_point()

# As we have two explanatory variables we also need to explore these and check for collinearity (see code below).
# Test: GLM
# Data: herbivore abundance 
# Null hypothesis:

ggplot(herbivore, aes(x=ANPP, y=herb.abund)) +
  geom_point()

# Check for collinearity
pairs(~herb.abund+ANPP+winter.precip, data=herbivore, upper.panel = NULL) # No correlation between ANPP and winter.precip


# Or using corrplot
M<-cor(herbivore[1:3], method="pearson")
corrplot(M, method="number", type="lower")
# Again no correlation between explanatory variables


# Ok great, no correlation between explanatory variables so we can use both in our model. 
# If winter precipitation was strongly correlated with ANPP we would have to leave this variable out of our model.
# From our scatter plot of the response and explanatory variable do you think we will accept or reject our null hypothesis?
#  Ok, now lets set up the model. 
# Its very similar to the linear models we have used previously
# but this time we use the glm() function and specify the error structure by setting family equal to poisson.

######
# 3. Preform GLM test
######
herb.mod <- glm(herb.abund ~ ANPP + winter.precip, family = poisson, data=herbivore) 
summary(herb.mod)

# Call:
#   glm(formula = herb.abund ~ ANPP + winter.precip, family = poisson, 
#       data = herbivore)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.73676  -0.70245  -0.06483   0.54208   2.47686  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     -3.741e-01  1.532e-01  -2.441   0.0146 *  
#   ANPP           6.467e-03  2.518e-04  25.687   <2e-16 ***
#   winter.precip  9.598e-05  1.103e-04   0.870   0.3842    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 752.28  on 197  degrees of freedom
# Residual deviance: 191.80  on 195  degrees of freedom
# AIC: 808.05
# 
# Number of Fisher Scoring iterations: 4


# You can see from the summary output that not all of our explanatory variables are significant. 
# Winter precipitation is not significant so we can remove this term from the model and try again.

######
# 3B. Preform GLM test
######
herb.mod<-glm(herb.abund ~ ANPP, family = poisson, data=herbivore) # overwrite the first model summary(herb.mod)
summary(herb.mod)

# Call:
#   glm(formula = herb.abund ~ ANPP, family = poisson, data = herbivore)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.65553  -0.72859  -0.05732   0.57165   2.43592  
# 
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -0.2660242  0.0890232  -2.988  0.00281 ** 
#   ANPP         0.0064326  0.0002484  25.898  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 752.28  on 197  degrees of freedom
# Residual deviance: 192.55  on 196  degrees of freedom
# AIC: 806.81
# 
# Number of Fisher Scoring iterations: 4

# Now that all our explanatory variables are significant we need to check the model validation plots.

######
# 4. Model validation 
######
par(mfrow=c(2,2)) 
plot(herb.mod)

# Are there any strong patterns in the residuals? 
# What about Cooks distance, have we got any outliers strongly influencing the model?

######
# 5. If you are happy with the validation plots now use the summary() 
# function to read the model output so we can start to report the results.
######
summary(herb.mod)

# The p-value is much smaller than 0.05 so you should reject your null hypothesis, 
# i.e. there is a significant relationship between the two variables. 
# To write this up in a clear statement we first need to get a test statistic, 
# for GLM’s we can use a pseudo-R2. To calculate this we need to use the following equation;
#     Pseudo-R2 = (Null deviance - Residual deviance) / Null deviance
# This value will vary between 0-1 with higher values meaning your model has greater explanatory power, 
# just like R2 in a linear model. Write a clear results statement.
192.55 / 752.28 # <- this is wrong
# [1] 0.2559552

# ------------------
# Results Statement:
# ------------------
# Annual net primary productivity significantly predicts the number of herbivores 
# (generalised linear model, Pseudo-R2 = 0.26, df = 197, p < 0.001).

# Now lets use the predict() function again to predict some values from our model.

# Make some new X data
newX<-data.frame(ANPP = seq(from=30, to=644, length=50)) 
newX
# Predictions using model
newY<-predict(herb.mod, newdata=newX, type="response", se=T)
# Confidence intervals is X +/- (2xSE) 
pred.plus<-newY$fit+2*newY$se 
pred.minus<-newY$fit-2*newY$se
# Housekeeping
herb.plot.metrics<-cbind(newX, newY, pred.plus, pred.minus) 
herb.plot.metrics
head(herb.plot.metrics) # view the first few lines of your new object

# Now make a plot including the modeled line and confidence intervals 
# using either base R line() or ggplot2 geom_line(). 
# [hint: we did this for sleep and iris data, but in this GLM our confidence intervals are called 
# pred.plus and pred.minus rather than lwr and upr.

ggplot(herbivore, aes(x=ANPP, y=herb.abund)) + 
  xlab("Annual net primary productivity") +
  ylab("Herbivore Abundance") + 
  geom_point() + 
  stat_smooth(method = "glm", fullrange=TRUE)




########################################
# Generalised linear model 2: count data
########################################
# For the final model in this practical go ahead and import the data set called “birds.csv” from moodle. 
# The data represent a nationwide survey of the number of small brown passerines nesting in 
# 127 representative districts in England, Scotland and Wales.
# Question: Is there any relationship between the number of birds and the area of the district 
# and does the relationship differ among countries? 
# What is your response variable? 
# What are your explanatory variables?
# 1. Write out your null hypothesis
# 2. Inspect the data (summary, str, names.) and inspect the data graphically (scatter & boxplot)
# 3. Model the data, remember this is counts of bird abundance so which type of model should you us?
#    Check to make sure all the model terms (explanatory variables) are all significant
# 4. Model validation. How do the residuals look?
# 5. Report results and plot your modeled lines (see below if you need help with last step).
birds <- read_csv("birds.csv")
# Parsed with column specification:
#   cols(
#     District = col_double(),
#     Area = col_double(),
#     Number = col_double(),
#     Country = col_character()
#   )
# Response: Number
# Explanatory: District, Area + Country
# Null Hypothesis: The district, area and country do not significantly predict the number of small brown passerines

# 2. Inspect the data 
str(birds)

ggplot(birds, aes(x=District, y=Number, size=Area, color=Country)) +
  geom_point()

ggplot(birds, aes(x=Area, y=Number, color=Country, size=District)) +
  geom_point()

ggplot(birds, aes(x=Country, y=Number)) +
  geom_boxplot()

# Important - Check for correlation!
ggplot(birds, aes(x=Area, y=District)) +
  geom_point()
shapiro.test(birds$Area) # p-value = 0.01098
shapiro.test(birds$District) #  p-value = 0.0003139
# significantly different to the normal distribution
cor.test(birds$Area, birds$District, method="spearman")
# p-value < 2.2e-16, rho = 0.9303642 
# So District and area are significantly correlated
# So remove from model

# 3. Model the data
birds.mod <- glm(Number ~ Country + Area, family = poisson, data=birds) 
summary(birds.mod)

# Call:
#   glm(formula = Number ~ Country + Area, family = poisson, data = birds)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -4.3713  -1.6887  -0.2873   1.2150   5.2871  
# 
# Coefficients:
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          2.6903438  0.0585742  45.931   <2e-16 ***
#   CountrySCOTLAND   -0.9225381  0.0673029 -13.707   <2e-16 ***
#   CountryWALES      -0.4787712  0.0553294  -8.653   <2e-16 ***
#   Area               0.0091844  0.0006711  13.686   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 833.35  on 126  degrees of freedom
# Residual deviance: 597.42  on 123  degrees of freedom
# AIC: 1252.1
# 
# Number of Fisher Scoring iterations: 4

# 4. Model validation.
par(mfrow=c(2,2)) 
plot(birds.mod)

par(mfrow=c(1,1)) 
barplot(birds.mod$residuals)

# 5. Report results
64.389 / 833.354
# [1] 0.07726488

ggplot(birds, aes(x=Area, y=Number, color=Country)) + 
  xlab("Area of District") +
  ylab("Number of small brown passerines") + 
  geom_point() + 
  stat_smooth(method = "glm", aes(color=Country))

# ------------------
# Results Statement:
# ------------------
# The District and the area of the district significantly predict the number of small brown passerines
# (generalised linear model, Pseudo-R2 = 0.08, df = 126, p < 0.001).



# Drawing with plot!
# Make new X data
min(birds$Area)
max(birds$Area)
newX<-expand.grid(Area = seq(from=0, to=160, by=1), Country=rep(as.factor(birds$Country))) 
newX
# Predictions using model [bird1 is the name of my model – what did you call yours?] 
newY<-predict(birds.mod, newdata=newX, type="response", se=T)
# Confidence intervals
pred.plus<-newY$fit+2*newY$se
pred.minus<-newY$fit-2*newY$se
# Housekeeping
bird.plot.metrics<-cbind(newX, newY, pred.plus, pred.minus) 
bird.plot.metrics
head(bird.plot.metrics)
par(mfrow=c(1,1))
plot(Number~Area, data=birds)
lines(fit ~ Area, data=bird.plot.metrics[bird.plot.metrics$Country=="ENGLAND",]) 
lines(fit ~ Area, data=bird.plot.metrics[bird.plot.metrics$Country=="WALES",], lty=2) 
lines(fit ~ Area, data=bird.plot.metrics[bird.plot.metrics$Country=="SCOTLAND",], lty=3)

# Other Models!!
###############
# binomial(link = "logit")
# gaussian(link = "identity")
# Gamma(link = "inverse")
# inverse.gaussian(link = "1/mu^2")
# poisson(link = "log")
# quasi(link = "identity", variance = "constant")
# quasibinomial(link = "logit")
# quasipoisson(link = "log")

birds.mod.gau <- glm(Number ~ District + Area, family = gaussian, data=birds) 
summary(birds.mod.gau)
par(mfrow=c(2,2)) 
plot(birds.mod.gau)

birds.mod.quasi <- glm(Number ~ District + Area, family = quasipoisson, data=birds) 
summary(birds.mod.quasi)
par(mfrow=c(2,2)) 
plot(birds.mod.quasi)


birds.mod.gamma <- glm(Number ~ District + Area, family = Gamma, data=birds) 
summary(birds.mod.gamma)
par(mfrow=c(2,2)) 
plot(birds.mod.gamma)