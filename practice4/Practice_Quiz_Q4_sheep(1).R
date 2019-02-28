# DD/MM/YY Your Name
# Practice Quiz Question 4
# Data: sheep fitness  
# Analysis: tests for relationships - GLM

# clears r console
rm(list=ls())
# set working directory
setwd("")
# check working directory
getwd()
# list of files in working directory
list.files()



##################################################################################################
# Generalised Linear Models
# Data: Sheep body size fitness example 

## Get the data
sheepfit <- read.csv("SheepFitness.csv", header=TRUE)
#sheep <- read.csv("SheepFitness.csv", header=TRUE)

## check data
names(sheepfit)
str(sheepfit)
summary(sheepfit)
# Data contains two continuous variables, measured body size and number of offspring (count)
####################################################
# Construct a model to investigate the relationship between the mother’s body mass 
# and the number of offspring she produced.

#1. Null Hypothesis - there is no relationship between fitness (number of offspring) and female body size 
####################################################
#--- MARK 1
####################################################
#2. Plot data
plot(fitness ~ body.size, data=sheepfit)
hist(sheepfit$body.size)
hist(sheepfit$fitness)

mean(sheepfit$fitness)
sd(sheepfit$fitness)

boxplot(sheepfit$fitness) # outliers
boxplot(sheepfit$body.size)

####################################################
#--- MARK 2
####################################################
#3.1 Preform regression test
sheepmod.lm <- lm(fitness ~ body.size, data=sheepfit)
summary(sheepmod.lm) 

####################################################
#4.1 model "validation"
hist(resid(sheepmod.lm)) # do residulas look normal - YES

par(mfrow=c(1,1))
plot(sheepmod.lm)
## strange patterns in residuals vs fitted and deviation from normal in Q-Q plot.
## offspring is count data so it would be better to model using poisson glm.
####################################################
#3.2 Preform regression test
## Now do it 'right' and fit a glm
sheepmod.glm <- glm(fitness ~ body.size, data=sheepfit, family=poisson)
summary(sheepmod.glm)

####################################################
#4.2 model "validation"
par(mfrow=c(1,1))
hist(resid(sheepmod.glm)) # do residulas look normal - YES

par(mfrow=c(2,2))
plot(sheepmod.glm)
## much better residuals patterns

#--- MARK 4
####################################################
#5. Report results and interpretation
summary(sheepmod.glm)
# Pseudo-R2 = (Null deviance-Residual deviance)/Null deviance
(85.081-48.042)/85.081
# Pseudo-R2 = 0.4353381

# Make new X data
min(sheepfit$body.size)
max(sheepfit$body.size)
newX<-data.frame(body.size = seq(from=48, to=90, length=1000)) # new x for body.size
newX
# Predictions of fitness using model
newY<-predict(sheepmod.glm, newdata=newX, type="response", se=T)
# confidence intervals
pred.plus<-newY$fit+2*newY$se
pred.minus<-newY$fit-2*newY$se

# Housekeeping
add.these2plot<-cbind(newX, newY, pred.plus, pred.minus)
add.these2plot
head(add.these2plot)

# Plot data
par(mfrow=c(1,1))
plot(fitness~body.size, data=sheepfit, xlab="Body size", ylab="Number of offspring") 
lines(fit ~ body.size, data=add.these2plot)
lines(pred.plus ~ body.size, data=add.these2plot)
lines(pred.minus ~ body.size, data=add.these2plot)

## Report results
# Body size was found to be a significant predictor of female sheep fitness as measured by 
# the number of offspring (Pseudo-R2=0.435, P<0.001)

#--- MARK 3

## OR ggplot2
library(ggplot2)
ggplot(sheepfit, aes(x=body.size, y=fitness)) + 
  geom_point() +
  ylab("Sheep fitness") +
  xlab("Female body size") +
  geom_line(data=add.these2plot, aes(body.size, fit), linetype=1) +
  geom_line(data=add.these2plot, aes(body.size, pred.plus), linetype=2) +
  geom_line(data=add.these2plot, aes(body.size, pred.minus), linetype=2) 

