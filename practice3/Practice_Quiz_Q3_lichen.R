# DD/MM/YY Your name
# Practice Quiz Question 3
# Data - lichen & polution 


##################################################################################################
# clears r console
rm(list=ls())
# set working directory
setwd("")
# check working directory
getwd()
# list of files in working directory
list.files()

#############################################################################################

lichen<-read.csv("lichen.csv") # import data

#### PART 1
# check data 
names(lichen)
summary(lichen)
str(lichen)


# test for normality 
hist(lichen$growth.rate)
shapiro.test(lichen$growth.rate)
# growth rate is significantly different to normal distribution (W = 0.975, p-value = 0.002)

hist(lichen$polution.index)
shapiro.test(lichen$polution.index)
# pollution index is significantly different to normal distribution (W = 0.963, p-value < 0.001)

hist(lichen$NH3)
shapiro.test(lichen$NH3)
# atmospheric ammonia is significantly different to normal distribution (W = 0.962, p-value < 0.001)

hist(lichen$bacteria)
shapiro.test(lichen$bacteria)
# bacteria number is significantly different to normal distribution (W = 0.966, p-value = 0.001)

hist(lichen$NOx)
shapiro.test(lichen$NOx)
# oxides of nitrogen  is significantly different to normal distribution (W = 0.946, p-value < 0.001)

# remove NAs
lichen<-na.omit(lichen)

### TESTING CORRELATIONS ####
### I would expect some text on what was found, not just the correlation plots

# pairs plot to view all relationships  
par(mfrow=c(1,1))
pairs(lichen[2:6])

# correlation matrix
cor(lichen[2:6], use="na.or.complete", method="spearman") # if NA not removed previously
cor(lichen[2:6], method="spearman")

## Three strongest correlations are;
# NH3 and NOx - strong positive relationship
# NH3 and polution.index - strong positive relationship
# NH3 and growth.rate - strong negative relationship

#############################################################################################
### TESTING AND REPORTING THE THREE STRONGEST CORRELATIONS

# Null hypothesis: There is no relationship between atmospheric ammonia (NH3) and oxides of nitrogen (NOx)
cor.test(lichen$NH3,lichen$NOx, method="spearman")
# RESULTS STATEMENT: There was a significant positive correlation between the NH3 and NOx (Spearmans; n=177, p<0.001, rho=0.87) 

# Null hypothesis: There is no relationship between atmospheric ammonia (NH3) and the pollution index
cor.test(lichen$NH3,lichen$polution.index, method="spearman")
# RESULTS STATEMENT: There was a significant positive correlation between the NH3 and pollution index (Spearmans; n=177, p<0.001, rho=0.80) 

# Null hypothesis: There is no relationship between atmospheric ammonia (NH3) and lichen growth rate
cor.test(lichen$NH3,lichen$growth.rate, method="spearman")
# RESULTS STATEMENT: There was a significant negative correlation between the NH3 and lichen growth rate (Spearmans; n=177, p<0.001, rho=-0.79) 

#############################################################################################
###### PART 2
### TESTING GROWTH RATE AND POLLUTION CAUSAL RELATIONSHIP (I.E. LINEAR MODEL)

### ANSWER SHOULD INCLUDE A NULL HYPOTHESIS
#### linear model
mod1<-lm(growth.rate ~ polution.index, data=lichen)
summary(mod1)

#############################################################################################
### REPORTING GROWTH RATE AND POLLUTION CAUSAL RELATIONSHIP (I.E. LINEAR MODEL)

# RESULTS STATEMENT: Pollution index was a significant negative predictor of lichen 
# growth rate (cm2/yr) (n=177, R2=0.57, P<0.001). As pollution index increased lichen growth rate reduced.

#############################################################################################
### PLOTTING RELATIONSHIP WITH FITTED LINE, ALSO NEED GROWTH RATE UNITS 

plot(growth.rate ~ polution.index, data=lichen, ylab="Growth rate (cm2/year)", xlab="Pollution index")
abline(mod1)

library(ggplot2)
ggplot(lichen, aes(x=polution.index, y=growth.rate)) + 
  geom_point() +
  geom_smooth(method='lm') +
  labs(y="Growth rate (cm2/year)", x="Pollution index")

#############################################################################################
