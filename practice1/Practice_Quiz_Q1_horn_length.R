# DD/MM/YY Your name
# Practice Quiz Question 1
# Data - wildebeest horn length

##################################################################################################
# clears r console
rm(list=ls())
# set working directory
setwd("")
# check working directory
getwd()
# list of files in working directory
list.files()

##################################################################################################
# Section 1
# Two groups: Wildebeests Male and Female
# Measured variable: horn length 

## Null Hypothesis: There is no significant difference in horn size between male and females

beest<-read.csv("wildebeest2.csv") # read in data

str(beest) # investigate data type

hist(beest$horn, breaks=10) # investigate data distribution, does not look normal

# Or in ggplot
library(ggplot2)
ggplot(beest, aes(x=horn)) +
  geom_histogram(binwidth=4, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(horn)), color="red", linetype="dashed", size=1)

# lets quickly check the data 
boxplot(beest$horn~beest$sex)
# look likes there is a difference between sexes

# We can use the tapply function to calculate the mean and the sd groups:
tapply(beest$horn, beest$sex, FUN=mean)
tapply(beest$horn, beest$sex, FUN=sd)

# Test for normality  
shapiro.test(beest$horn) #  significantly different from normal 

# Test for homogeneity of variances
bartlett.test(beest$horn, beest$sex) # no significant difference in variance between data sets

# Try data transformations
# Log10 transform 
horn.log<-log10(beest$horn)
# Square root transform 
horn.sqrt<-sqrt(beest$horn)
# Use cbind (column bind) to add the new objects to our dataframe
new.beest<-cbind(beest, horn.log, horn.sqrt)

# Test for normality  
hist(new.beest$horn.log)
shapiro.test(new.beest$horn.log) # still very significantly different from normal 

hist(new.beest$horn.sqrt)
shapiro.test(new.beest$horn.sqrt) # still very significantly different from normal 

# Go back to our orginal data
# Carry out non-parametric Mann-Whitney test (Wilcoxon)
wilcox.test(beest$horn~beest$sex)
## Horn length was significantly different between male and female 
## wildebeest (mean±sd; male=59.5±4.5, female=39.8±5.2, W=5; p<0.001)

# Create a plot to display your result
ggplot(beest, aes(factor(sex), horn)) + 
  geom_boxplot() + 
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab("Horn length (cm)")

# Or with standard R plotting
boxplot(beest$horn~beest$sex, ylab="Horn length (cm)", ylim=c(20,80))
##################################################################################################



