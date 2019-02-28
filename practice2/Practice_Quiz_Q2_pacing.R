# DD/MM/YY Your name
# Data: enrichment dataset 
# Analysis: tests for differences 

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

## Null Hypothesis: There is no significant difference in animal behaviour (pacing) between the differnet types of enrichment 

####################################################
# preliminary data exploration and visualisation

enrich<-read.csv("enrich.csv")  # import data 

# check data
names(enrich)
str(enrich)

hist(enrich$pacing, breaks=10)
# Or in ggplot
library(ggplot2)
ggplot(enrich, aes(x=pacing)) +
  geom_histogram(binwidth=4, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(pacing)), color="red", linetype="dashed", size=1)
# Quick boxplot 
boxplot(enrich$pacing~enrich$type)

# mean and sd per group
tapply(enrich$pacing, enrich$type, FUN=mean)
tapply(enrich$pacing, enrich$type, FUN=sd)

####################################################
## testing the assumptions and reporting

# Test for normality  
shapiro.test(enrich$pacing) 
# Not significantly different from normal (W = 0.96, p-value = 0.210)

# Test for homogeneity of variances
bartlett.test(enrich$pacing, enrich$type) # No significant difference in variance between treatment groups (K-squared=3.0, df=2, p-value=0.224)

####################################################
## selection and carrying out correct tests

# ANOVA to test for differences in the three groups
anova.pace<-aov(pacing~type, data=enrich)
summary(anova.pace) # significant difference between groups

# To find out which groups are different need to do a pos-hoc test 
# multcomp package to impliment a Tukey pos-hoc test
library(multcomp)

cld(glht(anova.pace, linfct=mcp(type="Tukey")))

####################################################
## reporting your results and presenting the data.

# RESULTS STATEMENT: Animals paced significantly less with enrichment treatment one (mean+/-SD, 21.61+/-3.5)
# than the other two treamtent types (F=11.82, df=2,31, p=0.002). There was no difference in pacing 
# behaviour between treatment two (30.4+/-5.6) and three (27.4+/-5.4).

## PLOT DATA 
# Create a plot to display your result
ggplot(enrich, aes(factor(type), pacing)) + 
  geom_boxplot() + 
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab("Pacing behaviour (min)")

boxplot(enrich$pacing~enrich$type, ylab="Pacing behaviour (min)", ylim=c(10,40))
text(1,28, "21.6")
text(2,40, "30.4")
text(3,37, "27.4")

##################################################################################################
