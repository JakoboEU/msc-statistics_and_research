# DD/MM/YY Your name
# Data: Full Practice Quiz 1 

# clear r console
rm(list=ls())
# set working directory     
setwd("")
# check working directory
getwd()
# list files in working directory
list.files()



####################################################################################
#                                   Question 1
####################################################################################
##Differences in insect density with insect sprays

# import data (load default R dataset)
Insect<-read.csv("InsectSprays.csv")
Insect

##NULL hypothesis: There is no significant difference in the insect density between spray treatments
####################################################################################
## Mark = 1 hypothesis
####################################################################################

#EXPLORE DATA
summary(Insect)
str(Insect)  ## continuous varible (count), spray with 6 factors.
hist(Insect$count) ##doesn't look normally distributed

boxplot(Insect$count~Insect$spray) ## looks like there are some differences - CDE look diff to ABF

####################################################################################
## Mark = 2 preliminary data exploration and visualisation
####################################################################################

## TEST FOR  NORMALITY 
shapiro.test(Insect$count)
## The distribution of insect counts is significantly different from a 
# normal distribution (W=0.922, p<0.001).
## TEST FOR GROUP VARIANCES - HOMOGENEITY 
bartlett.test(Insect$count~Insect$spray)
## The variation between groups is significantly different (K-squared = 25.96, df = 5, p<0.001).

######################################################
## Transformations
# Log10 transform  - creating a new object 
count.log<-log10(Insect$count+1) # you would need to add 1 to the data to preform log as you can't log zeros 
# you would not loss marks for not adding the 1 as we haven't covered this before
# Square root transform  - creating a new object 
count.sqrt<-sqrt(Insect$count)
# Use cbind (column bind) to add the new objects to Insect dataframe
new.Insect<-cbind(Insect, count.log, count.sqrt)
# re-test assumptions
shapiro.test(new.Insect$count.log)
## significantly different from a normal distribution (W=0.946, p=0.004).

shapiro.test(new.Insect$count.sqrt)
## sqrt transformed data normally distributed (W=0.967, p=0.058).
bartlett.test(new.Insect$count.sqrt~Insect$spray)
## variation between groups normally distributed
####################################################################################
## Mark = 3 for tesing the assumptions 
####################################################################################

## TESTING FOR DIFFERENCES - more than two groups and continuous variable, not normal
# therefore use a Kruskal Wallis test
kruskal.test(Insect$count~Insect$spray)

pairwise.wilcox.test(Insect$count, Insect$spray)
####################################################################################
## Mark = 2 for using kruskal on non-normal data  
####################################################################################

boxplot (Insect$count~Insect$spray, ylab="No. of insects", xlab="Spray treatment")
## Insect density was significantly different in areas treated with different
# insect sprays (Chi-sq=54.69, df=5, p<0.001), with densities significantly 
# lower in treatments C, D and E.
####################################################################################
## Mark = 2 for reporting results corectly and plotting 
####################################################################################

## TESTING FOR DIFFERENCES - more than two groups and continuous variable, transformed data normal
# therefore could use a one-way anova test
# ANOVA to test for differences
anova1<-aov(count.sqrt~spray, data=new.Insect)
summary(anova1) # significant difference between groups
## Insect density was significantly different in areas treated with different
# insect sprays (F=44.8, df=5,66, p<0.001), with densities significantly 
# lower in treatments C, D and E.
####################################################################################
## Mark = would also accept anova on sqrt transformed data 
####################################################################################

# Could use package multcomp to impliment a Tukey pos-hoc test
library(multcomp)
cld(glht(anova1, linfct=mcp(spray="Tukey")))



################################################################################
#                               QUESTION 2
################################################################################
# import data (load default R dataset)
raccoon<-read.csv("Raccoon.csv")
names(raccoon)
summary(raccoon)
str(raccoon)

##dataset has 100 obs of three variables. 52 females, 48 males. Uneven, but similar sample size in pops (21-30)

##NULL hypothesis: There is no significant difference in raccoon weight between sexes and population location

####################################################################################
## Mark = 1
####################################################################################

##DATA EXPLORATION
tapply(raccoon$weight, raccoon$sex, FUN=mean)
tapply(raccoon$weight, raccoon$sex, FUN=sd)
tapply(raccoon$weight, raccoon$pop, FUN=mean)
tapply(raccoon$weight, raccoon$pop, FUN=sd)

boxplot(raccoon$weight~raccoon$sex)
boxplot(raccoon$weight~ raccoon$pop)
boxplot(raccoon$weight~raccoon$pop+raccoon$sex)
##looks like no difference in weight between pops but females are heavier
####################################################################################
## Mark = 1
####################################################################################

## TESTING ASSUMPTIONS
shapiro.test(raccoon$weight)
bartlett.test(raccoon$weight ~ raccoon$sex)
# Raccoon weight is normally distributed (W=0.985, p=0.298) and variances are homogenous
#between factor levels (Bartlett test - sex K-sq=2.954, p=0.086, df=1)
bartlett.test(raccoon$weight ~ raccoon$pop)
# variances are also homogenous between factor levels (Bartlett test - pop K-sq=0.879, p=0.831., df=3)
####################################################################################
## Mark = 3
####################################################################################

##Testing for effects of two factors on weight of raccoons, so will use TWO WAY ANOVA. The assumptions are met 

## INTERACTION PLOT
interaction.plot(raccoon$pop,raccoon$sex,raccoon$weight)
## There appears to be an effect of sex, and there may be a very small interaction of population
# with north and south populations appearing different.

mod1<-aov(raccoon$weight~raccoon$sex+raccoon$pop)
anova(mod1)
mod2<-aov(raccoon$weight~raccoon$sex*raccoon$pop)
anova(mod2)
anova(mod1, mod2)

####################################################################################
## Mark = 3
####################################################################################

boxplot(raccoon$weight~raccoon$sex, ylab="Raccoon weight (lb)")

## REPORT RESULTS
# We used 2-way ANOVA to test the effect of sex and population location on raccoon weight. There was a significant
# main effect of sex on raccoon weight (F=207.97, df=1,95, p<0.001) with females being heavier (mean+/- SD, females
# 41.2+/-0.62, males 39.6+/-0.48). However, population location was not significant (F=0.11, df=3,95, P=0.95). There was 
# no significant interaction between sex and population (F=0.68, p=0.569).

####################################################################################
## Mark = 2
####################################################################################

mod3<-aov(raccoon$weight~raccoon$sex)
anova(mod3)
## if you also removed population location and re-run the test I'd also accept you reporting this output



################################################################################
#                               QUESTION 3
################################################################################ 
# 1A) There was a significant relationship between the difference in species richness between de-embanked
# and reference marshes and time, with the difference decreasing with time and approaching zero 
# after approximately 120 years (Fig. 2, R2=21.9%, p=0.05). The species richness of de-embanked sites
# declined significnatly with the percentage cover of Spartina anglica (R2=30.3, p=0.018).
####################################################################################
## Mark = 2
####################################################################################

#1B) 
-5.01+0.037*136
# difference in species richness is 0
####################################################################################
## Mark = 1
####################################################################################


#1C) I do not have much confidnece in the relationships because the R2 values for both Figs are small, 
#particulalry for Fig 2, and the p value for Fig 2 is large. The sample size is not very large. 
# The relationship in Fig 3 is very heavily influenced by one outlying value at 80%.
####################################################################################
## Mark = 2
####################################################################################

##2A) Overall, MAT, drought-stress, MAP and TCM explained 17% of the variation in NPP (adj Rsq=0.17, 
#F=6.751, df= 8,217, p<0.001). There is a significnat positive relationship between NPP and MAT (t=-3.22, p<0.001),
# and drought-stress (t=-3.82, p<0.001), and a significant negative relationships between NPP and MAP (t=-2.01, p=0.046).
# TCM was not a significant predictor of NPP.
####################################################################################
## Mark = 3
####################################################################################

##2B) I would remove the non-significant predictor (NPP) and re-run the model. If all other variables remained significant I
# would look at the model validation plots. I would be looking for the residuals in the q-q plot to fit the line and 
# I would check that there were no outliers having undue influence on the model (cooks distance).
####################################################################################
## Mark = 2
####################################################################################


################################################################################
#                               QUESTION 4
################################################################################ 
fat<-read.csv("bodyfat.csv")
names(fat)

##NULL hypothesis: There is no relationship between body fat and the expalnatory variables 
####################################################################################
## Mark = 1 
####################################################################################
str(fat)
summary(fat)
head(fat)
names(fat)

# restrict to only those variables we want to analyse 
fat <- fat[c(2,4:9)]

## VIEW DATA 
par(mfrow=c(1,1))
plot(Perc_bodyfat~age, data=fat)
plot(Perc_bodyfat~weight, data=fat) # etc or you could use scatterplotMatrix

library(car)
scatterplotMatrix(~+age+weight+height+chest+hip+ankle, data=fat,  diagonal=list(method="boxplot"))
## hip is not very normally distributed so will try with a log transformation
hist(fat$hip)
hist(fat$ankle)
shapiro.test((fat$hip))
shapiro.test((fat$ankle))
lghip<-log(fat$hip)
lgankle<-log(fat$ankle)
shapiro.test((fat$lghip))
shapiro.test((fat$lgankle))

scatterplotMatrix(~age+weight+height+chest+lghip+lgankle, data=fat, diagonal=list(method="boxplot"))
## Thats a little bit better as the boxplots are more centred. Logging is not essential as the 
# the data don't look too bad to begin with.
fat<-cbind(fat,lghip,lgankle)
####################################################################################
## Mark = 1 
####################################################################################

## CHECK FOR COLINEARITY OF EXPLANTORY VARIABLES
library(corrplot)
M<-cor(fat[2:9], method="pearson") # create matrix of pearson correlations - could also use spearsons as we have some non-normal data 
corrplot(M, method="number", type="lower")
## So hip and chest are strongly correlated with each other (r=0.83) and with weight (hip r=0.94; chest r=0.89)
# I'm going to leave out hip and chest from the model. You could also choose to leave out a different 
# combination of correlated variables, as long as you justify you reasoning. 
####################################################################################
## Mark = 2
####################################################################################

# MODELS
mod1<-lm(Perc_bodyfat~age+weight+height+lgankle, data=fat)
summary(mod1)

## Remove lgankle as not significant
mod2<-lm(Perc_bodyfat~age+weight+height, data=fat)
summary(mod2)

# VALIDATE MODEL
hist(resid(mod2))
par(mfrow=c(2,2))
plot(mod2)
# Doesn't look too bad. Residual vs Fitted doesn't show any patterns, Q-Q plot is pretty straight
# Two points are influencing the Cook's distance (214 and 40) but generally ok. 
####################################################################################
## Mark = 4
####################################################################################

## REPORTING RESULTS
# Age, weight, and height explained 58% of the variation in body fat (adj Rsq=0.58, 
# F=115, df= 3,249, p<0.001). There is a significnat positive relationship between body fat and age (t=4.838, p<0.001),
# and weight (t=17.124, p<0.001), and a significant negative relationship between body fat and height (t=-8.067, p<0.001).
####################################################################################
## Mark = 2
####################################################################################


###############################################################################
#                             QUESTION 5                                      
###############################################################################
titanic<-read.csv("Titanic.csv")
##NULL hypothesis: There is no association between passenger sex and survival 
####################################################################################
## Mark = 1 
####################################################################################
names(titanic)
str(titanic$Age)

boxplot(titanic$Freq~titanic$Sex)
boxplot(titanic$Freq~titanic$Survived)
boxplot(titanic$Freq~titanic$Class)
####################################################################################
## Mark = 1 
####################################################################################

## We have two categorical variables but the data is not in the right format for a chi-sq test

## MAKE A TABLE OF THE DATA
males<-subset(titanic, titanic$Sex=="Male") # subset males only
adultmales<-subset(males, males$Age=="Adult") # subset adults
males2<-tapply(adultmales$Freq,list(adultmales$Survived, adultmales$Class), FUN=sum)
## I could have typed the data into an oject instead - see below
male_class<-as.table(rbind (c(118,154,387,670) , c(57,14,75,192)))
####################################################################################
## Mark = 4 
####################################################################################

## CHI SQ TEST as we have two categorical variables.
chisq.test(males2)
chisq.test(males2)$expected
####################################################################################
## Mark = 2 
####################################################################################

## There was a significant association between class of male passenger and their survival (chi squared
# 37.99, df=3, p<0.001), with more 1st class passengers surviving thann the expected number
# and fewer 2nd class surviving than expected.
####################################################################################
## Mark = 2 
####################################################################################
