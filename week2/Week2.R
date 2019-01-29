# Date: 22/01/2019 
# Name: James Richardson
# Data: Various
# Analysis: Testing for differences Part 1: Comparing data with one categorical and one continuous variable

#
#
# One categorical variable and One continuous variable
# ----------------------------------------------------
# 2 Groups?
  # Yes -> Data Normal?
    # No ->  ** Wilcoxon Rank test
    # Yes -> Is data paired?
      # Yes -> ** Paired t-test
      # No -> ** Two sample independent t-test
  # No -> Data Normal?
    # Yes -> ** Anova
    # No -> ** Kruskal-Wallis

#########
# Set up
#########

# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/week2")
# list files in working directory
list.files() 

# Load libraries we require
library(tidyverse)
library(ggplot2)

####################
# One sample t-test
####################

# Enter data manually (Impala energy intake)
daily.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)

# Data: Impala energy intake 
# Analysis: one-sample t-test
# Null Hypothesis: there is no significant difference between wild Impala energy intake and the 
# recommended energy intake in captivity.
boxplot(daily.intake) # roughly what is the mean?
hist(daily.intake) # plot a histogram, does the data look vaguely normally distributed?
mean(daily.intake) # write down the mean? sd(daily.intake) # write down the standard deviation?
# mean = 6753.636

# Test for normality
# ------------------
# Is this data significantly different from normal?
shapiro.test(daily.intake) 

# Shapiro-Wilk normality test
# data:  daily.intake
# W = 0.95237, p-value = 0.6743
#
# The p-value for our Shapiro test comes back much greater than 0.05 
# so we can be confident that our data is NOT significantly different from normal
## ----------------------
# So our data is normal, we can proceed with a one-sample t-test.
# ----------------------

# One sample t-test 
t.test(daily.intake, mu=7725)

# One Sample t-test
# data:  daily.intake
# t = -2.8208, df = 10, p-value = 0.01814
# alternative hypothesis: true mean is not equal to 7725
# 95 percent confidence interval:
#  5986.348 7520.925
# sample estimates:
#  mean of x 
#6753.636 

# The test output gives you the value of the t-statistic, 
# the degrees of freedom and finally the significance value (p-value), 
# which tells you there is a significant difference between the wild Impala data and the recommended zoo energy intake.

# RESULTS STATEMENT: 
# ------------------
# The energy intake of wild Impala (mean±sd, 6754±1142) was significant difference 
# to the zoo recommended energy intake (7725 Kj/day) (t=-2.821, df=10, p=0.018).

# create pdf saved in working directory 
pdf("Impala.intake.pdf", width = 8, height = 6) 
boxplot(daily.intake, ylab="Wild energy intake (Kj/day)") # boxplot with y-axis label 
abline(h=7725, lty=3, lwd=2, col="grey") # horizontal line in plot at y=7725 
legend('topleft', c("Zoo intake"), lty=3, lwd=2, col="grey", bty='n', cex=1) # legend 
dev.off() # close plotting element

######################
# Two-sample t-test
######################

# Data: mtcars dataset
# Analysis: Two-sample t-test 
# Null Hypothesis: there is no significant difference in the mpg between automatic and manual cars  (0=automatic, 1=manual).
data(mtcars)
mtcars
names(mtcars)
str(mtcars) # what is this function doing? hist(mtcars$mpg) # histogram of fuel economy
# Histogram in ggplot
library(ggplot2) # open library for ggplot2 package 
ggplot(mtcars, aes(x=mpg)) +
  geom_histogram(binwidth=2, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(mpg)), color="red", linetype="dashed", size=1)

# Boxplot for automatic and manual cars 
boxplot(mtcars$mpg~mtcars$am)
# We can use the tapply function to calculate the mean and the sd of groups: 
tapply(mtcars$mpg, mtcars$am, FUN=mean)
# 0        1 
# 17.14737 24.39231 
tapply(mtcars$mpg, mtcars$am, FUN=sd)
# 0        1 
# 3.833966 6.166504 

# Before we can carry out our two-sample t-test, we must be satisfied that the data meet the assumptions of the t-test;
# 1) The data are continuous
# 2) The data are approximately normally distributed
# 3) The variances of the two sets of data are homogeneous (the same)
# We already know that our mpg variable is a continuous dataset. Looking at the histogram
# and boxplot we have created for this data would you say that the dataset meets the other two assumptions (discuss with a neighbour)?
  
# Test for normality 
shapiro.test(mtcars$mpg) # P-value is greater than 0.05 so the mpg variable is not significantly different from normal
# Test for homogeneity of variances 
bartlett.test(mtcars$mpg, mtcars$am) # P>0.05 so the variances of the two groups are not significantly different?


# Two sample t-test 
t.test(mtcars$mpg~mtcars$am)

# Welch Two Sample t-test
# 
# data:  mtcars$mpg by mtcars$am
# t = -3.7671, df = 18.332, p-value = 0.001374
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -11.280194  -3.209684
# sample estimates:
#   mean in group 0 mean in group 1 
# 17.14737        24.39231 

# RESULTS STATEMENT: 
# ------------------
# The mpg of automatic cars  (mean±sd, 17.14737±3.833966) vs manual cars (mean±sd, 24.39231±6.166504) was significant difference 
#  (t=-3.7671, df=18.332, p=0.001374).

# create pdf saved in working directory 
pdf("Automatic_vs_manual.pdf", width = 8, height = 6) 
ggplot(mtcars, aes(x=am, y=mpg, group=am)) + 
  geom_boxplot() +
  xlab("Manual vs Automatic") + ylab("Fuel Economy (mpg)") +
  scale_x_discrete(limits = c(0, 1), labels=c("Automatic", "Manual")) +
  stat_summary(aes(label=..y..), fun.y=mean, geom="text", size=4, vjust=-0.9)

dev.off() # close plotting element


###############
# Paired t-test
###############
# When comparing measurements taken from the same subject at two different times.

# Data: Blood pressure (n=15 athletes)
# Analysis: Paired t-test
# Null Hypothesis: there is no improvement in blood pressure after exercise.


# Create three objects in R (‘subjects’, 'before' and 'after') with the following data
subjects<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15) 
# OR 
subjects<-seq(from=1, to=15, by=1) 
before<-c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3, 14.2, 13.7, 16.8, 12.9, 14.5) 
after<-c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0, 13.2, 13.4, 13.4, 11.2, 13.5)

mean(before) # 14.46
sd(before) # 2.071852
mean(after) # 12.89333
sd(after) # 1.341889

# plot two boxplots next to each other
par(mfrow=c(1,2)) # plots are arranged in 1 row and 2 columns 
boxplot(before, ylim=c(10,20), main="Before")
boxplot(after, ylim=c(10,20), main="After")
# plot two histograms stacked 
par(mfrow=c(2,1)) 
hist(before)
hist(after)

# Paired t-tests have the same assumptions as standard t-test and from the boxplot and histogram - the data look roughly ok to proceed.
# Paired t-test 
t.test(before,after, paired=TRUE)

# Paired t-test
# 
# data:  before and after
# t = 6.0561, df = 14, p-value = 2.956e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.011831 2.121502
# sample estimates:
#   mean of the differences 
# 1.566667 

# RESULTS STATEMENT: 
# ------------------
# There is a significant difference in blood pressure before (mean±SD, 14.5±2.1) and after (12.9±1.3) exercise (t=6.0561, df=14, p<0.001).


#
# We now need to do some data restructuring so we can plot our results on one graph
# ---------------------------------------------------------------------------------
# Combine the data using the data.frame function 
blood<-data.frame(subjects, before, after)
blood # view data in the console
str(blood) # check data structure
# Open the reshape2 package
library(reshape2)
# Use the melt function to change the blood dataframe from wide to long form. 
blood.long<-melt(blood, id.vars=c("subjects"), measure.vars=c("before", "after"), variable.name="Condition", value.name="BP") 
# also change the column names 
str(blood.long) 
# see how the data structure has changed
blood.long # view data in the console
par(mfrow=c(1,1)) # change plotting window back to standard single graph format 
boxplot(BP~Condition, data=blood.long)

# ---------------------------------------------------------------------------------
# OR
# ---------------------------------------------------------------------------------
library(tidyr)
blood.long2 <- gather(blood,
                           value = "BP",
                           key = "Condition",
                           before, after)

ggplot(blood.long2, aes(x=Condition, y=BP, group=Condition)) + 
  geom_boxplot() +
  xlab("Before and After Exercise") + ylab("Blood Presure") +
  scale_x_discrete(limits = c("before", "after"), labels=c("Before", "After")) +
  stat_summary(aes(label=..y..), fun.y=mean, geom="text", size=4, vjust=-0.9)


#######################################################
# Mann-Whitney U test (aka Wilcoxon) – non-parametric
#######################################################

# Lets continue looking at two groups but now we will look at some data that does not meet the assumptions of the t-test.
# You may be familiar with the iris dataset (morphological variables of three plant species) built into R 
# (it was used in the R Basics Bonus Lab). 
# First load the data and then using the subset function we are going to separate the two species we are interested in,
# setosa and versicolor. 
# We are going to test the difference in petal width between setosa and veriscolor. 
# Make sure you write a clear hypothesis for your test.


# Data: Iris dataset
# Analysis: Mann-Whitney U test (aka Wilcoxon) – non-parametric
# Null Hypothesis: There is no significant difference in the petal width between the setosa and veriscolor species

data(iris)
iris
names(iris)  # examine variable names 
str(iris)  # examine the data structure
# subset only those two species we are interested in 
notvirginica<-subset(iris,Species!="virginica") # removes virginica species

ggplot(notvirginica, aes(Species, Petal.Width)) + 
  geom_boxplot()  +
  ylab("Petal width (cm)") +
  stat_summary(aes(label=..y..), fun.y=mean, geom="text", size=4)

ggplot(notvirginica, aes(x = Petal.Width, fill=Species)) + 
  geom_bar() + 
  facet_wrap(~Species, ncol=1)

# Do you think there will be a difference between species?
# Do you think the data will conform to the t-test assumptions of normality and homogeneity of variances?
shapiro.test(notvirginica$Petal.Width) # W = 0.82554, p-value = 1.634e-09
bartlett.test(notvirginica$Petal.Width, notvirginica$Species) # Bartlett's K-squared = 18.066, df = 1, p-value = 2.134e-05

# Lets try to transform the data and see if that helps with our normality issues.
# --------------------------------------------------------------------------------
# Log10 transform petal width - creating a new object called PW.log 
PW.log<-log10(notvirginica$Petal.Width)
# Square root transform petal width - creating a new object called PW.sqrt 
PW.sqrt<-sqrt(notvirginica$Petal.Width)
# Use cbind (column bind) to add the new objects to our dataframe 
new.notvirginica<-cbind(notvirginica, PW.log, PW.sqrt)
# Test transformed data for normality
hist(new.notvirginica$PW.log)
shapiro.test(new.notvirginica$PW.log) # still very significantly different from normal
hist(new.notvirginica$PW.sqrt)
shapiro.test(new.notvirginica$PW.sqrt) # still very significantly different from normal

# Ok, so our data are not normally distributed and the variance between groups are not equal.
# -------------------------------------------------------------------------------------------
# We now need to turn to a non-parametric Mann-Whitney U test, 
# which does not make assumptions about normality and homogeneity of variances. 
# It is a rank test, which means it converts the raw data into ranks before testing it. 
# These types of tests are generally less powerful than their parametric equivalents 
# but are excellent when your data fail the assumptions of t-tests and ANOVAs 
# (note: a one-way ANOVA carried out on two groups is equivalent to a two-sample t-test).

# Lets try the Mann-Whitney on our original (non-transformed) data.
# -----------------------------------------------------------------
wilcox.test(notvirginica$Petal.Width~notvirginica$Species)
# W = 0, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

# The p-value is much smaller than 0.05 so you should reject your null hypothesis, the two groups are significantly different.
mean(notvirginica[notvirginica$Species=="versicolor",]$Petal.Width)
sd(notvirginica[notvirginica$Species=="versicolor",]$Petal.Width)
mean(notvirginica[notvirginica$Species=="setosa",]$Petal.Width)
sd(notvirginica[notvirginica$Species=="setosa",]$Petal.Width)

# RESULTS STATEMENT: 
# ------------------
# There is a significant difference in the petal width between
# versicolor (mean±SD, 1.326±0.1977527) and setosa (0.246±0.1053856) (p-value < 0.001).


#########################################
# One-way ANOVA (analysis of variance)
#########################################
# Data: iris dataset
# One-way ANOVA
# Null Hypothesis: There is no significant difference in sepal width between three species of iris
data(iris) 
iris 
names(iris) 
str(iris)
# We can use the tapply function to calculate the mean and the standard deviation of groups:
tapply(iris$Sepal.Width, iris$Species, FUN=mean)
# setosa versicolor  virginica 
# 3.428      2.770      2.974 
tapply(iris$Sepal.Width, iris$Species, FUN=sd)
# setosa versicolor  virginica 
# 0.3790644  0.3137983  0.3224966 
hist(iris$Sepal.Width) 
boxplot(iris$Sepal.Width~iris$Species)

# Now we need to examine the test assumptions for the ANOVA, which are the same we looked at earlier for t-tests
# 1. The data are continuous
# 2. At least approximately normally distributed
# 3. The variances of the groups are homogenous
# Test for normality
shapiro.test(iris$Sepal.Width) # NOT significantly different from normal
# Test for homogeneity of variances
bartlett.test(iris$Sepal.Width, iris$Species) # NO significant difference in variance

# Ok, so our data meet the ANOVA assumptions, we can proceed with the test.
# ANOVA to test for differences in the three groups 
anova1<-aov(Sepal.Width~Species, data=iris)
summary(anova1) # use the summary function to extract your ANOVA results

#               Df Sum Sq Mean Sq F value Pr(>F)    
# Species       2  11.35   5.672   49.16 <2e-16 ***
# Residuals   147  16.96   0.115                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# The summary function gives us the critical F-value, p-value and the degrees of freedom we need to report in our results statement.

# Our ANOVA results tell us there is a difference but it does not tell us which groups are different from each other, 
# for this we will need to do a post hoc test. 
# There are many possible post hoc tests but for simplicity we will use the commonly applied Tukey post hoc test 
# implemented using the multcomp package.

# Tukey post hoc test 
library(multcomp)
# carry out post hoc test on our anova1 object – Species are our iris categories 
cld(glht(anova1, linfct=mcp(Species="Tukey")))
# setosa versicolor  virginica 
# "c"        "a"        "b" 

# If groups have the SAME letter they are NOT significantly different. 
# If they have different letters they are significantly different. 
# So we would write this formally as: 
# Sepal width was significantly different between all species (F=49.16; df= 2,147; p<0.001).



# To report the results in a plot we could use a boxplot, 
# or we could use a mean plot with error bars, or a bar graph with error bars. 
# Using the following function obtained from the R Cookbook website we can very quickly produce either of these error bar plots.
# http://www.cookbook-r.com/Manipulating_data/Summarizing_data/ its under the ‘Using ddply’ heading.
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
# Summarise data
SW.sum <- summarySE(iris, measurevar="Sepal.Width", groupvars=c("Species")) 
SW.sum

# Mean CI plot
ggplot(SW.sum, aes(x=Species, y=Sepal.Width)) +
  geom_errorbar(aes(ymin=Sepal.Width-ci, ymax=Sepal.Width+ci), width=.1) + 
  geom_point() +
  theme_bw()

# Bar graph with CI
ggplot(SW.sum, aes(x=Species, y=Sepal.Width)) +
  geom_bar(position=position_dodge(), stat="identity") + 
  geom_errorbar(aes(ymin=Sepal.Width-ci, ymax=Sepal.Width+ci), width=.1) + 
  theme_bw()

# Mean CI plot
pdf("Sepal_Width_Tukey.pdf", width = 5, height = 5) 
ggplot(SW.sum, aes(x=Species, y=Sepal.Width)) +
  geom_errorbar(aes(ymin=Sepal.Width-ci, ymax=Sepal.Width+ci), width=.1) + geom_point() +
  scale_y_continuous(name="Sepal Width (mm)") +
  annotate("text", x = 1, y = 3.6, label = "c") +
  annotate("text", x = 2, y = 2.9, label = "a") + annotate("text", x = 3, y = 3.1, label = "b") + theme_bw()
dev.off()


#######################################
# Kruskal-Wallis test (non-parametric)
#######################################
# What if I have more than two groups but the data fail the ANOVA test assumptions!! 
# There is a non-parametric equivalent, the Kruskal Wallis test. 

# Data: iris dataset – 3 groups 
# Test: kruskall-wallis
# Null Hypothesis:
data(iris)
iris 
names(iris) 
str(iris)
# We can use the tapply function to calculate the mean and the sd groups: 
tapply(iris$Petal.Width, iris$Species, FUN=mean)
# setosa versicolor  virginica 
# 0.246      1.326      2.026 
tapply(iris$Petal.Width, iris$Species, FUN=sd)
# setosa versicolor  virginica 
# 0.1053856  0.1977527  0.2746501
hist(iris$Petal.Width) 
boxplot(iris$Petal.Width~iris$Species)

# Test for normality 
shapiro.test(iris$Petal.Width) # W = 0.90183, p-value = 1.68e-08 <- Significant distance
# Test for homogeneity of variances
bartlett.test(iris$Petal.Width, iris$Species) # Bartlett's K-squared = 39.213, df = 2, p-value = 3.055e-09 <- Significant distance

# Log10 transform petal width - creating a new object called PW.log 
PW.log<-log10(iris$Petal.Width)
# Square root transform petal width - creating a new object called PW.sqrt 
PW.sqrt<-sqrt(iris$Petal.Width)
# Use cbind (column bind) to add the new objects to our dataframe 
new.iris<-cbind(iris, PW.log, PW.sqrt)
# Test for normality
hist(new.iris$PW.log)
shapiro.test(new.iris$PW.log) # still very significantly different from normal
hist(new.iris$PW.sqrt)
shapiro.test(new.iris$PW.sqrt) # still very significantly different from normal

# Kruskal-Wallis test 
kruskal.test(Petal.Width~Species, data = iris) # Kruskal-Wallis chi-squared = 131.19, df = 2, p-value < 2.2e-16

# Like an ANOVA, this does not tell us which groups (in our case, Species) are significantly different from each other. 
# To do this, we need to do pairwise Mann-Whitney U (Wilcoxon) tests. 
# We will need to subset each pair of species up before applying multiple Mann- Whitney tests.
# Subset Species into pairs - then conduct separate Mann-Whitney tests 
notvirginica<- subset(iris,Species!="virginica") 
notsetosa<-subset(iris,Species!="setosa") 
notversicolor<-subset(iris,Species!="versicolor")
# Conduct separate Mann-Whitney tests on pairs of Species 
wilcox.test(notvirginica$Petal.Width~notvirginica$Species) # W = 0, p-value < 2.2e-16
wilcox.test(notsetosa$Petal.Width~notsetosa$Species) # W = 49, p-value < 2.2e-16
wilcox.test(notversicolor$Petal.Width~notversicolor$Species) # W = 0, p-value < 2.2e-16

# RESULTS STATEMENT: 
# ------------------
# Petal width was significantly different between all species (Chi=131.19; df= 2; p<0.001).

# Pairwise wilcoxon test 
#  This gives the same result but we don’t have the fun of subsetting!
pairwise.wilcox.test(iris$Petal.Width,iris$Species)
#             setosa versicolor
#  versicolor <2e-16 -         
#  virginica  <2e-16 <2e-16    

# P value adjustment method: holm 
ggplot(iris, aes(x=Species, y=Petal.Width, group=Species)) + 
  geom_boxplot() +
  xlab("Species") + ylab("Petal Width (cm)") 
