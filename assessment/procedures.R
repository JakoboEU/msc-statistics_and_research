####################
# One sample t-test
####################

daily.intake <- c(5260, 5470, 5640, 6180, 6390, 6515, 6805, 7515, 7515, 8230, 8770)

# Explore
# -------
boxplot(daily.intake)
hist(daily.intake) 
mean(daily.intake)
sd(daily.intake)

shapiro.test(daily.intake) 

# Do Test
# -------
t.test(daily.intake, mu=7725)

# Report
# ------
# The energy intake of wild Impala (mean±sd, 6754±1142) was significant difference 
# to the zoo recommended energy intake (7725 Kj/day) (t=-2.821, df=10, p=0.018).

boxplot(daily.intake, ylab="Wild energy intake (Kj/day)") # boxplot with y-axis label 
abline(h=7725, lty=3, lwd=2, col="grey") # horizontal line in plot at y=7725 
legend('topleft', c("Zoo intake"), lty=3, lwd=2, col="grey", bty='n', cex=1) # legend 

######################
# Two-sample t-test
######################
data(mtcars)

# Explore
# -------
names(mtcars)
str(mtcars)

ggplot(mtcars, aes(x=mpg)) +
  geom_histogram(binwidth=2, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(mpg)), color="red", linetype="dashed", size=1)

ggplot(mtcars, aes(x=am, y=mpg, group=am)) + 
  geom_boxplot() +
  xlab("Manual vs Automatic") + ylab("Fuel Economy (mpg)") +
  scale_x_discrete(limits = c(0, 1), labels=c("Automatic", "Manual")) 

tapply(mtcars$mpg, mtcars$am, FUN=mean)
tapply(mtcars$mpg, mtcars$am, FUN=sd)

shapiro.test(mtcars$mpg)
bartlett.test(mtcars$mpg, mtcars$am)

# Do Test
# -------
t.test(mtcars$mpg~mtcars$am)

# Report
# ------
# The mpg of automatic cars  (mean±sd, 17.14737±3.833966) vs manual cars (mean±sd, 24.39231±6.166504) was significant difference 
#  (t=-3.7671, df=18.332, p=0.001374).
ggplot(mtcars, aes(x=am, y=mpg, group=am)) + 
  geom_boxplot() +
  xlab("Manual vs Automatic") + ylab("Fuel Economy (mpg)") +
  scale_x_discrete(limits = c(0, 1), labels=c("Automatic", "Manual")) +
  stat_summary(aes(label=..y..), fun.y=mean, geom="text", size=4, vjust=-0.9)

###############
# Paired t-test
###############
subjects<-seq(from=1, to=15, by=1) 
before<-c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3, 14.2, 13.7, 16.8, 12.9, 14.5) 
after<-c(12.0, 12.2, 11.2, 13.0, 15.0, 15.8, 12.2, 13.4, 12.9, 11.0, 13.2, 13.4, 13.4, 11.2, 13.5)

# Explore
# -------
mean(before)
sd(before) 
mean(after) 
sd(after)

par(mfrow=c(1,2))
boxplot(before, ylim=c(10,20), main="Before")
boxplot(after, ylim=c(10,20), main="After")

par(mfrow=c(2,1)) 
hist(before)
hist(after)

# Do Test
# -------
t.test(before,after, paired=TRUE)

# Report
# ------
# There is a significant difference in blood pressure before (mean±SD, 14.5±2.1) and after (12.9±1.3) exercise (t=6.0561, df=14, p<0.001).

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

#######################################################
# Mann-Whitney U test (aka Wilcoxon) – non-parametric
#######################################################
beest<-read.csv("../practice1/wildebeest2.csv")

# Explore
# -------
str(beest)
hist(beest$horn, breaks=10)
ggplot(beest, aes(x=horn)) +
  geom_histogram(binwidth=4, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(horn)), color="red", linetype="dashed", size=1)

boxplot(beest$horn~beest$sex)

tapply(beest$horn, beest$sex, FUN=mean)
tapply(beest$horn, beest$sex, FUN=sd)

shapiro.test(beest$horn)
bartlett.test(beest$horn, beest$sex) 

# Try data transformations
horn.log<-log10(beest$horn)
horn.sqrt<-sqrt(beest$horn)
new.beest<-cbind(beest, horn.log, horn.sqrt)

hist(new.beest$horn.log)
shapiro.test(new.beest$horn.log) # still very significantly different from normal 

hist(new.beest$horn.sqrt)
shapiro.test(new.beest$horn.sqrt) # still very significantly different from normal 

# Do Test
# -------
wilcox.test(beest$horn~beest$sex)

# Report
# ------
ggplot(beest, aes(factor(sex), horn)) + 
  geom_boxplot() + 
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab("Horn length (cm)")


#########################################
# One-way ANOVA (analysis of variance) (A)
#########################################
Insect<-read.csv("../full_practice/InsectSprays.csv")
Insect

# Explore
# -------
summary(Insect)
str(Insect) 
hist(Insect$count)

boxplot(Insect$count~Insect$spray) 

shapiro.test(Insect$count)
bartlett.test(Insect$count~Insect$spray)

# Try data transformations
count.log<-log10(Insect$count+1)
count.sqrt<-sqrt(Insect$count)
new.Insect<-cbind(Insect, count.log, count.sqrt)

shapiro.test(new.Insect$count.log) # significantly different from a normal distribution (W=0.946, p=0.004).

shapiro.test(new.Insect$count.sqrt) # sqrt transformed data normally distributed (W=0.967, p=0.058).

bartlett.test(new.Insect$count.sqrt~Insect$spray)

# Do Test
# -------
anova1<-aov(count.sqrt~spray, data=new.Insect)
summary(anova1) 

# Report
# ------
# Insect density was significantly different in areas treated with different
# insect sprays (F=44.8, df=5,66, p<0.001), with densities significantly 
# lower in treatments C, D and E.
library(multcomp)
cld(glht(anova1, linfct=mcp(spray="Tukey")))

boxplot (Insect$count~Insect$spray, ylab="No. of insects", xlab="Spray treatment")

#########################################
# One-way ANOVA (analysis of variance) (B)
#########################################
data(iris) 

# Explore
# -------
iris 
names(iris) 
str(iris)

tapply(iris$Sepal.Width, iris$Species, FUN=mean)
tapply(iris$Sepal.Width, iris$Species, FUN=sd)
hist(iris$Sepal.Width) 
boxplot(iris$Sepal.Width~iris$Species)

shapiro.test(iris$Sepal.Width)
bartlett.test(iris$Sepal.Width, iris$Species)

# Do Test
# -------
anova1<-aov(Sepal.Width~Species, data=iris)
summary(anova1)

# Report
# ------
library(multcomp)
cld(glht(anova1, linfct=mcp(Species="Tukey")))
# Sepal width was significantly different between all species (F=49.16; df= 2,147; p<0.001).
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

SW.sum <- summarySE(iris, measurevar="Sepal.Width", groupvars=c("Species")) 

ggplot(SW.sum, aes(x=Species, y=Sepal.Width)) +
  geom_errorbar(aes(ymin=Sepal.Width-ci, ymax=Sepal.Width+ci), width=.1) + geom_point() +
  scale_y_continuous(name="Sepal Width (mm)") +
  annotate("text", x = 1, y = 3.6, label = "c") +
  annotate("text", x = 2, y = 2.9, label = "a") + annotate("text", x = 3, y = 3.1, label = "b") + theme_bw()

##################
# Two-way ANOVA (A)
##################
moths <- read_csv('../week3/moths.csv')

# Explore
# -------
summary(moths)
head(moths)

ggplot(moths, aes(Location, moth_number, fill=Bait)) + 
  geom_boxplot()  +
  ylab("Moth Count")

interaction.plot(moths$Location, moths$Bait, moths$moth_number)

shapiro.test(moths$moth_number)
bartlett.test(moths$moth_number, moths$Location, moths$Bait)

# Do Test
# -------
output1<-aov(moth_number~Location+Bait, data=moths) 
summary(output1)

output2<-aov(moth_number~Location*Bait, data=moths) 
summary(output2)

anova(output1, output2)

# Report
# ------

##################
# Two-way ANOVA (B)
##################
raccoon<-read.csv("../full_practice/Raccoon.csv")

# Explore
# -------
tapply(raccoon$weight, raccoon$sex, FUN=mean)
tapply(raccoon$weight, raccoon$sex, FUN=sd)
tapply(raccoon$weight, raccoon$pop, FUN=mean)
tapply(raccoon$weight, raccoon$pop, FUN=sd)

boxplot(raccoon$weight~raccoon$sex)
boxplot(raccoon$weight~ raccoon$pop)
boxplot(raccoon$weight~raccoon$pop+raccoon$sex)

shapiro.test(raccoon$weight)
bartlett.test(raccoon$weight ~ raccoon$sex)
bartlett.test(raccoon$weight ~ raccoon$pop)


interaction.plot(raccoon$pop,raccoon$sex,raccoon$weight)

# Do Test
# -------
mod1<-aov(raccoon$weight~raccoon$sex+raccoon$pop)
anova(mod1)
mod2<-aov(raccoon$weight~raccoon$sex*raccoon$pop)
anova(mod2)
anova(mod1, mod2)

# Report
# ------
# We used 2-way ANOVA to test the effect of sex and population location on raccoon weight. There was a significant
# main effect of sex on raccoon weight (F=207.97, df=1,95, p<0.001) with females being heavier (mean+/- SD, females
# 41.2+/-0.62, males 39.6+/-0.48). However, population location was not significant (F=0.11, df=3,95, P=0.95). There was 
# no significant interaction between sex and population (F=0.68, p=0.569).
boxplot(raccoon$weight~raccoon$sex, ylab="Raccoon weight (lb)")

#######################################
# Kruskal-Wallis test (non-parametric)
#######################################
Insect<-read.csv("../full_practice/InsectSprays.csv")
Insect

# Explore
# -------
summary(Insect)
str(Insect) 
hist(Insect$count)

boxplot(Insect$count~Insect$spray) 

shapiro.test(Insect$count)
bartlett.test(Insect$count~Insect$spray)

# Do Test
# -------
kruskal.test(Insect$count~Insect$spray)
pairwise.wilcox.test(Insect$count, Insect$spray)

# Report
# ------
boxplot (Insect$count~Insect$spray, ylab="No. of insects", xlab="Spray treatment")

#####################
# Chi-squared test
#####################
titanic<-read.csv("../full_practice/Titanic.csv")

# Explore
# -------
names(titanic)
str(titanic$Age)

boxplot(titanic$Freq~titanic$Sex)
boxplot(titanic$Freq~titanic$Survived)
boxplot(titanic$Freq~titanic$Class)

## MAKE A TABLE OF THE DATA (A)
males<-subset(titanic, titanic$Sex=="Male") # subset males only
adultmales<-subset(males, males$Age=="Adult") # subset adults
males2<-tapply(adultmales$Freq,list(adultmales$Survived, adultmales$Class), FUN=sum)

## MAKE A TABLE OF THE DATA (B)
titantic_ds <- titanic[which(titanic$Sex=="Male" & titanic$Age=="Adult"),c(2,5,6)]
titantic_ds
titantic_tab <- with(titantic_ds, tapply(Freq, list(Survived, Class), FUN = identity))

# Do Test
# -------
chisq.test(males2)
chisq.test(males2)$expected

# Report
# ------
# There was a significant association between class of male passenger and their survival (chi squared
# 37.99, df=3, p<0.001), with more 1st class passengers surviving thann the expected number
# and fewer 2nd class surviving than expected.

######################
# Spearman correlation
######################
lichen <- read_csv("../practice3/lichen.csv")

# Explore
# -------
summary(lichen)

lichen_continuous<-na.omit(lichen[,2:6])
nrow(lichen_continuous)

pairs(lichen_continuous)

barplot(lichen$growth.rate)
barplot(lichen$polution.index)
barplot(lichen$NH3)
barplot(lichen$bacteria)
barplot(lichen$NOx)

lichen.shapiro.test <- sapply(lichen_continuous, FUN=shapiro.test)
lichen.shapiro.test[2,]


cor(lichen_continuous, method="spearman")

# Do Test
# -------
cor.test(lichen_continuous$NOx, lichen_continuous$NH3, method="spearman")
cor.test(lichen_continuous$polution.index, lichen_continuous$NH3, method="spearman")
cor.test(lichen_continuous$growth.rate, lichen_continuous$NH3, method="spearman")

# Report
# ------
# The three strongest significant correlations in the lichen data are a positive correlation 
# between NOx and NH3 (Spearmans; n=177, p<0.001, rho=0.86), a positive correlation between 
# polution.index and NH3 (Spearmans; n=177, p<0.001, rho=0.80) and a negative correlation between
# growth.rate and NH3 (Spearmans; n=177, p<0.001, rho=-0.78)
lichen.result.graph <- melt(lichen_continuous[,c(1,2,3,5)], measure.vars=c("polution.index", "growth.rate", "NOx"))

variable.names <- list(
  'polution.index'="Measure of pollution (no units)",
  'growth.rate'="Lichen growth rate (cm2/yr)",
  'NOx'="Oxides of nitrogen (g/ha/yr)"
)

ggplot(lichen.result.graph, aes(x=NH3, y=value, color=variable)) + 
  labs(title="Significant Correlations with Atmospheric Ammonia") +
  facet_grid(variable~., labeller=function(variable,value){ return(variable.names[value]);}) +
  geom_point() +
  ylab("") +
  xlab("atmospheric ammonia (ppb)") +
  guides(color=FALSE)

######################
# Pearsons correlation
######################
USArrests <- read_csv("../week4/USArrests.csv")

# Explore
# -------
str(USArrests)
mean(USArrests$Murder)  
sd(USArrests$Murder)  
mean(USArrests$UrbanPop)
sd(USArrests$UrbanPop)

shapiro.test(USArrests$Murder)
shapiro.test(USArrests$UrbanPop)

shapiro.test(sqrt(USArrests$Murder))
shapiro.test(log(USArrests$Murder)) 

ggplot(USArrests, aes(x=UrbanPop, y=Murder)) + 
  geom_point()

ggplot(USArrests, aes(x=UrbanPop, y=sqrt(Murder))) + 
  geom_point()

# Do Test
# -------
cor.test(USArrests$UrbanPop, USArrests$Murder)

# Report
# ------
nrow(USArrests)
# There is not a significant correlation between urban population and murders in the USA
# (Pearson's; n=50, p = 0.6312, rho=0.0696)

######################
# Regression
######################
fat<-read.csv("../full_practice/bodyfat.csv")
names(fat)

# Explore
# -------
str(fat)
summary(fat)
head(fat)
names(fat)

par(mfrow=c(1,1))
plot(Perc_bodyfat~age, data=fat)
plot(Perc_bodyfat~weight, data=fat) 

library(car)
scatterplotMatrix(~+age+weight+height+chest+hip+ankle, data=fat,  diagonal=list(method="boxplot"))
hist(fat$hip)
hist(fat$ankle)
shapiro.test((fat$hip))
shapiro.test((fat$ankle))

## hip is not very normally distributed so will try with a log transformation
lghip<-log(bodyfat$hip)
lgankle<-log(bodyfat$ankle)
fat<-cbind(bodyfat,lghip,lgankle)
scatterplotMatrix(~age+weight+height+chest+lghip+lgankle, data=bodyfat, diagonal=list(method="boxplot"))
shapiro.test((fat$lghip))
shapiro.test((fat$lgankle))


str(fat)
bodyfat_ds <- fat[c(2,4,5,6,7,19,20)]
str(bodyfat_ds)

pairs(bodyfat_ds[2:7])
cor(bodyfat_ds[2:7])

shapiro.test <- sapply(bodyfat_ds[2:7], FUN=shapiro.test)
shapiro.test[2,]

cor.test(fat$weight, bodyfat$chest) 
cor.test(fat$weight, bodyfat$hip)
cor.test(fat, bodyfat$hip) 
## So hip and chest are strongly correlated with each other (r=0.83) and with weight (hip r=0.94; chest r=0.89)
# I'm going to leave out hip and chest from the model. You could also choose to leave out a different 


# Do Test
# -------
mod1<-lm(Perc_bodyfat~age+weight+height+lgankle, data=fat)
summary(mod1)

## Remove lgankle as not significant
mod2<-lm(Perc_bodyfat~age+weight+height, data=fat)
summary(mod2)

# VALIDATE MODEL
hist(resid(mod2))
par(mfrow=c(2,2))
plot(mod2)

# Reportgemo
# ------
# Age, weight, and height explained 58% of the variation in body fat (adj Rsq=0.58, 
# F=115, df= 3,249, p<0.001). There is a significnat positive relationship between body fat and age (t=4.838, p<0.001),
# and weight (t=17.124, p<0.001), and a significant negative relationship between body fat and height (t=-8.067, p<0.001).
