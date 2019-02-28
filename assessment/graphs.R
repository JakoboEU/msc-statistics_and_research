
# Histogram
# ---------

ggplot(mtcars, aes(x=mpg)) +
  geom_histogram(binwidth=2, colour="black", fill="white") + 
  geom_vline(aes(xintercept=mean(mpg)), color="red", linetype="dashed", size=1)

ggplot(spermnum, aes(x=spermnum)) + 
  geom_histogram(bins=10)


# Box plot
# --------

ggplot(dataframe, aes(x=am, y=mpg, group=am)) + 
  geom_boxplot() +
  xlab("Manual vs Automatic") + ylab("Fuel Economy (mpg)") +
  scale_x_discrete(limits = c(0, 1), labels=c("Automatic", "Manual")) +
  stat_summary(aes(label=..y..), fun.y=mean, geom="text", size=4, vjust=-0.9)

ggplot(blood.long2, aes(x=Condition, y=BP, group=Condition)) + 
  geom_boxplot() +
  xlab("Before and After Exercise") + ylab("Blood Presure") +
  scale_x_discrete(limits = c("before", "after"), labels=c("Before", "After")) +
  stat_summary(aes(label=..y..), fun.y=mean, geom="text", size=4, vjust=-0.9)

ggplot(moths, aes(Location, moth_number)) + 
  geom_boxplot()  +
  ylab("Moth Count") 


# Bar Plot
# --------

ggplot(notvirginica, aes(x = Petal.Width, fill=Species)) + 
  geom_bar() + 
  facet_wrap(~Species, ncol=1)

ggplot(SW.sum, aes(x=Species, y=Sepal.Width)) +
  geom_bar(position=position_dodge(), stat="identity") + 
  geom_errorbar(aes(ymin=Sepal.Width-ci, ymax=Sepal.Width+ci), width=.1) + 
  theme_bw()


# Scatter
# -------

ggplot(SW.sum, aes(x=Species, y=Sepal.Width)) +
  geom_errorbar(aes(ymin=Sepal.Width-ci, ymax=Sepal.Width+ci), width=.1) + 
  geom_point() +
  theme_bw()

ggplot(squirrels, aes(x=brother.rs, y=sister.rs)) + 
  geom_point()

ggplot(spermnum, aes(x=time, y=spermnum)) + 
  xlab("Amount of time until disturbed (minutes)") +
  ylab("Number of sperm transferred") + 
  geom_point()

ggplot(spermnum, aes(x=time, y=spermnum)) + 
  xlab("Amount of time until disturbed (minutes)") +
  ylab("Number of sperm transferred") + 
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

ggplot(sleep, aes(x=log10(BodyWt), y=TotalSleep)) + 
  xlab("Log10 of Body Weight") +
  ylab("Total amount of time asleep") + 
  geom_point(aes(color=as.factor(Danger))) + 
  stat_smooth(aes(color=as.factor(Danger)), method = "lm", fullrange=TRUE)
