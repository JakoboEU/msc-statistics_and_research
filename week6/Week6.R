# Date: 19/02/2019 
# Name: James Richardson
# Student Number: 18057447

# Analysis: Multivariate statistics

# Script Set Up
# -------------
# clear r console
rm(list=ls())
# set working directory
setwd("/Users/jamese.richardson/Projects/Masters/statistics_and_research/week6")
# list files in working directory
list.files() 

# Load required libaries
library(tidyverse)
library(ggplot2)
library(vegan)


# Data:  varespec
data(varespec)


# Running the PCA:
# -----------------
# Running the ordination in R is simple if you have set up your data well 
# (this built in dataset is nicely set up, no missing, mistaken or odd values).
# We have several options for running PCA in R, for this course we will use the rda function (prcomp is another option).

# PCA analysis using rda method 
lichen.pca <- rda(varespec)
lichen.pca # gives the basic PCA result 
summary(lichen.pca) #

# The first line runs the PCA and creates an object called lichen.pca that holds the results. 
# Calling this object in the second line of code above gives the basic results,
# the total variation (also known as inertia) explained by the ordination (1826) 
# and the amount of variation explained by each ordination axis (also called an Eigenvalue). 
# What we would look for here is the first two or three axis explaining the vast majority of the variation.
#
# In the third line of code above we use the summary command to get the full results of the PCA. 
# This gives us all 23 axes and their contribution to the total variation explained. 
# It also gives us the PCA axis scores for each species and for each site in the dataset. 
# We can then visualise these scores using the plot function.


# Plotting the PCA:
# -----------------
plot(lichen.pca, scaling = -1)
plot(lichen.pca, display = "species", scaling = -1) 
plot(lichen.pca, display = "sites", scaling = -1) 
biplot(lichen.pca, scaling = -1)

# The first plot shows both the sites and species scores for the first two ordination axes. 
# The second plot displays just the species while the third plot is just the sites.
# The fourth plot is a biplot, this shows both species and sites but gives arrows indicating the direction 
# of influence of each species on the position of sites.
# 
# Which plot you use in a report depends on your aims, if you are only interested in the site differences 
# in composition you might opt for the plot that only displays the sites. 
# If you want to report which sites are more associated with different species then the biplot 
# would be more appropriate.
# You would also need to report the variation explained by each of the axis in your plot. 
# In this case we could calculate this from those give in the results or simply look at the 
# ‘Proportion Explained’ in the summary.
# Variation explained by axis one = 982.98/1826 Variation explained by axis one = 464.30/1826

# Environmental variables:
# ------------------------
# We can also explore the influence of multiple environmental variables on our species composition by using 
# the envfit function in the vegan package. 
# The varespec dataset also comes with an environmental dataset called varechem, 
# load this now and have a quick look at the variables.
# The function envfit will not only test the significance of each environmental variable against 
# the community composition calculated in the PCA it will also provide a very easy method of plotting environmental 
# vectors on to our ordination plot.
# Run the envfit function as show below and then run the ef object and look at your results, 
# how many environmental variables are having a significant influence on the species composition in the PCA?
# Now run the plotting functions shown below. 
# Try changing the p.max values and re-run the plotting code to see what this does to your plotted environmental vectors.

par(mfrow=c(1,1)) 
data(varechem) # load built in dataset
ef <- envfit(lichen.pca, varechem, permu = 999) 
ef
plot(lichen.pca, display = "sites") 
plot(ef, p.max = 0.1)

#############################################
# Non-metric multidimensional scaling (NMDS)
#############################################
# The next dataset we will use for this practical is call ‘marsh.csv’ and can be downloaded from Moodle. 
# Load this data in R and explore the variable names and attributes. 
marsh <- read_csv("marsh.csv")
names(marsh)
summary(marsh)

# This dataset contains species and environmental data from three saltmarsh sites in Ireland. 
# The first column gives the individual quadrat names; 
# the next 32 columns contain the species data and the remaining 12 columns are 
# environmental data and site factors (marsh name and type).
#
#
# The ‘type’ column has two categories, natural and managed. 
# Try plotting two or three of the environmental variables against site ‘type’. 
# Do you notice any obvious differences between natural and managed marsh?
ggplot(marsh, aes(x=type, y=Burrow.index)) + 
  geom_boxplot()

ggplot(marsh, aes(x=type, y=Soil.strgth)) + 
  geom_boxplot()

ggplot(marsh, aes(x=type, y=Plant.hght)) + 
  geom_boxplot()

ggplot(marsh, aes(x=type, y=Bio.mass)) + 
  geom_boxplot()

# Next pick one of the species columns and try plotting this against two or three of the environmental variables. 
# Do you find any strong patterns?
ggplot(marsh, aes(x=V.Agr.stol, y=Bio.mass)) + 
  geom_boxplot()

ggplot(marsh, aes(x=V.Agr.stol, y=Burrow.index)) + 
  geom_boxplot()

# If we carried on plotting like this to explore the data we would end up with a large number of plots, 
# and if you followed each plot with a statistical test (e.g. t-tests and/or regressions)
# we would have a massive amount of confusing interpretation to carry out!
# The vegan package allows us to calculate some basic diversity metrics using the specnumber and diversity functions. 
# These are really useful ways to calculate simple metrics (see code below).


# diversity metrics: species richness, Shannon diversity
spp.rich <- specnumber(marsh[2:33], MARGIN=1) # 2:33 selects just the species data 
shannon <- diversity(marsh[2:33])

# These objects can now easily be added to our “marsh” dataset using the cbind function. 
# Once you have added these objects to the marsh dataset create two boxplots to view species richness 
# and Shannon diversity for marsh type.
marsh <- cbind(marsh, spp.rich, shannon)
summary(marsh)

ggplot(marsh, aes(x=type, y=shannon)) + 
  geom_boxplot()

ggplot(marsh, aes(x=type, y=spp.rich)) + 
  geom_boxplot()

# Now we will carry out a NMDS ordination on the species data. 
# The metaMDS code below runs the ordination, k=2 restricts the analysis to two axes 
# and trymax=30 limits the number of runs/iterations to 30 
# (for very large datasets you can reduce this to speed up the computation time).
## NMDS ordination
marsh.nmds <- metaMDS(marsh[2:33], k=2, trymax = 30) 

scores(marsh.nmds)

marsh.nmds # look at the results output 

# Call:
#   metaMDS(comm = marsh[2:33], k = 2, trymax = 30) 
# 
# global Multidimensional Scaling using monoMDS
# 
# Data:     wisconsin(sqrt(marsh[2:33])) 
# Distance: bray 
# 
# Dimensions: 2 
# Stress:     0.1627012 
# Stress type 1, weak ties
# Two convergent solutions found after 22 tries
# Scaling: centring, PC rotation, halfchange scaling 
# Species: expanded scores based on ‘wisconsin(sqrt(marsh[2:33]))’ 

# The important things to note here are the Stress value and the Distance used. 
# Stress is a way of assessing the fit of the ordination, the lower the stress the better. 
# A general rule of thumb for NMDS is that stress values above 0.2 suggest the ordination 
# should be treated with caution and those above 0.3 suggest the placement of points on the plot 
# is very unlikely to be representing the true dissimilarity.
#
# The distance is the dissimilarity measure used in the ordination, in this case we went for the default Bray Curtis distance. 
# Run “?metaMDS” to see other distance measures you could have used.
# R can very easily calculate other dissimilarity measures and these can be used in the NMDS analysis. 
# For example, you could use the following code to first calculate a dissimilarity 
# matrix using the gower distance and then use this in the metaMDS function.
gower.dist <- vegdist(marsh[2:33], method="gower", upper=TRUE) # beta diversity matrix
gower.nmds <- metaMDS(gower.dist, k=2, trymax = 30)
# It all depends on which aspects of the composition you want to emphasis in the analysis.
# We should also check the NMDS scores, as we selected k=2 we only get axis 1 and 2 scores, 
# all the possible variation in the community data is fitted on to these two axes.
#
#
# Lets look at some plotting options. 
# The vegan package gives us some really nice plotting tools such as oriellipse, ordihull and oridispider. 
# See the code below for a couple of these and try out ordihull for yourselves.

## NMDS plot
plot(marsh.nmds, display = "sites")
with(marsh, ordiellipse(marsh.nmds, site, col=4, lwd=2, draw = "polygon", kind = c("sd"))) 
with(marsh, ordispider(marsh.nmds, site, label=TRUE))

# Ordiellipse has drawn polygons around the site centroids. 
# This polygon represents the standard deviation of point scores for that site. 
# Ordispider has drawn a line from each point back to the site centroid so you can visualise the spread of each site.

# Try changing the “site” part of the ordiellipse and oridspider with “type”. 
# What can you tell from this plot?
plot.new()

plot(marsh.nmds, display = "site")
with(marsh, ordiellipse(marsh.nmds, type, col=4, lwd=2, draw = "polygon", kind = c("sd"))) 
with(marsh, ordispider(marsh.nmds, type, label=TRUE))

# Lets add the NMDS axis 1 and 2 scores to the main “marsh” dataset

## Housekeeping: add nmds scores to ‘marsh’ dataset 
marsh.nmds.scores <- scores(marsh.nmds) 
marsh<-cbind(marsh, marsh.nmds.scores)
names(marsh)

# Simper is a nice easy tool to investigate which species are having the greatest 
# effect on community composition between pairs of sites.

# simper - discriminating species between groups 
simp<-simper(marsh[2:33], marsh$type, permutations = 999) 
summary(simp)

# Contrast: managed_natural 
#
#           average       sd  ratio      ava     avb cumsum     p    
# V.Atr.port 0.2146504 0.136593 1.5715 21.56818 58.4091 0.3615 0.001 ***
# V.Puc.mari 0.2021383 0.117244 1.7241 54.59091 19.4091 0.7020 0.001 ***
# ...
# ...


# After running the summary(simp) code you will get a table telling you each species contribution (contr) 
# to the composition differences; for this data Atr.port is the species causing most of 
# the variation between managed and natural sites. 
# The av.a and av.b tells you the average abundances per group. 
# The final column gives you a p-value so you can tell which species are significantly contributing 
# to the variation between site types.
#
# The next test we can easily undertake using our NMDS ordination is an Analysis of Similarities (anosim). 
# This provides a way to test significantly whether two groups (site types in our case) are significant different.

# anosim – Analysis of similarity
sim.type<-anosim(marsh[2:33], marsh$type, permutations = 999, distance = "bray") 
summary(sim.type)

# Call:
#   anosim(x = marsh[2:33], grouping = marsh$type, permutations = 999,      distance = "bray") 
# Dissimilarity: bray 
# 
# ANOSIM statistic R: 0.397 
# Significance: 0.001
# 
# Permutation: free
# Number of permutations: 999
# 
# Upper quantiles of permutations (null model):
#   90%    95%  97.5%    99% 
# 0.0576 0.0801 0.1032 0.1381 
# 
# Dissimilarity ranks between and within classes:
#         0%     25%     50%      75% 100%   N
# Between  5 843.250 1407.25 1786.250 2141 968
# managed  2 348.125  809.75 1308.625 2141 946
# natural  1 371.000  879.00 1375.500 2141 231


# The key results to take away from the summary are the ANOSIM statistic and the significance (p-value). 
# There are other ways to calculate similar statistics so if this is something you will do in the future 
# look at the function adonis, the technique of multivariate analysis of variance (MANOVA) 
# and especially the R-package “mvabund”.

# Environmental variables:
# ------------------------
# Now we want to check out the environmental variables in the dataset, we can do this with the envfit function. 
# The variables measured were all taken to learn something about the erosion/stability potential on coastal marsh
# and were carried out along a strong tidal gradient so you would expect 
# a fair amount of correlation between these variables. 
# What code could you use to look at the correlation in these variables in one single plot? 
pairs(marsh[34:43])
cor(marsh[34:43])
# Below we will again use the envfit function to look at the environmental data.

# envfit: Environmental variables fitted to an ordination
ef <- envfit(marsh.nmds, marsh[34:43], na.rm = TRUE, permu = 999) 
ef
# Plot the fitted vectors
plot(marsh.nmds, display = "sites")
plot(ef, p.max = 0.05)

# Ok, if we run the envfit object, ef, we get the results of our fitted vectors (env variables). 
# In this nicely constructed example all the variables are highly significant (p<0.001). 
# We can view the vectors with the above plotting code. 
# You can see from the plot of environmental vectors that many are correlated and are mainly 
# acting on axis 1 of the ordination. 
# This is a great example of where we could use PCA to reduce the environmental data to a single compound variable 
# (we will look at that soon).
# Another way to plot environmental variables on to your ordination plots is to use surface plotting 
# via the ordisurf function. 
# Like topographical mapping lines, ordisurf will plot an environmental variable as a surface over your ordination! 
# Awesome right! Lets have a go

# Fit environmental surface to ordination
plot(marsh.nmds, display = "sites", type="n")
with(marsh, ordihull(marsh.nmds, type, col = "black",lty = 2))
with(marsh, ordispider(marsh.nmds, type, label=TRUE))
with(marsh, ordisurf(marsh.nmds, Bio.mass, add = TRUE, col = "cornflowerblue"))

# What a great plot! 
# What we have done is plot all the variation in saltmarsh vegetation composition for both managed and natural marsh, 
# we have also demarcated the total compositional variation for each marsh type using ordihull and ordispider 
# and then shown a clear gradient of biomass as a surface! 
# Wow! 
# This shows that sites that have greater axis 1 scores have greater biomass scores than those with lower axis 1 scores, 
# and that natural sites mostly have greater biomass than managed sites. 
# Again, wow!
# Using these techniques you are able to clearly visualise 
# and summarise patterns using massive multivariate datasets.
# There are many plotting options we can use, have a go at changing the plotting functions, colours etc. 
# Below is another option for plotting points with different colours.

plot(marsh.nmds, display = "sites", type="n") 
points(marsh.nmds.scores[marsh$type=="managed",],pch=16, col="cornflowerblue") 
points(marsh.nmds.scores[marsh$type=="natural",],pch=16, col="green") 
with(marsh, ordisurf(marsh.nmds, Bio.mass, add = TRUE, col = "red"))

# For the next part of the practical we will try to reduce all the environmental data down to a 
# single compound variable that indicates the erosion/stability potential of coastal marsh. 
# To do this we need to use a PCA on only the environmental data. 
# We can also quickly plot the PCA to investigate differences in only environmental factors 
# [note: Environmental factors are now species in this PCA].


# PCA on env variable: data reduction 
env.pca <- rda(marsh[34:43], scale=TRUE) 
summary(env.pca)
biplot(env.pca)

# Ok, again we can see that the majority of the environmental vectors are running along the first axis. 
# Also, from the summary we can see that axis 1 explains the vast majority of the explained variance (eigenvalue). 
# This is good news, we can now extract the PCA scores and use these in further analysis.

# Extract PCA scores 
env.pca.scores<-scores(env.pca) 
env.pca.scores$sites
marsh<-cbind(marsh, env.pca.scores$sites) 
names(marsh)
names(marsh)[48] <- "stability.pc1" # change PC1 column name

# We can change the name of the PC1 variable to ‘stability.pc1’ as this compound viable 
# is telling us something about the marsh stability. 
# Try plotting this new variable with the species richness variable (spp.rich) 
# we calculated and combined with the “marsh” dataset earlier.
# You could also try modeling this relationship.
# 
ggplot(marsh, aes(x=stability.pc1, y=spp.rich)) + 
  geom_point()

hist(marsh$spp.rich)
hist(marsh$stability.pc1)

cor.test(marsh$spp.rich, marsh$stability.pc1, method="spearman")


marsh.model.glm <- glm(spp.rich~stability.pc1, data=marsh, family = poisson)
par(mfrow=c(2,2)) 
plot(marsh.model.glm)

summary(marsh.model.glm)

# Remember from the lecture that NMDS can be calculated with other dissimilarity measures. 
# We used the default Bray Curtis dissimilarity for the NMDS above, 
# now lets try it with the binary (presence/absence) Jaccard’s dissimilarity. 
# This measure will convert the abundance data to 1’s and 0’s.

# Jaccard dissimilarity
marsh.jacc<-vegdist(marsh[2:33], "jaccard", binary=TRUE) # dissimilarity calculation for species # data columns
# Rerun the ordination
par(mfrow=c(1,1)) 
jacc.nmds <- metaMDS(marsh.jacc, k=2, trymax = 30)
plot(jacc.nmds, display = "sites")
with(marsh, ordiellipse(jacc.nmds, site, col=4, lwd=2, draw = "polygon", kind = c("sd"))) 
with(marsh, ordispider(jacc.nmds, site, label=TRUE))

# You should be able to see from this new ordination that differences between marsh types are smaller 
# when using Jaccard’s dissimilarity (i.e. there is more overlap in polygons).
# We can use Procrustes rotation to evaluate the two different NMDS ordinations (Bray Curtis and Jaccards). 
# This test will tell us how strongly correlated these two ordinations are, 
# the higher the Procrustes correlation value the more similar the ordinations are. 
# With the code below we get a value of 0.85 suggesting that they are highly correlated.

proT1<-protest(marsh.nmds, jacc.nmds, permutations = 999) 
proT1
# Call:
#   protest(X = marsh.nmds, Y = jacc.nmds, permutations = 999) 
# 
# Procrustes Sum of Squares (m12 squared):        0.2842 
# Correlation in a symmetric Procrustes rotation: 0.846 
# Significance:  0.001 
# 
# Permutation: free
# Number of permutations: 999

plot(proT1)

