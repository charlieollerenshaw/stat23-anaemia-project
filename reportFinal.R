########################################################
#
# Written by Charlie Ollerenshaw and Miliana Bocher
#
# This file is rather lengthy given
# that it includes all exploratory data
# analysis, various attempts of clustering
# or other creation of new covariates, 
# the entire model building process
# and subsequent model evaluation.
#
#
# As such, the code is organised as
# follows: 
#
# Section 1: Exploratory Data Analysis
#
# This section largely consists of making
# new dataframes from subsections of the 
# given data and plots. These plots are 
# *NOT* final and are thus not labeled 
# to the caliber needed for this report.
# Plots used for the ultimate output are 
# at the end of this file.
# 
#
# Section 2: Model Building Process
#
# This section shows a host of different models,
# both linear and generalised linear. Models are
# numbered. 
#
#
# Section 3: Model Evaluation Process
#
# This section involves comparing some of our best
# models against one another based on metrics such 
# as AIC, R-squared, RMSE, score (using the formula
# in the project instructions), and diagnostic plots. 
#
# 
# Section 4: Preparation of File Output 
#
# This section prepares the output of this R
# code, which includes the finalised plots/figures
# used in the report (saved into 
# the marker's working directory) and the prediction file.
#
#
########################################################

####### Section 0: Importing Data and Packages ####### 

# importing packages
library(MASS)
library(ggplot2)
library(mgcv)
library(RColorBrewer)

# importing data
anemia <- read.csv("AnemiaData.csv")

# separate data into known (hbKnown) and unknown (hbUnknown) haemoglobin values
hbKnown <- subset(anemia, anemia$Haemoglobin != -1)
hbUnknown <- subset(anemia, anemia$Haemoglobin == -1)

####### Section 1: Exploratory Data Analysis ####### 

####### Section 1.1: Exploration of Haemoglobin (hb) Alone ####### 

# histogram / density plots of hb
hist(hbKnown$Haemoglobin)
plot(density(hbKnown$Haemoglobin))
abline(v = mean(hbKnown$Haemoglobin))
abline(v = median(hbKnown$Haemoglobin), col = "blue")
abline(v = quantile(hbKnown$Haemoglobin, probs = 0.25), col = "green")
abline(v = quantile(hbKnown$Haemoglobin, probs = 0.75), col = "green")

# numerical summary of hb
summary(hbKnown$Haemoglobin)
sd(hbKnown$Haemoglobin)

# extreme values of hb (based on medical literature)
hbSeverePreg <- hbKnown[hbKnown$Haemoglobin < 7 & hbKnown$Pregnant == "Yes", ]
hbSevereNonPreg <- hbKnown[hbKnown$Haemoglobin < 8 & hbKnown$Pregnant == "No", ]
summary(hbSeverePreg)
summary(hbSevereNonPreg)

####### Section 1.2: Adaptation of hbKnown for Further Analysis ####### 

# creation of animals dummy variable
hbKnown$Animals <- ifelse(hbKnown$Cows == "No" & hbKnown$Sheep == "No" & hbKnown$Horses == "No" & hbKnown$Goats == "No" & hbKnown$Chickens == "No", 0, 1)


####### Section 1.3: Exploration of Various Covariates ####### 

#### Region ####

## Plots

# violin plot of hb by region 
ggplot(data = hbKnown, mapping = aes(x = Region, y = Haemoglobin, fill = Region), ylim = ) + geom_violin() +
  xlab("Region") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

# boxplots looking at hb by region by rural designation
ggplot(data = hbKnown, mapping = aes(x = Region, y = Haemoglobin, fill = as.factor(Rural)), ylim = ) + geom_boxplot() +
  xlab("Region") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# boxplots showing potential interactions between pregnancy and region 
ggplot(data = hbKnown, mapping = aes(x = Region, y = Haemoglobin, fill = as.factor(Pregnant)), ylim = ) + geom_boxplot() +
  xlab("Region") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# boxplots showing potential interactions between animal ownership and region 
ggplot(data = hbKnown, mapping = aes(x = Region, y = Haemoglobin, fill = as.factor(Animals)), ylim = ) + geom_boxplot() +
  xlab("Region") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

## Numerical Analysis

# num of observations in each region
table(hbKnown$Region)
table(hbUnknown$Region)

# looking at summary statistics
aggregate(x = hbKnown$Haemoglobin, by = list(hbKnown$Region), FUN = function(x) {c(Mean=mean(x), VAR = var(x), SD=sd(x))})

#### Province ####

## Plots

# box plots of hb by province (cluttered)
ggplot(data = hbKnown, mapping = aes(x = Province, y = Haemoglobin, fill = Region), ylim = ) + geom_boxplot() +
  xlab("Province") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# violin plots of hb by province for the south/southeast/west regions
ggplot(data = (hbKnown[hbKnown$Region == "south" | hbKnown$Region == "southeast" | hbKnown$Region == "west", ]), mapping = aes(x = Province, y = Haemoglobin, fill = Region), ylim = ) + geom_violin() +
  xlab("Province") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# violin plots of hb by province for the north/northeast/east regions
ggplot(data = (hbKnown[hbKnown$Region == "north" | hbKnown$Region == "northeast" | hbKnown$Region == "east", ]), mapping = aes(x = Province, y = Haemoglobin, fill = Region), ylim = ) + geom_violin() +
  xlab("Province") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# violin plots of hb by province for the central/central highlands regions
ggplot(data = (hbKnown[hbKnown$Region == "central" | hbKnown$Region == "central_highlands", ]), mapping = aes(x = Province, y = Haemoglobin, fill = Region), ylim = ) + geom_violin() +
  xlab("Province") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# boxplots showing potential interactions between province and rural (with relation to hb)
ggplot(data = hbKnown, mapping = aes(x = Province, y = Haemoglobin, fill = as.factor(Rural)), ylim = ) + geom_boxplot() +
  xlab("Province") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# boxplots showing potential interactions between pregnancy and province 
ggplot(data = hbKnown, mapping = aes(x = Province, y = Haemoglobin, fill = as.factor(Pregnant)), ylim = ) + geom_boxplot() +
  xlab("Province") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# boxplots showing potential interactions between province and animal ownership (as a dummy) 
ggplot(data = hbKnown, mapping = aes(x = Province, y = Haemoglobin, fill = as.factor(Animals)), ylim = ) + geom_boxplot() +
  xlab("Province") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

#### Province Clustering by Similarity Across All Covariates ####

# turn all yes/no into 1/0 numerical values
hbKnown[hbKnown == "Yes"] <- as.numeric(1)
hbKnown[hbKnown == "No"] <- as.numeric(0)

# ensure entire columns are numeric types (for mean/sd calculation)
hbKnown$CleanWater <- as.numeric(hbKnown$CleanWater)
hbKnown$TreatedWater <- as.numeric(hbKnown$TreatedWater)
hbKnown$Electricity <- as.numeric(hbKnown$Electricity)
hbKnown$Toilet <- as.numeric(hbKnown$Toilet)
hbKnown$AnimCart <- as.numeric(hbKnown$AnimCart)
hbKnown$AgricLandOwn <- as.numeric(hbKnown$AgricLandOwn)
hbKnown$Cows <- as.numeric(hbKnown$Cows)
hbKnown$Horses <- as.numeric(hbKnown$Horses)
hbKnown$Goats <- as.numeric(hbKnown$Goats)
hbKnown$Sheep <- as.numeric(hbKnown$Sheep)
hbKnown$Chickens <- as.numeric(hbKnown$Chickens)
hbKnown$Rural <- as.numeric(hbKnown$Rural)
hbKnown$Pregnant <- as.numeric(hbKnown$Pregnant)
hbKnown$RecentBirth <- as.numeric(hbKnown$RecentBirth)

# vector of variables to be used in the clustering process
Vars <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24)

# summaries of each variable by province
ProvinceSummaries <- aggregate(hbKnown[,Vars], by = list(hbKnown$Province), FUN = function(x) {c(Mean=mean(x), SD=sd(x))})
rownames(ProvinceSummaries) <- ProvinceSummaries[,1]
ProvinceSummaries <- scale(ProvinceSummaries[,-1])

# find the distances between these summaries
Distances <- dist(ProvinceSummaries)

# cluster by combining the provinces with the smallest distances
ClusTree <- hclust(Distances, method="complete")
plot(ClusTree, xlab = "Province", ylab = "Separation")
abline(h=9, col="red", lty=2)
NewGroups <- paste("Province", cutree(ClusTree, h=9), sep="")
NewGroupsDFAll <- as.data.frame.matrix(table(rownames(ProvinceSummaries), NewGroups))
NewGroupsDFAll$Province <- rownames(NewGroupsDFAll)

# put dummy variables associated with each group into hbKnown
hbKnown <- merge(hbKnown, NewGroupsDFAll, by = "Province")

# create single factor covariate with each level corresponding to a province group
Province.Groups <- data.frame("Province" = sort(unique(hbKnown$Province)), 
                              "ProvinceGroup" = as.factor(c(1,2,1,3,1,4,3,1,1,4,3,3,1,5,3,2,6,4,3,1,7,3,4,8,6,1,3,1,1,1,4,1,9)))

# add single factor covariate into hbKnown
hbKnown <- merge(hbKnown, Province.Groups, by = "Province", all.x = T)

## Plots

# violin plots looking at hb by province group
ggplot(data = hbKnown, mapping = aes(x = ProvinceGroup, y = Haemoglobin, fill = ProvinceGroup), ylim = ) + geom_violin() +
  xlab("ProvinceGroup") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

## Numerical Analysis

# summary statistics for each variable by province group
statsByProvinceGroup <- aggregate(x = cbind(hbKnown[,Vars]), 
                                  by = list(hbKnown$ProvinceGroup), FUN = function(x) {c(Mean=mean(x), VAR = var(x), SD=sd(x))})


#### Ethnicity ####

## Plots

# boxplots showing hb levels by ethnicity
ggplot(data = hbKnown, mapping = aes(x = Ethnicity, y = Haemoglobin, fill = Ethnicity), ylim = ) + geom_boxplot() +
  xlab("Ethnicity") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

## Numerical Analysis

# num of observations for each ethnicity
table(hbKnown$Ethnicity, exclude = NULL)

# summary statistics for each variable by ethnicity
statsByEthnicity <- aggregate(x = cbind(hbKnown[,Vars]), 
                                  by = list(hbKnown$Ethnicity), FUN = function(x) {c(Mean=mean(x), VAR = var(x), SD=sd(x))})

#### Pregnancy Status ####

## Plots

# violin plot of hb by pregnancy 
ggplot(data = hbKnown, mapping = aes(x = as.factor(Pregnant), y = Haemoglobin, fill = as.factor(Pregnant)), ylim = ) + geom_violin() +
  xlab("Pregnant") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# same violin plot as above, but including a 'recent birth' interaction
ggplot(data = hbKnown, mapping = aes(x = as.factor(Pregnant), y = Haemoglobin, fill = as.factor(RecentBirth)), ylim = ) + geom_violin() +
  xlab("Pregnant") + ylab("Haemoglobin") + ggtitle("Haemoglobin by Pregnancy Status") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))


## Numerical Analysis

# summary statistics for each variable by pregnancy status
statsByPregnancy <- aggregate(x = cbind(hbKnown[,Vars]), 
                              by = list(hbKnown$Pregnant), FUN = function(x) {c(Mean=mean(x), VAR = var(x), SD=sd(x))})

# num of pregnant/non-pregnant women in dataset
table(hbKnown$Pregnant)

#### Agricultural Land Ownership Status ####

## Plots

# violin plot for hb by agricultural land ownership
ggplot(data = hbKnown, aes(x=as.factor(AgricLandOwn), y = Haemoglobin, fill = as.factor(AgricLandOwn))) + geom_violin() +
  xlab("Agricultural Land Ownership") + ylab("Haemoglobin") + 
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

# box plots for hb by province group, by agricultural land ownership
ggplot(data = hbKnown, mapping = aes(x = as.factor(ProvinceGroup), y = Haemoglobin, fill = as.factor(AgricLandOwn)), ylim = ) + geom_boxplot() +
  xlab("ProvinceGroup") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

# box plots for hb by region, by agricultural land ownership
ggplot(data = hbKnown, mapping = aes(x = Region, y = Haemoglobin, fill = as.factor(AgricLandOwn)), ylim = ) + geom_boxplot() +
  xlab("Region") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

# smoothed curve of hb and household size, by agricultural land ownership
ggplot(data = hbKnown, mapping = aes(x = HHSize, y = Haemoglobin, fill = as.factor(AgricLandOwn)), ylim = ) + geom_smooth() +
  xlab("HHSize") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

# box plots for hb by toilet access, by agricultural land ownership
ggplot(data = hbKnown, mapping = aes(x = as.factor(Toilet), y = Haemoglobin, fill = as.factor(AgricLandOwn)), ylim = ) + geom_boxplot() +
  xlab("Toilet") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# box plots for hb by electricity access, by agricultural land ownership
ggplot(data = hbKnown, mapping = aes(x = as.factor(Electricity), y = Haemoglobin, fill = as.factor(AgricLandOwn)), ylim = ) + geom_boxplot() +
  xlab("Electricity") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## Numerical Analysis

# number of observations in each group
table(hbKnown$AgricLandOwn)

###### HHUnder5s (Num of Household Members under Age 5) ######

## Plots

# box plots for hb by number of household members under age 5
ggplot(data = hbKnown, mapping = aes(x = as.factor(HHUnder5s), y = Haemoglobin, fill = as.factor(HHUnder5s)), ylim = ) + geom_boxplot() +
  xlab("HHUnder5s") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# violin plots showing the same as above, but by pregnancy status
ggplot(data = hbKnown, mapping = aes(x = as.factor(HHUnder5s), y = Haemoglobin, fill = as.factor(Pregnant)), ylim = ) + geom_violin() +
  xlab("HHUnder5s") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# "  ... ", but by recent birth status
ggplot(data = hbKnown, mapping = aes(x = as.factor(HHUnder5s), y = Haemoglobin, fill = as.factor(RecentBirth)), ylim = ) + geom_violin() +
  xlab("HHUnder5s") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# smoothed curve of hb by number of household members under age 5 
ggplot(data = hbKnown, mapping = aes(x = HHUnder5s, y = Haemoglobin)) + geom_smooth()

## Numerical Analysis

# number of observations in each "group" (since number of children under age 5 is discrete)
table(hbKnown$HHUnder5s)

# mean hb for individuals living in a household with each number of children under age 5
aggregate(x = hbKnown$Haemoglobin, by = list(hbKnown$HHUnder5s), FUN = mean)

#### HHSize ####

## Plots

# box plots of hb levels by discrete household size
ggplot(data = hbKnown, mapping = aes(x = as.factor(HHSize), y = Haemoglobin, fill = as.factor(HHSize)), ylim = ) + geom_boxplot() +
  xlab("HHSize") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# smoothed curve of hb by household size (continous)
ggplot(data = hbKnown, mapping = aes(x = HHSize, y = Haemoglobin)) + geom_smooth() 


#### TotalChildren ####

## Plots

# box plots of hb levels by discrete number of total children 
ggplot(data = hbKnown, mapping = aes(x = as.factor(TotalChildren), y = Haemoglobin, fill = as.factor(TotalChildren)), ylim = ) + geom_boxplot() +
  xlab("Total Children") + ylab("HB Level") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# smoothed curve of hb by total number of children (continuous)
ggplot(data = hbKnown, mapping = aes(x = TotalChildren, y = Haemoglobin)) + geom_smooth() 


#### Age ####

## Plots

# smoothed curve of hb by age (continuous)
ggplot(data = hbKnown, mapping = aes(x = Age, y = Haemoglobin)) + geom_smooth()

#### Education ####

## Plots

# box plots of hb levels on education 
ggplot(data = hbKnown, mapping = aes(x = Education, y = Haemoglobin, fill = as.factor(Education)), ylim = ) + geom_boxplot() +
  xlab("Rural") + ylab("Haemoglobin") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

## Numerical Analysis

# standard deviation of hb levels for different levels of education
aggregate(x = hbKnown$Haemoglobin, by = list(hbKnown$Education), FUN = sd)

# number of observations in each group
table(hbKnown$Education)

#### Rural ####

## Plots

# violin plots of hb levels by rural designation
ggplot(data = hbKnown, mapping = aes(x = as.factor(Rural), y = Haemoglobin, fill = as.factor(Rural)), ylim = ) + geom_violin() +
  xlab("Rural") + ylab("Haemoglobin") + ggtitle("Haemoglobin by Rural Status") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

## Numerical Analysis

# number of observations in each designation
table(hbKnown$Rural)

#### Wealth ####

## Plots

# density plot of wealth score, with lines for the quantiles
plot(density(hbKnown$WealthScore))
abline(v = mean(hbKnown$WealthScore))
abline(v = median(hbKnown$WealthScore), col = "blue")
abline(v = quantile(hbKnown$WealthScore, probs = 0.25), col = "green")
abline(v = quantile(hbKnown$WealthScore, probs = 0.75), col = "green")

# smoothed curve of hb and wealth score, with a zoom-in on wealth score between -1 and 1
ggplot(hbKnown, aes(WealthScore, Haemoglobin)) +geom_smooth()
ggplot(hbKnown, aes(WealthScore, Haemoglobin)) + geom_smooth() + xlim(-1,1)

# separation of wealth score into its different quantiles and the associated smoothed curves / boxplots
hbKnown$WealthQuartile<-cut(hbKnown$WealthScore,quantile(hbKnown$WealthScore),include.lowest=TRUE,labels=FALSE)
ggplot(hbKnown, aes(WealthScore, Haemoglobin, fill = as.factor(WealthQuartile))) +geom_smooth()
ggplot(hbKnown, aes(as.factor(WealthQuartile), Haemoglobin, fill = as.factor(WealthQuartile))) + geom_boxplot() +
  xlab("Wealth Quartile") + ylab("Haemoglobin") + theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

# smoothed curves of hb and wealth score, by region 
ggplot(data = hbKnown, mapping = aes(x = WealthScore, y = Haemoglobin, colour = Region)) + geom_smooth() +
  xlab("Wealth Score") + ylab("Haemoglobin") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

# smoothed curves of hb and wealth score, by cleanwater
ggplot(data = hbKnown, mapping = aes(x = WealthScore, y = Haemoglobin, colour = as.factor(CleanWater))) + geom_smooth() +
  xlab("Wealth Score") + ylab("Haemoglobin") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

# smoothed curves of hb and wealth score, by electricity
ggplot(data = hbKnown, mapping = aes(x = WealthScore, y = Haemoglobin, colour = as.factor(Electricity))) + geom_smooth() +
  xlab("Wealth Score") + ylab("Haemoglobin") +
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))

# box plots of wealth score, by regions
ggplot(data = hbKnown, aes(x=as.factor(Region), y = WealthScore, fill = as.factor(Region))) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# box plots of wealth score, by provinces
ggplot(data = hbKnown, aes(x=as.factor(Province), y = WealthScore, fill = as.factor(Province))) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# smoothed curve of wealth score and number of children under 5 per household, compared to that for haemoglobin and number 
# of children under 5 per household
ggplot(data = hbKnown, aes(x=HHUnder5s, y = WealthScore)) + geom_smooth()
ggplot(data = hbKnown, aes(x=HHUnder5s, y = Haemoglobin)) + geom_smooth()

# smoothed curve of wealth score and hb, by rural designation
ggplot(data = hbKnown, aes(x=WealthScore, y = Haemoglobin, group = Rural, colour = as.factor(Rural))) + geom_smooth() 

# smoothed curve of wealth score and hb, by ethnicity
ggplot(data = hbKnown, aes(x=WealthScore, y = Haemoglobin, fill = Ethnicity)) + geom_smooth() # doesn't tell us anything that the ethnicity and hb plots alone told us


## Numerical Analysis

# summary statistics of each variable by wealth quartile
statsByWealthQuartile <- aggregate(x = cbind(hbKnown[,Vars]), 
                                   by = list(hbKnown$WealthQuartile), FUN = function(x) {c(Mean=mean(x), VAR = var(x), SD=sd(x))})

# simple linear regression to see what % variation in wealth score is explained solely by province
summary(lm(WealthScore ~ Province, data = hbKnown)) # 0.41 R-squared

# correlation between hb and wealth score
cor(hbKnown$Haemoglobin, hbKnown$WealthScore) # low, likely explained by the non-linear relationship


####### Section 2: Model Building Process ####### 

####### Section 2.1: Creation of Dummy Variables for Analysis ####### 

# From here on out, hbKnown will be amended by adding
# all dummy variables for province, ethnicity, and region 
# and will be called 'dataWithDummies'. 

# Initial data cleaning: ordering the df by ID
hbKnown <- hbKnown[order(hbKnown$ID), ]

# Province Dummy Variables
provincedummies <- data.frame(matrix(ncol = 33, nrow = 4382))
colnames(provincedummies) <- unique(hbKnown$Province)
for (i in 1:33){
  provincedummies[,i] <- ifelse(hbKnown$Province == unique(hbKnown$Province)[i],1,0)
}
provincedummies$ID <- c(1:4382)
dataWithDummies <- merge(hbKnown, provincedummies, by = "ID")

# Ethnicity Dummy Variables
ethnicitydummies <- data.frame(matrix(ncol = 5, nrow = 4382))
colnames(ethnicitydummies) <- unique(hbKnown$Ethnicity)
for (i in 1:5){
  as.numeric(ethnicitydummies[,i] <- ifelse(hbKnown$Ethnicity == unique(hbKnown$Ethnicity)[i], 1,0))
}
ethnicitydummies$ID <- c(1:4382)
dataWithDummies <- merge(dataWithDummies, ethnicitydummies,by="ID")

# Region Dummy Variables
regiondummies <- data.frame(matrix(ncol = 8, nrow = 4382))
colnames(regiondummies) <- unique(hbKnown$Region)
for (i in 1:8) {
  as.numeric(regiondummies[,i] <- ifelse(hbKnown$Region == unique(hbKnown$Region)[i], 1, 0))
}
regiondummies$ID <- c(1:4382)
dataWithDummies <- merge(dataWithDummies, regiondummies, by = 'ID')

####### Section 2.2: Linear Models Based on EDA ####### 

#### noProvinceModel: All potential non-province/region covariates (according to EDA findings) ####

noProvinceModel <- lm(Haemoglobin ~ AgricLandOwn
                      + `Other/missing`
                      + Pregnant + RecentBirth + Pregnant*RecentBirth
                      + HHUnder5s 
                      + WealthScore + WealthScore*Rural + WealthScore*`Other/missing` + WealthScore*AgricLandOwn
                      + WealthQuartile + WealthScore*WealthQuartile
                      + Age
                      + Rural, data = dataWithDummies)
summary(noProvinceModel) # r-squared of 0.029
AIC(noProvinceModel) # 18513.29

# HHUnder5s, Rural, WealthScore, `Other/missing`*WealthScore, Age, WealthScore*WealthQuartile,
# Pregnant*RecentBirth, WealthScore*Rural, AgricLandOwn*WealthScore have no stat sig. impact at 5%

#### Model 1: Include Province (without Rural interaction) ####
model1 <- lm(Haemoglobin ~ AgricLandOwn
                 + `Other/missing`
                 + Pregnant + RecentBirth + Pregnant*RecentBirth
                 + HHUnder5s 
                 + WealthScore + WealthScore*Rural + WealthScore*`Other/missing` + WealthScore*AgricLandOwn
                 + WealthQuartile + WealthScore*WealthQuartile
                 + Age
                 + Province + Rural, data = dataWithDummies)
summary(model1) # r-squared of 0.20
AIC(model1) # 17704.6

# coefficient significance changes for most covariates, almost all provinces are statistically significant

#### Model 2: Include Province (with Rural interaction) ####
model2 <- lm(Haemoglobin ~ AgricLandOwn
                + `Other/missing`
                + Pregnant + RecentBirth + Pregnant*RecentBirth
                + HHUnder5s 
                + WealthScore + WealthScore*Rural + WealthScore*`Other/missing` + WealthScore*AgricLandOwn
                + WealthQuartile + WealthScore*WealthQuartile
                + Age
                + Province + Rural + Province*Rural, data = dataWithDummies)
summary(model2) # r-squared of 0.2178
AIC(model2) # 17664.53

# multiple NA values, notably fewer stat. sig. provinces

#### Model 3: Include Region ####
model3 <- lm(Haemoglobin ~ AgricLandOwn
             + `Other/missing`
             + Pregnant + RecentBirth + Pregnant*RecentBirth
             + HHUnder5s 
             + WealthScore + WealthScore*Rural + WealthScore*`Other/missing` + WealthScore*AgricLandOwn
             + WealthQuartile + WealthScore*WealthQuartile
             + Age
             + Province + Rural + Province*Rural
             + Region, data = dataWithDummies)
summary(model3) # NA values for all regions (perfectly incorporated by linear combination of provinces)
AIC(model3) # identical AIC (see above)

#### Model 4: Include all region / province dummies ####
model4 <- lm(Haemoglobin ~ AgricLandOwn
                 + `Other/missing`
                 + Pregnant + RecentBirth + Pregnant*RecentBirth
                 + HHUnder5s 
                 + WealthScore + WealthScore*Rural + WealthScore*`Other/missing` + WealthScore*AgricLandOwn
                 + WealthQuartile + WealthScore*WealthQuartile
                 + Age
                 + central + central_highlands + east + southeast + northeast + north + west + south
                 + Badakhshan + Badghis + Baghlan + Balkh + Bamyan + Daykundi + Farah + Faryab
                 + Ghazni + Ghor + Helmand + Herat + Jawzjan + Kabul + Kandahar + Kapisa + Khost
                 + Kunarha + Kunduz + Laghman + Logar + Nangarhar + Nooristan + Paktika + Paktya + Panjsher
                 + Parwan + Samangan + `Sar-e-Pul` + Takhar + Urozgan + Wardak + Zabul
                 + Rural 
                 + (Badakhshan + Badghis + Baghlan + Balkh + Bamyan + Daykundi + Farah + Faryab
                    + Ghazni + Ghor + Helmand + Herat + Jawzjan + Kabul + Kandahar + Kapisa + Khost
                    + Kunarha + Kunduz + Laghman + Logar + Nangarhar + Nooristan + Paktika + Paktya + Panjsher
                    + Parwan + Samangan + `Sar-e-Pul` + Takhar + Urozgan + Wardak + Zabul)*Rural, data = dataWithDummies)
summary(model4) # r-squared = 0.2178
AIC(model4) # virtually identical AIC

# all regions statistically significant, virtually no non-province/region coefficientes are stat. sig., 
# still a distinctly high number of NA values

#### Model 5: Just Province (nothing else) ####
model5 <- lm(Haemoglobin ~ Province, data = dataWithDummies)
summary(model5) # r-squared of 0.1925
AIC(model5) # 17740.05

# honestly, doesn't look much worse than the other models

####### Section 2.3: Clustering of Provinces ####### 

#### Model 6: Include all original covariates from noProvinceModel and ProvinceGroup ####

model6 <- lm(Haemoglobin ~ AgricLandOwn
             + `Other/missing`
             + Pregnant + RecentBirth + Pregnant*RecentBirth
             + HHUnder5s 
             + WealthScore + WealthScore*Rural + WealthScore*`Other/missing` + WealthScore*AgricLandOwn
             + WealthQuartile + WealthScore*WealthQuartile
             + Age
             + Rural
             + ProvinceGroup, data = dataWithDummies)
summary(model6) # r-squared of 0.072
AIC(model6) # 18239.7 (notably worse)

# some province groups significant, but model is notably worse than with all provinces and/or regions

#### Model 7: Remove stat. insig. covariates (most notably, some of the provinces) ####

model7 <- lm(Haemoglobin ~ AgricLandOwn 
             + `Other/missing`
             + Pregnant + RecentBirth
             + WealthScore + WealthScore*Rural
             + WealthQuartile
             + Age
             + Rural
             + Province5 + Province6 + Province7 + Province8 + Province9, 
             data = dataWithDummies)
summary(model7) # r-squared of 0.070
AIC(model7) # worsened AIC (?) despite reduction of covariates

# it's possible that this grouping doesn't work 
# some coeff are stat sig (e.g. AgricLandOwn) that were not anticipated to be, whereas some coeff that aren't
# (e.g. Pregnant*RecentBirth) maybe should be

####### Section 2.4: New Clustering of Provinces ####### 

# Clustering initially done to combine provinces across all covariates, 
# will now be done exclusively with reference to hb levels

#### Data Cleaning: Removal of Old Province Groups ####
colnames(dataWithDummies)
dataWithDummies <- dataWithDummies[, -c(30:39)] 

#### New Clustering ####
# (note: see full comments in Section 1.3)
ProvinceSummariesNew <- aggregate(hbKnown$Haemoglobin, by = list(hbKnown$Province), FUN = function(x) {c(Mean=mean(x), SD=sd(x))})
rownamesProv <- ProvinceSummariesNew[,1]
rownames(ProvinceSummariesNew) <- ProvinceSummariesNew[,1]
ProvinceSummariesNew <- scale(ProvinceSummariesNew[ ,-1])
rownames(ProvinceSummariesNew) <- rownamesProv

DistancesNew <- dist(ProvinceSummariesNew)
ClusTreeNew <- hclust(DistancesNew, method="complete")
plot(ClusTreeNew, xlab = "Province", ylab = "Separation")
abline(h=1, col="red", lty=2)
NewGroupsNew <- paste("Province", cutree(ClusTreeNew, h=1), sep="")
NewGroupsDFAllNew <- as.data.frame.matrix(table(rownames(ProvinceSummariesNew), NewGroupsNew))
NewGroupsDFAllNew$Province <- rownames(NewGroupsDFAllNew)

# new dataset with updated province groups
dataWithDummies <- merge(dataWithDummies, NewGroupsDFAllNew, by = "Province")
dataWithDummies <- dataWithDummies[order(dataWithDummies$ID), ]

#### Model 8: Re-run noProvinceModel with the addition of new province groups ####
model8 <- lm(Haemoglobin ~ AgricLandOwn
             + `Other/missing`
             + Pregnant + RecentBirth + Pregnant*RecentBirth
             + HHUnder5s 
             + WealthScore + WealthScore*Rural + WealthScore*`Other/missing` + WealthScore*AgricLandOwn
             + WealthQuartile + WealthScore*WealthQuartile
             + Age
             + Rural
             + Province1 + Province2 + Province3 + Province4 + Province5 + Province6 + Province7
             + Province8 + Province9 + Province10 + Province11 + Province12, data = dataWithDummies)
summary(model8) # r-squared of 0.198 (notably better than with previous grouping) 
AIC(model8) # notably better AIC than previous groupings

# Removing insignificant covariates one-by-one (omitted for space ...)

#### Model 9: Best linear model with new groupings ####
model9 <- lm(Haemoglobin ~ 
     Pregnant + RecentBirth + Pregnant*RecentBirth
   + HHUnder5s 
   + Province2 + Province3 + Province4 + Province5 + Province6 + Province7
   + Province8 + Province9 + Province10 + Province11
   + Rural 
   + `Other/missing`
   + WealthScore + WealthQuartile + WealthScore*WealthQuartile
   + (Province3 + Province5 + Province6 + Province7)*Rural, data = dataWithDummies)
summary(model9) # r-squared of 0.204
AIC(model9) # 17661.26
par(mfrow = c(2,2))
plot(model9)

####### Section 2.5: Fitting a Gamma GLM to Model 9 (best linear mode above) ####### 

#### GLM1: Gamma GLM, inverse link ####
glm1 <- glm(Haemoglobin ~ 
                         Pregnant + RecentBirth + Pregnant*RecentBirth
                       + HHUnder5s 
                       + Province2 + Province3 + Province4 + Province5 + Province6 + Province7
                       + Province8 + Province9 + Province10 + Province11
                       + Rural 
                       + `Other/missing`
                       + WealthScore + WealthQuartile + WealthScore*WealthQuartile
                       + (Province3 + Province5 + Province6 + Province7)*Rural,
                       data = dataWithDummies,
                       family = Gamma(link = "inverse"))
summary(glm1)

#### GLM2: Gamma GLM, identity link ####
glm2 <- glm(Haemoglobin ~ 
              Pregnant + RecentBirth + Pregnant*RecentBirth
            + HHUnder5s 
            + Province2 + Province3 + Province4 + Province5 + Province6 + Province7
            + Province8 + Province9 + Province10 + Province11
            + Rural 
            + `Other/missing`
            + WealthScore + WealthQuartile + WealthScore*WealthQuartile
            + (Province3 + Province5 + Province6 + Province7)*Rural,
            data = dataWithDummies,
            family = Gamma(link = "identity"))
summary(glm2)

#### GLM3: Gamma GLM, log link ####
glm3 <- glm(Haemoglobin ~ 
              Pregnant + RecentBirth + Pregnant*RecentBirth
            + HHUnder5s 
            + Province2 + Province3 + Province4 + Province5 + Province6 + Province7
            + Province8 + Province9 + Province10 + Province11
            + Rural 
            + `Other/missing`
            + WealthScore + WealthQuartile + WealthScore*WealthQuartile
            + (Province3 + Province5 + Province6 + Province7)*Rural,
            data = dataWithDummies,
            family = Gamma(link = "log"))
summary(glm3)
plot(glm3)


####### Section 3: Model Evaluation ####### 

# This section will compare the following models: 
# Model 4 (with all province/region dummy variables)
# Model 5 (with just province)
# Model 9 (best linear model)
# GLM3 (log-link Gamma GLM)

#### Diagnostic plots for all ####
plot(model4)
plot(model5) # yikes
plot(model9)
plot(glm3)

#### AIC for all ####
AIC(model4) # 17,664.79
AIC(model5) # 17,740.05
AIC(model9) # 17,661.26
AIC(glm3) # not comparable to the rest, but 18,629.34

#### Cross-validation approach ####

###
# Function which cross-validates our data in three folds.
#
# Inputs: 
# mymodel - the model (linear or generalised linear) to be tested.
# mydata - the data, which will be split into three folds used to train the model and form predictions.
#
# Output: 
# Prints a dataframe containing the mean Root-Mean-Squared Error, R-Squared/Deviance Explained, and Score (as defined in the project assignment) over the three folds.
###
crossvalidate <- function(mymodel, mydata){
  # Set seed 
  set.seed(64)
  
  # Creates 3 folds by randomly splitting the data.
  foldsample <- sample(1:3, size = nrow(mydata), replace = T, prob = NULL)
  
  # Trains the model taking out each fold.
  # Take out fold 1
  trainfold1 <- update(mymodel, data = mydata[foldsample != 1,])
  # Take out fold 2
  trainfold2 <- update(mymodel, data = mydata[foldsample != 2,])
  # Take out fold 3
  trainfold3 <- update(mymodel, data = mydata[foldsample != 3,])
  
  # IF clause needed depending on whether the model is an lm or glm.
  if(length(class(mymodel)) == 1){ # If mymodel is a linear model
    
    # Makes predictions on remaining data.
    # Predicts from fold 1
    predictfold1 <- predict(trainfold1, newdata = mydata[foldsample == 1,], se.fit = TRUE)
    # Predicts from fold 2
    predictfold2 <- predict(trainfold2, newdata = mydata[foldsample == 2,], se.fit = TRUE)
    # Predicts from fold 3
    predictfold3 <- predict(trainfold3, newdata = mydata[foldsample == 3,], se.fit = TRUE)
    
    # Calculate RMSE.
    # Fold 1
    rmsef1 <- sqrt( mean( (mydata[foldsample == 1,]$Haemoglobin - predictfold1$fit )^2 ) )
    # Fold 2
    rmsef2 <- sqrt( mean( (mydata[foldsample == 2,]$Haemoglobin - predictfold2$fit )^2 ) )
    # Fold 3
    rmsef3 <- sqrt( mean( (mydata[foldsample == 3,]$Haemoglobin - predictfold3$fit )^2 ) )
    
    # Extract R-Squared.
    # Fold 1
    rsqf1 <- summary(trainfold1)$adj.r.squared
    # Fold 2
    rsqf2 <- summary(trainfold2)$adj.r.squared
    # Fold 3
    rsqf3 <- summary(trainfold3)$adj.r.squared
    
    # Calculate Score.
    # Fold 1
    predError1 <- sqrt( var(mydata[foldsample != 1,]$Haemoglobin) + predictfold1$se.fit^2 )
    scoref1 <- sum( log(predError1) + (mydata[foldsample == 1,]$Haemoglobin - predictfold1$fit)^2 / (2*(predError1^2)) )
    # Fold 2
    predError2 <- sqrt( var(mydata[foldsample != 2,]$Haemoglobin) + predictfold2$se.fit^2 )
    scoref2 <- sum( log(predError2) + (mydata[foldsample == 2,]$Haemoglobin - predictfold2$fit)^2 / (2*(predError2^2)) )
    # Fold 3
    predError3 <- sqrt( var(mydata[foldsample != 3,]$Haemoglobin) + predictfold3$se.fit^2 )
    scoref3 <- sum( log(predError3) + (mydata[foldsample == 3,]$Haemoglobin - predictfold3$fit)^2 / (2*(predError3^2)) )
    
    # Calculate and return the means of these metrics.
    results <- data.frame("RMSE" = mean(c(rmsef1, rmsef2, rmsef3)),
                          "R-Squared" = mean(c(rsqf1, rsqf2, rsqf3)),
                          "Score" = mean(c(scoref1, scoref2, scoref3)))
    return(results)
  }
  
  else{ # If mymodel is a generalized linear model.
    
    # Makes predictions on remaining data.
    # Predicts from fold 1
    predictfold1 <- predict(trainfold1, newdata = mydata[foldsample == 1,], se.fit = TRUE, type = "response")
    # Predicts from fold 2
    predictfold2 <- predict(trainfold2, newdata = mydata[foldsample == 2,], se.fit = TRUE, type = "response")
    # Predicts from fold 3
    predictfold3 <- predict(trainfold3, newdata = mydata[foldsample == 3,], se.fit = TRUE, type = "response")
    
    # Calculate RMSE.
    # Fold 1
    rmsef1 <- sqrt( mean( (mydata[foldsample == 1,]$Haemoglobin - predictfold1$fit )^2 ) )
    # Fold 2
    rmsef2 <- sqrt( mean( (mydata[foldsample == 2,]$Haemoglobin - predictfold2$fit )^2 ) )
    # Fold 3
    rmsef3 <- sqrt( mean( (mydata[foldsample == 3,]$Haemoglobin - predictfold3$fit )^2 ) )
    
    # Extract Explained Deviance.
    # Fold 1
    exdevf1 <- 1 - summary(trainfold1)$deviance / summary(trainfold1)$null.deviance
    # Fold 2
    exdevf2 <- 1 - summary(trainfold2)$deviance / summary(trainfold2)$null.deviance
    # Fold 3
    exdevf3 <- 1 - summary(trainfold3)$deviance / summary(trainfold3)$null.deviance
    
    # Calculate Score.
    # Fold 1
    predError1 <- sqrt( summary(trainfold1)$dispersion * (predictfold1$fit)^2 + predictfold1$se.fit^2 )
    scoref1 <- sum( log(predError1) + (mydata[foldsample == 1,]$Haemoglobin - predictfold1$fit)^2 / (2*(predError1^2)) )
    # Fold 2
    predError2 <- sqrt( summary(trainfold2)$dispersion * (predictfold2$fit)^2 + predictfold2$se.fit^2 )
    scoref2 <- sum( log(predError2) + (mydata[foldsample == 2,]$Haemoglobin - predictfold2$fit)^2 / (2*(predError2^2)) )
    # Fold 3
    predError3 <- sqrt( summary(trainfold3)$dispersion * (predictfold3$fit)^2 + predictfold3$se.fit^2 )
    scoref3 <- sum( log(predError3) + (mydata[foldsample == 3,]$Haemoglobin - predictfold3$fit)^2 / (2*(predError3^2)) )
    
    # Calculate and return the means of these metrics.
    results <- data.frame("RMSE" = mean(c(rmsef1, rmsef2, rmsef3)),
                          "Explained Deviance" = mean(c(exdevf1, exdevf2, exdevf3)),
                          "Score" = mean(c(scoref1, scoref2, scoref3))) 
    return(results)
  }
}

# Cross-validation results for all models
crossvalidate(model4, dataWithDummies)
crossvalidate(model5, dataWithDummies)
crossvalidate(model9, dataWithDummies)
crossvalidate(glm3, dataWithDummies)

# initial thoughts: model9 outperforms all other models on RMSE and score
# model4 outperforms all others on R-squared (makes sense, has the most covariates)
# glm3 does not improve model9 as anticipated


####### Section 4: Preparation of Output ####### 

####### Section 4.1: Preparation of hbUnknown for Predictions ####### 

# Must create all needed groups/dummies/etc for hbUnknown (our unknown hb levels that we'd like to predict)

# Note: some provinces are missing from hbUnknown (will account for this in the dummy creation below)
# The same problem does not exist for ethnicity or region
unique(hbUnknown$Province)
unique(hbUnknown$Ethnicity)
unique(hbUnknown$Region)

# Ethnicity Dummy Variables
ethnicitydummies <- data.frame(matrix(ncol = 5, nrow = 1039))
colnames(ethnicitydummies) <- unique(hbUnknown$Ethnicity)
for (i in 1:5){
  as.numeric(ethnicitydummies[,i] <- ifelse(hbUnknown$Ethnicity == unique(hbUnknown$Ethnicity)[i], 1,0))
}
ethnicitydummies$ID <- c(4383:5421)
hbUnknown <- merge(hbUnknown, ethnicitydummies,by="ID")

# WealthQuartiles (based on the known wealth score quartiles)
hbUnknown$WealthQuartile<-cut(hbUnknown$WealthScore,quantile(hbKnown$WealthScore),include.lowest=TRUE,labels=FALSE)

# ProvinceGroups (also based on the known wealth score quartiles)
hbUnknown$Province1 <- ifelse(hbUnknown$Province == "Badakhshan" | hbUnknown$Province == "Samangan", 1,0)
hbUnknown$Province2 <- ifelse(hbUnknown$Province == "Badghis" | hbUnknown$Province == "Khost", 1,0)
hbUnknown$Province3 <- ifelse(hbUnknown$Province == "Baghlan" | hbUnknown$Province == "Herat", 1,0)
hbUnknown$Province4 <- ifelse(hbUnknown$Province == "Balkh" | hbUnknown$Province == "Faryab" | hbUnknown$Province == "Kapisa" | hbUnknown$Province == "Kunarha" | hbUnknown$Province == "Nangarhar" | hbUnknown$Province == "Paktika" | hbUnknown$Province == "Paktya" | hbUnknown$Province == "Takhar" | hbUnknown$Province == "Urozgan", 1,0)
hbUnknown$Province5 <- ifelse(hbUnknown$Province == "Bamyan" | hbUnknown$Province == "Ghazni" | hbUnknown$Province == "Wardak", 1,0)
hbUnknown$Province6 <- ifelse(hbUnknown$Province == "Daykundi" | hbUnknown$Province == "Farah" | hbUnknown$Province == "Kandahar", 1,0)
hbUnknown$Province7 <- ifelse(hbUnknown$Province == "Ghor" | hbUnknown$Province == "Helmand" | hbUnknown$Province == "Kabul" | hbUnknown$Province == "Parwan", 1,0)
hbUnknown$Province8 <- ifelse(hbUnknown$Province == "Jawzjan" | hbUnknown$Province == "Kunduz", 1,0)
hbUnknown$Province9 <- ifelse(hbUnknown$Province == "Laghman", 1,0)
hbUnknown$Province10 <- ifelse(hbUnknown$Province == "Logar", 1,0)
hbUnknown$Province11 <- ifelse(hbUnknown$Province == "Nooristan" | hbUnknown$Province == "Panjsher" | hbUnknown$Province == "Zabul", 1,0)
hbUnknown$Province12 <- ifelse(hbUnknown$Province == "Sar-e-Pul", 1,0)

# Make all needed factor covariates 1/0 numeric values
# turn all yes/no into 1/0 numerical values
hbUnknown[hbUnknown == "Yes"] <- as.numeric(1)
hbUnknown[hbUnknown == "No"] <- as.numeric(0)

# ensure entire columns are numeric types (for mean/sd calculation)
hbUnknown$Rural <- as.numeric(hbUnknown$Rural)
hbUnknown$Pregnant <- as.numeric(hbUnknown$Pregnant)
hbUnknown$RecentBirth <- as.numeric(hbUnknown$RecentBirth)

####### Section 4.2: Actual Predictions ####### 

# designate model9 to be the final model
finalmodel <- model9

# model predictions
predictions <- predict(finalmodel, newdata = hbUnknown, se.fit = T)

# create a dataframe with desired output
predictions_df <- data.frame("ID" = hbUnknown$ID,
                             "Prediction" = predictions$fit,
                             "Prediction Error Standard Deviation" = predictions$se.fit)


# output to a '.dat' file.
write.table(predictions_df, file = 'ICA2_Group159_pred.dat', col.names = FALSE, row.names = FALSE)

####### Section 4.3: Preparation of Figures for Final Report ####### 

set2 <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")

#### Figure 1: Density plot/histogram for known haemoglobin levels ####
png("ICA2_Group159_Fig1.png")
ggplot(data = hbKnown, mapping = aes(Haemoglobin)) + geom_histogram(aes(y = ..density..), colour = set2[3], size = 1) +
  geom_density(alpha = 0.75, linetype = 2, fill = set2[1]) + xlab("Haemoglobin (g/dL)") + ylab("Density") + 
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15)) + ggtitle("Figure 1: Density Plot and Histogram for Known Haemoglobin Levels")
dev.off()

#### Figure 2: Smoothed curve of wealth on hb ####
png("ICA2_Group159_Fig2.png")
ggplot(hbKnown, aes(WealthScore, Haemoglobin)) +geom_smooth(fill = set2[1]) +
  geom_vline(xintercept = quantile(hbKnown$WealthScore, probs = c(0.25,0.5,0.75)))+
  scale_fill_brewer(palette = "Set2")+ 
  geom_text(aes(x=-0.05 +quantile(hbKnown$WealthScore, probs = c(0.25)), label="Lower Quartile", y=12.5), colour="black", angle=90, size = 4.5) +
  geom_text(aes(x=-0.05 +quantile(hbKnown$WealthScore, probs = c(0.5)), label="Median", y=12.5), colour="black", angle=90, size = 4.5) +
  geom_text(aes(x=-0.05 +quantile(hbKnown$WealthScore, probs = c(0.75)), label="Upper Quartile", y=12.5), colour="black", angle=90, size = 4.5) +
  ggtitle(label="Figure 2: Impact of WealthScore on Haemoglobin",
          subtitle = "Vertical Lines Denote Quantiles of WealthScore")+
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))
dev.off()

#### Figure 3: Smoothed curve of wealth score and hb, by ethnicity ####
png("ICA2_Group159_Fig3.png")
ggplot(data = hbKnown, aes(x=WealthScore, y = Haemoglobin, fill = Ethnicity)) + geom_smooth() +
  scale_fill_brewer(palette = "Set2")+
  ggtitle(label="Figure 3: Interaction of WealthScore and Ethnicity on Haemoglobin")+
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))
dev.off()

#### Figure 4: Box plots of hb and pregnancy by recent birth ####
png("ICA2_Group159_Fig4.png")
ggplot(data = hbKnown, mapping = aes(x = as.factor(Pregnant), y = Haemoglobin, fill = as.factor(RecentBirth)), ylim = ) + geom_boxplot() +
  xlab("Pregnancy Status") + ylab("Haemoglobin") +  scale_x_discrete(labels=c("0" = "No", "1" = "Yes"))+
  ggtitle("Figure 4: Interaction of Pregnancy and Recent-Birth Status on Haemoglobin") + scale_fill_brewer(palette = "Set2", name = "Recent-Birth Status", labels = c("No", "Yes"))+
  theme(axis.text=element_text(size=15)) + theme(axis.title = element_text(size = 15))
dev.off()

