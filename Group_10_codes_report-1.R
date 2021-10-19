
datingSurvey = read.csv(file = 'speed-dating.csv', stringsAsFactors = F)
# 8738 obs, 123 variables

### Explore Data
str(datingSurvey)

#Subset needs-based Variables
library(dplyr)
surveyData_noChar <- subset(datingSurvey, has_null==0,select  = c (1:2,4:6,10:12,16:21,28:33,40:45,52:56,62:67,74:70,108,110:112,116,117,120:123))
#Subset data only int and numeric.
surveyData = surveyData_noChar %>% select(1:38,43:52)


#Impute missing variables is not required because I filtered using has_Null=0 during the subset creation

#Standardize variables using scale
# Convert all ints to numeric before standardizing all variables
#Specify columns to change
i <- c(1:8, 27:31,38,40,41,45:48)
#use apply function to change to numeric
surveyData[ ,i] <- apply(surveyData[ , i], 2, 
                         function(x) as.numeric(as.character(x)))

#Get classes to confirm change occurred
sapply(surveyData, class)

#Split into Test and Train
library(caTools)
set.seed(1706)
split = sample.split(surveyData$match,SplitRatio = 0.7)
train = surveyData[split,]
test = surveyData[!split,]

#K-means clustering
set.seed(1706)
km_2 = kmeans(train, centers = 2,iter.max = 100)
km_2

km_3 = kmeans(train, centers = 3,iter.max = 100)
km_3

#Finding the optimal clustering by examining within_ss and ratio_ss
set.seed((1706))
within_ss = sapply(X = 1:9,
                   FUN = function(x) kmeans(train,centers = x,iter.max = 100)$tot.withinss)
within_ss

ratio_ss = sapply(X = 1:9,
                  FUN = function(x) {
                    km = kmeans(train, centers = x,iter.max = 100)
                    ratio = km$betweenss/km$totss
                    return(ratio)
                  })
ratio_ss

dat = data.frame(clusters=1:9, within_ss, ratio_ss)

library(ggplot2)
ggplot(dat,aes(x=clusters,y=within_ss))+
  geom_line(color='steelblue',size=1.4)+
  scale_x_continuous(breaks=1:9,minor_breaks = 2:10)+
  geom_vline(xintercept = 3)

ggplot(dat,aes(x=clusters,y=ratio_ss))+ 
  geom_line(color='steelblue',size=1.4)+
  scale_x_continuous(breaks=1:9,minor_breaks = 1:9)+
  geom_vline(xintercept = 3)

#Using km3 as the cluster for exploration
k_segments = km_3$cluster
k_segments

combined <- cbind(train, k_segments)
combined
combined_data <- combined %>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  data.frame()
combined_data


##########################################  FCA Analysis #########################

### 1.) test suitability for Exploratory Factor Analysis

#Correlation Matrix
round(cor(surveyData), 3)
#The matrix shows some large and some small correlation which is an indicator the data might be suitable for EFA as 
# the matrix is not close to an identity matrix. Eg. inelligence_partner x sincere_partner has a vauue of (0.631)
library(corrplot)
corrplot(corr = cor(surveyData[,1:17]),type = 'upper', col = c('red','white','green'),method = 'square',diag = F)
# Green indicates significant correlation

library(ggcorrplot)
ggcorrplot(cor(surveyData[,1:17]),colors = c('red','white','green'),type = 'lower')


# Bartlett’s Test of Sphericity
install.packages("psych")
library(psych)
cortest.bartlett(cor(surveyData),n = 1048)
# The pvalue is less than .05 but is also 0, possibly another factor that this data is good for factor analysis



# KMO Measure of Sampling Adequacy (MSA) - If the variables are strongly related, partial correlations should be 
# small and MSA close to 1. If MSA > 0.5, data is suitable for factor analysis.
KMO(r = cor(surveyData))
# Overall MSA =  0.72

# All 3 tests indicate this is a good dataset to perform factor analysis

## 2.) Number of Factor Analysis
# Scree Plot
scree(cor(surveyData),factors = T, pc=T)
# Looking at Eigen values of factors and compeonents over 1 gives an indication 
#that a 2 factor solution is suitable

# Eigen value
data.frame(factor = 1:ncol(surveyData), eigen = eigen(cor(surveyData))$values)
## There are 6 eigen values above 1.0.


# Parallel Analysis
fa.parallel(surveyData,fa='fa',fm = 'pa')
# the first 2 factors are above the similated data. Thios is another indication that 2 factor solution is appriate

#Total Variance Explained
result = fa(r = surveyData,nfactors = 6,fm = 'pa',rotate = 'none')
result$Vaccounted

#look at comminality (reflects the amount of variance in a variable that can be explained by the factors.)
data.frame(communality = result$communality)
#funny_partner (0.647949671) and shared_interests_partner (0.502570920) are well represented by the corresponding 
# factor as it above .5

#Mapping varibales to factors
print(result$loadings, cut=0)
# we ar enow reduced to having 2 variables

print(result$loadings, cut=0.15)

# examine the matrix after an orthogonal rotation using varimax. Is the mapping of variables to factors more clear?
fa_varimax = fa(r = surveyData,nfactors = 6,fm = 'pa',rotate = 'varimax')
print(fa_varimax$loadings,cut=0.25)

print(fa_varimax$loadings,cut=0.25, sort=T)




