#Heat Pump Electricity Clustering
#Create hourly profiles that capture different heating patterns within a house 
#across one year of operation.
#can be found here: https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8151&type=Data%20catalogue#!/details

library(tidyverse)
library(lubridate)
library(cluster)
library(factoextra)
library(mclust)

hp<-read.csv(file = "heat_pump_data.csv")
#there is no timestamp (date-time) in the raw data so I will create one.
#I will also create a Date column, as well as a weekday
hp$Date <- make_date(year = hp$Year, month = hp$Month, day =hp$Day)
hp$dt <- make_datetime(year = hp$Year, month = hp$Month, day = hp$Day, hour = hp$Hour, min = hp$Minute)
hp$weekday <- weekdays(hp$Date, abbreviate = T)

head(hp)

#I am interested in the Ehp feature, which is the electricity demand of the heat pump, in Wh
sum(is.na (hp$Ehp))#zero NAs
length(unique(hp$dt))#all date-times are unique-no duplicates


##Check coverage with some summaries
hp_cov <- hp %>% group_by(Month) %>% summarise(total = n())%>% spread(Month, total)
hp_cov2 <- hp %>% group_by(weekday) %>% summarise(total = n()) %>% spread(weekday, total)

#### Spread Ehp to columns, to create a dataframe with a timestamp and 24 features. 
#I will be creating hourly consumption values so I want a dataframe with rows of unique dates
#and 24 columns of electricity consumption data
hp_cl <- hp[,c(2:5,9,21)]
hp_cl$Hour<-as.integer(hp_cl$Hour)

hp_Wide<-hp_cl %>%
  group_by(Date,Hour) %>%
  summarise(mean_Wh = sum(Ehp))%>% # calculate Ehp per hour from 2-min data
spread(Hour,mean_Wh) #creates a feature for every Hour, where entries are mean_Wh


hp_Wide[rowSums(is.na(hp_Wide)) > 0, ] #observe the NAs
hp_Wide<-na.omit(hp_Wide)#exclude them

#Feature scaling
#not necessary here - we have the same units in all features and
#we also have 1 house only!
#if we had more houses, we would probably scale so that energy profiles	are	
#clustered	based	on	their	"structural" similarities

##k means----

###need to find the value of k first. we will test 10 different values, from 1 to 10

#1. Elbow Method to find number of clusters
#wss - within sum of squares: measures the compactness (i.e goodness) of the clustering 
#and we want it to be as small as possible, but up to a point where adding more clusters
#is meaningful

## function to compute total within-cluster sum of squares
wss <- function(k) {
  kmeans(hp_Wide [,2:25], k, nstart = 10)$tot.withinss
}

#nstart option attempts multiple initial configurations and reports on the best one. 
#E.g, adding nstart=10 will generate 10 initial random centroids and choose the best one for the algorithm
k.values <- 1:10

# Compute and plot wss for k = 1 to k = 10
# extract wss for 1-10 clusters
wss_values <- map_dbl(1:10, wss)#map_dbl() always returns a double vector

str(wss_values)#I have a vector with wss for the 10 clusters
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE,  #type=b for 'points', pch 19 for black dot
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#this indicates 2 clusters

#alternatively, faster elbow plot from factoextra package:

fviz_nbclust(hp_Wide [,2:25], kmeans, method = "wss")#this points towards 2 as well!
fviz_nbclust(hp_Wide [,2:25], kmeans, method = "silhouette")#calculate Silhouette width too
#this is another way to estimate optimal number of clusters
#again, 2 clusters
#The silhouette analysis measures how well an observation is clustered and it estimates 
#the average distance between clusters. The silhouette plot displays a measure of how close 
#each point in one cluster is to points in the neighboring clusters.
#Observations with a large Si (almost 1) are very well clustered.
#A small Si (around 0) means that the observation lies between two clusters.
#Observations with a negative Si are probably placed in the wrong cluster.


##K means clustering

kclust <- kmeans(hp_Wide [,2:25],2)
print(kclust)
kclust$size
#52 % of the total variance in the data set is explained by the clustering

#I will need to merge my wide dataframe with my clusters
df <- cbind(hp_Wide, cluster = kclust $cluster)
df_long <- gather(df,"hour","Ehp",2:25) #make it long before plotting
#'gather' uses a key and a value column
#can also use pivot_longer

# Adding weekday and month columns 
df_long$wday <- wday(df_long$Date)
df_long$wday <- as.factor(df_long$wday)
df_long$hour<-as.integer(df_long$hour)
df_long$month<-month(df_long$Date)
df_long$cluster <- as.factor(df_long$cluster)

summary(df_long)

#lets observe electricity demand in my clusters by Hour
hp_sum <- df_long %>% group_by(cluster,hour) %>% summarise(mean_ehp = mean(Ehp))
#add 'month' in your group_by and see what happens!
ggplot(hp_sum)+
  geom_line(aes(x=hour,y=mean_ehp,group=cluster,col=as.factor(cluster)))+
  labs(title ="K-means clusters", x = "Time (Hour)", y = "Mean Wh of electricity")+
  theme_bw()+
  theme(legend.title=element_blank())



#how coherent are our clusters? what do they represent?
#lets add day names
df_long$day_of_week <- wday(df_long$Date,label=TRUE)

#I have 24 1-hour periods per day (my rows represent days!) so divide by 24
#remember, we clustered by day - so we want to see how these days/clusters are
#distributed across the data

clusters_wday<- table(df_long$cluster,df_long$day_of_week)/24
clusters_wday
#'table' uses the cross-classifying factors to build a contingency 
#table of the counts at each combination of factor levels
# my Clusters don't really differ in terms of day of the week

#create contingency table for months
t(table(df_long[,c(2,6)])/24)#t returns the transpose

#Try clustering using half-hourly values in your own time and explore once more 
#how your clusters differ!
#Try using principal components as well.

#DBSCAN
#install.packages("dbscan")
#see: https://www.rdocumentation.org/packages/dbscan/versions/1.1-10/topics/dbscan
library(dbscan)
#we need to convert our data frame into  matrix!
hp_matr <- as.matrix(hp_Wide[, 2:25])

#need to choose the optimal minPoints value - the min number of points needed to form a dense region
#this is usually 2*number of features.Here, I have 24 features.
#in small datasets (up to 4 features), use the number of features + 1

#need to find the eps distance too: how close points should be to each other to be considered a part of a cluster
dbscan::kNNdistplot(hp_matr, k =  47)#set k to be equal to minPoints-1, here 47.
#The aim is to determine the "knee", which corresponds to the optimal eps parameter.
#A knee corresponds to a threshold where a sharp change occurs along the k-distance curve
#I see a sharp change at around 550
abline(h =550, lty = 4)

res.db <- dbscan(hp_matr, 550, 48) #550 stands for the size of epsilon neighborhood, 48 for minPts
fviz_cluster(res.db, hp_matr, geom = "point")
print(res.db)

#Observations are represented by points in the plot, using principal components if ncol(data) > 2
#Most of our data are classed as noise, with very little data being clustered.
#With highly variable data like these, we should perhaps first consider dimensionality
#reduction methods or other pre-processing.

#Useful paper: https://eta-publications.lbl.gov/sites/default/files/15166-68355-1-pb_1.pdf

#For a short DBSCAN tutorial in R look at: https://www.rdocumentation.org/packages/dbscan/versions/1.1-10/topics/dbscan


#Gaussian Mixture Model
#Create clusters and observe what they represent, graphically

gmm = Mclust(hp_Wide [,2:25])

# optimal selected model
gmm$modelName #the algorithm tried several models - chose the one with the lowest 
#Bayesian information criterion (BIC): metric that measures the relative 
#prediction errors of different models

# optimal number of clusters
gmm$G
summary(gmm$BIC)#models with the smallest BIC are chosen
plot(gmm$BIC)
# probability for an observation to be in a given cluster
head(gmm$z)

# get probabilities, means, variances
summary(gmm, parameters = TRUE)

#Compare amount of the data within each cluster

table(hp_Wide$Date, gmm$classification)

#Lets create plots, like we did for k-means:

df_gmm <- cbind(hp_Wide, cluster = gmm$classification)
df_long2 <- gather(df_gmm,"hour","Ehp",2:25)

df_long2$wday <- wday(df_long2$Date)
df_long2$wday <- as.factor(df_long2$wday)
df_long2$hour<-as.integer(df_long2$hour)
df_long2$month<-month(df_long2$Date)
df_long2$cluster <- as.factor(df_long2$cluster)

summary(df_long2)

#lets observe electricity demand in my clusters by Hour
hp_sum2 <- df_long2 %>% group_by(cluster,hour) %>% summarise(mean_ehp = mean(Ehp))
ggplot(hp_sum2)+
  geom_line(aes(x=hour,y=mean_ehp,group=cluster,col=as.factor(cluster)))+
  labs(title ="Gaussian Mixture clusters", x = "Time (Hour)", y = "Mean Wh of electricity")+
  theme_bw()+
  theme(legend.title=element_blank())

#how coherent are our clusters? what do they represent?
#lets add day names
df_long2$day_of_week <- wday(df_long2$Date,label=TRUE)

#I have 24 1-hour periods per day (my rows represent days!)
#remember, we clustered by day - so we want to see how these days/clusters are
#distributed across the data

clusters_wday2<- table(df_long2$cluster,df_long2$day_of_week)/24
clusters_wday2
#'table' uses the cross-classifying factors to build a contingency 
#table of the counts at each combination of factor levels
# my Clusters don't really differ in terms of day of the week

#create contigency table for months
t(table(df_long2[,c(2,6)])/24)#t returns the transpose


