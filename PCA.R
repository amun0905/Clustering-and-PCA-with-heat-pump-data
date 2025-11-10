#PCA Using heat pump data for 1 year, every 2 minutes
#working with the house code 5132 from the dataset
library(tidyverse)
library(lubridate)
library(factoextra)

hp<-read.csv(file = "heat_pump_data.csv")

hp$Date <- make_date(year = hp$Year, month = hp$Month, day =hp$Day)
hp$dt <- make_datetime(year = hp$Year, month = hp$Month, day = hp$Day, hour = hp$Hour, min = hp$Minute)

#the Ehp feature is our electricity demand, in Wh

#I will choose 4 dates randomly and plot their energy demand patterns
unique_dates<- unique(hp$Date)
set.seed(12)
random_dates <- unique_dates[sample(length(unique_dates),4)]
random_dates
#"2015-02-15" "2015-02-05" "2014-11-08" "2015-02-16"

#create df which contains only these dates - only used for plotting
hp_red<-hp[hp$Date %in% random_dates, ]

hp_red$Date<-as.factor(hp_red$Date)

#plot average hourly electricity, by date

hp_sum <- hp_red %>% group_by(Hour,Date) %>% summarise(ehp = sum(Ehp))
ggplot(hp_sum)+
  geom_line(aes(x=Hour,y=ehp,group=Date,col=Date))+
  labs( x = "Hours", y = " Wh of electricity")+
  theme_bw()


#back to original data. need to transform to wide to perform PCA
#every column/feature should be a time in the day and every row should be a date
#I will also aggregate to hourly values!
sum(is.na (hp$Ehp))#zero NAs
length(unique(hp$dt))#all timestamps are unique-no duplicates

hp <- hp[,c(2:5,9,21)]
hp$Hour<-as.integer(hp$Hour)
hp_Wide<-hp %>%
  group_by(Date, Hour) %>%
  summarise(agg_Wh = sum(Ehp))%>% # calculate aggregate Ehp per hour from 2-min data
  spread(Hour,agg_Wh)

hp_Wide[rowSums(is.na(hp_Wide)) > 0, ] #observe the NAs
hp_Wide<-na.omit(hp_Wide)#exclude them

#PCA
pca <- prcomp(hp_Wide[,2:25],center=FALSE, scale=FALSE)#a base R function! we could have set scale=TRUE too
#But we would not be able to reproduce the graphs.
#would normally scale and center here. PCA requires numerical features to
#be scaled. Here, I only have electricity values for 1 house, so OK not to scale.
#also don't want to lose the original consumption values in this case, as I also want to plot them.
summary(pca)

#our components are sorted from largest to smallest with regards to their standard deviation
#Proportion of Variance: This is the amount of variance the component accounts 
#for in the data, i.e. PC1 accounts for >80% of total variance in the data alone

#Cumulative Proportion: This is simply the accumulated amount of explained variance,
#ie. if we used the first 3 components we would be able to account for >80% of total variance in the data

#Visualize scree plot. Shows the percentage of variances explained by each principal component.
fviz_eig(pca,ncp = 10)

#A PCA variable plot.

fviz_pca_var(pca)
#Graph of variables. 
#Overall, Variables contributing similar information are located close together, 
#that is, they are correlated.
#The further away from the plot origin a variable lies, the stronger the impact 
#that variable has on the model.


var <- get_pca_var(pca)#A simple method to extract the results, for variables, from a PCA output
var$contrib#percentage contribution of each feature to each component

fviz_contrib(pca, choice = "var", axes = 1, top = 10)
#visualize feature contribution to PC1 - axes stands for 'Dimension'

#we reconstruct our original data from our first 2 principal components
#this is a way to see how well our data was represented by PCs.
#this is 'reverse PCA'!
hp_pca <- pca$x[,1:2]%*%t(pca$rotation[,1:2])
#I am multiplying the first two PCs with the transpose of
#a matrix whose columns contain the 'rotation'-The relationship
#between the original variables and the principal components.
#Matrix multiplication rule: a 351x2 matrix (351 rows showing coordinates of each of the 2 PCs)
#times a 2x24 matrix (transpose of rotation) gives a 351x24 matrix!

hp_pca <- data.frame(hp_pca)
hp_pca$Date <- hp_Wide$Date #I now have a df quite similar to my hp!

#transform to long format for plotting
hp_pca_long <- gather(hp_pca,hour,Ehp,1:24)#leave the Date out of the gather range
#translation: create new columns hour and Ehp, taking values from cols 1:24

hp_pca_long$hour <- gsub("X","",(hp_pca_long$hour))#remove the X from hour
hp_pca_long$hour <- as.integer(hp_pca_long$hour)

head(hp_pca_long)

#will only plot the 4 random dates I chose earlier, to see how similar the daily
#patterns are to the ones we produced using our original raw data
hp_pca_long_red<-hp_pca_long[hp_pca_long$Date %in% random_dates, ]
hp_pca_long_red$Date<-as.factor(hp_pca_long_red$Date)

ggplot(hp_pca_long_red)+
  geom_line(aes(x=hour,y=Ehp,group=Date,col=Date))+
  labs(title ="pca", x = "Time (Hour)", y = "Mean Wh of electricity")+
  theme_bw()


#compare the plot with the one from original data - achieved a good
# representation of the variability in our data, with only 2 features, instead of 24!


