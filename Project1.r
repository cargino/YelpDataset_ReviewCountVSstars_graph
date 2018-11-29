#Project 
#Stat 128
#Cargino Chavez
#Prof. Michelle Norris

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library("plotGoogleMaps")
library(data.table)
library(dplyr)
library(magick)
library(plotly)
library(devtools)
install_github("yihui/animation")
library(animation)
library(gganimate)
library(plotly)


###############################
# Imported two 500 data point excel sheets with data from 
#Yelp about restaurants and reviews. This is a very small
#subset of the 170000 original data points and does not account
# for other data available for use. Also I have 
#decided to exclude restaurant below 3 stars because of
#my computer bad performance handling this size of data
#Dataset location: https://www.yelp.com/dataset

# Both datasets were originally in JSON format and had to be converted
# to excel. Due to the size I had to use a special tool: 
# https://github.com/nickbnf/glogg

#sIDE NOTE: I originally planned to do a heat map of the restaurants 
#with the best reviews based on the city. But the amount of data needed 
# to do that would sometimes crash my RStudio. Also i had issues with 
#the coordinate system in plotGoogleMaps. Might come back to this later
#original plan: https://datascienceplus.com/building-heatmaps-in-r/
options(scipen=999) 

                ####   ARIZONA DATASET  ####
AZ_Data$neighborhood=NULL
AZ_Backup=AZ_Data
AZ_Test=AZ_Backup
AZ_Data=AZ_Test
AZ_Data=AZ_Test%>%group_by(AZ_Data$city)%>%filter(n()>10)

#Decided to single out each city to see the patterns
chandler=subset(AZ_Data,city=="Chandler")
chandlerAvg=mean(AZ_Data$stars)

gilbert=subset(AZ_Data,city=="Gilbert")
gilbertAvg=mean(AZ_Data$stars)

glendale=subset(AZ_Data,city=="Glendale")
GlendaleAvg=mean(AZ_Data$stars)

mesa=subset(AZ_Data,city=="Mesa")
MesaAvg=mean(AZ_Data$stars)

peoria=subset(AZ_Data,city=="Peoria")
PeoriaAvg=mean(AZ_Data$stars)
  
phoenix=subset(AZ_Data,city=="Phoenix")
PhoenixAvg=mean(AZ_Data$stars)

scottsdale=subset(AZ_Data,city=="Scottsdale")
ScottsdaleAvg=mean(AZ_Data$stars)

tempe=subset(AZ_Data,city=="Tempe")
TempeAvg=mean(AZ_Data$stars)

theme_set(theme_bw())
data("AZ_Data",package = "ggplot2")

 
###Individual graphs###

graphChandler=ggplot(chandler,aes(chandler$stars,chandler$review_count,fill=chandler$stars))+
  geom_point(aes(col=review_count, size=review_count))+
  geom_smooth(method="loess", se=stars) + 
  xlim(c(3, 5)) + 
  ylim(c(0, 500)) + 
  labs(subtitle="Review Count vs stars", 
       y="Review Count", 
       x="Stars", 
       title="Chandler AZ", 
       caption = "Source: Yelp")
graphGilbert=ggplot(gilbert,aes(gilbert$stars,gilbert$review_count,fill=gilbert$stars))+
  geom_point(aes(col=review_count, size=review_count))+
  geom_smooth(method="loess", se=stars) + 
  xlim(c(3, 5)) + 
  ylim(c(0, 500)) + 
  labs(subtitle="Review Count vs stars", 
       y="Review Count", 
       x="Stars", 
       title="Gilbert AZ", 
       caption = "Source: Yelp")
graphGlendale=ggplot(glendale,aes(glendale$stars,glendale$review_count,fill=glendale$stars))+
  geom_point(aes(col=review_count, size=review_count))+
  geom_smooth(method="loess", se=stars) + 
  xlim(c(3, 5)) + 
  ylim(c(0, 500)) + 
  labs(subtitle="Review Count vs stars", 
       y="Review Count", 
       x="Stars", 
       title="Glendale AZ", 
       caption = "Source: Yelp")
graphMesa=ggplot(mesa,aes(mesa$stars,mesa$review_count,fill=mesa$stars))+
  geom_point(aes(col=review_count, size=review_count))+
  geom_smooth(method="loess", se=stars) + 
  xlim(c(3, 5)) + 
  ylim(c(0, 500)) + 
  labs(subtitle="Review Count vs stars", 
       y="Review Count", 
       x="Stars", 
       title="Mesa AZ", 
       caption = "Source: Yelp")
graphPeoria=ggplot(peoria,aes(peoria$stars,peoria$review_count,fill=peoria$stars))+
  geom_point(aes(col=review_count, size=review_count))+
  geom_smooth(method="loess", se=stars) + 
  xlim(c(3, 5)) + 
  ylim(c(0, 500)) + 
  labs(subtitle="Review Count vs stars", 
       y="Review Count", 
       x="Stars", 
       title="Peoria AZ", 
       caption = "Source: Yelp")
graphPhoenix=ggplot(phoenix,aes(phoenix$stars,phoenix$review_count,fill=phoenix$stars))+
  geom_point(aes(col=review_count, size=review_count))+
  geom_smooth(method="loess", se=stars) + 
  xlim(c(3, 5)) + 
  ylim(c(0, 500)) + 
  labs(subtitle="Review Count vs stars", 
       y="Review Count", 
       x="Stars", 
       title="Phoenix AZ", 
       caption = "Source: Yelp")
graphScottsdale=ggplot(scottsdale,aes(scottsdale$stars,scottsdale$review_count,fill=scottsdale$stars))+
  geom_point(aes(col=review_count, size=review_count))+
  geom_smooth(method="loess", se=stars) + 
  xlim(c(3, 5)) + 
  ylim(c(0, 500)) + 
  labs(subtitle="Review Count vs stars", 
       y="Review Count", 
       x="Stars", 
       title="Scottsdale AZ", 
       caption = "Source: Yelp")
graphTempe=ggplot(tempe,aes(tempe$stars,tempe$review_count,fill=tempe$stars))+
  geom_point(aes(col=review_count, size=review_count))+
  geom_smooth(method="loess", se=stars) + 
  xlim(c(3, 5)) + 
  ylim(c(0, 500)) + 
  labs(subtitle="Review Count vs stars", 
       y="Review Count", 
       x="Stars", 
       title="Tempe AZ", 
       caption = "Source: Yelp")
#### Animated Graph ###

# This method does not create a gif, its an animated graph
AZplot <- ggplot(AZ_Data, aes(stars,review_count, color = city)) +
  geom_point(aes(size = city, frame = review_count, ids = stars)) +
  scale_x_log10()
#Due to the size of the dataset it takes a moment to load --ignoring size warnings
#Tried doing a bigger set, but it would sometimes crash RStudio
AZ_Animation <- ggplotly(AZplot)
#Learned about ggplotly here: 

#https://plot.ly/ggplot2/animations/
##########


            ####   NEVADA DATASET  ####
#This dataset looks significantly different than the Arizona dataset 
#because of the lack of Reno. Apparently Yelp does not keep track of 
#Reno, at least on this dataset

NV_Data=NV_Data_xls
NV_Data$neighborhood=NULL
NV_Data=NV_Data%>%group_by(NV_Data$city)%>%filter(n()>10)

#Note: I am not including individuals graphs, I only did that to see
#the patterns as I was learning how the data was corrolated 

NVplot <- ggplot(NV_Data, aes(stars,review_count, color = city)) +
  geom_point(aes(size = city, frame = review_count, ids = stars)) +
  scale_x_log10()

#Due to the size of the dataset it takes a moment to load
#Tried doing a bigger set, but it would sometimes crash RStudio
NV_Animation <- ggplotly(NVplot)
