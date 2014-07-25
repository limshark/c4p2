#
#   File Name : plot3.R 
#
#   Author: Prasanna Limaye
#
#   Subject : Coursera Project 2 ( Exploratory )
#
#   Date Created: 20 July 2014
#
#   Objective: 
#
# Q3 : Of the four types of sources indiated by the type ( point, nonpoint, onroad,nonroad) variable, which of these four sources have seen decreases in emissions
#      fromm 1999 to 2008 for Baltimore City? which have seen increases in emission from 1999-2008 ? Use the ggplot2 plotting system to make a plot answer these
#       Questions. 
#

# we will use ggplot2 

library(ggplot2)

# open the rds files. 

pm25data <- readRDS("summarySCC_PM25.rds")
classification <- readRDS("Source_Classification_Code.rds")

# do the filtering get only data which has 4 data types and fips is baltimore. 
ind3 <- (((pm25data$type == "ON-ROAD") | (pm25data$type == "NON-ROAD") | (pm25data$type == "POINT") | (pm25data$type == "NONPOINT")) & (pm25data$fips == "24510"))
pm25InBaltimore <-  pm25data[ind3,]

# get the aggregate for Emission vs year for different types. 
df3 <- pm25InBaltimore 
ag3 <- aggregate(Emissions ~ year + type,data=df3,FUN = "sum")



# get the png file
png(file="plot3.png",bg="transparent",units="px",height=480,width=640)

#plot the stuff 
g3 <- ggplot(ag3, aes(year,Emissions)) 
p3 <- g3  + geom_point(colour="black",size=3) + labs(title = "PM2.5 Emission in Tons 'Baltimore' for different Types") + labs(x = "Year") + labs(y = expression('Emission in Tons for' * PM[2.5]))
p3 <- p3 + scale_x_continuous(breaks=c(1999,2002,2005,2008,2011)) + geom_smooth(method="lm", se = TRUE, aes(color=type),size=1) + facet_grid(. ~ type)
print(p3)
dev.off()
