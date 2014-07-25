#
#   File Name : plot6.R 
#
#   Author: Prasanna Limaye
#
#   Subject : Coursera Project 2 ( Exploratory )
#
#   Date Created: 20 July 2014
#
#   Objective: 
#   Q 6 : Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County
#         California ( fips = "06037" ). Which city has seen greater changes over time in motor behicle emissions. 
#

library(ggplot2)

# read rds file
pm25data <- readRDS("summarySCC_PM25.rds")
classification <- readRDS("Source_Classification_Code.rds")

# get the data for log angeles and baltimore. 
ind6 <-  ((pm25data$type == "ON-ROAD") & ((pm25data$fips == "24510")|(pm25data$fips == "06037")))
pm25MotorIn2Cities <-  pm25data[ind6,]
df6 <- pm25MotorIn2Cities 

# convert human readable form for fips 
df6$City <- ifelse(df6$fips == "06037","Los Angeles","Baltimore")

ag6 <- aggregate(Emissions ~ year+City,data=df6,FUN = "sum")

# chose the png file to open
png(file="plot6.png",bg="transparent",units="px",height=480,width=480)

# have two facet panels as per City and use different colour graphs to draw the trends in each city
g6 <- ggplot(ag6, aes(year,Emissions)) + geom_point(size=4)  + labs(title = "PM25 Emission 'Total' in Baltimore/LA for Motor Vehicle") + labs(x = "Year") + labs(y = expression(PM[2.5]))  
p6 <- g6 + geom_line(aes(colour=City),size=2) + facet_grid(. ~ City) + scale_x_continuous(breaks=c(1999,2002,2005,2008,2011))

print(p6)

dev.off()

