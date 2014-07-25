#
#   File Name : plot1.R 
#
#   Author: Prasanna Limaye
#
#   Subject : Coursera Project 2 ( Exploratory )
#
#   Date Created: 20 July 2014
#
#   Objective
#
# Q1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#    Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
#   for each of the years 1999, 2002, 2005, and 2008.
#

# read the files
pm25data <- readRDS("summarySCC_PM25.rds")
classification <- readRDS("Source_Classification_Code.rds")

# create aggregate, scale the y by 1000
ag1 <- aggregate(Emissions ~ year,data=pm25data,FUN = "sum")
ag1$Emissions <- ag1$Emissions/1000 

# open a plot1.png file
png(file="plot1.png",bg="transparent", units="px",height=480,width=640)

# do the plotting stuff using base graphics
plot(y=ag1$Emissions,x=ag1$year, main="All US Total PM2.5 Emissions in (Thousands Tons)", ylab="PM2.5 Emissions ( in '000s  Tons)", xlab="Year",type = "h", col = "Blue", lwd = 50,axes=FALSE)
lines(y=ag1$Emissions,x=ag1$year)
axis(1,at=ag1$year,labels=ag1$year)
axis(2)

# close the device
dev.off()

