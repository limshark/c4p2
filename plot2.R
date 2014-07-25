#
#   File Name : plot2.R 
#
#   Author: Prasanna Limaye
#
#   Subject : Coursera Project 2 ( Exploratory )
#
#   Date Created: 20 July 2014
#
# Objective
#
# Q 2. Have total emissions from  PM2.5  decreased in the BaltimoreCity, Maryland (fips=="24510") from 1999 to 2008? 
#     Use the base plotting system to make a plot answering this question.
# 

# read the files
pm25data <- readRDS("summarySCC_PM25.rds")
classification <- readRDS("Source_Classification_Code.rds")

# chose only the on-road catagory for the fips of Baltimore. then filter it. 
ind2 <-  (pm25data$type == "ON-ROAD") & (pm25data$fips == "24510")
pm25VehicleInBaltimore <-  pm25data[ind2,]

# get the date frame and get an aggregate Emissions vs year  getting the big total.
df2 <- pm25VehicleInBaltimore 
ag2 <- aggregate(Emissions ~ year,data=df2,FUN = "sum")

#open the png file
png(file="plot2.png",bg="transparent",units="px",height=480,width=480)

# plot the stuff
plot(y=ag2$Emissions,x=ag2$year, main="Total PM2.5 Emissions in 'Baltimore' for 'On-Road Vehicles'", ylab="Total PM2.5 Emissions in Tons", xlab="Year",type = "h", col = "Red", lwd = 50,axes=FALSE)
lines(y=ag2$Emissions,x=ag2$year)
axis(1,at=ag2$year,labels=ag2$year)
axis(2)

# close the device
dev.off()
