#
# Try reading in a file
#


pm25data <- readRDS("summarySCC_PM25.rds")
classification <- readRDS("Source_Classification_Code.rds")
pm25vsYears <- transform(pm25data,years=factor(year))
ag <- aggregate(Emissions ~ year,data=pm25data,FUN = "sum")
ag$Emissions <- ag$Emissions/1000 
png(file="limaye1.png",bg="transparent")
plot(y=ag$Emissions,x=ag$year, main="Total PM2.5 Emissions  (Thousands)", ylab="Total Emissions ( in '000s )", xlab="Year",type = "h", col = "Blue", lwd = 50,axes=FALSE)
lines(y=ag$Emissions,x=ag$year)
axis(1,at=ag$year,labels=ag$year)
axis(2)
dev.off()


#
# Question 2   subset of data type is "on-road"  and city is baltimore city ( Fips = 24510 )
# 

ind <-  (pm25data$type == "ON-ROAD") & (pm25data$fips == 24510)
pm25VehicleInBaltimore <-  pm25data[ind,]

df <- pm25VehicleInBaltimore 

ag <- aggregate(Emissions ~ year,data=df,FUN = "sum")

ag$Emissions <- ag$Emissions/1000 
png(file="limaye2.png",bg="transparent")
plot(y=ag$Emissions,x=ag$year, main="Total PM2.5 Emissions  (Thousands) in 'Baltimore' for On 'Road Vehicles'", ylab="Total Emissions ( in '000s )", xlab="Year",type = "h", col = "Red", lwd = 50,axes=FALSE)
lines(y=ag$Emissions,x=ag$year)
axis(1,at=ag$year,labels=ag$year)
axis(2)
dev.off()


