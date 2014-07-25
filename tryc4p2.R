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

png(file="limaye2.png",bg="transparent")
plot(y=ag$Emissions,x=ag$year, main="Total PM2.5 Emissions in 'Baltimore' for On 'Road Vehicles'", ylab="Total Emissions", xlab="Year",type = "h", col = "Red", lwd = 50,axes=FALSE)
lines(y=ag$Emissions,x=ag$year)
axis(1,at=ag$year,labels=ag$year)
axis(2)
dev.off()

#
# Q3 : drawing a ggplot2 graph between PM2.5 by levels against years in x axis. use 4 types for the plotting : point,nopoint,onroad and non onroad
#


ind3 <-  (((pm25data$type == "ON-ROAD") | (pm25data$type == "NON-ROAD") | (pm25data$type == "POINT") | (pm25data$type == "NONPOINT")) & (pm25data$fips == 24510))
pm25InBaltimore <-  pm25data[ind3,]

df3 <- pm25InBaltimore 

ag <- aggregate(Emissions ~ year+type,data=df3,FUN = "sum")

library(ggplot2)

png(file="limaye3.png",bg="transparent",units="px",height=480,width=480)
g <- ggplot(ag, aes(year,Emissions)) 
p <- g + geom_line(aes(color=type),size=4) + labs(title = "PM25 Emission Count in Baltimore for different Sources") + labs(x = "Year") + labs(y = expression(PM[2.5]))
print(p)
dev.off()



