#
# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#    Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
#   for each of the years 1999, 2002, 2005, and 2008.
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
# 2. Have total emissions from  PM2.5  decreased in the BaltimoreCity, Maryland (fips=="24510") from 1999 to 2008? 
#     Use the base plotting system to make a plot answering this question.
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
# Q3
#
ind3 <- (((pm25data$type == "ON-ROAD") | (pm25data$type == "NON-ROAD") | (pm25data$type == "POINT") | (pm25data$type == "NONPOINT")) & (pm25data$fips == 24510))
pm25InBaltimore <-  pm25data[ind3,]

df3 <- pm25InBaltimore 

ag <- aggregate(Emissions ~ year+type,data=df3,FUN = "sum")

library(ggplot2)

png(file="limaye3.png",bg="transparent",units="px",height=480,width=480)
g <- ggplot(ag, aes(year,Emissions)) 
p <- g + geom_line(aes(color=type),size=4) + labs(title = "PM25 Emission Count in Baltimore for different Sources") + labs(x = "Year") + labs(y = expression(PM[2.5]))
print(p)
dev.off()


#
#  4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
#  
#      Get all the coals yanked out from the data and display the graph
#

coalCombustion <- function(str) {
  # check if it is coal combustion related  stuff 
  
  #split into level1 and level3. 
  
  #level1  -  101,102,103
  #level2  -  001,002,003,008 
  
  validValues <- c("101001","101002","101003","101008",
                   "102001","102002","102003","102008",
                   "103001","103002","103003","103008")
  
  str1 <- substring(str,1,6)
  
  if(str1 %in% validValues ) { 
    return(TRUE)
  }
  else {
    return(FALSE)
  }
  
}

classprint <- function() {
  for ( i in seq_len(nrow(classification))) {
    if( lim1[[i]] == TRUE){
      print(classification$SCC.Level.Three[[i]])
    }
  }
}
  

# filter those SCC code which mathes the coal consumptions
#
#
  
ind4 <-  lapply(pm25data$SCC,coalCombustion)


pm25coal <-  pm25data[as.logical(ind4),]

df4 <- pm25coal 

ag <- aggregate(Emissions ~ year,data=df4,FUN = "mean")
ag$Emissions <- ag$Emissions/1000

library(ggplot2)

png(file="limaye4.png",bg="transparent",units="px",height=480,width=480)
g <- ggplot(ag, aes(year,Emissions)) 
p <- g + geom_line(size=3) + geom_point(color="red",size=5) 

p <- p    + labs(title = "PM25 Emission Count - Coal Combustion related Sources") 
p <- p   + labs(x = "Year") + labs(y = "PM2.5 Mean (In 000's)"  )


print(p)
dev.off()


#
# Plot5 : How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
#
  
ind5 <-  ((pm25data$type == "ON-ROAD") & (pm25data$fips == 24510))
pm25MotorInBaltimore <-  pm25data[ind5,]

df5 <- pm25MotorInBaltimore 

ag <- aggregate(Emissions ~ year+type,data=df5,FUN = "mean")

library(ggplot2)

png(file="limaye5.png",bg="transparent",units="px",height=480,width=480)
g <- ggplot(ag, aes(year,Emissions)) 
p <- g + geom_line(aes(color=type),size=2) + labs(title = "PM25 Emission Mean in Baltimore for Motor Vehicle") + labs(x = "Year") + labs(y = expression(PM[2.5]))
p <- p + geom_point(size=4,colour="black")
print(p)
dev.off()


