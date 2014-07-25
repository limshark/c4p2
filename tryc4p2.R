#
# Q1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#    Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
#   for each of the years 1999, 2002, 2005, and 2008.
#


pm25data <- readRDS("summarySCC_PM25.rds")
classification <- readRDS("Source_Classification_Code.rds")
pm25vsYears <- transform(pm25data,years=factor(year))
ag1 <- aggregate(Emissions ~ year,data=pm25data,FUN = "sum")
ag1$Emissions <- ag1$Emissions/1000 

png(file="plot1.png",bg="transparent", units="px",height=480,width=480)
plot(y=ag1$Emissions,x=ag1$year, main="Total PM2.5 Emissions in (Thousands Tons)", ylab="Total Emissions ( in '000s  Tons)", xlab="Year",type = "h", col = "Blue", lwd = 50,axes=FALSE)
lines(y=ag1$Emissions,x=ag1$year)
axis(1,at=ag1$year,labels=ag1$year)
axis(2)
dev.off()


#
# Q 2. Have total emissions from  PM2.5  decreased in the BaltimoreCity, Maryland (fips=="24510") from 1999 to 2008? 
#     Use the base plotting system to make a plot answering this question.
# 

ind2 <-  (pm25data$type == "ON-ROAD") & (pm25data$fips == "24510")
pm25VehicleInBaltimore <-  pm25data[ind2,]

df2 <- pm25VehicleInBaltimore 

ag2 <- aggregate(Emissions ~ year,data=df2,FUN = "sum")


png(file="plot2.png",bg="transparent",units="px",height=480,width=480)
plot(y=ag2$Emissions,x=ag2$year, main="Total PM2.5 Emissions in 'Baltimore' for 'On-Road Vehicles'", ylab="Total PM2.5 Emissions in Tons", xlab="Year",type = "h", col = "Red", lwd = 50,axes=FALSE)
lines(y=ag2$Emissions,x=ag2$year)
axis(1,at=ag2$year,labels=ag2$year)
axis(2)
dev.off()


#
# Q3 : Of the four types of sources indiated by the type ( point, nonpoint, onroad,nonroad) variable, which of these four sources have seen decreases in emissions
#      fromm 1999 to 2008 for Baltimore City? which have seen increases in emission from 1999-2008 ? Use the ggplot2 plotting system to make a plot answer these
#       Questions. 
#

ind3 <- (((pm25data$type == "ON-ROAD") | (pm25data$type == "NON-ROAD") | (pm25data$type == "POINT") | (pm25data$type == "NONPOINT")) & (pm25data$fips == "24510"))
pm25InBaltimore <-  pm25data[ind3,]

df3 <- pm25InBaltimore 

ag3 <- aggregate(Emissions ~ year + type,data=df3,FUN = "sum")


library(ggplot2)

png(file="plot3.png",bg="transparent",units="px",height=480,width=480)
g3 <- ggplot(ag3, aes(year,Emissions)) 
p3 <- g3 + geom_line(aes(color=type),size=4) + labs(title = "PM2.5 Emission in Tons in Baltimore for different Sources") + labs(x = "Year") + labs(y = expression(PM[2.5]))
p3 <- p3 + scale_x_continuous(breaks=c(1999,2002,2005,2008,2011))
print(p3)
dev.off()

#
# Q 4 : Across the united states, how have emissions from coal combustion-related sources changed from 1999-2008
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
  

# filter those SCC code which matthes the coal consumptions
#
#
  
ind4 <-  lapply(pm25data$SCC,coalCombustion)


pm25coal <-  pm25data[as.logical(ind4),]

df4 <- pm25coal 

ag4 <- aggregate(Emissions ~ year,data=df4,FUN = "sum")
ag4$Emissions <- ag4$Emissions/1000


library(ggplot2)

png(file="limaye4.png",bg="transparent",units="px",height=480,width=480)
g4 <- ggplot(ag4, aes(year,Emissions)) 
p4 <- g4 + geom_line(size=3) + geom_point(color="red",size=5) 

p4 <- p4    + labs(title = "PM25 Emission Total in (000'sTons) - Coal Combustion related Sources") 
p4 <- p4   + labs(x = "Year") + labs(y = "PM2.5 Total (In 000's) Tons"  )  + scale_x_continuous(breaks=c(1999,2002,2005,2008,2011))


print(p4)
dev.off()

#
#    Q5: How have emissions from motor behicle sources changed from 1999-2008 in Baltimore city.
#
ind5 <-  ((pm25data$type == "ON-ROAD") & (pm25data$fips == "24510"))
pm25MotorInBaltimore <-  pm25data[ind5,]

df5 <- pm25MotorInBaltimore 


ag5 <- aggregate(Emissions ~ year+type,data=df5,FUN = "sum")


library(ggplot2)

png(file="limaye5.png",bg="transparent",units="px",height=480,width=480)
g5 <- ggplot(ag5, aes(year,Emissions)) 
p5 <- g5 + geom_line(aes(color=type),size=2) + labs(title = "PM25 Emission Total Tons in Baltimore for Motor Vehicle") + labs(x = "Year") + labs(y = expression(PM[2.5]))
p5 <- p5 + geom_point(size=4,colour="black")  + scale_x_continuous(breaks=c(1999,2002,2005,2008,2011))
print(p5)
dev.off()


#
#  Q 6 : Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County
#         California ( fips = "06037" ). Which city has seen greater changes over time in motor behicle emissions. 
#

ind6 <-  ((pm25data$type == "ON-ROAD") & ((pm25data$fips == "24510")|(pm25data$fips == "06037")))
pm25MotorIn2Cities <-  pm25data[ind6,]
df6 <- pm25MotorIn2Cities 

df6$City <- ifelse(df6$fips == "06037","Los Angeles","Baltimore")

ag6 <- aggregate(Emissions ~ year+City,data=df6,FUN = "sum")




library(ggplot2)

png(file="limaye6.png",bg="transparent",units="px",height=480,width=480)
g6 <- ggplot(ag6, aes(year,Emissions)) + geom_point(size=4)  + labs(title = "PM25 Emission 'Total' in Baltimore/LA for Motor Vehicle") + labs(x = "Year") + labs(y = expression(PM[2.5]))  
p6 <- g6 + geom_line(aes(colour=City),size=2) + facet_grid(. ~ City) + scale_x_continuous(breaks=c(1999,2002,2005,2008,2011))
 
print(p6)
dev.off()


