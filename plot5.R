#
#   File Name : plot5.R 
#
#   Author: Prasanna Limaye
#
#   Subject : Coursera Project 2 ( Exploratory )
#
#   Date Created: 20 July 2014
#
#   Objective: 
#    Q5: How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore city.
#
library(ggplot2)

# read the rds file in project directory
pm25data <- readRDS("summarySCC_PM25.rds")
classification <- readRDS("Source_Classification_Code.rds")

# select baltimore and on-road classification. 
ind5 <-  ((pm25data$type == "ON-ROAD") & (pm25data$fips == "24510"))
pm25MotorInBaltimore <-  pm25data[ind5,]

# get the aggregate
df5 <- pm25MotorInBaltimore 
ag5 <- aggregate(Emissions ~ year+type,data=df5,FUN = "sum")


png(file="plot5.png",bg="transparent",units="px",height=480,width=640)

# do the plotting stuff
g5 <- ggplot(ag5, aes(year,Emissions)) 
p5 <- g5 + geom_line(aes(color=type),size=2) + labs(title = "PM25 Emission Total Tons in Baltimore for Motor Vehicle") + labs(x = "Year") + labs(y = expression('Total Emission for '*PM[2.5]))
p5 <- p5 + geom_point(size=4,colour="black")  + scale_x_continuous(breaks=c(1999,2002,2005,2008),limits=c(1996,2011)) + geom_smooth(method="lm",se = TRUE, col="steelblue")
print(p5)

dev.off()

