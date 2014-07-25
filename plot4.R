#
#   File Name : plot4.R 
#
#   Author: Prasanna Limaye
#
#   Subject : Coursera Project 2 ( Exploratory )
#
#   Date Created: 20 July 2014
#
#   Objective: 
#
# Q 4 : Across the united states, how have emissions from coal combustion-related sources changed from 1999-2008
#

library(ggplot2)

#
# from the SCC user manual, we had chosen explitely where coal is figured in Level.Three and combustion is figured in Level.One. 
# Though this can be debated, however this is as per my best judgement. 
#

coalCombustion <- function(str) {
  # this function checks if the Code passed belongs to the coal combustion related. if yes then return TRUE
  
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



#
# filter those SCC code which matthes the coal consumptions
#
#
# 

#read the rds file
pm25data <- readRDS("summarySCC_PM25.rds")
classification <- readRDS("Source_Classification_Code.rds")

# run over all the SCC code to check if it belongs to the coal consumption
# get it as index.
ind4 <-  lapply(pm25data$SCC,coalCombustion)

#filter those rows. 
pm25coal <-  pm25data[as.logical(ind4),]

# get the aggregate and scale by 1000
df4 <- pm25coal 
ag4 <- aggregate(Emissions ~ year,data=df4,FUN = "sum")
ag4$Emissions <- ag4$Emissions/1000


# open the png file
png(file="plot4.png",bg="transparent",units="px",height=480,width=640)

# do the ggplotting stuff - we will use emission vs year, red points on black line, with count scaled to 000s, label it and chose tick marks
g4 <- ggplot(ag4, aes(year,Emissions)) 
p4 <- g4 + geom_line(size=3) + geom_point(color="red",size=5) + geom_smooth(method="lm", se=TRUE, colour="steelblue")
p4 <- p4    + labs(title = "all US - PM25 Emission Total in (000's Tons) - Coal Combustion related Sources") 
p4 <- p4   + labs(x = "Year") + labs(y = "PM2.5 Total (In 000's) Tons"  )  + scale_x_continuous(breaks=c(1999,2002,2005,2008,2011))

print(p4)

# close the device
dev.off()
