## This function retrieves the data from the file
readFile <- function(){
    
    ##Set the date limit
    d1 <- as.Date('01/02/2007',format ='%d/%m/%Y')
    d2 <- as.Date('02/02/2007',format ='%d/%m/%Y')
    
    fileURL <- 'household_power_consumption.txt';
    
    ##Mange the file data
    dt <- read.table(fileURL,sep=";",na.strings = "?",header = TRUE)
    
    dt_toUse <- subset(dt, as.Date(as.character(Date),format ='%d/%m/%Y')>=d1
                       & as.Date(as.character(Date),format ='%d/%m/%Y')<=d2
    )
    
    return(dt_toUse)
    
}


plot3 <- function(){
    
    Sys.setenv(TZ='EST')
    #Read the data
    dt <- readFile()
    
    #Just because I am using an French operating system
    Sys.setenv(LANG = "EN")
    
    #For this project, I choose to use the zoo package, it's more easy!
    require(zoo)
    
    #Create a new data frame 
    
    dt_touse <- data.frame(Date=paste(as.Date(as.character(dt$Date),format ='%d/%m/%Y'),dt$Time),
                           Sub_metering_1=dt$Sub_metering_1,
                           Sub_metering_2=dt$Sub_metering_2,
                           Sub_metering_3=dt$Sub_metering_3
                           )
    
    ##Create and Plot the ZOO objects
    zoo1 <- zoo(dt_touse$Sub_metering_1,
               strptime(unique(dt_touse$Date),
                        format = "%Y-%m-%d %H:%M:%S"
               ),
               frequency = 1
    )
    
    zoo2 <- zoo(dt_touse$Sub_metering_2,
                strptime(unique(dt_touse$Date),
                         format = "%Y-%m-%d %H:%M:%S"
                ),
                frequency = 1
    )
    zoo3 <- zoo(dt_touse$Sub_metering_3,
                strptime(unique(dt_touse$Date),
                         format = "%Y-%m-%d %H:%M:%S"
                ),
                frequency = 1
    )
    
    #To use all the screen space
    par(mfrow=c(1, 1),xpd=FALSE)
    
    #Initiating the plot and add the first graph: zoo1
    plot(zoo1,ylab = "Energy sub metering",xlab="")
    
    #Add the second graph
    lines(zoo2,col="red",lwd=2.5)
    
    #Add the third one!
    lines(zoo3,col="blue",lwd=2.5)
    
    #Customize and add the legend
    legend(x="topright",
           legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
           col=c("black","red","blue"),
           lwd = c(2.5,2.5,2.5),
           cex = 0.75
           )
    
    #Generate the PNG file
    dev.copy(png,file="plot3.png")
    dev.off()
}

