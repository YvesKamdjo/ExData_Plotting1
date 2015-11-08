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

#We 4 graphics to draw, each will be done by a single function.

#Draw the first
part1 <- function(dt){
    
   
    #Just because I am using an French operating system
    Sys.setenv(LANG = "EN")
    
    #For this project, I choose to use the zoo package, it's more easy!
    require(zoo)
    
    #Create a new data frame with 2 columns: Concate Date and Time
    
    dt_touse <- data.frame(Date=paste(as.Date(as.character(dt$Date),format ='%d/%m/%Y'),dt$Time),
                           Global_Active_Power=dt$Global_active_power)
    
    ##Plot the ZOO object
    tss <- zoo(dt_touse$Global_Active_Power,
               strptime(unique(dt_touse$Date),
                        format = "%Y-%m-%d %H:%M:%S"
               ),
               frequency = 1
    )
    
    plot(tss,ylab = "Global Active Power (kilowatts)",xlab="")
    
}

#The second
part2 <- function(dt){
    
    
    #Just because I am using an French operating system
    Sys.setenv(LANG = "EN")
    
    #For this project, I choose to use the zoo package, it's more easy!
    require(zoo)
    
    #Create a new data frame with 2 columns: Concate Date and Time
    
    dt_touse <- data.frame(Date=paste(as.Date(as.character(dt$Date),format ='%d/%m/%Y'),dt$Time),
                           Global_Active_Power=dt$Voltage)
    
    ##Plot the ZOO object
    tss <- zoo(dt_touse$Global_Active_Power,
               strptime(unique(dt_touse$Date),
                        format = "%Y-%m-%d %H:%M:%S"
               ),
               frequency = 1
    )
    
    plot(tss,ylab = "Voltage",xlab="")
    
}

#The third
part3 <- function(dt){
    
    #For this project, I choose to use the zoo package, it's more easy!
    require(zoo)
    
    #Create a new data frame with 2 columns: Concate Date and Time
    
    dt_touse <- data.frame(Date=paste(as.Date(as.character(dt$Date),format ='%d/%m/%Y'),dt$Time),
                           Sub_metering_1=dt$Sub_metering_1,
                           Sub_metering_2=dt$Sub_metering_2,
                           Sub_metering_3=dt$Sub_metering_3
    )
    
    ##Plot the ZOO object
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
    
    plot(zoo1,ylab = "Energy sub metering",xlab="")
    lines(zoo2,col="red")
    lines(zoo3,col="blue")
    
    legend(x="topright",
           legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
           col=c("black","red","blue"),
           cex = 0.75,lwd = c(2.5,2.5,2.5)
    )
    
}

#And the last!
part4 <- function(dt){
    
    
    #Just because I am using an French operating system
    Sys.setenv(LANG = "EN")
    
    #For this project, I choose to use the zoo package, it's more easy!
    require(zoo)
    
    #Create a new data frame with 2 columns: Concate Date and Time
    
    dt_touse <- data.frame(Date=paste(as.Date(as.character(dt$Date),format ='%d/%m/%Y'),dt$Time),
                           Global_reactive_power=dt$Global_reactive_power)
    
    ##Plot the ZOO object
    tss <- zoo(dt_touse$Global_reactive_power,
               strptime(unique(dt_touse$Date),
                        format = "%Y-%m-%d %H:%M:%S"
               ),
               frequency = 1
    )
    
   
    
    plot(tss,ylab = "Global_reactive_power (kilowatts)",xlab="")
    
}


#Plot4 generate plot4.PNG file!
plot4 <- function(){
    
    dt <- readFile()
    #Open the graph in a new window
    windows(width=10, height=8)
    
    ##Create 2 rows and 2 columns
    par(mfrow=c(2, 2))
    
    ##Plot 4 graphs in the same view
    part1(dt)
    part2(dt)
    part3(dt)
    part4(dt)
    
    ##Generate the PNG file
    dev.copy(png,file="plot4.png")
    dev.off()
    
}