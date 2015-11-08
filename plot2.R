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

 #I am using a French operating system, dates are in french
plot2 <- function(){
    
    #Read the data
    dt <- readFile()
    
    #For this project, I choose to use the "zoo package" to deal with Times Series Objects
    require(zoo)
    
    #Create a new data frame with 2 columns: Concate Date and Time
    
    dt_touse <- data.frame(Date=paste(as.Date(as.character(dt$Date),format ='%d/%m/%Y'),dt$Time),
                           Global_Active_Power=dt$Global_active_power
                           )
    
    ##Create the ZOO object with 2 variables
    z <- zoo(dt_touse$Global_Active_Power,#The Y-axis variable
               strptime(unique(dt_touse$Date),
                        format = "%Y-%m-%d %H:%M:%S"
                        ),#X-axis, the date
                frequency = 1
               )
    
    par(mfrow=c(1, 1))#to plot one graph in the whole window
    
    plot(z,ylab = "Global Active Power (kilowatts)",xlab="")
    
    #Generate a PNG file
    dev.copy(png,file="plot2.png")
    dev.off()
}

