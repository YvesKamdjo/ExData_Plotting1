## This function retrieves the data from the file
readFile <- function(){
    
    ##Set the date limit
    d1 <- as.Date('01/02/2007',format ='%d/%m/%Y')
    d2 <- as.Date('02/02/2007',format ='%d/%m/%Y')
     
    fileURL <- 'household_power_consumption.txt';
    
    ##Mange the file data
    dt <- read.table(fileURL,sep=";",na.strings = "?",header = TRUE)
    
    ##Filter data from 2007-01-02 to 2007-02-02
    dt_toUse <- subset(dt, as.Date(as.character(Date),format ='%d/%m/%Y')>=d1
                       & as.Date(as.character(Date),format ='%d/%m/%Y')<=d2
                 )
    
    return(dt_toUse)
    
}

##This function plots the histogram with the R basic system.
plot1 <- function(){
    ##Get the data
    dt <- readFile()
    
    ##Plot the histogram
    with(dt,
         hist(Global_active_power, 
              col = "red",
              main = "Global Active Power",
              xlab = "Global Active Power (kilowatts)",
              ylim = c(0,1200))
         )
    ##Transform to PNG
    dev.copy(png,file="plot1.png")
    dev.off()
}