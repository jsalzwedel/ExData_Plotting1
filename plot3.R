############################################################
# Make a plot showing three different energy sub meterings
# vs time from the electric power consumption dataset.  Save
# it to a png.
############################################################

plot3 <- function(){
    # first read in the data
    data <- read.table("household_power_consumption.txt", sep = ";",
                       na.strings="?", header = T)
    # convert the date column from strings to dates
    data$Date <- as.Date(data$Date, "%d/%m/%Y")
    
    #subset the data to use the specified dates (2007-02-01 and 2007-02-02)
    dates <- c("2007-02-01","2007-02-02")
    dataDates <- data[data$Date %in% as.Date(dates),]
    
    # Copy the data to a new data frame with a POSIXlt datetime column
    newData <- within(dataDates, 
                      datetime <- as.POSIXlt(paste(Date,Time), 
                                             format = "%Y-%m-%d %H:%M"))
    
    # Open a graphics device for the plot
    png(file = "plot3.png")
    
    # Draw the plot
    with(newData,plot(datetime, Sub_metering_1, ylab = "Energy sub metering",
                      xlab = "", type = "l"))
    with(newData,lines(datetime, Sub_metering_2, ylab = "Energy sub metering", 
                       xlab = "", type = "l", col="red"))
    with(newData,lines(datetime, Sub_metering_3, ylab = "Energy sub metering", 
                       xlab = "", type = "l", col="blue"))
    
    # Add the legend
    legend("topright",
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           col = c("black","red","blue"), lty=c(1,1))
    
    # Close the graphics device file
    dev.off()
}