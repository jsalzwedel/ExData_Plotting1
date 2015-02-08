############################################################
# Make a canvas showing several plots made from the 
# electric power consumption dataset.  Save the canvas to
# a png.
############################################################

plot4 <- function(){
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
    
    # Open a graphics device for the plots
    png(file = "plot4.png")
    
    # Prepare a canvas to draw the 2 by 2 plots
    par(mfrow = c(2,2))
    
    # Draw the plots
    plotGlobalActivePower(newData)
    plotVoltage(newData)
    plotSubMetering(newData)
    plotGlobalReactivePower(newData)
    
    # Close the graphics device file
    dev.off()
}


plotGlobalActivePower <- function(data){
    # Draw a plot of Global Active Power vs Time for the prepared data set
    with(data,plot(datetime, Global_active_power,
                      ylab = "Global Active Power",
                      xlab = "", type="l"))
}

plotVoltage <- function(data){
    # Draw a plot of Voltage vs Time for the prepared data set
    with(data,plot(datetime, Voltage,
                   ylab = "Voltage",
                   xlab = "datetime", type="l"))
}

plotSubMetering <- function(data){
    # Draw a plot showing three different energy sub meterings vs time for the
    # prepared data set
    with(data,plot(datetime, Sub_metering_1, ylab = "Energy sub metering",
                      xlab = "", type = "l"))
    with(data,lines(datetime, Sub_metering_2, ylab = "Energy sub metering", 
                       xlab = "", type = "l", col="red"))
    with(data,lines(datetime, Sub_metering_3, ylab = "Energy sub metering", 
                       xlab = "", type = "l", col="blue"))
    
    # Add a legend (no border box)
    legend("topright",
           legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
           col = c("black","red","blue"), lty=c(1,1), bty = "n")
    
}

plotGlobalReactivePower <- function(data){
    # Draw a plot of Global Reactive Power vs Time for the prepared data set
    with(data,plot(datetime, Global_reactive_power,
                   ylab = "Global_reactive_power",
                   xlab = "datetime", type="l"))
}