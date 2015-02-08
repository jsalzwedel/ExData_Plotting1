############################################################
# Make a plot of Global Active Power vs time from the
# electric power consumption dataset.  Save it to a png.
############################################################

plot2 <- function(){
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
    
    # Draw the histogram
    with(newData,plot(datetime, Global_active_power,
                      ylab = "Global Active Power (kilowatts)",
                      xlab = "", type="l"))
    
    # Save it to file
    dev.copy(png, "plot2.png")
    dev.off()
}