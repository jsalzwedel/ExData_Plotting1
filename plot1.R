############################################################
# Make a plot of Global Active Power vs Frequency from the
# electric power consumption dataset.  Save it to a png.
############################################################

plot1 <- function(){
    # first read in the data
    data <- read.table("household_power_consumption.txt", sep = ";",
                       na.strings="?", header = T)
    
    # convert the date column from strings to dates
    data$Date <- as.Date(data$Date, "%d/%m/%Y")
    
    #subset the data to use the specified dates (2007-02-01 and 2007-02-02)
    dates <- c("2007-02-01","2007-02-02")
    dataDates <- data[data$Date %in% as.Date(dates),]
    
    # Draw the histogram
    with(dataDates,hist(Global_active_power, col = "red",
                        xlab = "Global Active Power (kilowatts)",
                        main = paste("Global Active Power")))
    
    # Save it to file
    dev.copy(png, file = "plot1.png")
    dev.off()
}