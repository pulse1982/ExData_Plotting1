#Descrition of plot4.R script:
#
#Objectives:
#1. load data from "household_power_consumption.txt" file, 
#   prepare it for plotting by selecting specific set of rows, checking data integrety and data types, 
#   removing records with NA values, and converting column data types to appropriate ones 
#   (e.g. Date/Time/Numeric) in case required. 
#2. build ".png" file with required plot using the Base Plotting system

#Requirements:
#1. "household_power_consumption.txt" file needs to be unzipped,
#   and stored in the R Working Directory together with this R script.
#2. dplyr R package needs to be loaded (library(dplyr)) before running this script.

#Functions:
#this script includes 2 functions:
#1. ExA_read() - loads and prepares the data from "household_power_consumption.txt" file
#2. Plot4() - use ExA_read() function to load and prepare the data, and creates ".png" file 
#             with required plots in the R Working Directory.
#How to use:
#1. unzip "household_power_consumption.txt" to R Working Directory
#2. put this R script to R Working Directory
#3. make sure that dplyr R package is loaded
#4. call Plot4() function
#5. find output "plot4.png" file in R Working Directory

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------

#Description of ExA_read function:
#Objective:
#   load data from "household_power_consumption.txt" file, and 
#   prepare it for plotting by selecting specific set of rows, checking data integrety and data types, 
#   removing records with NA values, and converting column data types to appropriate ones 
#   (e.g. Date/Time/Numeric) in case required.
#   Returns a data frame with records selected by time period, and appropriate Date/Time/Numeric field types.

ExA_read <- function () {
        Sys.setlocale("LC_TIME", "English") #make sure that weekday names ("Thu", "Fri",..) are plotted in english
        
        my_data <- read.table(                                                         
                "household_power_consumption.txt", sep = ";", header = TRUE, #read txt file from R Working Directory 
                na.strings = "?", stringsAsFactors = FALSE                  #indicate to read.table that NA values are represented by "?"
        )                                                                               #and load Date and Time types as characters
        
        my_data.f <- filter(my_data, Date == '1/2/2007' | Date == '2/2/2007')           #use dplyr packege to choose required time period
        
        my_data.f$Date <- as.Date(my_data.f$Date, format = "%d/%m/%Y")                  #convert  "Date" field from character to date type
        my_data.f$datetime <- paste(my_data.f$Date, my_data.f$Time)                     #convert  "Time" field from character
        my_data.f$Time <- strptime(my_data.f$datetime, format = "%Y-%m-%d %H:%M:%S")    #to POSIXlt format
        
        return(my_data.f)                                                               #return prepared dataset
}

#Description of Plot4 function:
#Objective:
#use ExA_read() function to load and prepare the data from "household_power_consumption.txt", 
#and create "plot4.png" file with required plotsin the R Working Directory

Plot4 <- function () {
        df <- ExA_read()                                               #prepare dataset for plotting
        
        png(                                                           #prepare png graphics file device for plotting
                filename = "plot4.png", width = 480, height = 480,
                units = "px", pointsize = 14, bg = "white", res = NA,
                restoreConsole = TRUE
        )
        
        par(mfrow = c(2, 2), mar = c(4,4,2,2))                         #configure plot parameters and 
        
        plot(                                                          #create first plot
                df$Time,df$Global_active_power, type = "l",
                ylab = "Global Active Power", xlab = ""
        )
        
        plot(                                                          #create second plot
                df$Time,df$Voltage, type = "l",
                ylab = "Voltage", xlab = "datetime"
        )
        
        plot(
                df$Time,df$Sub_metering_1, type = "l", xlab = "",      #create 3rd plot with one line
                ylab = "Energy sub metering"
        )
        
        lines(df$Time, df$Sub_metering_2, col = "red")                 #add 2nd and 3rd line to the 3rd plot
        lines(df$Time, df$Sub_metering_3, col = "blue")
        
        legend(                                                        #add 3rd plot legend
                "topright", cex = 0.8, bty = "n",
                col = c("black", "red", "blue"), lty = c(1,1,1),
                legend = c("Sub_metering_1", "Sub_metering_2", 
                           "Sub_metering_3")
        )
        
        plot(                                                           #create 4th plot
                df$Time,df$Global_reactive_power, type = "l",
                ylab = "Global_reactive_power", xlab = "datetime"
        )
        
        dev.off()                                                       #switch off graphics file device
}