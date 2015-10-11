#Descrition of plot1.R script:
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
#2. Plot1() - use ExA_read() function to load and prepare the data, and creates ".png" file 
#             with required plots in the R Working Directory.
#How to use:
#1. unzip "household_power_consumption.txt" to R Working Directory
#2. put this R script to R Working Directory
#3. make sure that dplyr R package is loaded
#4. call Plot1() function
#5. find output "plot1.png" file in R Working Directory

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
                           na.strings = "?", stringsAsFactors = FALSE                   #indicate to read.table that NA values are represented by "?"
                   )                                                                    #and load Date and Time types as characters
        
        my_data.f <- filter(my_data, Date == '1/2/2007' | Date == '2/2/2007')           #use dplyr packege to choose required time period
        
        my_data.f$Date <- as.Date(my_data.f$Date, format = "%d/%m/%Y")                  #convert  "Date" field from character to date type
        my_data.f$datetime <- paste(my_data.f$Date, my_data.f$Time)                     #convert  "Time" field from character
        my_data.f$Time <- strptime(my_data.f$datetime, format = "%Y-%m-%d %H:%M:%S")    #to POSIXlt format
        
        return(my_data.f)                                                               #return prepared dataset
}

#Description of Plot1 function:
#Objective:
#use ExA_read() function to load and prepare the data from "household_power_consumption.txt", 
#and create ".png" file with required plotsin the R Working Directory
Plot1 <- function () {
        df <- ExA_read()                                              #prepare dataset for plotting
        
        png(                                                          #prepare png graphics file device for plotting      
                filename = "plot1.png", width = 480, height = 480,
                units = "px", pointsize = 16, bg = "white", res = NA, 
                restoreConsole = TRUE
        )
        
        par(                                                          #configure plot parameters
                mar = c(6,6,2,1), cex.axis = 0.7, cex.lab = 0.8, 
                cex.main = 1, font.main = 2
        )
        
        hist(                                                         #create a plot without y axis
                df$Global_active_power, main = "Global Active Power",
                xlab = "Global Active Power (kilowatts)",yaxt = 'n', 
                col = "red"
        )
        
        axis(                                                         #create y axis of the plot
                side = 2, at = seq(0,1200, 200), 
                labels = seq(0,1200,200)
        )
        
        dev.off()                                                     #switch off graphics file device
}