# cr1.plot2 function
#
# When write_png="yes" plot to png file, otherwise - plot on default device
#
# if variable "cr1.data" absent in your env, when it invokes "cr1.initEnv"
cr1.plot2 <- function(write_png="yes", filename="plot2.png"){
  if(!exists("cr1.data")) cr1.initEnv()
  
  if(write_png=="yes") png(filename)
  
  par(mfrow=c(1,1))
  with(cr1.data,plot(datetime, Global_active_power, type="l", 
                     ylab="Global Active Power (kilowatts)", xlab=""))
  if(write_png=="yes") garbage <- dev.off()
}

# Initialize environment function (duplicated in all plot*.R files)
#
# When specified - download file
# When specified - unzip file
# By default - set locale (important for non English users)
#            - create "cr1.data" data set 
cr1.initEnv <- function(set_locale="yes", 
                        unzip="no", download="no"){
  
  if(download=="yes") 
    download.file("https://d396qusza40orc.cloudfront.net/
                  exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                  destfile="./data/PowerData.zip")
  
  if(unzip=="yes") unzip("./data/PowerData.zip")
  
  # set English locale, to have correct weekdays
  if(set_locale=="yes") garbage <- Sys.setlocale("LC_TIME", "English")
  
  hpc <- read.delim("./household_power_consumption.txt",
                    sep=";", na.strings="?", 
                    colClasses=c(rep("factor", 2), rep("numeric",7)), 
                    header=TRUE)
  
  tr <- hpc$Date == "2/2/2007" | hpc$Date =="1/2/2007"
  work <- hpc[tr, ]
  
  work$datetime <- strptime(paste(work$Date, work$Time), "%d/%m/%Y %H:%M:%S", tz="GMT")
  
  cr1.data <<- work[, c("datetime", "Global_active_power", "Global_reactive_power", 
                        "Voltage", "Global_intensity", "Global_intensity", 
                        "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")]
}