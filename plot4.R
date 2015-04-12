# cr1.plot4 function
#
# When write_png="yes", plot into file, Otherwise - plot on default device
#
# if variable "cr1.data" absent in your env, when invoke "cr1.InitEnv"
cr1.plot4 <- function(write_png="yes", filename="plot4.png"){
  if(!exists("cr1.data")) cr1.initEnv()
  
  if(write_png=="yes") png(filename)
  par(mfrow=c(2,2))
  
  # 1
  with(cr1.data,plot(datetime, Global_active_power, type="l", 
                     ylab="Global Active Power", xlab=""))
  
  # 2
  with(cr1.data,plot(datetime, Voltage, type="l", ylab="Voltage", xlab="datetime"))
  
  # 3
  plot(cr1.data$datetime, cr1.data$Sub_metering_1, type="l", 
       ylab="Energy sub metering", xlab="")
  points(cr1.data$datetime, cr1.data$Sub_metering_2, type="l", col = "red")
  points(cr1.data$datetime, cr1.data$Sub_metering_3, type="l", col = "blue")
  legend("topright", legend=paste("Sub_metering_", c("1", "2", "3")), 
         col=c("black", "red", "blue"), bt="n", cex=0.8, lty = 1)
  
  # 4
  with(cr1.data,plot(datetime, Global_reactive_power, type="l", xlab="datetime"))
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