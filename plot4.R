plot4 <- function()
{
        #Assuming that the text file has been downloaded to the working directory
        data<-read.table("household_power_consumption.txt",header=TRUE,sep=";",stringsAsFactors=FALSE)
        
        #subset relevant rows
        pow <- data$Date=="1/2/2007" | data$Date=="2/2/2007"
        powcon <- data[pow,]
        
        #transform dates 
        powcon[,1]<-as.Date(powcon[,1],"%d/%m/%Y")
        
        #convert times into number of minutes elapsed since midnight
        p<-0.01*as.numeric(gsub(":","",powcon[,2]))
        for(i in seq_along(p)){
                p[i]<-60*floor(p[i]/100)+p[i]%%100
        }
        powcon[,2]<-p
        
        #replace ? by NA and change character columns to numeric
        for(i in 3:9){
                powcon[,i] <- gsub("\\?",NA,powcon[,i])
                powcon[,i] <- as.numeric(powcon[,i])
        }
        #specify order of displaying graphs
        png(file = "plot4.png", bg = "transparent")
        par(mfrow = c(2,2),mar= c(5,4,4,2))
        
        #plot each of the graphs 
        with(powcon,{
                #graph1
                plot(x = seq(1:nrow(powcon)), y=powcon$Global_active_power,type="l",main = "", ylab = "Global active power", xlab="", xaxt="n")
                axis(side = 1, at = c(1,1440,2880), labels = c("Thu","Fri","Sat"))
                
                #graph2
                plot(x = seq(1:nrow(powcon)), y=powcon$Voltage,type="l",main = "", ylab = "Voltage", xlab="datetime", xaxt="n")
                axis(side = 1, at = c(1,1440,2880), labels = c("Thu","Fri","Sat"))
                
                #graph3
                plot(x = seq(1:nrow(powcon)), y=powcon[,7],type="l",main = "", ylab = "Energy sub metering", xlab="", xaxt="n")
                lines(x = seq(1:nrow(powcon)),y = powcon[,8],col="red")
                lines(x = seq(1:nrow(powcon)),y = powcon[,9],col="blue")
                legend("topright",lty="solid",bty = "n",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
                axis(side = 1, at = c(1,1440,2880), labels = c("Thu","Fri","Sat"))
                
                #graph4
                plot(x = seq(1:nrow(powcon)), y=powcon$Global_reactive_power,type="l",main = "", ylab = "Global_reactive_power", xlab="datetime", xaxt="n")
                axis(side = 1, at = c(1,1440,2880), labels = c("Thu","Fri","Sat"))
                
        })
        dev.off()
}
