plot3 <- function()
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
        
        #plot: vector of x values is s sequence from 1 to 2880
        #vector of y values is Sub_metering_1
        #type of graph "l" is a line graph
        #no main title, yaxis title is set, no x axis title , x axis removed.
        png(file = "plot3.png", bg = "transparent")
        plot(x = seq(1:nrow(powcon)), y=powcon[,7],type="l",main = "", ylab = "Energy sub metering", xlab="", xaxt="n")
        
        #add graphs of other variables
       
        lines(x = seq(1:nrow(powcon)),y = powcon[,8],col="red")
        lines(x = seq(1:nrow(powcon)),y = powcon[,9],col="blue")
        
        #x axis remade with tick marks at beginning, middle and end, labels as required
        axis(side = 1, at = c(1,1440,2880), labels = c("Thu","Fri","Sat"))
        #boxes the plot as required
        box()
        #add legend
        legend("topright",lty="solid",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        dev.off()
        
        }
