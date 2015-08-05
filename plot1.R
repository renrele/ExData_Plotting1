plot1 <- function()
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
        
        #plot
        png(file = "plot1.png", bg = "transparent")
        hist(powcon$Global_active_power,col="red",main = "Global active power",xlab = "Global active power (kilowatts)")
        dev.off()
        
        
        
        
        
        
        
}