
#This R script has been developed by Arkasama Bandyopadhyay at Texas A&M University in 2021. 
#The script provides the methodology for the paper "Residential appliance usage patterns from overall
#energy consumptiondata: a statistical machine learning approach" submitted to ASME IMECE 2021. 

#Set path for input data file


#Note: all data is for year 2018 in Austin

#Read in data from csv file 
#The input data file is not provided on Github but can be requested free of charge from Pecan Street as an academic researcher
raw_data <- read.csv("15minute_data_austin.csv",header = TRUE, sep = ",", row.names=NULL, stringsAsFactors = FALSE)

#Define column names 
#-----------House IDs----------------------
houseid <- as.matrix(raw_data[,1])
#Houseids for the 25 different homes
house_id_vector<- as.vector(unique(houseid))
#------------Date and Time-----------------
dateandtime<- as.matrix(raw_data[,2])
Hours <- format(as.POSIXct(strptime(dateandtime,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
Date_data <- format(as.POSIXct(strptime(dateandtime,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
#-----------------------------------

#--------------HVAC usage-----------------
hvac_1<- as.matrix(raw_data[,3])
hvac_2<- as.matrix(raw_data[,4])
hvac_3<- as.matrix(raw_data[,5])
#-----------------------------------

#-------------Energy bought from the grid--------------
grid<- as.matrix(raw_data[,32])
#-----------------------------------

#-------------Solar generation--------------
solar<- as.matrix(raw_data[,68])
#-----------------------------------



#-------------Calculate proportion of overall electricity consumption used by HVAC system--------------
#Pick a house id vector
i=house_id_vector[8]
#Period of analysis (in days)
no_of_days=5
x=1
#Initiating matrices
percent_HVAC_1=matrix(0, nrow=96*no_of_days, ncol=1)
hvac=matrix(0, nrow=96*no_of_days, ncol=1)
consump=matrix(0, nrow=96*no_of_days, ncol=1)

for (j in 1:nrow(raw_data)){ #go through each row
  if (houseid[j]==i & Date_data[j]>=as.Date("0018-08-13") & Date_data[j]<=as.Date("0018-08-17")) {
    hvac[x,1] <- hvac_1[j,1] #hvac energy usage
    consump[x,1] <-(grid[j,1]+solar[j,1]) #overall energy consumption
    percent_HVAC_1[x,1] <- hvac_1[j,1]*100/(grid[j,1]+solar[j,1]) #percentage of HVAC usage
    x=x+1
  }
}

#Plots 
#Aug 13
plot(percent_HVAC_1[1:96], type='l')
#Aug 14
plot(percent_HVAC_1[97:192], type='l')
#Aug 15
plot(percent_HVAC_1[193:288], type='l')
#Aug 16
plot(percent_HVAC_1[289:384], type='l')
#Aug 17
plot(percent_HVAC_1[385:480], type='l')



#-------------Weather data--------------
#Set path for input weather data file


library(readxl)
#Read in data
#The input data file is not provided on Github
raw_weather <- read_excel("weather_data_winderground.xlsx",sheet = 1, col_names = TRUE)

temp_13_17 <- as.matrix(raw_weather[,3]) #temperature


#Interpolating hourly values of temperature to get 15 min values

#Temperature
x <- c(1: length(temp_13_17))
xout <- seq(from = 1, to = length(temp_13_17), by = 0.25)
interp_temp <- approx(x, temp_13_17, xout, method = "linear")
temp_15 <-as.data.frame(interp_temp[2])
temp_15 <-temp_15[-nrow(temp_15),]#deleting the last row


#Plotting temperature (Fig 1)
#-------------------------------------------------------------------
library(ggplot2)
library(scales)
date_vector <- seq(ISOdatetime(2018, 08, 13, 0, 0, 0), ISOdatetime(2018, 08, 17, 23, 59, 0), by="15 min", tz="CST")
df<- data.frame(min_plot=date_vector, 
                temp_15)

my_plot1<-ggplot(df, aes(x=date_vector)) + 
  geom_line(aes(y = temp_15), color = "#D55E00", size=2) +labs(title="Ambient temperature data for Austin, TX: August 13-17, 2018",x="Time of Day", y = expression(paste("Temperature (", degree, "F)")))+theme(plot.title = element_text(hjust = 0.5))
myplot1_final <- my_plot1+annotate(geom="text", x=as.POSIXct("2018-08-13 11:45:00",format="%Y-%m-%d %H:%M:%S"), y=103, label="August 13", fontface="bold", size=8)+
  annotate(geom="text", x=as.POSIXct("2018-08-14 11:45:00",format="%Y-%m-%d %H:%M:%S"), y=103, label="August 14", fontface="bold", size=8)+
  annotate(geom="text", x=as.POSIXct("2018-08-15 11:45:00",format="%Y-%m-%d %H:%M:%S"), y=103, label="August 15", fontface="bold", size=8)+
  annotate(geom="text", x=as.POSIXct("2018-08-16 11:45:00",format="%Y-%m-%d %H:%M:%S"), y=103, label="August 16", fontface="bold", size=8)+
  annotate(geom="text", x=as.POSIXct("2018-08-17 11:45:00",format="%Y-%m-%d %H:%M:%S"), y=103, label="August 17", fontface="bold", size=8)+
  geom_vline(xintercept=as.numeric(df$min_plot[c(96, 192, 288, 384, 480)]), linetype=4, color="black")+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                                                                                                  colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                                                                                                      colour ="white")) +theme(axis.text=element_text(size=26), axis.title=element_text(size=22))+ theme(plot.title = element_text(size=22, face="bold", color="black"))+theme(axis.line = element_line(size = 1, colour = "black", linetype=1)) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("8 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(limits=c(60, 104))+ 
  theme(axis.text.x=(element_text(angle=60, vjust=0.5)))+ theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) + theme(legend.title=element_blank(), 
                                                                                                                                                                                                                                                                                                           legend.text = element_text(size=26),legend.key=element_rect(fill='white'), legend.position='bottom')








#Plotting HVAC usage/Overall Consumption (Fig 2)
#-------------------------------------------------------------------
library(ggplot2)
library(scales)
date_vector <- seq(ISOdatetime(2018, 08, 13, 0, 0, 0), ISOdatetime(2018, 08, 13, 23, 59, 0), by="15 min", tz="CST")
df<- data.frame(min_plot=date_vector, percent_HVAC_1[1:96])
my_plot1<-ggplot(df, aes(x=date_vector)) + 
  geom_line(aes(y = percent_HVAC_1[1:96]), color = "darkred") +labs(title="August 13",x="Time of Day", y = "")+theme(plot.title = element_text(hjust = 0.5))
myplot1_final <- my_plot1+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                     colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                         colour ="white")) +theme(axis.line = element_line(size = 1, colour = "black", linetype=1))+theme(axis.text=element_text(size=16), axis.title=element_text(size=20))+ theme(plot.title = element_text(size=28, face="bold", color="black")) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(expand= c(0,0), limits=c(-4, 80))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) +theme(legend.text=element_text(color="black",size=22))+ theme(legend.title = element_text(size=22, color = "white"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           legend.text = element_text(size=22),legend.key=element_rect(fill='white'), legend.position='bottom')






date_vector <- seq(ISOdatetime(2018, 08, 14, 0, 0, 0), ISOdatetime(2018, 08, 14, 23, 59, 0), by="15 min", tz="CST")
df<- data.frame(min_plot=date_vector, percent_HVAC_1[97:192])
my_plot2<-ggplot(df, aes(x=date_vector)) + 
  geom_line(aes(y = percent_HVAC_1[97:192]), color = "darkred") +labs(title="August 14",x="Time of Day", y = "")+theme(plot.title = element_text(hjust = 0.5))
myplot2_final <- my_plot2+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                     colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                         colour ="white")) +theme(axis.line = element_line(size = 1, colour = "black", linetype=1))+theme(axis.text=element_text(size=16), axis.title=element_text(size=20))+ theme(plot.title = element_text(size=28, face="bold", color="black")) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(expand= c(0,0), limits=c(-6, 80))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) +theme(legend.text=element_text(color="black",size=22))+ theme(legend.title = element_text(size=22, color = "white"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           legend.text = element_text(size=22),legend.key=element_rect(fill='white'), legend.position='bottom')







date_vector <- seq(ISOdatetime(2018, 08, 15, 0, 0, 0), ISOdatetime(2018, 08, 15, 23, 59, 0), by="15 min", tz="CST")
df<- data.frame(min_plot=date_vector, percent_HVAC_1[193:288])
my_plot3<-ggplot(df, aes(x=date_vector)) + 
  geom_line(aes(y = percent_HVAC_1[193:288]), color = "darkred") +labs(title="August 15",x="Time of Day", y = "")+theme(plot.title = element_text(hjust = 0.5))
myplot3_final <- my_plot3+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                     colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                         colour ="white")) +theme(axis.line = element_line(size = 1, colour = "black", linetype=1))+theme(axis.text=element_text(size=16), axis.title=element_text(size=20))+ theme(plot.title = element_text(size=28, face="bold", color="black")) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(expand= c(0,0), limits=c(-6, 80))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) +theme(legend.text=element_text(color="black",size=22))+ theme(legend.title = element_text(size=22, color = "white"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           legend.text = element_text(size=22),legend.key=element_rect(fill='white'), legend.position='bottom')






date_vector <- seq(ISOdatetime(2018, 08, 16, 0, 0, 0), ISOdatetime(2018, 08, 16, 23, 59, 0), by="15 min", tz="CST")
df<- data.frame(min_plot=date_vector, percent_HVAC_1[289:384])
my_plot4<-ggplot(df, aes(x=date_vector)) + 
  geom_line(aes(y = percent_HVAC_1[289:384]), color = "darkred") +labs(title="August 16",x="Time of Day", y = "")+theme(plot.title = element_text(hjust = 0.5))
myplot4_final <- my_plot4+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                     colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                         colour ="white")) +theme(axis.line = element_line(size = 1, colour = "black", linetype=1))+theme(axis.text=element_text(size=16), axis.title=element_text(size=20))+ theme(plot.title = element_text(size=28, face="bold", color="black")) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(expand= c(0,0), limits=c(-6, 80))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) +theme(legend.text=element_text(color="black",size=22))+ theme(legend.title = element_text(size=22, color = "white"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           legend.text = element_text(size=22),legend.key=element_rect(fill='white'), legend.position='bottom')






library(gridExtra)
library(grid)
grid.arrange(myplot1_final, myplot2_final, myplot3_final,myplot4_final, top = textGrob("HVAC usage as a percent of total electricity consumption",gp=gpar(fontsize=30,font=1)), nrow=2, ncol=2)


#---------l_1 trend filtering and regression (also includes code for Fig 3)------------
library(CVXR)
library(forecast)
#The smoothing process is inspired by https://cvxr.rbind.io/cvxr_examples/cvxr_l1-trend-filtering/. 
v=c(1:96)#August 13
y <- percent_HVAC_1[v] 
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)

library(ggplot2)
library(scales)
cvxr_data <- data.frame(x = seq(ISOdatetime(2018, 08, 13, 0, 0, 0), ISOdatetime(2018, 08, 13, 23, 59, 0), by="15 min", tz="CST"),
                        y = percent_HVAC_1[v],
                        l1_50 = betaHat_50, 
                        temp_data = temp_15[v])
my_plot1<-ggplot(data = cvxr_data) +
  geom_line(mapping = aes(x = x, y = y), color = 'darkred') +
  labs(x = "Time of Day", y = "", title="August 13") +
  geom_line(mapping = aes(x = x, y = l1_50), color = 'darkgreen', size = 1)+
  geom_line(mapping = aes(x = x, y = temp_data), color = "#D55E00", size = 1) 
myplot1_final <- my_plot1+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                     colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                         colour ="white")) +theme(axis.line = element_line(size = 1, colour = "black", linetype=1))+theme(axis.text=element_text(size=16), axis.title=element_text(size=20))+ theme(plot.title = element_text(size=28, face="bold", color="black")) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(expand= c(0,0), limits=c(-4, 100))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) +theme(legend.text=element_text(color="black",size=22))+ theme(legend.title = element_text(size=22, color = "white"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            legend.text = element_text(size=22),legend.key=element_rect(fill='white'), legend.position='bottom')






v=c(97:192) #August 14
y <- percent_HVAC_1[v] 
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)

library(ggplot2)
cvxr_data_2 <- data.frame(x = seq(ISOdatetime(2018, 08, 14, 0, 0, 0), ISOdatetime(2018, 08, 14, 23, 59, 0), by="15 min", tz="CST"),
                          y = percent_HVAC_1[v],
                          l1_50 = betaHat_50, 
                          temp_data = temp_15[v])
my_plot2<-ggplot(data = cvxr_data_2) +
  geom_line(mapping = aes(x = x, y = y), color = 'darkred') +
  labs(x = "Time of Day", y = "", title="August 14") +
  geom_line(mapping = aes(x = x, y = l1_50), color = 'darkgreen', size = 1)+
  geom_line(mapping = aes(x = x, y = temp_data), color = "#D55E00", size = 1) 
myplot2_final <- my_plot2+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                     colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                         colour ="white")) +theme(axis.line = element_line(size = 1, colour = "black", linetype=1))+theme(axis.text=element_text(size=16), axis.title=element_text(size=20))+ theme(plot.title = element_text(size=28, face="bold", color="black")) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(expand= c(0,0), limits=c(-4, 100))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) +theme(legend.text=element_text(color="black",size=22))+ theme(legend.title = element_text(size=22, color = "white"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            legend.text = element_text(size=22),legend.key=element_rect(fill='white'), legend.position='bottom')





v=c(193:288) #August 15
y <- percent_HVAC_1[v]
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)

library(ggplot2)
cvxr_data_3 <- data.frame(x = seq(ISOdatetime(2018, 08, 15, 0, 0, 0), ISOdatetime(2018, 08, 15, 23, 59, 0), by="15 min", tz="CST"),
                          y = percent_HVAC_1[v],
                          l1_50 = betaHat_50, 
                          temp_data = temp_15[v])
my_plot3<-ggplot(data = cvxr_data_3) +
  geom_line(mapping = aes(x = x, y = y), color = 'darkred') +
  labs(x = "Time of Day", y = "", title="August 15") +
  geom_line(mapping = aes(x = x, y = l1_50), color = 'darkgreen', size = 1)+
  geom_line(mapping = aes(x = x, y = temp_data), color = "#D55E00", size = 1) 
myplot3_final <- my_plot3+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                     colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                         colour ="white")) +theme(axis.line = element_line(size = 1, colour = "black", linetype=1))+theme(axis.text=element_text(size=16), axis.title=element_text(size=20))+ theme(plot.title = element_text(size=28, face="bold", color="black")) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(expand= c(0,0), limits=c(-6, 103))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) +theme(legend.text=element_text(color="black",size=22))+ theme(legend.title = element_text(size=22, color = "white"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            legend.text = element_text(size=22),legend.key=element_rect(fill='white'), legend.position='bottom')






v=c(289:384)#August 16
y <- percent_HVAC_1[v] 
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)

library(ggplot2)
cvxr_data_4 <- data.frame(x = seq(ISOdatetime(2018, 08, 13, 0, 0, 0), ISOdatetime(2018, 08, 13, 23, 59, 0), by="15 min", tz="CST"),
                          y = percent_HVAC_1[v],
                          l1_50 = betaHat_50, 
                          temp_data = temp_15[v])
my_plot4<-ggplot(data = cvxr_data_4) +
  geom_line(mapping = aes(x = x, y = y), color = 'darkred') +
  labs(x = "Time of Day", y = "", title="August 16") +
  geom_line(mapping = aes(x = x, y = l1_50), color = 'darkgreen', size = 1)+
  geom_line(mapping = aes(x = x, y = temp_data), color = "#D55E00", size = 1) 
myplot4_final <- my_plot4+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                     colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                         colour ="white")) +theme(axis.line = element_line(size = 1, colour = "black", linetype=1))+theme(axis.text=element_text(size=16), axis.title=element_text(size=20))+ theme(plot.title = element_text(size=28, face="bold", color="black")) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous(expand= c(0,0), limits=c(-6, 103))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) +theme(legend.text=element_text(color="black",size=22))+ theme(legend.title = element_text(size=22, color = "white"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            legend.text = element_text(size=22),legend.key=element_rect(fill='white'), legend.position='bottom')






library(gridExtra)
library(grid)
grid.arrange(myplot1_final, myplot2_final, myplot3_final,myplot4_final, nrow=2, ncol=2)

v=c(385:480)#August 17
y <- percent_HVAC_1[v] 
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)

#----------Regression----------------------
v=c(1:96)#August 13
y <- percent_HVAC_1[v] 
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)
fit1 <- Arima(betaHat_50,order=c(1,0,0),xreg=temp_15[v],include.constant=TRUE)
residual_Aug13<- fit1$residuals



v=c(97:192) #August 14
y <- percent_HVAC_1[v] 
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)
fit1 <- Arima(betaHat_50,order=c(1,0,0),xreg=temp_15[v],include.constant=TRUE)
residual_Aug14<- fit1$residuals

v=c(193:288)#August 15
y <- percent_HVAC_1[v] 
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)
fit1 <- Arima(betaHat_50,order=c(1,0,0),xreg=temp_15[v],include.constant=TRUE)
residual_Aug15<- fit1$residuals

v=c(289:384)#August 16
y <- percent_HVAC_1[v] 
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)
fit1 <- Arima(betaHat_50,order=c(1,0,0),xreg=temp_15[v],include.constant=TRUE)
residual_Aug16<- fit1$residuals

v=c(385:480)#August 17
y <- percent_HVAC_1[v] 
lambda_1 <- 0.1
beta <- Variable(length(y))
objective_1 <- Minimize(0.5 * p_norm(y - beta) +
                          lambda_1 * p_norm(diff(x = beta, differences = 2), 1))
p1 <- Problem(objective_1)
betaHat_50 <- solve(p1)$getValue(beta)
fit1 <- Arima(betaHat_50,order=c(1,0,0),xreg=temp_15[v],include.constant=TRUE)
residual_Aug17<- fit1$residuals

#-----Plotting residuals (Fig 4)-------------
library(ggplot2)
library(scales)
date_vector <- seq(ISOdatetime(2018, 08, 13, 0, 0, 0), ISOdatetime(2018, 08, 13, 23, 59, 0), by="15 min", tz="CST")
df<- data.frame(min_plot=date_vector, 
                supp = c(rep("August 13", 96), rep("August 14", 96),rep("August 15", 96), rep("August 16", 96),rep("August 17", 96)),
                len=c(residual_Aug13, residual_Aug14, residual_Aug15, residual_Aug16, residual_Aug17))
my_plot1<-ggplot(data=df, aes(x=min_plot, y=len, group=supp)) + 
  geom_line(aes(color=supp), size=2) + 
  scale_color_manual(values=c("#0072B2", "#FF3300", "#E69F00", "#56B4E9", "#CC79A7"))+labs(title="Residual plot",x="Time of Day", y = "Regression errors")+theme(plot.title = element_text(hjust = 0.5))
myplot1_final <- my_plot1+theme(plot.background = element_rect(fill = "white"),panel.background = element_rect(fill = "white", colour = "white"), panel.grid.major = element_line(size = 0.5, linetype ='solid', colour = "white"),  panel.grid.minor = element_line(size = 0.25, linetype = 'blank',
                                                                                                                                                                                                                                                                     colour = "white"), legend.background = element_rect(fill="white",size=0.5, linetype="solid", 
                                                                                                                                                                                                                                                                                                                         colour ="white")) +theme(axis.text=element_text(size=26), axis.title=element_text(size=26))+ theme(plot.title = element_text(size=30, face="bold", color="black"))+theme(axis.line = element_line(size = 1, colour = "black", linetype=1)) +theme(plot.title = element_text(hjust = 0.5))+scale_x_datetime(breaks=date_breaks("4 hour"),labels = date_format("%H:%M",tz="America/Chicago") )+ scale_y_continuous( limits=c(-4, 4))+ 
  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.title.x = element_text(colour = "black"),axis.title.y = element_text(color = "black"))+ theme(axis.text.x = element_text(color="black"), axis.text.y = element_text(colour="black")) + theme(legend.title=element_blank(), 
                                                                                                                                                                                                                                                   legend.text = element_text(size=26),legend.key=element_rect(fill='white'), legend.position='bottom')











