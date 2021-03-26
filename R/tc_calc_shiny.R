##########################################################################
#TC calc shiny
##########################################################################

#load libraries-----------------------------------------------------------

library("tidyverse")
library("zip")

#--------------------------calibration------------------------------------

# calculation of coefficients from NDIR calibration, intercept set to zero
NDIR_calib <- read.csv("NDIR-integrals-20200224-offline.csv", header = T)
intercept <- 0
calib <- lm(I(area - intercept) ~ 0+ amount, data = NDIR_calib)
coef <- as.data.frame(calib$coefficients)
coef <- rbind(intercept, as.data.frame(calib$coefficients))
currentCalConstant <<- (mean(NDIR_calib$CH4.area)-coef[1,])/coef[2,]

#--------------------------define function-------------------------------

data.load.func = function(filename) {
  df <-  as.data.frame(read.csv(file = filename, sep = ",", skip = 28, header = T ))
  df$time_s <- seq(1:length(df$CO2_ppm))
  df <-df[,c(21,16)]
  #baseline correction
  df$CO2_ppm <- df$CO2_ppm-median(sort(df$CO2_ppm,decreasing=F)[1:length(which(df$CO2_ppm < 0))])
  #Baseline correction: set all remaining negative values to zero
  df$CO2_ppm[df$CO2_ppm < 0] <- 0
  #import CalConstant and calculate the calibration constant factor
  CalConstant <-  as.data.frame(read.csv(file = filename, sep = ",", skip = 18, header = F ))
  CalConstant <- as.numeric(CalConstant[1,1])
  CalConstFactor <- CalConstant/currentCalConstant
  
  #model
  colnames(df) = c("x", "y")
  model<-loess(y~x, span=0.05, data=df)
  mod.fun<-function(x) predict(model,newdata=x)
  
  #CH4 corrections
  CH4_area <- integrate(mod.fun,280,380)
  
  #calibration peak correction factor with CH4
  calibration_peak_correction_factor <- mean(NDIR_calib$CH4.area)/CH4_area$value 
  
  #Calculate area for each peak and total
  #total carbon
  total_area <- integrate(mod.fun,50,250)
  total_area <- total_area$value*calibration_peak_correction_factor
  amount.tc <- (total_area-coef[1,])/coef[2,]
  amount.tc <<- amount.tc*CalConstFactor
} 

#load data, run calculation ----------------------------------------------

filename <- input$fileUploadedTC$datapath
#file name for output
filename.text <<- input$fileUploadedTC$name

df.amount.tc <- NULL
for (i in filename){
  data.load.func(i)
  input_data <- read.csv(file = i, sep = ",", col.names = paste0("C",seq_len(20)), fill = TRUE, header = T)
  filter_area_tc = as.numeric(input_data[min(grep('FilterArea',input_data$C1))+1,1])
  df.amount.tc <- rbind(df.amount.tc, data.frame(amount.tc, filter_area_tc))
}

# combine file name with ouput data
df.amount.tc <- cbind(filename.text,df.amount.tc)
colnames(df.amount.tc) <- c("sample_name","TC_amount_ugC","filter_area_tc_cm2")
df.amount.tc <<- df.amount.tc

##########################################################################
#end
##########################################################################



