##########################################################################
#OC calc shiny
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

  #total protocol length
  total.length <- length(df$x)
  
  #CH4 corrections
  CH4_area <- integrate(mod.fun,total.length-140,total.length)
  
  #calibration peak correction factor with CH4
  calibration_peak_correction_factor <- mean(NDIR_calib$CH4.area)/CH4_area$value 

  #Calculate area for each peak and total. Standard integration S1 50-350, S2 350-610, S3 610-1050, CH4 1050-1190
  #OC S1
  OC_areaS1 <- integrate(mod.fun,50,S1_length)
  OC_areaS1 <- OC_areaS1$value*calibration_peak_correction_factor
  amount.S1 <- (OC_areaS1-coef[1,])/coef[2,]
  amount.S1 <<- amount.S1*CalConstFactor
  #OC S2
  OC_areaS2 <- integrate(mod.fun,S1_length,S2_length)
  OC_areaS2 <- OC_areaS2$value*calibration_peak_correction_factor
  amount.S2 <- (OC_areaS2-coef[1,])/coef[2,]
  amount.S2 <<- amount.S2*CalConstFactor
  #OC S3
  OC_areaS3 <- integrate(mod.fun,S2_length,S3_length)
  OC_areaS3 <- OC_areaS3$value*calibration_peak_correction_factor
  amount.S3 <- (OC_areaS3-coef[1,])/coef[2,]
  amount.S3 <<- amount.S3*CalConstFactor
  #total carbon
  total_area <- integrate(mod.fun,50,total.length-140)
  total_area <- total_area$value*calibration_peak_correction_factor
  amount.tc <- (total_area-3*coef[1,])/coef[2,]
  amount.tc <<- amount.tc*CalConstFactor
} 

#load data, run calculation ----------------------------------------------

#get file name
filename <- input$fileUploadedOC$datapath
#file name for output
filename.text <<- input$fileUploadedOC$name

#length of 
S1_length <- (110+as.numeric(input$inTextS1))
S2_length <- (140+S1_length+as.numeric(input$inTextS2))
S3_length <- (80+S2_length+as.numeric(input$inTextS3))

#create an empty df
df.amount <- NULL
#loop function
for (i in filename){
  data.load.func(i)
  df.amount <- rbind(df.amount, data.frame(amount.S1, amount.S2, amount.S3,amount.tc))
}

# combine file name with ouput data
df.amount.oc <- cbind(filename.text,df.amount)
colnames(df.amount.oc) <- c("sample_name","OC_amount_S1_ugC","OC_amount_S2_ugC","OC_amount_S3_ugC", "OC_amount_total_ugC")
df.amount.oc <<- df.amount.oc


##########################################################################
#end
##########################################################################



