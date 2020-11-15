##########################################################################
#OC calc shiny
##########################################################################

#load libraries-----------------------------------------------------------

library("tidyverse")
library("zip")

#clean up environment-----------------------------------------------------
rm(list=setdiff(ls(), c()))
if(!is.null(dev.list())) dev.off()

#--------------------------calibration------------------------------------

# calculation of coefficients from NDIR calibration
NDIR_calib <- read.csv("NDIR-integrals-20200224-offline.csv", header = T)
calib <- lm(area~amount, data = NDIR_calib)
coef <- as.data.frame(calib$coefficients)
currentCalConstant <<- (mean(NDIR_calib$CH4.area)-coef[1,])/coef[2,]

#--------------------------define function-------------------------------

data.load.func = function(filename) {
  #filename = "mr01-169-e_20200728_oc_removal.txt"
  df <-  as.data.frame(read.csv(file = filename, sep = ",", skip = 28, header = T ))
  df$time_s <- seq(1:length(df$CO2_ppm))
  df <-df[,c(21,16)]
  #baseline correction with the 20 smallest values
  df$CO2_ppm <- df$CO2_ppm-mean(sort(df$CO2_ppm,decreasing=F)[1:20])
  #import CalConstant and calculate the calibration constant factor
  CalConstant <-  as.data.frame(read.csv(file = filename, sep = ",", skip = 18, header = F ))
  CalConstant <- as.numeric(CalConstant[1,1])
  CalConstFactor <- CalConstant/currentCalConstant
  
  #model
  colnames(df) = c("x", "y")
  model<-loess(y~x, span=0.05, data=df)
  mod.fun<-function(x) predict(model,newdata=x)
  
  # #plot model with ggplot
  # theme_set(theme_classic(base_size = 11,base_family = "Helvetica"))
  # ggplot(df, aes(x, y)) +
  #   geom_point() +
  #   ylab("A.U.") +
  #   xlab("time (s)") +
  #   geom_smooth(formula = y~x, method = "loess",span=0.05, se = FALSE)

  #Corrections
  #CH4
  CH4_area <- integrate(mod.fun,1050,1190)
  
  #calibration peak correction factor with CH4
  calibration_peak_correction_factor <- 57355.30357/CH4_area$value 

  #Calculate area for each peak and total
  #OC S1
  OC_areaS1 <- integrate(mod.fun,50,350)
  OC_areaS1 <- OC_areaS1$value*calibration_peak_correction_factor
  amount.S1 <- (OC_areaS1-coef[1,])/coef[2,]
  amount.S1 <<- amount.S1*CalConstFactor
  #OC S2
  OC_areaS2 <- integrate(mod.fun,350,610)
  OC_areaS2 <- OC_areaS2$value*calibration_peak_correction_factor
  amount.S2 <- (OC_areaS2-coef[1,])/coef[2,]
  amount.S2 <<- amount.S2*CalConstFactor
  #OC S3
  OC_areaS3 <- integrate(mod.fun,610,1050)
  OC_areaS3 <- OC_areaS3$value*calibration_peak_correction_factor
  amount.S3 <- (OC_areaS3-coef[1,])/coef[2,]
  amount.S3 <<- amount.S3*CalConstFactor
  #total carbon
  total_area <- integrate(mod.fun,50,1050)
  total_area <- total_area$value*calibration_peak_correction_factor
  amount.tc <- (total_area-coef[1,])/coef[2,]
  amount.tc <<- amount.tc*CalConstFactor
} 

#load data, run calculation ----------------------------------------------

filename <- input$fileUploaded$datapath
df.amount <- NULL
for (i in filename){
  data.load.func(i)
  df.amount <- rbind(df.amount, data.frame(amount.S1, amount.S2, amount.S3,amount.tc))
}

colnames(df.amount) <- c("S1 (ug C)","S2 (ug C)","S3 (ug C)", "TC (ug C)")
df.amount

##########################################################################
#end
##########################################################################



