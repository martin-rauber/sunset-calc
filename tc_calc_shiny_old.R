###########################################################################
#TC calc shiny
###########################################################################

#load libraries------------------------------------------------------------

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
  df <-  as.data.frame(read.csv(file = filename, sep = ",", skip = 28, header = T ))
  df$time_s <- seq(1:length(df$CO2_ppm))
  df <-df[,c(21,16)]
  #baseline correction with the 20 smallest values
  df$CO2_ppm <- df$CO2_ppm-mean(sort(df$CO2_ppm,decreasing=F)[1:20])
  #import CalConstant and calculate the calibration constant factor
  CalConstant <-  as.data.frame(read.csv(file = filename, sep = ",", skip = 18, header = F ))
  CalConstant <- as.numeric(CalConstant[1,1])
  CalConstFactor <- CalConstant/currentCalConstant
  CalConstFactor
  
  # ###  #temp
  # df <-  as.data.frame(read.csv(file = "/Users/martinrauber/OneDrive/Uni Bern/EC-yield_and_charring_calculation_R/R skript development/tc-calc/Alessandra TC test set/Ale-TC-offline-1.txt", sep = ",", skip = 28, header = T ))
  # CalConstant <-  as.data.frame(read.csv(file = "/Users/martinrauber/OneDrive/Uni Bern/EC-yield_and_charring_calculation_R/R skript development/tc-calc/Alessandra TC test set/Ale-TC-offline-1.txt", sep = ",", skip = 18, header = F ))
  # CalConstant <- as.numeric(CalConstant[1,1])
  # CalConstant
  # str(CalConstant)
  # CalConstFactor <- currentCalConstant/CalConstant
  # CalConstFactor
  # 
  # df$time_s <- seq(1:length(df$CO2_ppm))
  # plot(df$CO2_ppm)
  # plot(df$pressure.psig)
  # df <-df[,c(21,16)]
  # ####
  
  #model
  colnames(df) = c("x", "y")
  model<-loess(y~x, span=0.05, data=df)
  mod.fun<-function(x) predict(model,newdata=x)
  #plot model with ggplot
  # theme_set(theme_classic(base_size = 11,base_family = "Helvetica"))
  # ggplot(df, aes(x, y)) +
  #   geom_point() +
  #   ylab("A.U.") +
  #   xlab("time (s)") +
  #   geom_smooth(formula = y~x, method = "loess",span=0.05, se = FALSE)
  # 
  #total carbon area
  total_area <<- integrate(mod.fun,50,250)
  #CH4 peak
  CH4_area <<- integrate(mod.fun,280,380)
  
  #TC raw amount
  peak.amount.tc.raw <<-  (total_area$value-coef[1,])/coef[2,]
  #calibration peak correction factor with CH4
  calibration_peak_correction_factor <- 57355.30357/CH4_area$value 
  total_CH4_corr <- total_area$value*calibration_peak_correction_factor
  #TC with CH4 correction and CalConstant correction
  peak.amount.tc.ch4corr <<-  (total_CH4_corr-coef[1,])/coef[2,]
  peak.amount.tc.ch4corr <<- peak.amount.tc.ch4corr*CalConstFactor
  peak.amount.tc.ch4corr
} 

#load data, run calculation --------------------------------------------------------

filename <- input$fileUploaded$datapath
df.amount <- NULL
for (i in filename){
  data.load.func(i)
  df.amount <- rbind(df.amount, data.frame(peak.amount.tc.ch4corr))
}

colnames(df.amount) <- c("TC (µg C)")
df.amount

###########################################################################
#end
###########################################################################



