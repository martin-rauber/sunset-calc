#------------------------Plot theme------------------------------ 

library("tidyverse")
setwd("~/OneDrive/Uni Bern/EC-yield_and_charring_calculation_R/R skript development/sunset-calc/readme")

#ggplot theme
theme_set(theme_classic(base_size = 11,base_family = "Helvetica"))

#manual palette for ggplot
palette1 <- c("steelblue1","darkorchid2","chocolate1")

#load txt files Ale ---------------------------------------------
df.example.online <-  as.data.frame(read.csv(file = "20201006_UD20_DOC_3.txt", sep = ",", skip = 28, header = T ))
df.example.online$time_s <- seq(1:length(df.example.online$CO2_ppm))
df.example.online$MICADAS_mode <- rep("online",length(df.example.online$CO2_ppm))
df.example.offline <-  as.data.frame(read.csv(file = "2020_05_14-thai-oc-removal-c21-b.txt", sep = ",", skip = 28, header = T ))
df.example.offline$time_s <- seq(1:length(df.example.offline$CO2_ppm))
df.example.offline$MICADAS_mode <- rep("offline",length(df.example.offline$CO2_ppm))
#bind online and offline together
df.example <- rbind(df.example.online,df.example.offline)
str(df.example)
df.example.online$pressure.psig
#------------------------Plots-------------------------------------- 

#plot ndir
df.plot.ndir.ndir <- ggplot(df.example, aes(time_s, CO2_ppm, colour = MICADAS_mode)) + 
  geom_point()+
  geom_line()+
  ylab("CO2 (ppm)") +
  xlab("time (s)") +
#   #scale_y_continuous(expand = c(0, 0), limits = c(0,20))+
   scale_colour_manual(values = palette1)
df.plot.ndir.ndir

#plot pressure
df.plot.ndir.pressure <- ggplot(df.example, aes(time_s, pressure.psig, colour = MICADAS_mode)) + 
  geom_point()+
  geom_line()+
  ylab("pressure (psi)") +
  xlab("time (s)") +
  #   #scale_y_continuous(expand = c(0, 0), limits = c(0,20))+
  scale_colour_manual(values = palette1)
df.plot.ndir.pressure


