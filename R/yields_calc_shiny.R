####################################################################################
#settings
####################################################################################

#select the fitting type values: 
fitting_type <<- "poly"                                     #poly, expo, or manual
manual.coef = c(7645, -0.371, 5.32E-4)                    #enter manual coefficients if fitting_type manual is selected

#enter a filename for the export files
result_filename <<- input$fileUploadedOC$name   

#set value for outlier removal (1.5*IQR recomended)
IQR <- 1.5

####################################################################################
#script
####################################################################################

#load libraries---------------------------------------------------------------------

library("ggpubr")
library("pastecs")
library("dplyr")
library("ggplot2")
library("pracma")
library("signal")
library("gridGraphics")
library("pdftools")
library("outliers")
library("zip")
library("stringr")

#load function----------------------------------------------------------------------
data_load_func = function(filename) {
  cooldown = read.csv("cooldown_data.csv", sep = ",", header = T)
  dat = as.data.frame(read.csv(file = filename, sep = ",", skip = 28, header = T ))[,c(1:18)]
  tabla_complete <<- rbind(dat, cooldown)
  yield_calc = function(tabla_complete, fitting_type, manual.coef) {source("yields_calc_ext.R")}
  yield_calc(tabla_complete)
  
} 

#
#load data, run calculation --------------------------------------------------------
filename <<- input$fileUploadedOC$datapath
df_raw <- NULL
for (i in filename){
  data_load_func(i)
  input_data <- read.csv(file = i, sep = ",", col.names = paste0("C",seq_len(20)), fill = TRUE, header = T)
  filter_area_oc = as.numeric(input_data[min(grep('FilterArea',input_data$C1))+1,1])
  df_raw <- rbind(df_raw, data.frame(tabla_resultados2$EC_yield_S1,tabla_resultados2$EC_yield_S2,tabla_resultados2$EC_yield_S3,tabla_resultados2$charring_S1,tabla_resultados2$charring_S2,tabla_resultados2$charring_S3,tabla_resultados2$charring_total,filter_area_oc))
}

#rm(list=setdiff(ls(), c("df","result_filename", "csv_raw", "csv_stat", "csv_mean")))
#df_raw$filter <- c(rep(result_filename, length(df_raw$tabla_resultados2.EC_yield_S1)))
df_raw$filter <- result_filename
colnames(df_raw) <- c("EC_yield_S1", "EC_yield_S2", "EC_yield_S3", "charring_S1", "charring_S2", "charring_S3", "charring_total", "filter_area_oc_cm2", "filter_name")

#extract specific data--------------------------------------------------------------
df_length_raw <- length(df_raw$EC_yield_S1)
df_charr_raw <- data.frame("charring value" = c(df_raw$charring_S1,df_raw$charring_S2,df_raw$charring_S3,df_raw$charring_total), "Sunset step" = c(rep("S1",df_length_raw), rep("S2",df_length_raw), rep("S3",df_length_raw), rep("total",df_length_raw)), stringsAsFactors = FALSE)
df_yield_raw <- data.frame("EC-yield" = c(df_raw$EC_yield_S1,df_raw$EC_yield_S2,df_raw$EC_yield_S3), "Sunset step"= c(rep("S1",df_length_raw), rep("S2",df_length_raw), rep("S3",df_length_raw)), stringsAsFactors = FALSE)

#plots with raw data----------------------------------------------------------------

#plot: EC-yield (raw data)
theme_set(theme_classic(base_size = 13))
plot_yield_raw <- ggboxplot(width=0.33, data=df_yield_raw, x = "Sunset.step", y = "EC.yield",  xlab="Sunset step", ylab = "EC-yield")
#plot: charring for each Sunset step (raw data)
plot_charr_raw <- ggboxplot(data=df_charr_raw, x="Sunset.step", y="charring.value", xlab="Sunset step", ylab = "charring")+
  geom_hline(yintercept = 0, color="red", lty= 5)

fig_EC_yield_charring_raw <- ggarrange(plot_yield_raw, plot_charr_raw, ncol = 2, nrow = 1)
fig_EC_yield_charring_raw <- fig_EC_yield_charring_raw + theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
fig_EC_yield_charring_raw <- annotate_figure(fig_EC_yield_charring_raw, top = text_grob("Raw data", color = "darkred", face = "bold", size = 12))

#remove outliers-------------------------------------------------------------------
iqr_scores <- abs(scores(df_raw[,3:6],type="iqr"))
outlier_rows <- unique(which((iqr_scores>IQR) == TRUE, arr.ind=TRUE)[,1])
n_outlier_rows <- length(outlier_rows)
if (n_outlier_rows == 0) {
  df <- df_raw
} else {
  df <- df_raw[-c(outlier_rows),]
}

#extract specific data w/o outliers-------------------------------------------------
df_length <- length(df$EC_yield_S3)
df_charr <- data.frame("charring value" = c(df$charring_S1,df$charring_S2,df$charring_S3,df$charring_total), "Sunset step" = c(rep("S1",df_length), rep("S2",df_length), rep("S3",df_length), rep("total",df_length)), stringsAsFactors = FALSE)
df_yield <- data.frame("EC-yield" = c(df$EC_yield_S1,df$EC_yield_S2,df$EC_yield_S3), "Sunset step"= c(rep("S1",df_length), rep("S2",df_length), rep("S3",df_length)), stringsAsFactors = FALSE)

#calculate stats and generate plots-------------------------------------------------

#general stats
stat <- stat.desc(df[, -9])
#mean of each EC-yield and each Sunset step
df_mean <- stat[9,]

#calculate stats and generate plots-------------------------------------------------

#EC-yield and charring plots after outlier removal

#plot: EC-yield
theme_set(theme_classic(base_size = 13))
plot_yield <- ggboxplot(width=0.33, data=df_yield, x = "Sunset.step", y = "EC.yield",  xlab="Sunset step", ylab = "EC-yield", outlier.shape=NA)+
  geom_jitter(width=0.1,height=0, colour = "#424242")
#plot: charring for each Sunset step
plot_charr <- ggboxplot(data=df_charr, x="Sunset.step", y="charring.value", xlab="Sunset step", ylab = "charring", outlier.shape=NA)+
  geom_jitter(width=0.1,height=0, colour = "#424242")+
  geom_hline(yintercept = 0, color="red", lty= 5)

fig_EC_yield_charring_wo_outliers <- ggarrange(plot_yield, plot_charr, ncol = 2, nrow = 1)
fig_EC_yield_charring_wo_outliers <- fig_EC_yield_charring_wo_outliers + theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))
fig_EC_yield_charring_wo_outliers <- annotate_figure(fig_EC_yield_charring_wo_outliers, top = text_grob("Outliers removed", color = "darkblue", face = "bold", size = 12))

#stats table
colnames(stat) <- c("EC-yield S1", "EC-yield S2", "EC-yield S3", "charring S1", "charring S2", "charring S3", "charring total", "filter_area_oc_cm2")
stat_table <- ggtexttable(format(stat[c(9,4:6,8,12,13),c(1:7)], digits = 3),  theme = ttheme("blank", base_size=10))
colnames(stat) <- c("EC_yield_S1", "EC_yield_S2", "EC_yield_S3", "charring_S1", "charring_S2", "charring_S3", "charring_total", "filter_area_oc_cm2") #rename columns for csv-output

#generate summary pdfs---------------------------------------------------------------

#summary pdf page 1
fig_summary1_temp <- ggarrange( plot_fit, fig_EC_yield_charring_raw, fig_EC_yield_charring_wo_outliers, stat_table, nrow = 4, ncol = 1)
fig_summary1_temp <- annotate_figure(fig_summary1_temp, top = text_grob(paste("\n","Summary:","\n","slope, EC-yield, charring, and statistics",sep=""), color = "black" , face = "bold", size = 16),)
#save as pdf in tempdir
tempPdf1 <- file.path(tempdir(), "yield-calc_summary1.pdf")
ggsave(filename = tempPdf1, plot = fig_summary1_temp , width = 8.3, height = 11.7)
#summary QQ-Plots-------------------------------------------------------------------

#QQ-plots EC
plot_qq_EC_raw <- ggqqplot(df_raw$EC_yield_S3, ylab = "EC-yield total", title = "Raw data")
plot_qq_EC_corr <- ggqqplot(df$EC_yield_S3, ylab = "EC-yield total", title = "Outliers removed")
#QQ-plots charring S1
plot_qq_S1_raw <- ggqqplot(df_raw$charring_S1, ylab = "Charring S1")
plot_qq_S1_corr <- ggqqplot(df$charring_S1, ylab = "Charring S1")
#QQ-plots charring S2
plot_qq_S2_raw <- ggqqplot(df_raw$charring_S2, ylab = "Charring S2")
plot_qq_S2_corr <- ggqqplot(df$charring_S2, ylab = "Charring S2")
#QQ-plots charring S1
plot_qq_S3_raw <- ggqqplot(df_raw$charring_S3, ylab = "Charring S3")
plot_qq_S3_corr <- ggqqplot(df$charring_S3, ylab = "Charring S3")

#summary pdf page 2
fig_summary2_temp <- ggarrange( plot_qq_EC_raw, plot_qq_EC_corr, plot_qq_S1_raw, plot_qq_S1_corr,plot_qq_S2_raw, plot_qq_S2_corr, plot_qq_S3_raw, plot_qq_S3_corr, nrow = 4, ncol = 2, align = "hv")
fig_summary2_temp <- annotate_figure(fig_summary2_temp, top = text_grob(paste("\n","Outliers: QQ-Plots","\n",sep=""), color = "black" , face = "bold", size = 16),bottom = text_grob(paste("\n","Total number of filters: ",df_length_raw,"\n","Outliers removed: ",n_outlier_rows,"\n","Filters used for calculation: ", c(df_length_raw-n_outlier_rows),"\n", sep=""), color = "black" , face = "plain", size = 13),)
#save as pdf in tempdir
tempPdf2 <- file.path(tempdir(), "yield-calc_summary2.pdf")
ggsave(filename = tempPdf2, plot = fig_summary2_temp , width = 8.3, height = 11.7)
#end--------------------------------------------------------------------------------



