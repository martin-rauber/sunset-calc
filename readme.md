
<h2 align="center">Sunset-calc</h2>

<h4 align="center">Analyse OC/EC data</h4>

## Overview

<img src="./R/sunset-calc-logo/sunset-calc-logo.png" width="170" align="right"/>

Sunset-calc is a [R shiny app](https://shiny.rstudio.com) to calculate the amount of carbon for TC, Swiss_3S, and Swiss4S protocols from raw files recorded using a commercial thermo-optical OC/EC analyser (Model 5L, [Sunset Laboratory Inc.](https://www.sunlab.com), OR, United States). Be aware that this app only works with the designated Sunset protocols and does not take the given filter area into account. You can upload one or multiple files, however, **each file must contain only one Sunset run**. If you have multiple runs in a single txt file, please use the 'file splitter' first.

## TC calc

Program to calculate TC for TC files. You get a .zip file containing a .csv file with the amount of carbon (µg C) for each step and the total carbon as well as the name of the sample corresponding to the input file name. If you compare the OC calc result to a result calculated by another software, be aware of the filter area that you used.

![TC calc demo](./demo/tc-calc-demo.gif)

## OC calc

Program to calculate S1, S2, and S3 OC and total for Sunset OC-removal (Swiss_3S) files. See [Zhang et al., 2012](https://doi.org/10.5194/acp-12-10841-2012) for details. If you use a modified protocol, you need to adjust the set time in seconds for each modified step. Please do not adjust the times if you don't know what you are doing. When you reload the app, the times will go back to the default value, which corresponds to the standard Swiss_3S. You get a .zip file containing a .csv file with the amount of carbon (µg C) for each step and the total carbon as well as the name of the sample corresponding to the input file name. If you compare the OC calc result to a result calculated by another software, be aware of the filter area that you used.

![OC calc demo](./demo/oc-calc-demo.gif)

## Swiss 4S calc

Program to calculate S1, S2, S3, S4, and total for Swiss_4S files. The calculation is basically the same as the OC calc for Swiss_3S, however, it is adjusted for the additional (EC) step at the end.

## OC/EC yield

Program to calculate the amount in S1, S2, S3, and total OC (Swiss_3S file) using the OC calc script. The amount of EC is calculated with the uploaded EC file using the TC calc script. Additionally, the EC yield and charring is calculated using the Swiss_3S file. The result from each calculation is then used to perform the amount correction.

## Yield calc

This app calculates the EC yield and charring for one or multiple OC removal runs with the Swiss_3S protocol. This app is useful for the optimisation (time, temperature) of the OC/EC separtion. Frequently, you will need perform the OC removal on multiple filters cuts to get enough EC for a radiocarbon measurement. Yield calc calculates EC yield and charring, removes potential outliers and gives you the result you need. A generic file is used for laser signal correction. Optionally, you can upload your own file (TC, EC, or Swiss_4S) for temperature dependent laser signal correction. 

## File splitter

The file splitter splits a Sunset txt raw file with multiple runs in one to multiple files with one run. The app is no-frills; upload the file and get a zip file with each run in a single txt file. Result txt file nomenclature: [sample number]-[file name]-[sample name]-split.txt <br /> Additional to splitting files, the file splitter also shows you a table with some relevant information regarding each file in a table, which you can copy and download.

## How does it work?

The *Sunset calc* app is made with [shinydashboard](https://rstudio.github.io/shinydashboard/), which contains the apps *TC calc*, *OC calc*, *Swiss 4S calc*, *OC/EC yield*, and *Yield calc* linked in the sidebar. Additionally, there is a *file splitter* app in the sidebar. The plots immediately shown after file upload are generated independently from the calculation in the app, the calculation takes place in a linked R script and is triggered by pressing the 'Calculate & Download' button.

### Calculation

#### Calibration coefficients and calibration constant

First, the coefficients from NDIR calibration are calculated. The amount reflects the known amount of analyte (sucrose solution) added, the area is the calculated area with the code below. Please be aware that the values storred file are valid for the Sunset device at [LARA](https://www.14c.unibe.ch) and might be significantly different on an other device. The csv file is imported is assigned the variable `NDIR_calib`. A linear regression model using the lm() function is made and the coefficients stored to the variable `coef`. Using the coefficients, the `currentCalConstant` is calculated. This CalConstant is later used to for correction should the sample have used a different calibration constant (e.g. when measuring online with GIS/MICADAS). Initially, the calculated intercept was used, however, trials showed that the results match the Sunset Calc426 (program provided by Sunset for analysis) better when the intercept was set to zero.

    # calculation of coefficients from NDIR calibration, intercept set to zero
    NDIR_calib <- read.csv("NDIR-integrals-20200224-offline.csv", header = T)
    intercept <- 0
    calib <- lm(I(area - intercept) ~ 0+ amount, data = NDIR_calib)
    coef <- as.data.frame(calib$coefficients)
    coef <- rbind(intercept, as.data.frame(calib$coefficients))
    currentCalConstant <<- (mean(NDIR_calib$CH4.area)-coef[1,])/coef[2,]

#### Calculation function

The uploaded Sunset raw file is imported to a data frame, a new column for time added and all unnecessary columns except `time_s` and `CO2_ppm` removed.

      df <-  as.data.frame(read.csv(file = filename, sep = ",", skip = 28, header = T ))
      df$time_s <- seq(1:length(df$CO2_ppm))
      df <-df[,c(21,16)]

A baseline correction is then performed:

      df$CO2_ppm <- df$CO2_ppm-median(sort(df$CO2_ppm,decreasing=F)[1:length(which(df$CO2_ppm < 0))])
      df$CO2_ppm[df$CO2_ppm < 0] <- 0

The calibration constant (`CalConstant`) value is imported from the file and the calibration constant factor (`CalConstFactor`) calculated. The `CalConstant` in a file could be different due to a new calibration, an old file, or an online measurement. Online measurements are different due to a different back pressure to the NDIR. Pressure as well as temperature affect CO<sub>2</sub> measurements with NDIR ([Yasuda et al., 2012](https://doi.org/10.3390/s120303641)).

      CalConstant <-  as.data.frame(read.csv(file = filename, sep = ",", skip = 18, header = F ))
      CalConstant <- as.numeric(CalConstant[1,1])
      CalConstFactor <- CalConstant/currentCalConstant

The local regression model is made, the CH<sub>4</sub> area integrated and with that the `calibration_peak_correction_factor` calculated. Finally, the TC area is integrated and corrected with the calibration peak correction factor. Then, the amount in µg C is calculated using the calibration coefficients and corrected with the `CalConstFactor`.

      colnames(df) = c("x", "y")
      model<-loess(y~x, span=0.05, data=df)
      mod.fun<-function(x) predict(model,newdata=x)

The shown script is valid for both TC, OC, and Swiss 4S calculation, only the following section is different:

#### TC


    #CH4 correction
      CH4_area <- integrate(mod.fun,280,380)

    #calibration peak correction factor with CH4
      calibration_peak_correction_factor <- mean(NDIR_calib$CH4.area)/CH4_area$value 

    #Calculate area for each peak and total
    #total carbon
      total_area <- integrate(mod.fun,50,250)
      total_area <- total_area$value*calibration_peak_correction_factor
      amount.tc <- (total_area-coef[1,])/coef[2,]
      amount.tc <<- amount.tc*CalConstFactor

#### OC

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

#### Swiss 4S

    #total protocol length
      total.length <- length(df$x)
      
      #CH4 corrections
      CH4_area <- integrate(mod.fun,total.length-140,total.length)
      
      #calibration peak correction factor with CH4
      calibration_peak_correction_factor <- mean(NDIR_calib$CH4.area)/CH4_area$value 

      #Calculate area for each peak and total.
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
      #EC S4
      EC_areaS4 <- integrate(mod.fun,S3_length,S4_length)
      EC_areaS4 <- EC_areaS4$value*calibration_peak_correction_factor
      amount.S4 <- (EC_areaS4-coef[1,])/coef[2,]
      amount.S4 <<- amount.S4*CalConstFactor
      #total carbon
      total_area <- integrate(mod.fun,50,total.length-140,subdivisions=10000)
      total_area <- total_area$value*calibration_peak_correction_factor
      amount.tc <- (total_area-4*coef[1,])/coef[2,]
      amount.tc <<- amount.tc*CalConstFactor

The calculation code from above was wrapped into a function:

      data.load.func = function(filename) {
    #code from above here
    } 

This function is executed for each uploaded file:

#### TC

      filename <- input$fileUploaded$datapath
    #file name for output
      filename.text <<- input$fileUploaded$name

      df.amount <- NULL
      for (i in filename){
      data.load.func(i)
      df.amount <- rbind(df.amount, data.frame(amount.tc))
    }

    # combine file name with ouput data
      df.amount <- cbind(filename.text,df.amount)
      colnames(df.amount) <- c("sample name","TC (ug C)")
      df.amount

#### OC

    #get file name
      filename <- input$fileUploaded$datapath
    #file name for output
      filename.text <<- input$fileUploaded$name
    #length of 
      S1_length <<- (110+as.numeric(input$inTextS1))
      S2_length <<- (490+as.numeric(input$inTextS2))
      S3_length <<- (690+as.numeric(input$inTextS3))

    #create an empty df
      df.amount <- NULL
    #loop function
      for (i in filename){
      data.load.func(i)
      df.amount <- rbind(df.amount, data.frame(amount.S1, amount.S2, amount.S3,amount.tc))
    }

    # combine file name with ouput data
      df.amount <- cbind(filename.text,df.amount)
      colnames(df.amount) <- c("sample name","S1 (ug C)","S2 (ug C)","S3 (ug C)", "total (ug C)")
      df.amount

#### Swiss 4S

    #get file name
    filename <- input$fileUploaded$datapath
    #file name for output
    filename.text <<- input$fileUploaded$name
    #length of 
    S1_length <- (110+as.numeric(input$inTextS1))
    S2_length <- (490+as.numeric(input$inTextS2))
    S3_length <- (690+as.numeric(input$inTextS3))
    S4_length <- (1050+as.numeric(input$inTextS4))

    #create an empty df
    df.amount <- NULL
    #loop function
    for (i in filename){
      data.load.func(i)
      df.amount <- rbind(df.amount, data.frame(amount.S1, amount.S2, amount.S3,amount.S4,amount.tc))
    }

    # combine file name with ouput data
    df.amount <- cbind(filename.text,df.amount)
    colnames(df.amount) <- c("sample name","S1 (ug C)","S2 (ug C)","S3 (ug C)","S4 (ug C)", "total (ug C)")
    df.amount

The resulting `df.amount` is handled back to the shiny app for output.

### File splitter

The file splitter is very simple and consists of three sections:

#### import

    #get the filename
      filename.long <- input$fileUploaded$name
    #shortened filename without txt extension
      filename.short <- str_sub(filename.long, end=-5)
    #get the file datapath
      filedatapath <- input$fileUploaded$datapath   
    #read file in df
      df.head <- (read_csv(file = filedatapath, skip = 28))[1,]
    #create colnames
      df.colnames <- colnames(df.head, do.NULL = TRUE, prefix = "col")
    #read the the file
      df <- read_csv(file = filedatapath, col_names =  df.colnames)
    #get the index of each row which contains the word "Sample"
      df.rowindex.newfile <- which(df$FID1=="Sample",arr.ind=T)
    # subset to get the sample name
      df.samplename <- df[(df.rowindex.newfile+1),1]
    #get row index of for the end of the file:
      df.rowindex.endfile <- c((df.rowindex.newfile[2:length(df.rowindex.newfile)])-1,length(df$FID1))

#### split

    #split df and save as txt
      for (i in seq(1:length(df.rowindex.newfile))) {
        write.csv(df[df.rowindex.newfile[i]:df.rowindex.endfile[i],], row.names=FALSE, quote=FALSE, file = paste0(i,"-",   filename.short,"-", df.samplename[i,],"-split",".txt"))
          }

#### output

    #create a list of txt files
      file.list <- paste(list.files(getwd(), pattern = "*-split*.txt"), sep = "")
    #make zip file
      zip(zipfile=fname, files=file.list)
    #remove the created files
      file.remove(file.list)

### OC/EC yield

The server side of the `oc_ec_yield_app.R` contains the following script:

        #run yields calc, oc-calc, and tc calc, then return the desired data
        source("yields_calc_shiny.R", local = TRUE)
        source("oc_calc_shiny.R", local = TRUE)
        source("tc_calc_shiny.R", local = TRUE)
        return(list(df.yield=df_raw, df.amount.oc=df.amount.oc, df.amount.tc=df.amount.tc))

As soon as the user presses the 'Calculate & Download' button, the uploaded OC data is run with the `yields_calc_shiny.R` and `oc_calc_shiny.R` script and the EC data is run in the `tc_calc_shiny.R` script. the return call gives the results from each calculation. The results are stored in the new data frame `df.result`. Using this data frame, the corrected OC and EC amounts are calculated. `TCcalculated` is the sum of OC and EC, `ECcorr` is the EC yield corrected amount of EC, and `OCcorr` is the EC yield corrected amount of OC. Finally, the data frame is sorted and column names applied.

      output$downloadData <- downloadHandler(
        filename = 'OC_EC_yield_result.zip',
        content = function(fname) {
          df.result <- cbind(datasetInput()$df.yield,datasetInput()$df.amount.oc,datasetInput()$df.amount.tc)
          df.result$TCcalculated <- df.result[,10]+df.result[,12]
          df.result$ECcorr <- df.result[,12]/df.result[,1]
          df.result$OCcorr <- df.result[,10]-(df.result$ECcorr-df.result[,12])
          df.result <- cbind(df.result[,6],df.result[,11],df.result[,1:4],df.result[,7:10],df.result[,12],df.result$TCcalculated,df.result$OCcorr,df.result$ECcorr)
          colnames(df.result) <-  c("sample name OC","sample name TC","EC yield","charring S1","charring S2","charring S3","S1 (ug C)","S2 (ug C)", "S3 (ug C)","total OC (ug C)","EC (ug C)", "TC calculated (ug C)", "corr. OC (ug C)", "corr. EC (ug C)")
          print(df.result)

          fs <- c( "oc-ec-yield-result.csv","yield-calc_summary1.pdf","yield-calc_summary2.pdf")
          write.csv(df.result, file = "oc-ec-yield-result.csv", row.names=FALSE)
          print (fs)

          zip(zipfile=fname, files=fs)

          #remove temporary files form folder again
          ##path csv and pdf
          file.list.rem.csv <- paste(getwd(), "/",list.files(getwd(), pattern = "*result.csv"), sep = "")
          file.list.rem.pdf <- paste(getwd(), "/",list.files(getwd(), pattern = "*.pdf"), sep = "")
          #removal
          file.remove(file.list.rem.csv)
          file.remove(file.list.rem.pdf)
        },
        contentType = "application/zip"
      )

The exported csv file contains the sample name for both the OC file as well as the EC file to avoid mistakes before EC yield and charring results as well as the raw and calculated amount of carbon for each fraction. Additional to the result csv file, a summary pdf file generated by the yield calculation is included in the zip download.

## About Sunset calc

### Info

This app was created by [Martin Rauber](https://martin-rauber.com) for [LARA](https://www.14c.unibe.ch), the Laboratory for the Analysis of Radiocarbon with AMS at the University of Bern. The EC yield calculation script is part of [COMPYCALC](https://doi.org/10.5281/zenodo.4318834) and adapted from there. Sunset calc is available online: [14c.unibe.ch/sunsetcalc](https://www.14c.unibe.ch/sunsetcalc). Please get in touch for any bug fixes and suggestions!

### License

Sunset calc is released under the [MIT License](./LICENCE.txt).
