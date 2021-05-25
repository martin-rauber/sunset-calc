library("shiny")
library("shinyWidgets")
library("tidyverse")
library("shinythemes")
#ggplot theme
theme_set(theme_classic())
# ui.R

ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  setBackgroundColor("#ecf0f5"),
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file ----
      fileInput("fileUploaded", "Drag & Drop File(s)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Text input for OC removal step length
      textInput("inTextS1", "S1", value = 240),
      textInput("inTextS2", "S2", value = 120),
      textInput("inTextS3", "S3", value = 360),
      textInput("inTextS4", "S4", value = 280),
      
      # Output: Download a file ----
      downloadButton("downloadData", "Calculate & Download"),
      
      # CSS style for the download button ----
      tags$style(type='text/css', "#downloadFile { width:100%; margin-top: 35px;}")),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("NDIR", plotOutput('plots_ndir', width = "100%", height = "360px")),
                  tabPanel("Temperature", plotOutput('plots_temperature',width = "100%", height = "360px")),
                  tabPanel("Pressure", plotOutput('plots_pressure',width = "100%", height = "360px")),
                  tabPanel("Laser", plotOutput('plots_laser',width = "100%", height = "360px"))
      )
    )
  )
)
)
#server
server <- function(input, output) {
  
  datasetInput <- reactive({
    source("swiss4s_calc_shiny.R", local = TRUE)
    return(list(df.amount=df.amount)) 
  })
  
  #observe input for plot
  observe({
    data = input$fileUploaded
    if(is.null(data))
      return(NULL)
    
    #load data for plot
    df <- do.call("rbind", sapply(data$datapath, read.csv, sep=",",skip = 28, header = T, simplify = FALSE))
    df_lenght <-  length((as.data.frame(read.csv(file = data$datapath[1], sep = ",", skip = 28, header = T )))$CO2_ppm)
    df$time_s <- rep(seq(1:df_lenght),length(data$name))
    df$file_name <- rep(data$name,each=df_lenght)
    S1_length <- (110+as.numeric(input$inTextS1))
    S2_length <- (490+as.numeric(input$inTextS2))
    S3_length <- (690+as.numeric(input$inTextS3))
    S4_length <- (1080+as.numeric(input$inTextS4))
    #plot output
    output$plots_ndir = renderPlot({
      ggplot(df, aes(x = time_s, y = CO2_ppm,colour = file_name)) +
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(),legend.title = element_blank())+
        ylab(bquote(paste("NDIR CO"["2"]*" (ppm)")))+
        xlab("time (s)") +
        annotate("rect", xmin = 50, xmax = S1_length, ymin = 0, ymax = max(df$CO2_ppm)+0.1*max(df$CO2_ppm), fill = "red", color ="NA", alpha = .1)+
        annotate("rect", xmin = S1_length, xmax = S2_length, ymin = 0, ymax = max(df$CO2_ppm)+0.1*max(df$CO2_ppm), fill = "green", color ="NA", alpha = .1)+
        annotate("rect", xmin = S2_length, xmax = S3_length, ymin = 0, ymax = max(df$CO2_ppm)+0.1*max(df$CO2_ppm), fill = "blue", color ="NA", alpha = .1)+
        annotate("rect", xmin = S3_length, xmax = S4_length, ymin = 0, ymax = max(df$CO2_ppm)+0.1*max(df$CO2_ppm), fill = "yellow", color ="NA", alpha = .1)+
        annotate("text", x = (50+S1_length)*0.5,hjust = 0.5, y = max(df$CO2_ppm)+0.05*max(df$CO2_ppm), label = "S1")+
        annotate("text", x = S1_length+(S2_length-S1_length)*0.5,hjust = 0.5, y = max(df$CO2_ppm)+0.05*max(df$CO2_ppm), label = "S2")+
        annotate("text", x = S2_length+(S3_length-S2_length)*0.5,hjust = 0.5, y = max(df$CO2_ppm)+0.05*max(df$CO2_ppm), label = "S3")+
        annotate("text", x = S3_length+(S4_length-S3_length)*0.5,hjust = 0.5, y = max(df$CO2_ppm)+0.05*max(df$CO2_ppm), label = "S4")+
        geom_point()
    })
    output$plots_temperature = renderPlot({
      ggplot(df, aes(x = time_s, y = Sample_temperature,colour = file_name)) +
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(), legend.title = element_blank())+
        ylab("temperature (°C)") +
        xlab("time (s)") +
        annotate("rect", xmin = 50, xmax = S1_length, ymin = 0, ymax = max(df$Sample_temperature)+0.1*max(df$Sample_temperature), fill = "red", color ="NA", alpha = .1)+
        annotate("rect", xmin = S1_length, xmax = S2_length, ymin = 0, ymax = max(df$Sample_temperature)+0.1*max(df$Sample_temperature), fill = "green", color ="NA", alpha = .1)+
        annotate("rect", xmin = S2_length, xmax = S3_length, ymin = 0, ymax = max(df$Sample_temperature)+0.1*max(df$Sample_temperature), fill = "blue", color ="NA", alpha = .1)+
        annotate("rect", xmin = S3_length, xmax = S4_length, ymin = 0, ymax = max(df$Sample_temperature)+0.1*max(df$Sample_temperature), fill = "yellow", color ="NA", alpha = .1)+
        annotate("text", x = (50+S1_length)*0.5,hjust = 0.5, y = max(df$Sample_temperature)+0.05*max(df$Sample_temperature), label = "S1")+
        annotate("text", x = S1_length+(S2_length-S1_length)*0.5,hjust = 0.5, y = max(df$Sample_temperature)+0.05*max(df$Sample_temperature), label = "S2")+
        annotate("text", x = S2_length+(S3_length-S2_length)*0.5,hjust = 0.5, y = max(df$Sample_temperature)+0.05*max(df$Sample_temperature), label = "S3")+
        annotate("text", x = S3_length+(S4_length-S3_length)*0.5,hjust = 0.5, y = max(df$Sample_temperature)+0.05*max(df$Sample_temperature), label = "S4")+
        geom_point()
    })
    output$plots_pressure = renderPlot({
      ggplot(df, aes(x = time_s, y = pressure.psig,colour = file_name)) +
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(),legend.title = element_blank())+
        xlab("time (s)") +
        scale_y_continuous(
          "pressure (psi)", 
          sec.axis = sec_axis(~ . * 68.9476, name = "pressure (mbar)")
        )+
        annotate("rect", xmin = 50, xmax = S1_length, ymin = 0, ymax = max(df$pressure.psig)+0.1*max(df$pressure.psig), fill = "red", color ="NA", alpha = .1)+
        annotate("rect", xmin = S1_length, xmax = S2_length, ymin = 0, ymax = max(df$pressure.psig)+0.1*max(df$pressure.psig), fill = "green", color ="NA", alpha = .1)+
        annotate("rect", xmin = S2_length, xmax = S3_length, ymin = 0, ymax = max(df$pressure.psig)+0.1*max(df$pressure.psig), fill = "blue", color ="NA", alpha = .1)+
        annotate("rect", xmin = S3_length, xmax = S4_length, ymin = 0, ymax = max(df$pressure.psig)+0.1*max(df$pressure.psig), fill = "yellow", color ="NA", alpha = .1)+
        annotate("text", x = (50+S1_length)*0.5,hjust = 0.5, y = max(df$pressure.psig)+0.05*max(df$pressure.psig), label = "S1")+
        annotate("text", x = S1_length+(S2_length-S1_length)*0.5,hjust = 0.5, y = max(df$pressure.psig)+0.05*max(df$pressure.psig), label = "S2")+
        annotate("text", x = S2_length+(S3_length-S2_length)*0.5,hjust = 0.5, y = max(df$pressure.psig)+0.05*max(df$pressure.psig), label = "S3")+
        annotate("text", x = S3_length+(S4_length-S3_length)*0.5,hjust = 0.5, y = max(df$pressure.psig)+0.05*max(df$pressure.psig), label = "S4")+
        geom_point()
    })
    output$plots_laser = renderPlot({
      ggplot(df, aes(x = time_s, y = laserTrans,colour = file_name)) +
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(), legend.title = element_blank())+
        ylab("transmission (A.U.)") +
        xlab("time (s)") +
        annotate("rect", xmin = 50, xmax = S1_length, ymin = 0, ymax = max(df$laserTrans)+0.1*max(df$laserTrans), fill = "red", color ="NA", alpha = .1)+
        annotate("rect", xmin = S1_length, xmax = S2_length, ymin = 0, ymax = max(df$laserTrans)+0.1*max(df$laserTrans), fill = "green", color ="NA", alpha = .1)+
        annotate("rect", xmin = S2_length, xmax = S3_length, ymin = 0, ymax = max(df$laserTrans)+0.1*max(df$laserTrans), fill = "blue", color ="NA", alpha = .1)+
        annotate("rect", xmin = S3_length,xmax = S4_length, ymin = 0, ymax = max(df$laserTrans)+0.1*max(df$laserTrans), fill = "yellow", color ="NA", alpha = .1)+
        annotate("text", x = (50+S1_length)*0.5,hjust = 0.5,  y = max(df$laserTrans)+0.05*max(df$laserTrans),  label = "S1")+
        annotate("text", x = S1_length+(S2_length-S1_length)*0.5,hjust = 0.5, y = max(df$laserTrans)+0.05*max(df$laserTrans), label = "S2")+
        annotate("text", x = S2_length+(S3_length-S2_length)*0.5,hjust = 0.5, y = max(df$laserTrans)+0.05*max(df$laserTrans), label = "S3")+
        annotate("text", x = S3_length+(S4_length-S3_length)*0.5,hjust = 0.5, y = max(df$laserTrans)+0.05*max(df$laserTrans), label = "S4")+
        geom_point()
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = paste('Swiss4S_amount_calculation_result_',system("git rev-parse --short HEAD", intern=TRUE),".csv",sep=""),
    content = function(file) {
      write.csv(datasetInput()$df.amount, file, row.names=FALSE)
    }
  )
  
}



shinyApp(ui = ui, server = server) 

