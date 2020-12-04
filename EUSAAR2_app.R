library(shiny)
library(shinyWidgets)
library(tidyverse)
#ggplot theme
theme_set(theme_classic())

# server.R
server <- function(input, output) {
  
  datasetInput <- reactive({
    source("EUSAAR2_calc_shiny.R", local = TRUE)
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
    S4_length <- (1050+as.numeric(input$inTextS4))
    output$plots_ndir = renderPlot({
      ggplot(df, aes(x = time_s, y = CO2_ppm,colour = file_name)) +
        #theme(legend.position = "none")+
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(),legend.title = element_blank())+
        ylab(bquote(paste("NDIR CO"["2"]*" (ppm)")))+
        xlab("time (s)") +
        annotate("rect", xmin = 50, xmax = S1_length, ymin = 0, ymax = max(df$CO2_ppm), fill = "red", color ="red", alpha = .05)+
        annotate("rect", xmin = S1_length, xmax = S2_length, ymin = 0, ymax = max(df$CO2_ppm), fill = "green", color ="green", alpha = .05)+
        annotate("rect", xmin = S2_length, xmax = S3_length, ymin = 0, ymax = max(df$CO2_ppm), fill = "blue", color ="blue", alpha = .05)+
        annotate("rect", xmin = S3_length, xmax = S4_length, ymin = 0, ymax = max(df$CO2_ppm), fill = "yellow", color ="yellow", alpha = .05)+
        annotate("text", x = 70, y = max(df$CO2_ppm)-60, label = "S1")+
        annotate("text", x = S1_length+20, y = max(df$CO2_ppm)-60, label = "S2")+
        annotate("text", x = S2_length+20, y = max(df$CO2_ppm)-60, label = "S3")+
        annotate("text", x = S3_length+20, y = max(df$CO2_ppm)-60, label = "S4")+
        geom_point()
    })
    output$plots_pressure = renderPlot({
      ggplot(df, aes(x = time_s, y = pressure.psig,colour = file_name)) +
        #theme(legend.position = "none")+
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(),legend.title = element_blank())+
        xlab("time (s)") +
        scale_y_continuous(
          "pressure (psi)", 
          sec.axis = sec_axis(~ . * 68.9476, name = "pressure (mbar)")
        )+
        geom_point()
    })
    output$plots_temperature = renderPlot({
      ggplot(df, aes(x = time_s, y = Sample_temperature,colour = file_name)) +
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(), legend.title = element_blank())+
        ylab("temperature (°C)") +
        xlab("time (s)") +
        geom_point()
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = 'EUSAAR2_amount_calculation_result.zip',
    content = function(fname) {
      
      fs <- c("EUSAAR2_amount_calculation_result.csv")
      write_csv(datasetInput()$df.amount, file = "EUSAAR2_amount_calculation_result.csv")
      print (fs)
      
      zip(zipfile=fname, files=fs)
      
      file.list.rem <- paste(getwd(), "/",list.files(getwd(), pattern = "*result.csv"), sep = "")
      file.remove(file.list.rem)
      
    },
    contentType = "application/zip"
  )
  
}

# ui.R
library(shinythemes)

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
                  tabPanel("Pressure", plotOutput('plots_pressure',width = "100%", height = "360px"))
      )
    )
  )
)
)

shinyApp(ui = ui, server = server) 

