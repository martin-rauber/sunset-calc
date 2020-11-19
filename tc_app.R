# server.R
server <- function(input, output) {
  
  datasetInput <- reactive({
    source("tc_calc_shiny.R", local = TRUE)
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
    output$plots_ndir = renderPlot({
      ggplot(df, aes(x = time_s, y = CO2_ppm,colour = file_name)) +
        #theme(legend.position = "none")+
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(),legend.title = element_blank())+
        ylab("NDIR CO2 (ppm)") +
        xlab("time (s)") +
        geom_point()
    })
    output$plots_pressure = renderPlot({
      ggplot(df, aes(x = time_s, y = pressure.psig,colour = file_name)) +
        #theme(legend.position = "none")+
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(),legend.title = element_blank())+
        ylab("pressure (psi)") +
        xlab("time (s)") +
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
    filename = 'TC_amount_calculation_result.zip',
    content = function(fname) {
      
      fs <- c("TC_amount_calculation_result.csv")
      write.csv(datasetInput()$df.amount, file = "TC_amount_calculation_result.csv")
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
  setBackgroundColor("#ecf0f5"),
 sidebarLayout(
    sidebarPanel(
      # Input: Select a file ----
      fileInput("fileUploaded", "Drag & Drop File(s)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      
      # Output: Download a file ----
      downloadButton("downloadData", "Calculate & Download"),
      
      # CSS style for the download button ----
      tags$style(type='text/css', "#downloadFile { width:100%; margin-top: 35px;}")),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset with different plots
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

