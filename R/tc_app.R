library("shinythemes")
#ui
ui <- shinyUI(fluidPage(
  setBackgroundColor("#ecf0f5"),
  sidebarLayout(
    sidebarPanel(
      # Input: Select a file ----
      fileInput("fileUploadedTC", "Drag & Drop File(s)",
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
    source("tc_calc_shiny.R", local = TRUE)
    return(list(df.amount.tc=df.amount.tc)) 
  })
  
  #observe input for plot
  observe({
    data = input$fileUploadedTC
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
        ylab(bquote(paste("NDIR CO"["2"]*" (ppm)")))+
        xlab("time (s)") +
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
    output$plots_laser = renderPlot({
      ggplot(df, aes(x = time_s, y = laserTrans,colour = file_name)) +
        theme(legend.position="right", legend.box="horizontal", legend.margin=margin(), legend.title = element_blank())+
        ylab("transmission (A.U.)") +
        xlab("time (s)") +
        geom_point()
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = 'TC_amount_calculation_result.csv',
    content = function(file) {
      write.csv(datasetInput()$df.amount, file, row.names=FALSE)
    }
  )
  
}



shinyApp(ui = ui, server = server)

