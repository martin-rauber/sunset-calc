library("shinythemes")
library("shinyWidgets")
library("tidyverse")
library("zip")

# server.R
server <- function(input, output) {
  
  datasetInput <- reactive({
    
    #run yields calc, oc-calc, and tc calc, then return the desired data
    source("yields_calc_shiny.R", local = TRUE)
    source("oc_calc_shiny.R", local = TRUE)
    source("tc_calc_shiny.R", local = TRUE)
    return(list(df.yield=df_raw, df.amount.oc=df.amount.oc, df.amount.tc=df.amount.tc))

  })
  
  output$downloadData <- downloadHandler(
    filename = 'oc-ec-yield-result.csv',
    content = function(file) {
      df.result <- cbind(datasetInput()$df.yield,datasetInput()$df.amount.oc,datasetInput()$df.amount.tc)
      df.result$TCcalculated <- df.result[,10]+df.result[,12]
      df.result$ECcorr <- df.result[,12]/df.result[,1]
      df.result$OCcorr <- df.result[,10]-(df.result$ECcorr-df.result[,12])
      df.result <- cbind(df.result[,6],df.result[,11],df.result[,1:4],df.result[,7:10],df.result[,12],df.result$TCcalculated,df.result$OCcorr,df.result$ECcorr)
      colnames(df.result) <-  c("sample name OC","sample name TC","EC yield","charring S1","charring S2","charring S3","S1 (ug C)","S2 (ug C)", "S3 (ug C)","total OC (ug C)","EC (ug C)", "TC calculated (ug C)", "corr. OC (ug C)", "corr. EC (ug C)")
      print(df.result)
      write.csv(df.result, file, row.names=FALSE)
      #remove temporary files form folder again
      ##path csv and pdf
      file.list.rem.pdf <- paste(getwd(), "/",list.files(getwd(), pattern = "*.pdf"), sep = "")
      #removal
      file.remove(file.list.rem.pdf)
    }
  )
  
  
}

# ui.R

ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  setBackgroundColor("#ecf0f5"),
  sidebarLayout(
    sidebarPanel(
      # Input: OC ----
      fileInput("fileUploadedOC", "Drag & Drop OC File(s)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Text input for OC removal step length
      textInput("inTextS1", "S1", value = 240),
      textInput("inTextS2", "S2", value = 120),
      textInput("inTextS3", "S3", value = 360),
      
      # Input: EC ----
      fileInput("fileUploadedTC", "Drag & Drop EC File(s)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      
      # Output: Download a file ----
      downloadButton("downloadData", "Calculate & Download"),
      
      # CSS style for the download button ----
      tags$style(type='text/css', "#downloadFile { width:100%; margin-top: 35px;}")),
    
    # Main panel for displaying outputs ----
    mainPanel()
  )
)
)

shinyApp(ui = ui, server = server)

