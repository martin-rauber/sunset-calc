# server.R
server <- function(input, output) {
  
  datasetInput <- reactive({
    source("oc_calc_shiny.R", local = TRUE)
    return(list(df.amount=df.amount)) 
  })
  
  output$downloadData <- downloadHandler(
    filename = 'OC_amount_calculation_result.zip',
    content = function(fname) {
      
      fs <- c("OC_amount_calculation_result.csv")
      write.csv(datasetInput()$df.amount, file = "OC_amount_calculation_result.csv")
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

