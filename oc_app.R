# server.R
server <- function(input, output) {
  
  datasetInput <<- reactive({
    source("OC_calc_shiny.R", local = TRUE)
    return(list(df.amount=df.amount)) 
  })
  
  output$downloadData <- downloadHandler(
    filename = 'amount_calculation_result.zip',
    content = function(fname) {
      
      fs <- c("amount_calculation_result.csv")
      write.csv(datasetInput()$df.amount, file = "amount_calculation_result.csv")
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
  #img(src = "oc-calc-logo.png", height = 72, width = 72),
  # HTML('<img src="oc-calc-logo.png", height="72px"    
  #         style="float:right"/>','<p style="color:black"></p>'),
  setBackgroundColor("#ecf0f5"),
  titlePanel("Sunset integration for the Swiss3S protocol"),
  # App subtitle
  h4(HTML("Upload the Sunset txt raw file(s) and click 'Calculate & Download'")),
  h5(HTML("You will get a file containing the amount of S1, S2, and S3 as well as the total amount of Swiss3S (TC)")),
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
