library("shinythemes")
library("shinyWidgets")
library("tidyverse")
library("readr")
library("stringr")
library("zip")
#ui
ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  setBackgroundColor("#ecf0f5"),
  titlePanel(h2("4S/3S Converter",
                h5("Upload the Swiss_4S protocol raw file(s) and click 'Convert!'"))),
  sidebarLayout(
    sidebarPanel(
      # Spinner to notify user when task takes more than 300ms
      add_busy_spinner(spin = "fading-circle", color = "#3C8DBC", timeout = 300, position = "top-right"),
      # Input: Select a file ----
      fileInput("fileUploaded", "Drag & Drop File(s)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      
      # Output: Download a file ----
      downloadButton("downloadData", "Convert!"),
      
      # CSS style for the download button ----
      tags$style(type='text/css', "#downloadFile { width:100%; margin-top: 35px;}"),
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        id = 'dataset',
      )
    )
  ),
  
)
)
#server
server <- function(input, output) {
  
  datasetInput <- reactive({
  })
  
  output$downloadData <- downloadHandler(
    filename = paste('converter_result_',system("git rev-parse --short HEAD", intern=TRUE),".zip",sep=""),
    content = function(fname) {
      #--------------------------import------------------------------------------
      #get the filepath
      filepaths <- input$fileUploaded$datapath
      #get the filename
      filename <- input$fileUploaded$name
      #--------------------------convert-----------------------------------------
      #convert file and save as txt
      j <- 0
      for (i in filepaths){
        j <- j+1
        input_data4S <- read.csv(file = i, sep = ",", header = T )
        #distinguish samples that were processed with 'File splitter' and samples that were not
        if (input_data4S[1,1] != 'Sample'){
          input_data4S <- read.csv(file = i, skip=28, sep = ",", header = T )
          df.colnames <- colnames(input_data4S, do.NULL = TRUE, prefix = "col")
          input_data4S <- read_csv(file = i, col_names =  df.colnames)
        }
        #remove last step of 4S protocol
        ConvertedTo3SOutput <- head(input_data4S,-340)
        write.csv(ConvertedTo3SOutput, row.names=FALSE, quote=FALSE, file = paste0(str_sub(filename[j], end=-5), "-converted",".txt"))
      }
      #--------------------------output------------------------------------------
      #create a list of txt files
      file.list <- paste(list.files(getwd(), pattern = "*-converted*.txt"), sep = "")
      #make zip file
      zip(zipfile=fname, files=file.list)
      #remove the created files
      file.remove(file.list)
      
    },
    contentType = "application/zip"
  )

  #observe input for table output
  observe({
    data = input$fileUploaded
    if(is.null(data))
      return(NULL)

  })
}



shinyApp(ui = ui, server = server)