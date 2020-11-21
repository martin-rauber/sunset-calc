library("shinythemes")
library("shinyWidgets")
library("tidyverse")
library("readr")
library("stringr")
library("zip")

# server.R
server <- function(input, output) {
  
  datasetInput <- reactive({
  })
  
  output$downloadData <- downloadHandler(
    filename = 'file_splitter_result.zip',
    content = function(fname) {
      #--------------------------import-----------------------------------------
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
      #--------------------------split------------------------------------------
      #split df and save as txt
      for (i in seq(1:length(df.rowindex.newfile))) {
        write.csv(df[df.rowindex.newfile[i]:df.rowindex.endfile[i],], row.names=FALSE, quote=FALSE, file = paste0(i,"-", filename.short,"-", df.samplename[i,],"-split",".txt"))
      }
      #--------------------------output------------------------------------------
      #create a list of txt files
      file.list <- paste(list.files(getwd(), pattern = "*-split*.txt"), sep = "")
      #make zip file
      zip(zipfile=fname, files=file.list)
      #remove the created files
      file.remove(file.list)

    },
    contentType = "application/zip"
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

