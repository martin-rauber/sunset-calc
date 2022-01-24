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
  titlePanel(h2("File splitter",
                h5("Upload a Sunset raw file with multiple runs and click 'Split!' to split into single txt files"))),
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
      downloadButton("downloadData", "Split!"),
      
      # CSS style for the download button ----
      tags$style(type='text/css', "#downloadFile { width:100%; margin-top: 35px;}"),
      
    ),
    
    
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("File information", DT::dataTableOutput("infotable1"))
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
    filename = paste('file_splitter_result_',system("git rev-parse --short HEAD", intern=TRUE),".zip",sep=""),
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
  
  #observe input for table output
  observe({
    data = input$fileUploaded
    if(is.null(data))
      return(NULL)

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
    #--------------------------get info---------------------------------------
    # subset to get the sample name
    df.samplename <- df[(df.rowindex.newfile+1),1]
    # subset to get the parameter file name
    df.parname <- df[(df.rowindex.newfile+3),1]
    # subset to get the name of the analyst
    df.analyst <- df[(df.rowindex.newfile+13),1]
    # subset to get the CalConst
    df.CalConst <- df[(df.rowindex.newfile+17),1]
    # subset to get the filter area
    df.filterarea <- df[(df.rowindex.newfile+19),1]
    #--------------------------combine to df for table-----------------------
    #combine to df
    df.table <- bind_cols(df.samplename,df.parname,df.analyst,df.CalConst,df.filterarea)
    colnames(df.table) <- c("Sampe name", "Parameter file", "Analyst", "CalConst", "Area (sq cm)")
    print(df.table)
    #render table for output
    output$infotable1 <- DT::renderDataTable({
      DT::datatable( data = df.table,
                     extensions = 'Buttons', 
                     options = list(
                       dom = 'Bfrtip',
                       buttons = 
                         list('copy', 'print', list(
                           extend = 'collection',
                           buttons = c('csv', 'excel', 'pdf'),
                           text = 'Download'
                         )),
                       pageLength = -1
                     )
                  )
    })
  })
}



shinyApp(ui = ui, server = server)

