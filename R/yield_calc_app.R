library("shinythemes")
library("shinyalert")
#ui
ui <- shinyUI(fluidPage(
  setBackgroundColor("#ecf0f5"),
  titlePanel(h2("Yield calc",
                h5("Upload the OC Swiss3S protocol raw file(s) and click 'Calculate & Download'"))),
  sidebarLayout(
    sidebarPanel(
      # Spinner to notify user when task takes more than 300ms
      add_busy_spinner(spin = "fading-circle", color = "#3C8DBC", timeout = 300, position = "top-right"),
      # Input: Select a file ----
      fileInput("fileUploadedOC", "Drag & Drop File(s)",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Shinyalert for Input2 ----
      useShinyalert(),
      
      # Input2: Select a file ----
      fileInput("fileUploadedCooldown", "OPTION: Drag & Drop TC, EC, or Swiss_4S file for cooldown",
                multiple = FALSE,
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
#server
server <- function(input, output) {
  
  datasetInput <<- reactive({
    source("yields_calc_shiny.R", local = TRUE)
    return(list(df1=df_raw, df2=df, df3=stat, df4=df_mean,tempPdf1=tempPdf1,tempPdf2=tempPdf2))
  })
  
  output$downloadData <- downloadHandler(
    filename = paste('yield-calc-result_',system("git rev-parse --short HEAD", intern=TRUE),".zip",sep=""),
    content = function(fname) {
      
      fs <- c("raw-result.csv", "clean-result.csv", "stat-result.csv", "mean-result.csv","yield-calc-summary.pdf")
      write.csv(datasetInput()$df1, file = "raw-result.csv", row.names=FALSE)
      write.csv(datasetInput()$df2, file = "clean-result.csv", row.names=FALSE)
      write.csv(datasetInput()$df3, file = "stat-result.csv")
      write.csv(datasetInput()$df4, file = "mean-result.csv", row.names=FALSE)
      pdf_combine(c(datasetInput()$tempPdf1, datasetInput()$tempPdf2), output = "yield-calc-summary.pdf")
      print (fs)
      
      zip(zipfile=fname, files=fs)
      
      #remove temporary files form folder again
      ##path csv and pdf
      file.list.rem.csv <- paste(getwd(), "/",list.files(getwd(), pattern = "*result.csv"), sep = "")
      file.list.rem.pdf <- paste(getwd(), "/",list.files(getwd(), pattern = "*.pdf"), sep = "")
      #removal
      file.remove(file.list.rem.csv)
      file.remove(file.list.rem.pdf)
      
    },
    contentType = "application/zip"
  )
  
  #observe input
  observe({
    if(length(c(input$fileUploadedCooldown$name))>1) {
      shinyalert(
        title = "Oops!",
        text = "Please upload only one file",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#3c8dbc",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }else{
      return(NULL)
    }
  })
  
}


shinyApp(ui = ui, server = server)

