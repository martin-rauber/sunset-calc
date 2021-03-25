library("shinythemes")
library("shinyWidgets")
library("tidyverse")
library("zip")
#ui
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
#server
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
      df.result$TCcalculated <- df.result[,14]/df.result[,8]+df.result[,16]/df.result[,17]
      df.result$ECcorr <- df.result[,16]/(df.result[,3]*df.result[,17])
      df.result$OCcorr <- df.result[,14]/df.result[,8]-(df.result$ECcorr-df.result[,16]/df.result[,17])
      df.result <- cbind(df.result[,10],df.result[,15],df.result[,1:7],df.result[,11:14],df.result[,16],df.result$TCcalculated,df.result$OCcorr,df.result$ECcorr)
      colnames(df.result) <-  c("sample_name_OC","sample_name_TC","EC_yield_S1","EC_yield_S2","EC_yield_S3","charring_S1","charring_S2","charring_S3","charring_total","OC_amount_S1_ugC","OC_amount_S2_ugC", "OC_amount_S3_ugC","OC_amount_total_ugC","EC_amount_ugC", "TC_amount_calculated_ugCcm2", "OC_amount_corr_ugCcm2", "EC_amount_corr_ugCcm2")
      print(df.result)
      write.csv(df.result, file, row.names=FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)

