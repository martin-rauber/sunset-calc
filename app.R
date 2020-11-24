library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
#ggplot theme
theme_set(theme_classic())

###
ui <- dashboardPage(skin="blue",
    dashboardHeader(title = "Sunset calc"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("TC calc", tabName = "tc_calc"),
      menuItem("OC calc", tabName = "oc_calc"),
      menuItem("File splitter", tabName = "file_splitter"),
      menuItem("Readme", tabName = "Readme")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tc_calc",
              h2("TC calc"),
              p("Upload the TC protocol raw file(s) and click 'Calculate & Download'"),
              source("tc_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "oc_calc",
              h2("OC calc"),
              p("Upload the Swiss_3S protocol raw file(s) and click 'Calculate & Download'"),
              source("oc_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "file_splitter",
              h2("File splitter"),
              p("Upload a Sunset raw file with multiple runs and click 'Split!' to split into single txt files"),
              source("file_splitter_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "Readme",
              includeMarkdown("readme.md")
      )
    )
  )
)

server <- function(input,output,session){
}
shinyApp(ui,server)


