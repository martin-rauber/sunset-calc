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
      menuItem("Swiss 4S calc", tabName = "swiss4s_calc"),
      menuItem("OC/EC yield", tabName = "oc_ec_yield"),
      menuItem("yield calc", tabName = "yield_calc"),
      menuItem("File splitter", tabName = "file_splitter"),
      menuItem("Readme", tabName = "Readme")
    )
  ),

  dashboardBody(
    #tags$style(type = "text/css", ".tab {height: calc(100vh - 80px) !important;}"),
    tabItems(
      tabItem(tabName = "tc_calc",
              h2("TC calc"),
              p("Upload the TC protocol raw file(s) and click 'Calculate & Download'"),
              HTML('<style>.shiny-frame {width: 100%; height: calc(100vh - 80px)}</style>'),
              source("tc_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "oc_calc",
              h2("OC calc"),
              p("Upload the Swiss_3S protocol raw file(s) and click 'Calculate & Download'"),
              HTML('<style>.shiny-frame {width: 100%; height: calc(100vh - 80px)}</style>'),
              source("oc_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "swiss4s_calc",
              h2("Swiss 4S calc"),
              p("Upload the Swiss_4S protocol raw file(s) and click 'Calculate & Download'"),
              HTML('<style>.shiny-frame {width: 100%; height: calc(100vh - 80px)}</style>'),
              source("swiss4s_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "oc_ec_yield",
              h2("OC/EC yield"),
              p("Upload the OC Swiss3S and EC (TC) protocol raw file(s) and click 'Calculate & Download'"),
              HTML('<style>.shiny-frame {width: 100%; height: calc(100vh - 80px)}</style>'),
              source("oc_ec_yield_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "yield_calc",
              h2("yield calc"),
              p("Upload the OC Swiss3S protocol raw file(s) and click 'Calculate & Download'"),
              HTML('<style>.shiny-frame {width: 100%; height: calc(100vh - 80px)}</style>'),
              source("yield_calc_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "file_splitter",
              h2("File splitter"),
              p("Upload a Sunset raw file with multiple runs and click 'Split!' to split into single txt files"),
              HTML('<style>.shiny-frame {width: 100%; height: calc(100vh - 80px)}</style>'),
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


