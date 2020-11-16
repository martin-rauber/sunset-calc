library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(skin="blue",
  dashboardHeader(title = "Sunset calc"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("TC calc", tabName = "first_app"),
      menuItem("OC calc", tabName = "second_app"),
      menuItem("Readme", tabName = "Readme")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "first_app",
              h2("TC calc"),
              p("Upload the TC protocol .txt raw file(s) and click 'Calculate & Download'"),
              source("tc_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "second_app",
              h2("OC calc"),
              p("Upload the Swiss_3S protocol txt raw file(s) and click 'Calculate & Download'"),
              source("oc_app.R", local = TRUE)$value
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


