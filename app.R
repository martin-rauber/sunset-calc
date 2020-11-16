library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Sunset calc"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("TC calc", tabName = "first_app"),
      menuItem("OC calc", tabName = "second_app"),
      menuItem("Help", tabName = "Help"),
      menuItem("About", tabName = "About")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "first_app",
              source("tc_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "second_app",
              source("oc_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "Help",
              h2("Help"),
              p("how to use this app")
      ),
      tabItem(tabName = "About",
              h2("About"),
              p("created by Martin Rauber"),
              tags$a(href="https://martin-rauber.com", "martin-rauber.com")
      )
    )
  )
)

server <- function(input,output,server){}

shinyApp(ui,server)
