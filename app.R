library(shiny)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Sunset calc"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("TC calc", tabName = "first_app"),
      menuItem("OC calc", tabName = "second_app")
      # menuItem("TC calc", tabName = "third_app"),
      # menuItem("Help", tabName = "Help"),
      # menuItem("About", tabName = "About")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "first_app",
              source("tc_app.R", local = TRUE)$value
      ),
      tabItem(tabName = "second_app",
              source("oc_app.R", local = TRUE)$value
      )
      # tabItem(tabName = "Help",
      #         h2("Help"),
      #         p("how to use this app")
      # ),
      # tabItem(tabName = "About",
      #         h2("About"),
      #         p("created by Martin Rauber"),
      #         tags$a(href="https://martin-rauber.com", "martin-rauber.com")
      # )
    )
  )
)

server <- function(input,output,session){

  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("TC calc", tabName = "first_app"),
      menuItem("OC calc", tabName = "second_app")
    )
  })
  isolate({updateTabItems(session, "first_app", "second_app")})
}
shinyApp(ui,server)

# #### tests
# library(diffr)
# diffr("oc_app.R", "tc_app.R")
# diffr("OC_calc_shiny.R", "tc_calc_shiny.R")


