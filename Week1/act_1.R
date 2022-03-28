# Author: H. David Shea
# 6 Aug 2021

# Dependencies ----
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
# UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Week 1 Activity 1"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            tabName = "boxes",
            text = "Boxes",
            icon = icon("chart-bar")
        )
    )),
    dashboardBody(tabItems(
        tabItem(
            tabName = "boxes",
            fluidRow(
                box(
                    width = 6,
                    title = "Box 1",
                    status = "primary"
                ),
                box(
                    width = 6,
                    title = "Box 2",
                    status = "primary"
                )
            )
        )
    )),
    title = "Dashboard example"
)

# Define server logic required to draw a histogram
# Server ----
server <- function(input, output) {

}

# Run the application
shinyApp(ui = ui, server = server)
