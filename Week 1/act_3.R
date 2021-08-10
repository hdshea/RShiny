# Author: H. David Shea
# 6 Aug 2021

# Dependencies ----
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
# UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Week 1 Activity 3"),
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
                    title = "Box 1-1",
                    status = "primary"
                ),
                box(
                    width = 6,
                    title = "Box 1-2",
                    status = "primary"
                )
            ),
            fluidRow(
                box(
                    width = 3,
                    title = "Box 2-1",
                    status = "primary"
                ),
                box(
                    width = 3,
                    title = "Box 2-2",
                    status = "primary"
                ),
                box(
                    width = 3,
                    title = "Box 2-3",
                    status = "primary"
                ),
                box(
                    width = 3,
                    title = "Box 2-4",
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
