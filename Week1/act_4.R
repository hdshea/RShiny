# Author: H. David Shea
# 6 Aug 2021

# Dependencies ----
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
# UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Week 1 Activity 4"),
    dashboardSidebar(sidebarMenu(
        menuItem(
            tabName = "box2",
            text = "Boxes 1x2",
            icon = icon("battery-quarter")
        ),
        menuItem(
            tabName = "box22",
            text = "Boxes 2x2",
            icon = icon("battery-half")
        ),
        menuItem(
            tabName = "box24",
            text = "Boxes 2+4",
            icon = icon("battery-three-quarters")
        )
    )),
    dashboardBody(tabItems(
        tabItem(
            tabName = "box2",
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
        ),
        tabItem(
            tabName = "box22",
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
                    width = 6,
                    title = "Box 2-1",
                    status = "primary"
                ),
                box(
                    width = 6,
                    title = "Box 2-2",
                    status = "primary"
                )
            )
        ),
        tabItem(
            tabName = "box24",
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
