# Author: H. David Shea
# 6 Aug 2021

library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)

# UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Week Two"),
    dashboardSidebar(
        actionBttn(
            "actionBttn",
            label = "Button",
            style = "pill",
            color = "default",
            size = "lg",
            block = F,
            no_outline = TRUE
        )
    ),
    dashboardBody(
        fluidRow(
            box(
                width = 6,
                selectInput("select",
                            label = h3("Select box"),
                            choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                            selected = 1),
                hr(),
                fluidRow(column(3, verbatimTextOutput("select_value"))),
                br(),
                sliderInput(
                    "slider1",
                    label = h3("Slider"),
                    min = 0,
                    max = 100,
                    value = 50
                ),
                hr(),
                fluidRow(column(3, verbatimTextOutput("slider_value"))),
                br(),
                textInput("text", label = h3("Text input"), value = "Enter text..."),
                hr(),
                fluidRow(column(3, verbatimTextOutput("text_value")))
            ),
             tabBox(
                 width = 6,
                 tabPanel(title = "Scatter Plot",
                          echarts4rOutput("scatter")),
                 tabPanel(title = "Bar Chart",
                          echarts4rOutput("bar"))
             )
        ),
        fluidRow(box(width = 6,
                     DTOutput("datatable")),
                 box(width = 6,
                     leafletOutput("map")))
    ),
    title = "Dashboard example"
)

# Server ----
server <- function(input, output) {
    output$datatable <- renderDT({
        datatable(iris)
    })

    df <- data.frame(
        x = seq(50),
        y = rnorm(50, 10, 3),
        z = rnorm(50, 11, 2),
        w = rnorm(50, 9, 2)
    )

    output$scatter <- renderEcharts4r({
        df %>%
            e_charts(x) %>%
            e_scatter(y, z) %>%
            e_visual_map(z, scale = e_scale) %>% # scale color
            e_legend(FALSE) # hide legend
    })

    output$bar <- renderEcharts4r({
        df %>%
            e_charts(x) %>%
            e_bar(y, name = "Serie 1") %>%
            e_step(z, name = "Serie 2") %>%
            e_title("Bar and step charts")
    })

    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(lng = 174.768,
                       lat = -36.852,
                       popup = "The birthplace of R")
    })

    output$select_value <- renderPrint({ input$select })

    output$slider_value <- renderPrint({ input$slider1 })

    output$text_value <- renderPrint({ input$text })

    observeEvent(input$actionBttn, {
        showModal(modalDialog(
            title = "Action Button Clicked",
            easyClose = T,
            footer = NULL,
            fluidRow(box(
                width = 12,
                status = "primary",
                "Hello World!"
            ))
        ))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
