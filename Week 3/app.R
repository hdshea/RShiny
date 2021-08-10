# Author: H. David Shea
# 6 Aug 2021

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)
library(googlesheets4)
library(scales)

# Define UI for template application
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "Property Tracker"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        includeCSS("www/style.css"),
        chooseSliderSkin( skin = "Shiny", color = "black"),
        fillPage(fluidRow(
            box(
                width = 6,
                height = "40vh",
                title = "Inputs",
                status = "primary",
                column(
                    width = 6,
                    uiOutput("property_type"),
                    uiOutput("property_price"),
                    uiOutput("bedroom_count")
                ),
                column(
                    width = 6,
                    HTML("<b>Property Types Selected:</b>"),
                    verbatimTextOutput("type_value"),
                    br(),
                    HTML("<b>Maximum Property Price:</b>"),
                    verbatimTextOutput("price_value"),
                    br(),
                    HTML("<b>Minimum Bedroom Count:</b>"),
                    verbatimTextOutput("bedroom_value")
                )
            ),
            tabBox(
                width = 6,
                height = "40vh",
                tabPanel(title = NULL,
                         icon = icon("dollar"),
                         echarts4rOutput("scatter", height = "30vh")),
                tabPanel(title = NULL,
                         icon = icon("bed"),
                         echarts4rOutput("bar", height = "30vh"))
            )
        ),
        fluidRow(box(width = 6,
                     height = "45vh",
                     status = "primary",
                     DTOutput("property_data")),
                 box(width = 6,
                     height = "45vh",
                     status = "primary",
                     leafletOutput("map", height = "43vh"))
        ))
    ),
    title = "Property Tracker"
)

# Server logic
server <- function(input, output) {
    cache_directory <- ".cache/" # can add to config file
    gs4_auth(email = "hdshea@comcast.net", cache = cache_directory)
    property_data <- range_read(ss = "https://docs.google.com/spreadsheets/d/1OgPF8K-rBBlz5s89nvhYRMXX6gjB_8NXgPYosPtEXfE/edit#gid=0")

    output$property_type <- renderUI({
        choices <- unique(property_data$property_type)

        selectInput(
            "property_type",
            label = "Property Type",
            choices = choices,
            selected = choices,
            multiple = TRUE
        )
    })

    output$property_price <- renderUI({
        min <- min(property_data$property_price)
        max <- max(property_data$property_price)
        sliderInput(
            "property_price",
            label = "Property Price",
            min = min,
            max = max,
            value = max
        )
    })

    output$bedroom_count <- renderUI({
        min <- min(property_data$bedroom_count)
        max <- max(property_data$bedroom_count)
        numericInput(
            "bedroom_count",
            label = "Bedroom Count",
            min = min,
            max = max,
            value = min)
    })

    reactive_property_data <- reactive({
        req(
            input$property_type,
            input$property_price,
            input$bedroom_count
        )
        property_data %>%
            filter(
                property_type %in% input$property_type,
                property_price <= input$property_price,
                bedroom_count >= input$bedroom_count
            )

    })

    output$property_data <- renderDT({
        datatable(
            reactive_property_data(),
            rownames = FALSE,
            colnames = c("Property ID", "Property Type", "Bedroom Count",
                         "Property Area", "Area Unit", "Property Price",
                         "Latitude", "Longitude", "Address"),
            options = list(
                scrollY = "25vh",
                scrollX = "100%",
                autoWidth = TRUE,
                columnDefs = list(list(width = '250px', targets = c(8)))
            )
        ) %>%
            formatCurrency(columns = c(6)) %>%
            formatRound(columns = c(7, 8), digits = 6)
    })

    output$scatter <- renderEcharts4r({
        reactive_property_data() %>%
            e_charts(property_id) %>%
            e_scatter(property_price, name = "Property Price", symbol_size = 15, color = "black") %>%
            e_axis_labels(y = "Price", x = "ID") %>%
            e_legend(show = FALSE) %>%
            e_labels(
                position = "top",
                color = "#111",
                # Custom JS formatter
                formatter = htmlwidgets::JS(
                    "function(params){
                        return('$' + parseInt(params.value[1]/1000000) + 'M')
                    }")) %>%
            e_tooltip(
                position = "right",
                # Custom JS formatter
                formatter = htmlwidgets::JS(
                    "function(params){
                        var formatter = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD'});

                        return( 'Property ID: ' + params.value[0]+ '<br/>' + 'Property Price: ' + formatter.format(params.value[1]/ 1000000)+ 'M')
                    }")
            ) %>%
            e_y_axis(formatter = e_axis_formatter(style = "currency")) %>%
            e_x_axis(axisLabel = list(interval = 1))
    })

    output$bar <- renderEcharts4r({
        reactive_property_data() %>%
            e_charts(property_id) %>%
            e_bar(bedroom_count, color = "black", barWidth = 10) %>%
            e_legend(show = FALSE) %>%
            e_tooltip(
                position = "right",
                # Custom JS formatter
                formatter = htmlwidgets::JS(
                    "function(params){
                    var formatter = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD'});

                    return( 'Property ID: ' + params.value[0]+ '<br/>' + 'Bedroom Count: ' + params.value[1])
                    }")
            ) %>%
            e_axis_labels(y = "Bedroom Count", x = "ID")
    })

    output$map <- renderLeaflet({
        reactive_property_data() %>%
            mutate(
                popup = paste( # Mutate a column for popup text
                    "<center><b>Address</b><br>",address,"</center>"
                )
            ) %>%
            leaflet() %>%
            addProviderTiles(provider = "Esri.WorldImagery") %>% # satallite imagery provider tile
            addMarkers(lng = ~long,
                       lat = ~lat,
                       label = ~property_id,
                       popup = ~popup)
    })

    # Filter outputs
    output$type_value <- renderPrint({ input$property_type })
    output$price_value <- renderPrint({ dollar(input$property_price) })
    output$bedroom_value <- renderPrint({ input$bedroom_count })

    # observeEvent(input$actionBttn, {
    #     showModal(modalDialog(
    #         title = "Action Button Clicked",
    #         easyClose = T,
    #         footer = NULL,
    #         fluidRow(box(
    #             width = 12,
    #             status = "primary",
    #             "Hello World!"
    #         ))
    #     ))
    # })
}

# Run the application
shinyApp(ui = ui, server = server)
