#
# This is a Shiny web application featuring data for Biodiversity in National Parks
#
# Author: H. David Shea
# 10 Aug 2021

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)
library(googlesheets4)

ui <-  dashboardPage(
    dashboardHeader(title = "Biodiversity"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(tabName = "species",
                     text = "Species")
            ,
            menuItem(tabName = "map",
                     text = "Map")
            ,
            menuItem(tabName = "journal",
                     text = "Journal")
        )
    ),
    dashboardBody(
        includeCSS("www/style.css"),
        tabItems(
            tabItem(
                tabName = "species",
                fluidPage(
                    fluidRow(
                        box(
                            width = 4,
                            status = "primary",
                            title = "Filters",
                            uiOutput("park_picker"),
                            div(class = "filter_box",
                                uiOutput("category_check_box"))
                        ),
                        box(
                            width = 4,
                            status = "primary",
                            title = "Overview",
                            uiOutput("vb_present"),
                            uiOutput("vb_concern"),
                            uiOutput("vb_endangered")
                        ),
                        box(
                            width = 4,
                            status = "primary",
                            title = "Category Count",
                            echarts4rOutput("category_count_bar", height = "33vh")
                        )
                    ),
                    fluidRow(
                        box(
                            width = 12,
                            status = "primary",
                            title = "Table",
                            DTOutput("species_table")
                        )
                    )
                )
            ),
            tabItem(
                tabName = "map",
                fillPage(
                    fluidRow(
                        box(
                            width = 12,
                            status = "primary",
                            chooseSliderSkin(skin = "Shiny", color = "green"),
                            uiOutput("acreage_slider")
                        )
                    ),
                    fluidRow(
                        box(
                            width = 12,
                            status = "primary",
                            leafletOutput("map", height = "75vh")
                        )
                    )
                )
            ),
            tabItem(
                tabName = "journal",
                fluidPage(
                    fluidRow(
                        box(
                            width = 12,
                            status = "primary",
                            fluidRow(
                                column(
                                    width = 1,
                                    actionBttn("journal_entry_add", icon = icon("plus"))
                                ),
                                column(
                                    width = 1,
                                    actionBttn("journal_entry_edit", icon = icon("edit"))
                                ),
                                column(
                                    width = 1,
                                    actionBttn("journal_entry_delete", icon = icon("minus-circle"))
                                ),
                                column(
                                    width = 9,
                                    ""
                                )
                            ),
                            DTOutput("journal")
                        )
                    )
                )
            )
        ),
    ),
    title = "Biodiversity in US National Parks"
)

server <- function(input, output) {
    # Server Shared Data
    parks_data <- read.csv("data/parks.csv")
    species_data <- read.csv("data/species.csv")

    cache_directory <- ".cache/"
    gs4_auth(email = "hdshea@comcast.net", cache = cache_directory)
    journal_url <- "https://docs.google.com/spreadsheets/d/18kMzWEk0oAy0THVrzh8LS6LLgJcVTCLgxTqT0g8Ko-o/edit#gid=0"
    journal_data <- range_read(ss = journal_url)

    ### Species Tab
    ## Filters Box
    # Park selection
    output$park_picker <- renderUI({
        #choices <- parks_data$Park.Name
        pickerInput(
            "park_picker",
            "Select Park",
            choices = parks_data$Park.Name
        )
    })

    # Species categories selection
    output$category_check_box <- renderUI({
        req(input$park_picker)

        categories <- species_data %>%
            filter(Park.Name == input$park_picker) %>%
            select(Category) %>%
            pull() %>%
            unique()
        categories <- categories[categories != ""]

        checkboxGroupButtons(
            "category_check_box",
            label = "Species Category",
            choices = categories,
            selected = categories,
            status = "default",
            size = "normal",
            direction = "vertical",
            width = "100%"
        )
    })

    # Reactive species data to filter Inputs
    reactive_species_data <- reactive({
        req(
            input$park_picker,
            input$category_check_box
        )

        species_data %>%
            filter(Park.Name == input$park_picker,
                   Category %in% input$category_check_box)
    })

    ## Overview Box
    # Present value box
    output$vb_present <- renderValueBox({
        present <- reactive_species_data() %>%
            filter(Occurrence == "Present") %>%
            nrow()

        valueBox(value = format(present, big.mark = ","), subtitle = "Present Species")
    })

    # Concern value box
    output$vb_concern <- renderValueBox({
        concern <- reactive_species_data() %>%
            filter(Conservation.Status == "Species of Concern") %>%
            nrow()

        valueBox(value = format(concern, big.mark = ","), subtitle = "Species of Concern")
    })

    # Endangered value box
    output$vb_endangered <- renderValueBox({
        endangered <- reactive_species_data() %>%
            filter(Conservation.Status == "Endangered Species") %>%
            nrow()

        valueBox(value = format(endangered, big.mark = ","), subtitle = "Endangered Species")
    })


    ## Category Count Box
    output$category_count_bar <- renderEcharts4r({
        reactive_species_data() %>%
            select(Species.ID, Category) %>%
            group_by(Category) %>%
            table() %>%
            as.data.frame() %>%
            group_by(Category) %>%
            summarise(count = sum(Freq)) %>%
            e_chart(Category) %>%
            e_bar(count, name = "Count Category", color = "green") %>%
            e_legend(show = F) %>%
            e_tooltip() %>%
            e_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
            e_grid(top = 10)
    })

    ## Table Box
    output$species_table <- renderDT({
        reactive_species_data() %>%
            select(
                "Common Name" = Common.Names,
                "Scientific Name" = Scientific.Name,
                Category,
                Occurrence,
                Nativeness,
                Abundance,
                "Conservation Status" = Conservation.Status
            ) %>%
            datatable(
                rownames = FALSE,
                options = list(paging = FALSE, scrollY = "30vh", scrollX = "100%")
            )
    })

    ### Map Tab
    ## Parks Acreage
    # Slider Box
    output$acreage_slider <- renderUI({
        max_acreage <- max(parks_data$Acres) #8323148
        min_acreage <- min(parks_data$Acres) #5550

        sliderInput(
            "acreageSlider",
            "Acreage Slider",
            min_acreage,
            max_acreage,
            value = c(min_acreage, max_acreage),
            post = " acres"
        )
    })

    # Reactive parks map data to acreage slider
    reactive_map_data <- reactive({
        req(input$acreageSlider)

        parks_data %>%
            filter(Acres >= input$acreageSlider[1],
                   Acres <= input$acreageSlider[2]
            )
    })

    ## Map Box
    output$map <- renderLeaflet({
        reactive_map_data() %>%
            mutate(
                popup = paste(
                    "<b>",Park.Name,"</b>",
                    "<br>",
                    State,
                    "<br>",
                    format(Acres, big.mark = ","), "acres"
                )
            ) %>%
            leaflet() %>%
            addProviderTiles(provider = "OpenTopoMap") %>%
            addCircleMarkers(
                lng = ~Longitude,
                lat = ~Latitude,
                radius = ~Acres/150000,
                label = ~Park.Name,
                popup = ~popup,
            ) %>%
            setView(lat = "44.00000", lng = "-120.50000", zoom = 3)
    })

    ### Journal Tab
    r <- reactiveValues()
    r$journal_data <- journal_data

    output$journal <- renderDT({
        datatable(
            r$journal_data,
            rownames = FALSE,
            selection = list(mode = 'single', target = 'row'),
            options = list(scrollY = "75vh", paging = FALSE)
        )
    })

    ## Add Entry Box
    # Add entry event
    observeEvent(input$journal_entry_add, {
        showModal(
            modalDialog(
                title = "New Entry",
                footer = fluidRow(
                            column(
                                width = 6,
                                actionBttn("journal_entry_save", icon = icon("save"))
                            ),
                            column(
                                width = 6,
                                actionBttn("journal_entry_dismiss", icon = icon("times"))
                            )
                        ),
                fluidRow(
                    column(
                        width = 6,
                        textInput("trip_name", "Trip Name", value = "Trip Name"),
                        textInput("trip_date", "Trip Date", value = "Trip Date")
                    ),
                    column(
                        width = 6,
                        textInput("park_name", "Park Name", value = "Park Name"),
                        textAreaInput("trip_notes", "Notes", value = "Notes...")
                    )
                )
            )
        )
    })

    # Dismiss event
    observeEvent(input$journal_entry_dismiss, {
        removeModal()
    })

    # Save event
    observeEvent(input$journal_entry_save, {
        new_entry <- tibble(
            "Trip Name" = c(input$trip_name),
            "Trip Date" = c(input$trip_date),
            "Park Name" = c(input$park_name),
            "Notes" = c(input$trip_notes)
        )

        sheet_append(ss = journal_url, new_entry)
        showNotification("Entry added", type = "message")
        r$journal_data <- r$journal_data %>% rbind(new_entry)
        removeModal()
    })

    # Edit entry
    observeEvent(input$journal_entry_edit, {
        s <- input$journal_rows_selected

        if(length(s) == 0) {
            showModal(
                modalDialog(
                    title = "Edit Entry",
                    easyClose = TRUE,
                    footer = NULL,
                    fluidRow(
                        box(
                            width = 12,
                            status = "primary",
                            "Please select a record from the table to edit"
                        )
                    )
                )
            )
        } else {
            showModal(
                modalDialog(
                    title = "Edit Entry",
                    footer = fluidRow(
                        column(
                            width = 6,
                            actionBttn("journal_edit_save", icon = icon("save"))
                        ),
                        column(
                            width = 6,
                            actionBttn("journal_edit_dismiss", icon = icon("times"))
                        )
                    ),
                    fluidRow(
                        column(
                            width = 6,
                            textInput("edit_trip_name", "Trip Name", value = r$journal_data[s, 1]),
                            textInput("edit_trip_date", "Trip Date", value = r$journal_data[s, 2])
                        ),
                        column(
                            width = 6,
                            textInput("edit_park_name", "Park Name", value = r$journal_data[s, 3]),
                            textAreaInput("edit_trip_notes", "Notes", value = r$journal_data[s, 4])
                        )
                    )
                )
            )
        }
    })

    # Journal edit dismiss
    observeEvent(input$journal_edit_dismiss, {
        removeModal()
    })

    # Journal edit save
    observeEvent(input$journal_edit_save, {
        s <- input$journal_rows_selected

        r$journal_data[s, 1] <- input$edit_trip_name
        r$journal_data[s, 2] <- input$edit_trip_date
        r$journal_data[s, 3] <- input$edit_park_name
        r$journal_data[s, 4] <- input$edit_trip_notes

        sheet_write(data = r$journal_data, ss = journal_url, sheet = 1)

        showNotification("Edit applied", type = "message")

        removeModal()
    })

    # Delete entry
    observeEvent(input$journal_entry_delete, {
        s <- input$journal_rows_selected

        if(length(s) == 0) {
            showModal(
                modalDialog(
                    title = "Delete Entry",
                    easyClose = TRUE,
                    footer = NULL,
                    fluidRow(
                        box(
                            width = 12,
                            status = "primary",
                            "Please select a record from the table to delete."
                        )
                    )
                )
            )
        } else {
            trip_name <- r$journal_data[s, 1]
            showModal(
                modalDialog(
                    title = "Delete Entry",
                    footer = fluidRow(
                        column(
                            width = 6,
                            actionBttn("journal_delete_yes", icon = icon("check-circle"))
                        ),
                        column(
                            width = 6,
                            actionBttn("journal_delete_no", icon = icon("times"))
                        )
                    ),
                    fluidRow(
                        box(
                            width = 12,
                            status = "primary",
                            str_c("OK to delete the entry for '", trip_name, "'?", sep = "")
                        )
                    )
                )
            )
        }
    })

    # Delete entry no
    observeEvent(input$journal_delete_no, {
        removeModal()
    })

    # Save event
    observeEvent(input$journal_delete_yes, {
        r$journal_data <- r$journal_data %>%
            filter(row_number() != input$journal_rows_selected)

        sheet_write(data = r$journal_data, ss = journal_url, sheet = 1)

        showNotification("Selection deleted", type = "message")

        removeModal()
    })
}

shinyApp(ui = ui, server = server)
