#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(data.table)
library(DT)
library(stringr)
library(tools)
library(dplyr)

mydata <- read.csv("hiv-aids-data.csv")

ui <- fluidPage(
  
  # Theme selector --------------------------------------------------
  shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("united"),
  
  # Application title -----------------------------------------------
  titlePanel("HIV & AIDS DIAGNOSES IN NYC"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      downloadButton("download1", "Download HIV/AIDS Data"),
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y",
                  label = "Y-axis",
                  choices = c("Total # of HIV Diagnoses" = "TOTAL_HIV_DIAGNOSES",
                              "HIV Diagnoses Per 100,000 Population" ="HIV_DIAGNOSES_PER_100K_POPULATION",
                              "Total # of concurrent HIV/AIDS Diagnoses" ="TOTAL_CONCURRENT_HIV_AIDS_DIAGNOSES",
                              "Total # of AIDS Diagnoses" ="TOTAL_NUMBER_OF_AIDS_DIAGNOSES",
                              "AIDS Diagnoses Per 100,000 Population" ="AIDS_DIAGNOSES_PER_100K_POPULATION"),
                  selected = "Total # of concurrent HIV/AIDS Diagnoses"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x",
                  label = "x-axis",
                  choices = c("Year" = "YEAR",
                              "Race" = "RACE",
                              "Age" = "AGE",
                              "Neighborhood" = "NEIGHBORHOOD",
                              "Total # of HIV Diagnoses" = "TOTAL_HIV_DIAGNOSES",
                              "HIV Diagnoses Per 100,000 Population" ="HIV_DIAGNOSES_PER_100K_POPULATION",
                              "Total # of concurrent HIV/AIDS Diagnoses" ="TOTAL_CONCURRENT_HIV_AIDS_DIAGNOSES",
                              "Total # of AIDS Diagnoses" ="TOTAL_NUMBER_OF_AIDS_DIAGNOSES",
                              "AIDS Diagnoses Per 100,000 Population" ="AIDS_DIAGNOSES_PER_100K_POPULATION"),
                  selected = "Year"),
      
      # Select variable for color -----------------------------------
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("Race" = "RACE",
                              "Year" = "YEAR",
                              "Age" = "AGE",
                              "Neighborhood" = "NEIGHBORHOOD"),
                  selected = "Race"),
      
      # Set alpha level ---------------------------------------------
      sliderInput(inputId = "alpha", 
                  label = "Alpha:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      # Set point size ----------------------------------------------
      sliderInput(inputId = "size", 
                  label = "Size:", 
                  min = 0, max = 5, 
                  value = 2),
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
    
    # Enter text for plot title ---------------------------------------------
    textInput(inputId = "plot_title", 
              label = "Plot title", 
              placeholder = "Enter text to be used as plot title"),
    
    # Horizontal line for visual separation -----------------------
    hr(),
    
    # Select which types of neighborhoods to plot ------------------------
    selectizeInput(inputId = "selected_type",
                       label = "Select neighborhoods(s):",
                       multiple = TRUE,
                       choices = c("High Bridge - Morrisania", "Greenwich Village - SoHo", "Long Island City - Astoria",
                                   "Upper Eastside", "West Queens","Chelsea - Clinton","Southeast Queens", "East New York",
                                   "East Flatbush - Flatbush", "Greenpoint", "Canarsie - Flatlands", "Port Richmond",
                                   "Williamsburg - Bushwick", "Jamaica", "Lower Manhattan","East Harlem","Flushing - Clearview",
                                   "Washington Heights - Inwood","Union Square - Lower Eastside","Upper Westside","Coney Island - Sheepshead Bay",
                                   "Gramercy Park - Murray Hill", "Bedford Stuyvesant - Crown Heights","Northeast Bronx"),
                       selected = c("Upper Eastside", "High Bridge - Morrisania", "Jamaica", "East Harlem", "Northeast Bronx", "Lower Manhattan")),
    
  
    
    # Select sample size ----------------------------------------------------
    numericInput(inputId = "n_samp", 
                 label = "Sample size:", 
                 min = 1, max = nrow(mydata), 
                 value = 50)
    

    ),
    
    # Output --------------------------------------------------------
    mainPanel(
  
      
      # Output: Scatterplot ----
      plotOutput(outputId = "scatterplot"),
      br(),
      
      # Output: Histogram ----
      plotOutput(outputId = "histplot"),
      br(),
      
      # Output: Line plot ----
      plotOutput(outputId = "lineplot"),
      br(),
      
      
      # Print number of obs plotted ---------------------------------
      uiOutput(outputId = "n"),
      br(), br(), 
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "datatable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types ------
  neighborhood_subset <- reactive({
    req(input$selected_type) 
    filter(mydata, NEIGHBORHOOD %in% input$selected_type)
  })
  
  # Update the maximum allowed n_samp for selected type movies ------
  observe({
    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(50, nrow(neighborhood_subset())),
                       max = nrow(neighborhood_subset())
    )
  })
  
  
  # Create new df that is n_samp obs from selected type neighborhood ------
  neighborhood_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(neighborhood_subset(), input$n_samp)
  })
  
  # Convert plot_title toTitleCase ----------------------------------
  pretty_plot_title <- reactive({ toTitleCase(input$plot_title) })
  
  # Create scatterplot object the plotOutput function is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = neighborhood_sample(), aes_string(x = input$x, y = input$y,
                                     color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           y = toTitleCase(str_replace_all(input$y, "_", " ")),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = pretty_plot_title()
           )
  })
  
  # Create histogram object
  output$histplot <- renderPlot({
    ggplot(data = neighborhood_sample(), aes(x = TOTAL_NUMBER_OF_AIDS_DIAGNOSES, color = RACE)) +
      geom_histogram(stat='bin', position = 'identity', fill = "lightblue", linetype="dashed")  +
      labs(x = "Total Number of AIDS Diagnoses",
           y = "Frequency",
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = pretty_plot_title()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  })
  
  
  # Create lineplot object
  output$lineplot <- renderPlot({
    yeardata <- neighborhood_sample() %>% 
      group_by(YEAR) %>%
      summarize(sum = sum(TOTAL_CONCURRENT_HIV_AIDS_DIAGNOSES))
    ggplot(data = yeardata, aes(x = YEAR, y = sum)) +
      geom_line(color = "red") + geom_point() + 
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = pretty_plot_title())
  })
  
  # Print number of cases plotted ----------------------------------
  output$n <- renderUI({
    types <- neighborhood_subset()$NEIGHBORHOOD %>% 
      factor(levels = input$selected_type) 
    counts <- table(types)
    
    HTML(paste("There are", counts, input$selected_type, "cases in this dataset. <br>"))
  })
  
  
  # Print data table if checked -------------------------------------
  output$datatable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = neighborhood_sample()[1:10,], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

