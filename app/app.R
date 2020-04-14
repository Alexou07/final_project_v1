#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(tidyverse)
library(markdown)


national_stats <- read_csv("national_stats.csv")
source("functions.R")



# Define UI for app that draws a histogram ----
ui <- navbarPage("Obesity and Demographics in the US",
               
               tabPanel("Plot",
                        # Sidebar layout with input and output definitions ----
                        sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                                selectInput("state", "Choose a State", choices = national_stats %>%
                                                select(location_desc)),
                                selectInput("demo", "Choose a Variable", choices = c("Gender" = "gender",
                                                                                     "Race" = "race_ethnicity",
                                                                                     "Income" = "income"))
                            ),
                            # Main panel for displaying outputs ----
                            mainPanel(
                                # Output: Histogram ----
                                plotOutput("Plot")
                            )
                        )),
               
               tabPanel("About")
            )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    
    x <- reactive({
        bind_rows(get_female_obesity(input$state), get_male_obesity(input$state),
            get_race_obesity(input$state), get_income_obesity(input$state))
    })
    
    
    output$Plot <- renderPlot({
        req(nrow(x()) > 0)
        
        p <- x() %>%
            select(year_end, obesity, input$demo) %>%
            ggplot(data = subset(., !is.na(get(input$demo))), 
                    mapping = aes(year_end, obesity, color = get(input$demo))) + 
            geom_line() +
            facet_wrap(~get(input$demo)) +
            labs(title = "Evolution of obesity rates in state population by variable",
                color = "Variable") +
            ylab("Percentage") +
            xlab("Year") +
            theme_classic()
        
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
