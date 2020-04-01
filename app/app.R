#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(tidyverse)
library(dplyr)
national_stats <- read_csv("national_stats.csv")
source("functions.R")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Obesity Rates"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            selectInput("state", "Choose a State", choices = c("California", "Florida", 
                                                               "New York", "Texas")),
            br()
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Histogram ----
            plotOutput("distPlot")
            
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    

    output$distPlot <- renderPlot({
        
        x <- get_female_obesity(input$state) 
        y <- get_male_obesity(input$state) 
        xy <- rbind(x, y)
            
        ggplot(xy, aes(year_end, obesity, color = gender)) + geom_line()+
            facet_wrap(~gender) +
            labs(title = "Evolution of obesity rates in population by gender") +
            ylab("Percentage") +
            xlab("Year") +
            theme_classic()
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
