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
food_stats <- read_csv("food_stats.csv")
source("functions.R")



# Define UI for app that draws a histogram ----
ui <- navbarPage("Obesity and Demographics in the US",
              
            # Plot tab panel ----
               tabPanel("Plot",
                        # Sidebar layout with input and output definitions 
                        sidebarLayout(
                            
                            # Sidebar panel for inputs
                            sidebarPanel(
                                h3("Change over time"),
                                selectInput("state", "Choose a State", choices = national_stats %>%
                                                select(location_desc)),
                                selectInput("demo", "Choose a Variable", choices = c("Gender" = "gender",
                                                                                     "Race" = "race_ethnicity",
                                                                                     "Income" = "income")),
                                br(),
                                p("This plot shows the evolution of obesity rates in US states over the past few years. 
                                You may choose a state and variable to examine how this evolution changes amongst certain 
                                demographics. For example, the gender variable will let you observe how obesity rates 
                                  have evovled for women and men in the state of your choice. Explore the data as you wish!")
                            ),
                            # Main panel for displaying outputs 
                            mainPanel(
                                # Output: Histogram 
                                plotOutput("Plot")
                            )
                        )),
            
            # Maps tab panel ----
               tabPanel("Maps",
                        # Sidebar layout with input and output definitions 
                        sidebarLayout(# Sidebar panel for inputs 
                                      sidebarPanel(
                                          selectInput("abbv", "Choose a State (abbreviation)", 
                                                      choices = national_stats %>%
                                                          select(location_abbr)),
                                          selectInput("stores", "Choose a Variable", 
                                                      choices = c("Low access to stores (%)" = "pct_laccess_pop15",
                                                                  "Low Income, low store access (%)" = "pct_laccess_lowi15",
                                                                  "Grocery Stores/1,000 pop, 2014" = "grocpth14",
                                                                  "Convenience stores/1,000 pop, 2014" = "convspth14",
                                                                  "Farmers' markets/1,000 pop, 2016" = "fmrktpth16",
                                                                  "Farmers' markets that accept SNAP (%)" = "pct_fmrkt_snap16",
                                                                  "Farmers markets that sell fruits & vegetables (%)" = "pct_fmrkt_frveg16",
                                                                  "Diabetes rate in adults (%)" = "diabetes",
                                                                  "Obesity rate in adults (%)" = "obesity"))
                                      ),
                                      # Main panel for displaying outputs 
                                      mainPanel(
                                          # Output: Map of counties
                                          plotOutput("mapPlot")
                                      )
                        )),
               
            # Regression tab panel ----
               tabPanel("Regression",
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("state_2", "State (abbreviation)", 
                                            choices = national_stats %>%
                                                select(location_abbr)),
                                selectInput("xvar", "X variable",
                                            choices = c("Low access to stores (%)" = "pct_laccess_pop15",
                                                        "Low Income, low store access (%)" = "pct_laccess_lowi15",
                                                        "Grocery Stores/1,000 pop, 2014" = "grocpth14",
                                                        "Convenience stores/1,000 pop, 2014" = "convspth14",
                                                        "Farmers' markets/1,000 pop, 2016" = "fmrktpth16",
                                                        "Farmers' markets that accept SNAP (%)" = "pct_fmrkt_snap16",
                                                        "Farmers markets that sell fruits & vegetables (%)" = "pct_fmrkt_frveg16",
                                                        "Physical inactivity (%)" = "physical_activity")),
                                selectInput("yvar", "Y variable",
                                            choices = c("Obesity rate (%)" = "obesity",
                                                        "Diabetes rate (%)" = "diabetes"))
                                    
                                ),
                            # Main panel for displaying outputs 
                            mainPanel(
                                
                                tabsetPanel(type = "tabs",
                                            
                                            tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                                            tabPanel("Distribution", # Plots of distributions
                                                     fluidRow(
                                                         column(6, plotOutput("distribution1")),
                                                         column(6, plotOutput("distribution2")))
                                            ),
                                            tabPanel("Model Summary", verbatimTextOutput("summary")) # Regression output
                                )
                            )
                            )
                        ),
            # Data tab panel ----
               tabPanel("Data", 
                        # Data as datatable
                        DT::dataTableOutput('tbl')), 
            
            # About tab panel ----
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
    
    output$mapPlot <- renderPlot({
        
       m <- food_stats %>%
            filter(state == input$abbv) %>%
            ggplot(mapping = aes(long, lat, group = group, fill = get(input$stores))) +
            geom_polygon(color = "#ffffff", size = .25) +
            coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
           scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
            labs(title = "Distribution of variable by county",
                 fill = "Distribution") +
            theme(legend.title = element_text(),
                  legend.key.width = unit(.5, "in")) 
       
       print(m)
        
    })
    
    output$summary <- renderPrint({
        fit <- lm(get(input$xvar) ~ get(input$yvar), data = food_stats)
        summary(fit)
    })
    
    output$tbl = DT::renderDataTable({
        DT::datatable(food_stats, options = list(lengthChange = FALSE))
    })
    
    output$scatterplot <- renderPlot({
        f <- food_stats %>%
            filter(state == input$state_2) %>%
            ggplot(aes(get(input$xvar), get(input$yvar))) + geom_point() +
            geom_smooth(method = lm) +
            xlab("X Variable") +
            ylab("Y variable") +
            theme_classic() 
        
        print(f)
    })
    
    output$distribution1 <- renderPlot({
        food_stats %>%
            filter(state == input$state_2) %>%
            ggplot(aes(get(input$xvar))) + geom_histogram()
    }, height=300, width=300)
    
    output$distribution2 <- renderPlot({
        food_stats %>%
            filter(state == input$state_2) %>%
            ggplot(aes(get(input$yvar))) + geom_histogram()
    }, height=300, width=300)
}
    


# Run the application 
shinyApp(ui = ui, server = server)
