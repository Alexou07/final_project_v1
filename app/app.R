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
library(shinythemes)
library(tidycensus)
library(urbnmapr)
library(scales)
library(janitor)


national_stats <- read_csv("national_stats.csv")
food_stats <- read_csv("food_stats.csv")
source("functions.R")



# Define UI for app that draws a histogram ----
ui <- navbarPage("Obesity and Demographics in the US",
                 theme = shinytheme("flatly"),
              
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
                                                                  "Obesity rate in adults (%)" = "obesity",
                                                                  "Poverty rate (%)" = "poverty")),
                                          p("These maps show the prevalence of specific variables in counties of US states. Select the state you
                                            wish to observe as well as the variable. Make sure to look at the variable name to check how it is 
                                            measured (i.e. in percentage). For example, 'the low access to stores' variable shows the percent of the
                                            population that have low access to food stores in counties. The scale on the right side of the map shows 
                                            which colors correspond to which distribution. Have fun playing around!")
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
                                                        "Physical inactivity (%)" = "physical_inactivity",
                                                        "Poverty rate (%)" = "poverty")),
                                selectInput("yvar", "Y variable",
                                            choices = c("Obesity rate (%)" = "obesity",
                                                        "Diabetes rate (%)" = "diabetes")),
                                p("How are factors correlated with obesity and diabetes rates in US States? First, pick a state 
                                  to find out. Pick an independent variable and a dependent one to see how the relationship plays 
                                  out. For example, is an increase in access to convenience stores postiviely correlated with obesity rates in
                                  the states? By how much? Do keep in mind that these are correlations, and that no direct conclusions can be made 
                                  from the data. Also, do not forget to check out the distribution and summary tabs for more information.")
                                    
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
               tabPanel("About",
                        h1("Background"),
                        p("In 2018, the Commonwealth Fund called the rising rates of obesity in America", 
                        a("a Public Health Crisis.", href = "https://www.commonwealthfund.org/blog/2018/rising-obesity-united-states-public-health-crisis"),
                        "A number of states in US boast a population where over 35% of inhabitants are obese. Obesity in itself is
                        linked o chronic diseases including type 2 diabetes, hyperlipidemia, high blood pressure, cardiovascular disease, 
                        and cancer. It also accounts for 18% of deaths among Americans."),
                        p("Faced with this public crisis, many health experts emphasize food and exercise as a solution. However, despite",
                        a("a thriving US weight-loss industry,", href = "https://www.webwire.com/ViewPressRel.asp?aId=209054"),
                        "the rates of obesity do not seem to be slowing down. As such it seems that there might be other factors that influence this growth."),
                        p("This project aims to fill the gaps in linking obesity to factors that have been underexplored. While food intake and
                        exercise definetely impact one's health, other factors such as poverty and access to grocery stores may also play
                        an undiagnosed role."),
                        p("Ultimately this project aims to aid in answering the following questions: Can obesity be strictly linked to food and 
                        exercise? If not, how might we change our approach to tackling its growing problem?
                          "),
                        p("This project's repo can be found at the following link:",
                          a("https://github.com/Alexou07/final_project_v1", href = "https://github.com/Alexou07/final_project_v1")),
                        h1("The Data"),
                        p("This project draws data from the following websites and datasets."),
                        p(a("The United States Census Bureau", href = "https://www.census.gov/data/datasets/2015/demo/saipe/2015-state-and-county.html")),
                        p(" - The Small Area Income and Poverty Estimates (SAIPE) Program 2015 Dataset. This dataset provides the poverty rate for states and
                          counties in the US. It also breaks it down by demographics."),
                        p(a("The Center for Disease Control and Prevention.", href = "https://www.cdc.gov/")),
                        p(" - The Nutrition, Physical Activity, and Obesity - American Community Survey Dataset. This dataset provides rates of obesity and physical
                          ailments in the US, as well as factors that may influence these rates (i.e. physical activity)."),
                        p(a("The United States Department of Agriculture Economic Research Service.", href = "https://www.ers.usda.gov/")),
                        p(" - The Food Access Research Atlas Dataset. This dataset provides food access data for populations within census tracts; and
                          offers census-tract-level data on food access that can be downloaded for community planning or research purposes."),
                        h1("About Me"),
                        p("I am a senior at Harvard University, pursuing a degree in Psychology with a minor in Philosophy. I am interested in tackling large-scale
                          social issues through the careful manipulation of quantitative data. I took on this project with the aim of making the complex problem
                          of obesity in the United States more approachable and visualizable."),
                        p("You can reach me at the following places:"),
                        p(a("Linkedin", href = "https://www.linkedin.com/in/alexandra-ubalijoro-57816b17a/")),
                        p(a("Github", href = "https://github.com/Alexou07")),
                        p(a("Email", href = "mailto:a.ubalijoro@gmail.com"))
                        )
                   
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
           xlab("Longitude") + 
           ylab("Latitude") +
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
            ggplot(aes(get(input$xvar))) + geom_histogram() +
            xlab("X variable") +
            theme_classic()
    }, height=300, width=300)
    
    output$distribution2 <- renderPlot({
        food_stats %>%
            filter(state == input$state_2) %>%
            ggplot(aes(get(input$yvar))) + geom_histogram() +
            xlab("X variable") +
            theme_classic()
    }, height=300, width=300)
}

# Run the application 
shinyApp(ui = ui, server = server)
