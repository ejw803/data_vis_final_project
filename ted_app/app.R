
# TED APP -----------------------------------------------------------------

# Load packages -----------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggthemes)


# Load data ---------------------------------------------------------------
ted_region_dat <- read_csv("data/processed/ted_region_dat.csv") %>% 
    # standardized naming convention
    janitor::clean_names() %>% 
    # mutate character variables to factor
    mutate_if(is.character, as.factor)


# Define UI --------------------------------------------------------------

ui <- fluidPage(
    
    # App title ----
    titlePanel("Total Economic Database, by Region from 1950 to 2021"),
    
    # Set up sidebar panel ---
    sidebarLayout(
        position = "right",
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # drop down widget to select continuous var
            selectInput(
                "y_variable",
                "Select Variable",
                # named list btwn what user sees and what we actually use
                choices = list(
                    "Real GDP" = "real_gdp",
                    "Per Capita Income" = "per_capita_income",
                    'Employment' = "employment",
                    'Population' = "population",
                    'Total Hours Worked' = "total_hours",
                    "Average Hours Worked" = "average_hours_worked",
                    "Output Per Hour Worked" = "output_per_hour_worked",
                    "Output per Employed Person" = "output_per_employed_person"
                )
            ),
            
            hr(),
            
            # radio button widget to select fill variable
            pickerInput(
                "region",
                "Select Regions:",
                choices = c(
                    "Africa",
                    "Asia",
                    "Central and Eastern Europe and Central Asia",
                    'Latin America',
                    "Middle East",
                    "North America",
                    "Oceania",
                    "Western Europe"
                ),
                options = list(`actions-box` = TRUE),
                multiple = T,
                selected = "North America"
            ),
            

        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            tabsetPanel(
                tabPanel("Plot", plotOutput("line_plot"))
                )
        )
    )
)


# Define server -----------------------------------------------------------
server <- function(input, output) {
   
    selected <- reactive(filter(ted_region_dat, region %in% input$region))
    
    # Histogram of CDC data, histogram of weight grouped by gender
    output$line_plot <- renderPlot({
        
        # x-axis label
        y_label <- switch(
            input$y_variable,
            "real_gdp" = "Real GDP",
            "per_capita_income" = "Per Capita Income",
            "employment" = 'Employment',
            "population" = 'Population',
            "total_hours" = 'Total Hours Worked',
            "average_hours_worked" = "Average Hours Worked",
            "output_per_hour_worked" = "Output Per Hour Worked",
            "output_per_employed_person" = "Output per Employed Person"
        )
        

        # Plot
        ggplot(selected(), aes_string(x = "year", y = input$y_variable)) +
            geom_line(
                aes_string(color = "region"),
                size = 0.75
            ) +
            # theme
            theme_minimal() +
            # get lables back
            theme(
                axis.title = element_text(),
                legend.position = "top"
            ) +
            guides(
                color = guide_legend(
                    title.position = "top",
                    title.hjust = 0.5
                )
            ) +
            # labels
            labs(
                x = "Year",
                y = y_label,
                # Color label connected to input
                color = "Regions"
            )
        
        
    })
    
}


# RUN APPLICATION ---------------------------------------------------------
shinyApp(ui = ui, server = server)
