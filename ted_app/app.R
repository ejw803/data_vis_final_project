
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
    titlePanel("Total Economic Development by Region from 1950 to 2021"),
    
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
                "regions",
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
                multiple = T
            ),
            
            # horizontal line,
            hr(),
            
            # slider to set year range
            sliderInput("range", 
                        label = "Selected Year Range",
                        min = 1950, max = 2021, value = c(1950, 2021),
                        sep = "",
                        animate = TRUE
            ),

            br(),
            br(),
            
            # Animated slider bar to select the number of breaks to use on y-axis (dollars)
            sliderInput(
                inputId = "y_breaks",
                label = "Number of Vertical Breaks",
                min = 3,
                max = 20,
                value = 3,
                animate = TRUE
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
        
        # fill variable
        # fill_var <- switch(
        #     input$regions,
        #     "real_gdp" = "Real GDP",
        #     "per_capita_income" = "Per Capita Income",
        #     "employment" = 'Employment',
        #     "population" = 'Population',
        #     "total_hours" = 'Total Hours Worked',
        #     "average_hours_worked" = "Average Hours Worked",
        #     "output_per_hour_worked" = "Output Per Hour Worked",
        #     "output_per_employed_person" = "Output per Employed Person"
        # )
        
        # x is chosen through `selectInput` and the inputID is `y_variable`
        x    <- ted_region_dat %>% pull(input$y_variable)
        
        args$min <- as.numeric(input$range[1]) 
        args$max <- as.numeric(input$range[2]) 
        
        # Plot
        ggplot(ted_region_dat, aes_string(x = year, y = input$y_variable)) +
            geom_line(
                aes_string(color = regions),
                breaks = bin_breaks,
                color = "black"
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
                color = input$regions
            )
        
        
    })
    
}


# RUN APPLICATION ---------------------------------------------------------
shinyApp(ui = ui, server = server)
