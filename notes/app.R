#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)

imdb_tv <- read_csv("imdb_popular_tvshows.csv") %>%
    filter(!is.na(episodeRating)) %>%
    mutate(episodeNumber = parse_number(episodeNumber),
           seasonNumber = parse_number(seasonNumber)
           )
# could also use tidyr::drop_na(episodeRating)

shows <- imdb_tv %>% 
    pull(Show_Name) %>% 
    unique() %>%
    sort()

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("IMDb Popular TV Show Episode Ratings"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("selectShow", 
                        label = h3("Select a show"), 
                        choices = shows,
                        multiple = TRUE,
                        selected = "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("episodePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    selectedData <- reactive({
        # subset data
        plotData <- 
            imdb_tv %>%
            filter(Show_Name %in% input$selectShow)
        
        plotData
    }) %>% bindEvent(input$selectShow)
    
    output$episodePlot <- renderPlot({
        show_data <- selectedData()

        # draw the plot
        
        if(max(show_data$seasonNumber) == 1){
            # make a scatterplot
            my_plot <- show_data %>%
                ggplot(aes(x = episodeNumber,
                           y = episodeRating
                           )) +
                geom_point() +
                geom_line()
            
        }
        if(max(show_data$seasonNumber) > 1){
            my_plot <- show_data %>%
                ggplot(aes(x = factor(seasonNumber), 
                           y = episodeRating,
                           fill = Show_Name
                )) +
                geom_boxplot() +
                facet_wrap(~Show_Name)
        }
        
        my_plot

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
