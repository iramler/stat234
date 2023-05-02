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

imdb_tv <- read_csv("imdb_popular_tvshows.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("IMDb Popular TV Show Episode Ratings"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("selectShow", 
                        label = h3("Select a show"), 
                        choices = list("Narcos",
                                       "The Witcher",
                                       "Westworld"), 
                        selected = "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # subset data
        plotData <- 
            imdb_tv %>%
            filter(Show_Name == input$selectShow)
        # draw the plot
        
        plotData %>%
            ggplot(aes(x = factor(seasonNumber), 
                       y = episodeRating)) +
            geom_boxplot()

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
