library(tidyverse)
library(shiny)
library(CodeClanData)

brewers <- unique(beer$brewer)

beer <- beer %>% 
  mutate(brand = str_remove(brand, "\\\xca"),
         calories = as.numeric(calories)) %>% 
  pivot_longer(percent:carbohydrates,
               names_to = "attribute",
               values_to = "value")


ui <- fluidPage(
  titlePanel(tags$h1("Beer")),
  
  fluidRow(
    column(
      width = 6,
      selectInput(
        "brewer_input",
        "Brewer",
        brewers
      )
    ),
    column(width = 6,
           radioButtons(
             "attribute_input",
             "Property of Beer",
             c("percent", "calories", "carbohydrates")
           )
    )
  ),
  
  fluidRow(plotOutput("beer_plot"))
)

server <- function(input, output) {
  
  output$beer_plot <- renderPlot({
    beer  %>% 
      filter(brewer == input$brewer_input & attribute == input$attribute_input) %>% 
      ggplot(aes(brand, value, colour = value)) + #
      geom_col(show.legend = FALSE) +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_color_gradient(low = "darkorange", high = "darkorange4")
  })
}

shinyApp(ui = ui, server = server)
