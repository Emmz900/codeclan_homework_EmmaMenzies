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
    )
  ),
  
  fluidRow(
    column(width = 4,
           plotOutput("percent_plot")
    ),
    column(width = 4,
           plotOutput("calorie_plot")
    ),
    column(width = 4,
           plotOutput("carbs_plot")
    )
  )
)

server <- function(input, output) {
  
  output$percent_plot <- renderPlot({
    beer  %>% 
      filter(brewer == input$brewer_input & attribute == "percent") %>% 
      ggplot(aes(brand, value, colour = value)) + #
      geom_col(show.legend = FALSE) +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_color_gradient(low = "darkorange", high = "darkorange4") +
      ylab("Alcohol Percentage")
  })
  
  output$carbs_plot <- renderPlot({
    beer  %>% 
      filter(brewer == input$brewer_input & attribute == "carbohydrates") %>% 
      ggplot(aes(brand, value, colour = value)) + #
      geom_col(show.legend = FALSE) +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_color_gradient(low = "darkorange", high = "darkorange4") +
      ylab("Carbohydrates")
  })
  
  output$calorie_plot <- renderPlot({
    beer  %>% 
      filter(brewer == input$brewer_input & attribute == "calories") %>% 
      ggplot(aes(brand, value, colour = value)) + #
      geom_col(show.legend = FALSE) +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_color_gradient(low = "darkorange", high = "darkorange4") +
      ylab("Calories")
  })
}

shinyApp(ui = ui, server = server)
