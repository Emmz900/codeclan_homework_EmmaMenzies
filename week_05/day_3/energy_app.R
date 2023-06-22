library(tidyverse)
library(shiny)
library(CodeClanData)
library(bslib)

years <- unique(energy_scotland$Year)
sectors <- unique(energy_scotland$Sector)
col_scheme <- c(
  "Renewables" = "#8cd98c",
  "Pumped hydro" = "#66c2ff",
  "Nuclear" = "#a3a3c2",
  "Coal" = "#d98c8c",
  "Oil" = "#ffd966",
  "Gas" = "#cc99ff")

ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "cerulean"),
  
  titlePanel(tags$h1("Energy Production in Scotland")),
  
  
  fluidRow(
  # Slider to choose year to show in first graph  
    column(
      width = 6,
      sliderInput(
        "year_input",
        "Choose a year",
        min = min(years),
        max = max(years),
        value = 2005, #initial value
        step = 1,
        round = TRUE,
        ticks = FALSE,
        sep = ""
      )
    ),
    
    # Buttons to choose which energy source for the second graph
    column(
      width = 6,
      radioButtons(
        "sector_input",
        "Choose a Sector",
        sectors,
        inline = TRUE
      )
    )
  ),
  
  fluidRow(
    # plot of energy by sector in selected year
    column(width = 6,
           plotOutput("energy_by_sector")
    ),
    # plot of energy over time by selected sector
    column(width = 6,
           plotOutput("energy_by_year")
    )
  )
  
)

server <- function(input, output) {
  
  output$energy_by_sector <- renderPlot({
    energy_scotland %>% 
      filter(Year == input$year_input) %>% # only selected year
      ggplot(aes(Sector, EnergyProd, fill = Sector)) +
      geom_col(show.legend = FALSE) +
      ylab("Energy (MW)") +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = col_scheme) + #fill according to defined colour scheme
      theme(
        panel.grid.major.y = element_line(colour = "lightgrey"),
        panel.background = element_blank(),
        text = element_text(size = 16),
        axis.line.y = element_line(colour = "lightgrey"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  
  output$energy_by_year <- renderPlot({
    energy_scotland %>% 
      filter(Sector == input$sector_input) %>% 
      ggplot(aes(Year, EnergyProd, colour = input$sector_input, size = 2)) +
      geom_line(show.legend = FALSE)+
      ylab("Energy (MW)") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = 2005:2020) +
      scale_colour_manual(values = col_scheme) +
      theme(
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.background = element_blank(),
        text = element_text(size = 16),
        axis.line = element_line(colour = "lightgrey")
      )
  })
}

shinyApp(ui = ui, server = server)
