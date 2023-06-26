library(tidyverse)
library(CodeClanData)
library(shiny)
library(plotly)
library(bslib)

# Set up -------
publishers <- sort(unique(game_sales$publisher))

## Colour scheme --------------
colour_scheme <- c(
  "Xbox" = "chartreuse",
  "X360" = "chartreuse2",
  "XB" = "chartreuse3",
  "XOne" = "chartreuse4",
  "Playstation" = "royalblue",
  "PS" = "royalblue1",
  "PS2" = "dodgerblue",
  "PS3" = "dodgerblue2",
  "PS4" = "royalblue2",
  "PSP" = "royalblue3",
  "PSV" = "royalblue4",
  "Nintendo" = "firebrick",
  "3DS" = "firebrick1",
  "DS" = "firebrick2",
  "GBA" = "brown2",
  "GC" = "brown3",
  "Wii" = "firebrick3",
  "WiiU" = "firebrick4",
  "PC" = "black",
  "Total" = "grey75")

## Platform categories -------------
game_sales <- game_sales %>% 
  mutate(
    platform_cat = factor(
      case_when(
        str_detect(platform, "PS") ~ "Playstation",
        str_detect(platform, "X") ~ "Xbox",
        str_detect(platform, "Wii|GC|GB|DS") ~ "Nintendo",
        .default = platform
      )
    )
  )

unique(game_sales$platform)

## Theme ------------
my_theme <- theme(
  axis.text = element_text(size = 12, colour = "grey75"),
  axis.title = element_text(size = 10, colour = "grey75"),
  plot.title = element_text(size = 16, colour = "grey75"),
  legend.text = element_text(size = 10, colour = "grey75"),
  legend.title = element_text(size = 12, colour = "grey75"),
  legend.background = element_rect(fill = "transparent"),
  panel.background = element_rect(fill = "transparent"),
  panel.border = element_rect(fill = "transparent", colour = "transparent"),
  plot.background = element_rect(fill = "transparent", colour = NA)
)

# UI -----------
ui <- fluidPage(
  
  theme = bs_theme(bootswatch = "slate"),
  
  ## title -------
  titlePanel(
    h1("Games Released by Platform", align = "center")
  ),
  
  ## Input row ---------
  fluidRow(
    ### input publisher -------------
    column(
      width = 6,
      selectInput(
        "publisher_input",
        h2("Choose a Publisher"),
        publishers
      )
    ),
    
    # Could add a selection between platform and genre
    
    ### second column heading -----------
    column(
      width = 6,
      br(),
      h2("All data from 1988-2016")
    )
  ),
  
  ## First plots -----------
  fluidRow(
    
    ### number of games by publisher over time ----------
    # allows the user to input a publisher and see how many games have been
    # released over time. Since this is continuous data a line graph is
    # most informative.
    column(
      width = 6,
      plotlyOutput("games_released_plot")
    ),
    
    ### games by publisher across platform ----------
    # This plot is interesting to see the proportion of games released to each 
    # platform by each publisher. It also allows comparison of the number of 
    # games released between publishers
    column(
      width = 6,
      plotlyOutput("games_plot")
    )
  ),
  
  fluidRow(
    br()
  ),
  
  ## Second Plots -----------
  fluidRow(
    ### top 10 games by publisher -----------
    # this works with the user input and above graph to highlight the most
    # popular games from this publisher.
    # Users are likely to recognise popular games more than publisher hence 
    # this is a useful addition.
    # The comparison of user and critic scores may also be interesting
    # as these can often vary quite a bit
    column(
      width = 6,
      plotlyOutput("top_games_plot")
    ),
    
    ### games per platform ----------
    # This highlights the overall distribution of games per platform 
    # as well as the breakdown of specific platforms.
    # it works well with the above graph of publishers to add extra context.
    # The bars are stacked into the platform categories provided in the above graph
    column(
      width = 6,
      plotlyOutput("platform_plot")
    )
  )
)

# server ----------
server <- function(input, output, session) {
  
  filtered_games <- reactive({
    game_sales %>% 
      filter(publisher == input$publisher_input)
  })
  
  ## games by time plot (top left) -------
  output$games_released_plot <- renderPlotly({
    ggplotly(
      filtered_games() %>% 
        group_by(year_of_release, platform_cat, .drop = FALSE) %>% 
        summarise(total_games_plat = n()) %>% 
        ungroup() %>% 
        group_by(year_of_release) %>% 
        mutate(total_games = sum(total_games_plat)) %>% 
        ggplot() +
        geom_line(aes(year_of_release, total_games_plat, colour = platform_cat),
                  size = 2,
                  linetype = "longdash") +
        #geom_point(aes(year_of_release, total_games_plat, colour = platform_cat))+
        geom_line(aes(year_of_release, total_games, colour = "Total"),
                  size = 2.5) +
        #geom_point(aes(year_of_release, total_games, colour = "Total")) +
        theme(panel.grid = element_blank()) +
        scale_y_continuous(breaks = seq(0, 100, 5)) +
        scale_x_continuous(breaks = seq(1988, 2016, 4)
                           #,limits = c(1988, 2016)
        ) +
        scale_colour_manual(values = colour_scheme) +
        labs(
          title = "Number of Games Released per year",
          x = "",
          y = "Total Number of Games"
        ) +
        my_theme
    )
  }
  #, bg = "transparent"
  )
  
  ## top 10 games plot (bottom left) --------
  output$top_games_plot <- renderPlotly({
    ggplotly(
      filtered_games() %>% 
        group_by(name) %>% 
        summarise(User = mean(user_score),
                  Critic = mean(critic_score)/10) %>% 
        arrange(desc(User)) %>% 
        slice_head(n = 10) %>% 
        pivot_longer(User:Critic, names_to = "Reviewer", values_to = "Score") %>% 
        ggplot() +
        geom_col(aes(x = reorder(name, -Score),
                     y = Score,
                     fill = Reviewer),
                 position = "dodge",
        ) +
        scale_fill_manual(values = c("User" = "burlywood1",
                                     "Critic" = "burlywood3")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank()) +
        labs(
          title = "Top 10 Games Released by Selected Publisher",
          x = "",
          y = "Mean Rating"
        ) +
        my_theme
    )
  }
  #, bg = "transparent"
  ) 
  
  ## games by publisher plot with plotly -> didn't match general theme 
  # therefore used a simpler plot, could use ggplotly on ggplot object?
  
  # output$games_plot <- renderPlotly({
  #   game_sales %>% 
  #     group_by(publisher, platform_cat) %>% 
  #     summarise(total_games = n()) %>% 
  #     plot_ly(y = ~ reorder(publisher, total_games, sum),
  #             x = ~ total_games,
  #             color = ~ platform_cat,
  #             colors = colour_scheme,
  #             type = "bar",
  #             textfont = list(color = "#000000", size = 16)
  #     ) %>% 
  #     layout(barmode = "stack",
  #            title = "Number of Games Released by Publisher Across Each Platform",
  #            yaxis = list(title = ""),
  #            xaxis = list(title = "Number of Games released")
  #     ) %>% 
  #     layout(paper_bgcolor = "transparent")
  # })
  
  ## games by publisher plot (top right) ------------
  output$games_plot <- renderPlotly({
    ggplotly(
      game_sales %>% 
        group_by(publisher, platform_cat) %>% 
        summarise(total_games = n()) %>% 
        ggplot(aes(y = reorder(publisher, -total_games, sum),
                   x = total_games,
                   fill = platform_cat)) +
        geom_col(colour = "grey75") +
        labs(
          title = "Number of Games Released by Publisher Across Each Platform",
          y = "",
          x = "Total Number of Games",
          fill = "Platform"
        ) +
        scale_x_continuous(breaks = seq(0, 1000, 100)) +
        scale_fill_manual(values = colour_scheme) +
        theme(
          #axis.text.y = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank()
        ) +
        my_theme
    )
  }
  #, bg = "transparent"
  )
  
  
  
  ## games by platform (bottom right) --------
  output$platform_plot <- renderPlotly({
    ggplotly(
      game_sales %>% 
        group_by(platform_cat, platform) %>% 
        summarise(total = n()) %>% 
        ggplot(aes(reorder(platform_cat, -total, sum), total, fill = platform, label = platform)) +
        geom_col(colour = "grey75", show.legend = FALSE) +
        scale_fill_manual(values = colour_scheme) +
        geom_text(
          colour = "grey75",
          size = 4,
          position = position_stack(vjust = 0.5)
        ) +
        scale_y_continuous(breaks = seq(0, 1000, 100)) +
        theme(panel.grid = element_blank()) +
        labs(
          title = "Games Released on Each Platform",
          x = "Platform",
          y = "Total Number of Games"
        ) +
        my_theme
    )
  }
  #, bg = "transparent"
  )
  
  
}

shinyApp(ui, server)


