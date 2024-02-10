### Packages
library(shiny)
library(tidyverse)
library(palmerpenguins)



ui <- fluidPage(
  titlePanel('Land Use Priority Areas in Hawaii'),
  sidebarLayout(
    sidebarPanel('put my widgets here',

             radioButtons(
               inputId = 'penguin_species',
               label = 'Choose penguin species',
               choices = c('Adelie', 'Gentoo', 'Chinstrap')
             )),
    mainPanel('put my graph here',
              plotOutput(outputId = 'penguin_plot')
              )

  ) ### end of sidebarlayout
) ### end of my fluidpage


server <- function(input, output) {


  penguin_select <- reactive({
    penguins_df <- penguins %>%
      filter(species == input$penguin_species)
    return(penguins_df)
  }) ### end of penguin_select reactive

  output$penguin_plot <- renderPlot({

    ggplot(data = penguin_select()) + ### adding the () for penguin select
    ## lets you know that its a reactive function not a regular df
      geom_point(aes(x = flipper_length_mm, y = body_mass_g))

  }) ### end of penguin plot
}


shinyApp(ui = ui, server = server)
