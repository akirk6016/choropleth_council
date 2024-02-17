#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(leaflet)

# Define server logic required to draw a histogram
function(input, output, session) {

    # You can access the values of the check box widget (as a vector)
    # with input$checkGroup,
    output$value <- renderPrint({ input$checkGroup})

    output$placeholder_plot <- renderPlot({
      ggplot()+
        geom_sf(data = ahupuaa_raw_sf) +
        theme_bw()
    }) #end placeholder_plot for max's widget

    output$Plotoutput <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })

    reactive_map <- reactive({
      filter_selection <- input$data

      subset_ahupuaa <- ahupuaa_island[ahupuaa_island$island == filter_selection, ]

      return(subset_ahupuaa)
    })

    output$dustin_plot <- renderPlot({
      ggplot() +
        geom_sf(data = reactive_map()) +
        theme_bw()
    })

}
