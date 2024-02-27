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

      subset_ahupuaa <- data_sf_clean[data_sf_clean$mokupuni == filter_selection, ]
      subset_vec <- vect(subset_ahupuaa)
      return(subset_vec)
    })

    reactive_map2 <- reactive({

      multi_layer_reactive_rs <- crop(multi_layer_rs, extent(reactive_map()))
      multi_layer_reactive_df <- as.data.frame(multi_layer_reactive_rs, xy = TRUE)

      return(multi_layer_reactive_df)
    })

    output$dustin_plot <- renderPlot({
      ggplot() +
        geom_sf(data = reactive_map()) +
        geom_tile(data = reactive_map2(), inherit.aes = FALSE, aes(x = x, y = y), fill = "#882222")
        theme_bw()
    })

}
