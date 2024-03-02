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

    # model_select <- reactive({
    #   model_df <- ahu_carbon_df %>%
    #     filter(model = input$model_output)
    #   return(model_df)
    # }) ## end of model selection
    #

    # output$model_plot <- renderPlot({
    #   ggplot(data = model_select()) +
    #     geom_col(aes(x = mokupuni, y = area_hecta, fill = gridcode))
    # }) ## end of model plot

      output$model_plot <- renderPlot({
        ggplot(ahu_carbon_df,
               aes(x = mokupuni, y = area_hecta, fill = gridcode)) +
          geom_col(position = "dodge", color = "black", linewidth = 0.2) +
          theme_bw() +
          scale_fill_manual(values = c("grey", "forestgreen")) +
          labs(x = "Mokupuni", y = "Area (ha)",
               fill = "Model Output", title = "Carbon Storage") +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())
      })


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

      return(subset_ahupuaa)
    })

    reactive_map2 <- reactive({
      filter_selection <- input$data

      subset_ahupuaa <- data_sf_clean[data_sf_clean$mokupuni == filter_selection, ]

      multi_layer_reactive_rs <- crop(multi_layer_rs, extent(subset_ahupuaa))
      multi_layer_reactive_df <- as.data.frame(multi_layer_reactive_rs, xy = TRUE)

      return(multi_layer_reactive_df)
    })

    reactive_map3 <- reactive({
      filter_selection <- input$data

      subset_ahupuaa <- data_sf_clean[data_sf_clean$mokupuni == filter_selection, ]
      vect(subset_ahupuaa)
      landuse_reactive_sf <- st_crop(landuse_sf, subset_ahupuaa)

      return(landuse_reactive_sf)
    })

    output$dustin_plot <- renderPlot({
      ggplot() +
        geom_sf(data = reactive_map3(), aes(fill = resample)) +
        geom_sf(data = reactive_map(), fill = NA, color = "black") +
        geom_tile(data = reactive_map2(), inherit.aes = FALSE, aes(x = x, y = y)) +
        theme_bw()
    })

}
