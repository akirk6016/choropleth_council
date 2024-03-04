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

    model_select <- reactive({
      model_df <- ahu_model_join_select_df %>%
        dplyr::filter(model_type == input$model_type)
      return(model_df)
    }) ## end of model_select reactive function

    output$model_plot <- renderPlot({
      ggplot(data = model_select()) +
        geom_col(position = "dodge", color = "black", linewidth = 0.2,
                 aes(x = mokupuni, y = area_hecta, fill = gridcode)) +
        theme_bw() +
        labs(x = "Mokupuni", y = "Area (ha)",
             fill = "Model Output") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    }) ## end of reactive model plot

    output$interactive_map <- renderTmap({
      # set the mode to interactive
      tmap_mode(mode = "view")
      tmap_options(check.and.fix = TRUE)

      # define the layers
      tm_shape(ahupuaa_raw_sf, name = "Ahupua'a") +
        tm_polygons(alpha = 0.2, border.col = "black",
                    border.alpha = 0.6, lwd = 0.2) +
        tm_shape(carbon_sf1, name = "Carbon Model") +
        tm_polygons(col = "gridcode", border.alpha = 0,
                    title = "Model Output") +
        tm_shape(food_sf1, name = "Food Model") +
        tm_polygons(col = "gridcode", border.alpha = 0,
                    title = "Model Output") +
        tm_scale_bar()
    }) ## end of interactive tmap


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
        labs(x = "Longitude", y = "Latitude", fill = "Landuse Coverage") +
        theme_bw()
    })

}
