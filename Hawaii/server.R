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
        labs(x = "Mokupuni", y = "Area (ha)",
             fill = "Model Output") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_blank())
    }, bg = "transparent") ## end of reactive model plot

    output$interactive_map <- renderTmap({
      # set the mode to interactive
      tmap_mode(mode = "view")
      tmap_options(check.and.fix = TRUE)

      # define the layers
      tm_shape(ahupuaa_raw_sf, name = "Ahupua'a") +
        tm_polygons(alpha = 0.2, border.col = "black",
                    border.alpha = 0.6, lwd = 0.2) +
        tm_shape(carbon_sf1, name = "Prioritize carbon storage") +
        tm_polygons(col = "gridcode", border.alpha = 0, palette = "Oranges",
                    title = "Carbon output") +
        tm_shape(food_sf1, name = "Prioritize food production") +
        tm_polygons(col = "gridcode", border.alpha = 0, palette = "Greens",
                    title = "Food output") +
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

    landuse_select <- reactive({
     landuse_df <- area_landuse_sum_dustin %>%
       filter(county == input$data)
      return(landuse_df)
    }) ## end of model_select reactive function

    output$landuse_plot <- renderPlot({
      ggplot(data = landuse_select()) +
        geom_col(position = "dodge", linewidth = 0.2,
                 aes(x = moku, y = landuse_ha, fill = resample)) +
        labs(x = "Moku", y = "Area in Hectares",
             fill = "Land Cover Class") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        facet_wrap(~mokupuni, scales = "free")
    }, bg = "transparent") ## end of reactive landuse plot

    reactive_map <- reactive({
      subset_ahupuaa <- area_landuse_sum_dustin %>%
        filter(county == input$data)
      return(subset_ahupuaa)
    })

    reactive_map2 <- reactive({
      subset_ahupuaa <- data_sf_clean_dustin %>%
        filter(county == input$data) %>%
        dplyr::select(c(moku, county, geometry))

      multi_layer_reactive_rs <- crop(multi_layer_rs_dustin, extent(subset_ahupuaa))
      multi_layer_reactive_df <- as.data.frame(multi_layer_reactive_rs, xy = TRUE)

      return(multi_layer_reactive_df)
    })

    # reactive_map3 <- reactive({
    #   subset_ahupuaa <- data_sf_clean %>%
    #     filter(county == input$data) %>%
    #     dplyr::select(c(moku, county, geometry))
    #   vect(subset_ahupuaa)
    #   landuse_reactive_sf <- st_crop(landuse_sf, subset_ahupuaa)
    #
    #   return(landuse_reactive_sf)
    # })

    output$county_plot <- renderPlot({
      ggplot() +
        geom_sf(data = reactive_map(), aes(fill = resample, color = resample)) +
        geom_sf(data = reactive_map(), fill = NA, color = "black", lwd =0.1) +
        geom_tile(data = reactive_map2(), inherit.aes = FALSE, aes(x = x, y = y), color = "violet") +
        labs(x = "Longitude", y = "Latitude", fill = "Landuse Coverage") +
        theme_bw()
    })

}
