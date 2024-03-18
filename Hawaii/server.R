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

    output$overview_image <- renderImage({

      list(src = "www/hawaii_image.jpeg",
           width = "75%",
           height = 500)
    }, deleteFile = F)


    model_select <- reactive({
      model_df <- max_ahu_model_join_select_df %>%
        dplyr::filter(model_type == input$model_type)
      return(model_df)
    }) ## end of model_select reactive function

    color_select <- reactive({
      if(input$model_type == "Carbon"){
        values = carbon_pal
      } else {
        values = food_pal
      }
    }) ##

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
              legend.background = element_blank()) +
        scale_fill_manual(values = color_select())
    }, bg = "transparent") ## end of reactive model plot

    output$interactive_map <- renderTmap({
      # set the mode to interactive
      tmap_mode(mode = "view")
      tmap_options(check.and.fix = TRUE)

      # define the layers
      tm_shape(max_ahupuaa_raw_sf, name = "Ahupua'a") +
        tm_polygons(alpha = 0.2, border.col = "black",
                    border.alpha = 0.6, lwd = 0.2) +
        tm_shape(max_carbon_sf, name = "Prioritize carbon storage") +
        tm_polygons(col = "gridcode", border.alpha = 0, palette = "Oranges",
                    title = "Carbon output") +
        tm_shape(max_food_sf, name = "Prioritize food production") +
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
       filter(mokupuni == input$data)
      return(landuse_df)
    }) ## end of model_select reactive function

    output$landuse_plot <- renderPlot({
      ggplot(data = landuse_select()) +
        geom_col(position = "dodge", linewidth = 0.2,
                 aes(x = moku, y = landuse_ha, fill = resample), color = "black") +
        labs(x = "Moku", y = "Area in Hectares",
             fill = "Land Cover Class") +
        scale_fill_manual(values = c("Agriculture" = "coral2",
                                     "Urban or Built-Up" = "peachpuff4",
                                     "Forest Land" = "darkgreen",
                                     "Water" = "cyan2",
                                     "Wetlands" = "cyan4",
                                     "Rangeland" = "gold2",
                                     "Barren" = "peachpuff2")) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        theme(panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              panel.background = element_blank(),
              plot.background = element_blank(),
              legend.background = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        ggtitle('Total Hectares of Landuse Coverage in:') +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        facet_wrap(~mokupuni, scales = "free")
    }, bg = "transparent") ## end of reactive landuse plot

    reactive_map <- reactive({
      subset_ahupuaa <- area_landuse_sum_dustin %>%
        filter(mokupuni == input$data)
      return(subset_ahupuaa)
    })

    reactive_map2 <- reactive({
      subset_ahupuaa <- data_sf_clean_dustin %>%
        filter(mokupuni == input$data) %>%
        dplyr::select(c(moku, mokupuni, county, geometry))

      multi_layer_reactive_rs <- crop(multi_layer_rs_dustin, extent(subset_ahupuaa))
      multi_layer_reactive_df <- as.data.frame(multi_layer_reactive_rs, xy = TRUE)

      return(multi_layer_reactive_df)
    })

    # reactive_map3 <- reactive({
    #   subset_ahupuaa <- data_sf_clean_dustin %>%
    #     filter(county == input$data) %>%
    #     dplyr::select(c(moku, county, geometry))
    #
    #   return(subset_ahupuaa)
    # })

    output$county_plot <- renderPlot({
      ggplot() +
        geom_sf(data = reactive_map(), aes(fill = resample)) +
        geom_sf(data = reactive_map(), fill = NA, color = "black", lwd =0.1) +
        scale_fill_manual(values = c("Agriculture" = "coral3",
                                     "Urban or Built-Up" = "peachpuff4",
                                     "Forest Land" = "darkgreen",
                                     "Water" = "cyan2",
                                     "Wetlands" = "cyan4",
                                     "Rangeland" = "gold2",
                                     "Barren" = "peachpuff2")) +
        geom_tile(data = reactive_map2(), inherit.aes = TRUE, aes(x = x, y = y), fill = "hotpink", color = "hotpink") +
        labs(x = "Longitude", y = "Latitude", fill = "Landuse Coverage", color = "") +
        ggtitle(req(input$data)) +
        theme_bw() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
    })





## Abigail Server Work

datasetInput <- eventReactive(input$update, {
  switch(input$dataset,
         "1" = top_1,
         "2" = top_2,
         "3" = top_3,
         "4" = top_4,
         "5" = top_5,
         "6" = top_6,
         "7" = top_7,
         "8" = top_8,
         "9" = top_9,
         "10" = top_10,
         "11" = top_11,
         "12" = top_12,
         "13" = top_13,
         "14" = top_14,
         "15" = top_15,
         "16" = top_16)

}, ignoreNULL = FALSE) ## end of dataset selection


output$unions_plot <-  renderPlot({
  ggplot(data = unions_nrc_counts, aes(x = n, y = sentiment, fill = sentiment)) +
    geom_col() +
    facet_wrap(~interview) +
    labs(x = "Wordcount", y = "Sentiment") +
    theme_bw() +
    scale_fill_manual(values =met.brewer("Benedictus", 10)) +
    guides(fill="none") +
    theme(
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.background = element_blank())
}, bg = "transparent") ## end of static plot

# Show the first "n" observations ----
# The use of isolate() is necessary because we don't want the table
# to update whenever input$obs changes (only when the user clicks
# the action button)

output$view <- renderTable({
  head(datasetInput(), n = isolate(input$words))
})

}
