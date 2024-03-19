#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(leaflet)

# Define UI for application that draws a histogram

navbarPage(
  "Land Use Prioritization in Hawaii",

  theme = bslib::bs_theme(bootswatch = "morph"),

  tabPanel("Overview",
           p('The purpose of this shiny app is to visual land use prioritization scenarios in Hawaii.
We will use various scenarios to map how prioritiziation changes based on the order of goal implementation. The raster layers
showing the results of land prioritization models were developed by Nākoa Farrant, a PhD candidate at the Bren School of Environemntal Science & Management. Additional
spatial data was pulled in as needed. We will also use interview data retrieved ScholarSpace, a repository managed by the University of Hawaii at Manoa,
to conducr a sentiment analysis on the interviewees experiences growing up on Pioneer Mill.'),

           p("Web developers: Abigail Kirk, Dustin Duncan, Maxwell Pepperdine", align = 'left'),

           imageOutput("overview_image"), align = 'center'

           ),

  tabPanel("Land Prioritization Models",
           fluidPage(
             div(
               sidebarLayout(
                 sidebarPanel(style = "max-height: 30vh;",
                              radioButtons(
                                inputId = "model_type",
                                label = h5("Choose model type"),
                                choices = c("Prioritize carbon storage" = "Carbon",
                                            "Prioritize food production" = "Food")
                              ), ## end of radio buttons
                              width = 3
                 ),
             mainPanel(
                       fluidRow(
                         column(
                           width = 12,
                           offset = 0,
                           div(plotOutput("model_plot")), align = 'center'),
                           div(p(style = "text-align: font-size = 13px",
                                 strong("Area (ha) of model output on each mokupuni (island)"))),
                           div(p(style = "text-align: font-size = 12px",
                                 "Land prioritization models that maximized only carbon sequestion are
                                 shown in shades of orange. A value of 1 indicates an area that
                                 the model identified as important for carbon sequestration, and a value
                                 of 0 indicates an area not identfied by the model as important
                                 for this metric. Models that prioritized land acquisition based on
                                 potential for food production are shown in green. Again, a value of 1
                                 indicates areas with a high potential for food production, and values of
                                 0 not."))),
                       fluidRow(
                         column(
                           width = 12,
                           offset = 0,
                           div(tmapOutput(outputId = "interactive_map")), align = 'center'),
                           div(p(style = "text-align: font-size = 13px",
                                 strong("Interactive map showing model results"))),
                           div(p(style = "text-align: font-size = 12px",
                                 "This interactive map shows the results of the land prioritization models
                                 developed by Nākoa Farrant. The color scheme
                                 for each model output aligns with the bar graph above, and
                                 a shapefile of Ahupuaʻa's are included below the model results. Users can click on
                                 each Ahupua'a boundary to see its name, size, and associated Moku and Mokupuni."))),

                   )## end of main panel
                  ## end of main panel fluid row

                )## end of sidebar Layout

             ), ## end of div()

           ) ## end of fluidPage


  ), ## end of tab panel

  # tabPanel("Max's widget",
  #          fluidPage(
  #            div(
  #              sidebarLayout(
  #                sidebarPanel("put my widgets here",
  #
  #                 radioButtons(
  #                   inputId = "model_type",
  #                   label = h4("Choose model type"),
  #                   choices = c("Prioritize carbon storage" = "Carbon",
  #                               "Prioritize food production" = "Food")
  #                 ) ## end of radio buttons
  #                 ), mainPanel("Area (ha) of model ouput on each island",
  #                          plotOutput("model_plot"),
  #                          h3("Interactive Map"),
  #                          tmapOutput(outputId = "interactive_map", width = "100%")
  #                 )## end of main panel
  #
  #
  #
  #              ) ## end of sidebar Layout
  #
  #            ), ## end of div()
  #
  #          ) ## end of fluidPage
  #
  #
  #       ), ## end of tab panel

  # tabPanel("Pioneer Mill Interview Text Analysis",
  #          fluidPage(
  #            div(
  #              sidebarLayout(
  #                sidebarPanel(
  #                  selectInput("dataset", "Choose an Interview:",
  #                              choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")),
  #
  #                  # Input: Specify the number of observations to view ----
  #                  numericInput("words", "Number of top words to view:", 10),
  #
  #
  #
  #                  # Input: actionButton() to defer the rendering of output ----
  #                  # until the user explicitly clicks the button (rather than
  #                  # doing it immediately when inputs change). This is useful if
  #                  # the computations required to render output are inordinately
  #                  # time-consuming.
  #                  actionButton("update", "Update View"),
  #
  #                ),
  #
  #              )
  #
  #            )),
  # ), ## end of tab panel

  # My goal is to make a fluid page with one row on the top with two selectInputs and two rows on the bottom with one plot in each of the two bottom rows

  tabPanel("Land Use Coverage",
           p("Visualize land coverage on the Hawai'ian Islands as it compares to the land prioritization models.
             Total hectares of land coverage are divided by moku on each island to provide a more digestible
             visualization for different subsets of each island."),
           fluidPage(
             fluidRow(
               column(width = 6, selectInput("data", "Select Island", choices = unique(data_sf_clean_dustin$mokupuni)), align = "center"),
               column(width = 6, selectInput("raster", "Select Prioritization", choices = c("Food", "Carbon", "Neither")), align = "center")

               ),

             fluidRow(
               column(
                 width = 7,
                 offset = 2,
              mainPanel(
                "Land Coverage in the Hawai'ian Islands",)),
                column(
                width = 8,
                offset = 2,
                plotOutput(outputId = "landuse_plot"),
                p("Land prioritization models are colored either pink or cyan to effectively distinguish them from the land
                cover classes. If you would like to see just the land coverage, you may select \'Neither\' from
                  the \'Select Prioritization\' options to see just the land coverage on each island."),
                plotOutput(outputId = "county_plot")
                ),
                align = 'center',
                style = 'width = 100%;'
                ))
              ),

  tabPanel("Choropleths",
           fluidPage(
             p(style = "text-align: font-size = 13px",
               "Choropleth maps use differences in shading and coloring to indicate the average value of
               certain properties or quantities within areas. Below, users can select different islands and
               model prioritization status to see how land prioritized for food production and carbon storage
               vary spatially within Ahupua'a boundaries across the Hawai'ian Islands. Darker shades of green and orange in the food and
               carbon consideration choropleths indicate greater counts of land identified as high potential
               for these respective prioritizations."),
             fluidRow(
               column(width = 6, selectInput("island", "Select Island", choices = unique(data_choropleths_dustin$mokupuni)), align = "center"),
               column(width = 6, selectInput("choropleth", "Select Prioritization", choices = c("Prioritized", "Not Prioritized")), align = "center")

             ),
             fluidRow(
               column(
                 width = 7,
                 offset = 2,
                 mainPanel(
                   "Carbon and Food Prioritization Distributions within Hawai'ian Islands"
                 ),
                 splitLayout(cellWidths = c("50%", "50%"), plotOutput(outputId = "choropleth_food"),
                 plotOutput(outputId = "choropleth_carbon"))
               ),

             )
           )),

tabPanel("Pioneer Mill Interview Text Analysis",
        p('Pioneer Mill, established in 1860, was the first sugar plantation in Hawaii to grow sugarcane commercially. In its peak in the 1960s,
        it produced 60,000 tones of sugar annually. Pioneer Mill was also famous for erecting what was at the time Hawaiis largest smokestack
        in 1928. The smokestack used the leftover bagasse as fuel for electricity, and the smoky-sweet emissions became a staple of the local environment.
        The plantation ceased operations in 1999, and since then the smokestack has been saved as a historic landmark. The following sentiment analysis
        was conducted using interview data from individuals who grew up on or near the plantation.'),
        p(style = "text-align: font-size = 9px",
        'Interview Data Citation: Pioneer Smokestack| Lahaina Restoration Foundation. (n.d.). Retrieved March 11, 2024, from https://www.lahainarestoration.org/smokestack.html'),
        p(style = "text-align: font-size = 9px",
        'Widget Source Citation: Mine Cetinkaya-Rundel; Professor at Duke University + Developer Educator at Posit PBC
        https://github.com/rstudio/shiny-examples/tree/main/007-widgets'),
        p(style = "text-align: font-size = 9px",
          'NRC Lexicon Citation: Saif M. Mohammad and Peter D. Turney. 2013. Crowdsourcing a Word-Emotion Association Lexicon. Computational Intelligence, 29(3), 436-465.'),


        # \n Interview Data Citation: Pioneer Smokestack| Lahaina Restoration Foundation. (n.d.). Retrieved March 11, 2024, from https://www.lahainarestoration.org/smokestack.html
        # \n Widget Source Citation: Mine Cetinkaya-Rundel; Professor at Duke University + Developer Educator at Posit PBC
        # https://github.com/rstudio/shiny-examples/tree/main/007-widgets
        # \n NRC Lexicon Citation: Saif M. Mohammad and Peter D. Turney. 2013. Crowdsourcing a Word-Emotion Association Lexicon. Computational Intelligence, 29(3), 436-465.'
        #       ),

            # end of summary paragraph
         fluidPage(
           div(
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", "Choose an Interview:",
                             choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")),

                 # Input: Specify the number of observations to view ----
                 numericInput("words", "Number of top words to view:", 10),



                 # Input: actionButton() to defer the rendering of output ----
                 # until the user explicitly clicks the button (rather than
                 # doing it immediately when inputs change). This is useful if
                 # the computations required to render output are inordinately
                 # time-consuming.
                 actionButton("update", "Update View"),

               ), ## end of sidebar panel
            # Main panel for displaying outputs ----
            mainPanel(

         # Output: Header ----
           h4("Sentiment Analysis", style = "text-align: center"),
              p(style = "text-align: font-size = 12px",
              "The following table shows the top words in the interview text The plot above the tables shows the sentiment analysis conducted.
                Using the NRC Lexicon, the analysis classifies each word using the following sentiments:
                anger, anticipation, disgust, fear, joy, negative, positive, sadness, surprise, and trust. The table shows the total sentiment counts for each chapter."),
          plotOutput(outputId = "unions_plot"),


      # Output: Header + table of distribution ----
       h4("Observations", style = "text-align: center"),
        tableOutput("view"), align = 'center'

              ) ## end of main panel


             ) ## end of sidebar layout

           )), ## end of div and fluid page
) ## end of tab panel

)

