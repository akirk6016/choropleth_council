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
  "This is the shell of our shiny",

  theme = bslib::bs_theme(bootswatch = "morph"),

  tabPanel("Overview",
           p('The purpose of this shiny app is to visual land use prioritization scenarios in Hawaii.
We will use various scenarios to map how prioritiziation changes based on the order of goal implementation.
We will also use spatial data of priority areasidentfied by residents to compare how the modelled scenarios compare to how people perceive priority needs.
Additionally, we will use interview data retrieved ScholarSpace, a repository managed by the University of Hawaii at Manoa,
to conducr a sentiment analysis on the interviewees experiences growing up on Pioneer Mill'),

           imageOutput("overview_image")

           ),

  tabPanel("Land prioritization models",
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
                           div(h5("Area (ha) of model ouput on each island", align = 'center')),
                           div(plotOutput("model_plot")), align = 'center'),
                           div(p(style = "text-align: font-size = 12px",
                                 "Area (ha) of model output on each mokupuni (island). Land
                                 prioritization models that maximized only carbon sequestion are
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
                           div(h5("Interactive map showing model results"), align = 'center'),
                           div(tmapOutput(outputId = "interactive_map")), align = 'center'),
                           div(p(style = "text-align: font-size = 12px",
                                 "This interactive map shows the results of land prioritization models
                                 developed by Nākoa Farrant, a PhD candidate at the
                                 Bren School of Environemntal Science & Management. The color scheme
                                 for each model outout aligns with the bar graph above, and
                                 a shapefile of Ahupuaʻa's is included below the model results. "))),

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

  tabPanel("Dustin's widget",
           fluidPage(
             fluidRow(
               column(
                 width = 4,
                 offset = 4,
               selectInput("data",
                           "Choose your county",
                           choices = c("Hawai'i",
                                       "Maui County",
                                       "Honolulu County",
                                       "Kaua'i and Ni'ihau")),
               align = "center"
               )

             ),
             fluidRow(
               column(
                 width = 6,
                 offset = 3,
              mainPanel(
                "Landuse Coverage in the Hawai'ian Islands",)),
                column(
                width = 8,
                offset = 2,
                plotOutput(outputId = "landuse_plot"),
                plotOutput(outputId = "county_plot")
                ),
                align = 'center',
                style = 'width = 100%;'
                ))
              ),

  tabPanel("Group widget",
           fluidPage(
             div(
               selectInput("his input here",
                           "Dustin's Widget",
                           choices = c("Survey Sentiment Analysis",
                                       "Keywords for Cultural Sites"))
             )
           )),
  collapsible = TRUE
)

# tabPanel("Pioneer Mill Interview Text Analysis",
#         p('Pioneer Mill, established in 1860, was the first sugar plantation in Hawaii to grow sugarcane commercially. In its peak in the 1960s,
#         it produced 60,000 tones of sugar annually. Pioneer Mill was also famous for erecting what was at the time Hawaiis largest smokestack
#         in 1928. The smokestack used the leftover bagasse as fuel for electricity, and the smoky-sweet emissions became a staple of the local environment.
#         The plantation ceased operations in 1999, and since then the smokestack has been saved as a historic landmark. The following sentiment analysis
#         was conducted using interview data from individuals who grew up on or near the plantation.
#
#         Summary Data Citation:
#         Interview Data Citation:
#         Widget Source Citation
#         NRC Lexicon Citation:'
#               ),
#
#             # end of summary paragraph
#          fluidPage(
#            div(
#              sidebarLayout(
#                sidebarPanel(
#                  selectInput("dataset", "Choose an Interview:",
#                              inputId =
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
#                ), ## end of sidebar panel
#             # Main panel for displaying outputs ----
#             mainPanel(
#
#          # Output: Header ----
#            h4("Sentiment Analysis"),
#           tableOutput("unions_plot"),
#
#
#       # Output: Header + table of distribution ----
#        h4("Observations"),
#         tableOutput("view")
#
#               ) ## end of main panel
#
#
#              ) ## end of sidebar layout
#
#            )), ## end of div and fluid page
# ), ## end of tab panel



