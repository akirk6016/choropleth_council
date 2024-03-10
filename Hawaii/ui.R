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
           p('The purpose of this shiny app is to visual land use prioritization scenarios
             in Hawaii. We will use various scenarios to map how prioritiziation changes based
             on the order of goal implementation. We will also use spatial data of priority areas
             identfied by residents to compare how the modelled scenarios compare to how people
             perceive priority needs. Additionally, we will use interview data to conduct a sentiment
             analysis on how residents qualified priorty areas. Further analyses will compare relative
             costs of solutions to build a trade-off curve'),

           ),

  tabPanel("Max's widget",
           fluidPage(
             div(
               sidebarLayout(
                 sidebarPanel(style = "max-height: 30vh;",
                              "put my widgets here",
                              radioButtons(
                                inputId = "model_type",
                                label = h4("Choose model type"),
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
                           div("Area (ha) of model ouput on each island", align = 'center'),
                           div(plotOutput("model_plot")), align = 'center')),
                       fluidRow(
                         column(
                           width = 12,
                           offset = 0,
                           div(h3("Interactive Map"), align = 'center'),
                           div(tmapOutput(outputId = "interactive_map")), align = 'center'))
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
