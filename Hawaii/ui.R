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
                 sidebarPanel("put my widgets here",

                  radioButtons(
                    inputId = "model_type",
                    label = h4("Choose model type"),
                    choices = c("Prioritize carbon storage" = "Carbon",
                                "Prioritize food production" = "Food")
                  ) ## end of radio buttons
                  ),
                 mainPanel("Area (ha) of model ouput on each island",
                           plotOutput("model_plot"),
                           h4("Interactive Map"),
                           tmapOutput(outputId = "interactive_map")
                 ) ## end of main panel

               ) ## end of sidebar Layout

             ), ## end of div()

           ) ## end of fluidPage

        ), ## end of tab panel

  tabPanel("Abigail's Widget",
           fluidPage(
             div(
               sidebarLayout(
                 sidebarPanel(
               sliderInput("decimal", "Comparative Cost of Food to Carbon",
                           min = 0, max = 3,
                           value = 0.5, step = 0.1),
             ),
             mainPanel("Cost Trade-offs",

                       tableOutput("abigail_plot"))
             )

             )

           )),

  tabPanel("Dustin's widget",
           fluidPage(
             div(
               selectInput("data",
                           "Dustin's Widget",
                           choices = c("Ni'ihau",
                                       "Kaua'i",
                                       "Moloka'i",
                                       "O'ahu",
                                       "Hawai'i",
                                       "Molokini",
                                       "Kaho'olawe",
                                       "Maui",
                                       "Lāna'i"))

             ),
             mainPanel("Title",
                       plotOutput(outputId = "dustin_plot"))
           )),

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
