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


# Define UI for application that draws a histogram

navbarPage(
  "This is the shell of our shiny",

  theme = bslib::bs_theme(bootswatch = "morph"),

  tabPanel("Overview",
           p('The purpose of this shiny app is to visual land use prioritization scenarios
             in Hawaii. We will use various scenarios to map how prioritiziation changes based
             on the order of goal implementation. We will also use spatial data of priority areas
             identfied by residents to compare how the modelled scenarios compare to how people
             perceive priority needs.'),

           ),

  tabPanel("Max's widget",
           fluidPage(
             div(
               selectInput("his input here",
                           "Max's Widget",
                           choices = c("What",
                                       "do",
                                       "you",
                                       "want?"))
             ),

             checkboxGroupInput("checkGroup", label = h3("Model Parameters"),
                                choices = list("Food Production" = 1,
                                               "Carbon Storage" = 2,
                                               "Choice 3" = 3),
                                selected = 1),


             hr(),
             fluidRow(column(3, verbatimTextOutput("value")))

           )), #end of fluidPage

  tabPanel("Abigail's Cost Tradeoffs Widget",
           fluidPage(
             div(
               selectInput("her_input_here",
                           "Abigail's Widget",
                           choices = c("What",
                                       "do",
                                       "you",
                                       "want?"))
             )
           )),

  tabPanel("Dustin's widget",
           fluidPage(
             div(
               selectInput("his input here",
                           "Dustin's Widget",
                           choices = c("What",
                                       "do",
                                       "you",
                                       "want?"))
             )
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
