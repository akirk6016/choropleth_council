#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram

navbarPage(
  "This is the shell of our shiny",
  tabPanel("Max's widget",
           fluidPage(
             div(
               selectInput("his input here",
                           "Max's Widget",
                           choices = c("What",
                                       "do",
                                       "you",
                                       "want?"))
             )
           )),

  tabPanel("Abigail's widget",
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
