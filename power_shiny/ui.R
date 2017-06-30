#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Power Calculation"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("alt",
                  "Alternative:",
                  choices = c("Left" = "left", "two", "right")),
      conditionalPanel(
        condition = "input.alt == 'left'",
        sliderInput("n",
                    "Sample Size:",
                    min = 1,
                    max = 100,
                    value = 10),
        sliderInput("alpha",
                    "Significance level:",
                    min = .01,
                    max = .3,
                    value = .05,
                    round = -2),
        sliderInput("delta",
                    "Practical Difference:",
                    min = .1,
                    max = 6,
                    value = 1,
                    round = -1),
        sliderInput("sigma",
                    "Sigma:",
                    min = .1,
                    max = 6,
                    value = 1,
                    round = -1)
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("power_plot", height = "290px", width = "80%"),
       plotOutput("power_plot2", height = "290px", width = "80%"),
       tableOutput("pwr_tbl")
    )
  )
))
