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
    # Title
    titlePanel("Find the right dataset inside an R packages"),

    # Sidebar with inputs and search options
    sidebarLayout(
        sidebarPanel(
            shiny::headerPanel("Instructions"),
            shiny::div("You can use this tool to search for all datasets provided with a package. Fill in the package name and add searchstrings if necessary. This tool will do its best to give you the Observations and Variables per dataset.", style = "text-align: justify"),
            shiny::div("However due to the nature of the datasets you always need to verify the suggested dataset fit your needs.", style = "text-align: justify"),
            shiny::headerPanel("Parameters"),
            shiny::textInput(inputId = "package", label = "Package name",
                             value = "datasets", placeholder = "example: datasets"),
            shiny::textInput(inputId = "filterClass", label = "Textfilter for class", value = "",
                             placeholder = "example: data.frame"),
            shiny::textInput(inputId = "filterName", label = "Textfilter for name/description", value = "",
                             placeholder = "example: Air"),
            shiny::sliderInput(inputId = "NbrObs", label = "Number of observations", min = 1,
                               max = 100000, value = c(1, 1000000)),
            shiny::sliderInput(inputId = "NbrVars", label = "Number of variables", min = 1,
                               max = 1000, value = c(1, 1000)),
            shiny::submitButton(text = "Search for datasets")
        ),
        
        # Output found datasets if any
        mainPanel(
            plotOutput("plot", height="800px"),
            shiny::dataTableOutput("table")
        )
    )
))
