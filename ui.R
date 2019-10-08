library(shiny)

cuts <- HVM$cut

shinyUI(fluidPage(

    titlePanel("Carcass Calculator"),

    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "pounds_of_meat",
                         label = "Pounds of Meat:",
                         value = 1000,
                         min = 0,
                         max = 100000000),
            checkboxGroupInput(inputId = "cut",
                               label = "Cuts of Meat",
                               cuts,
                               selected = c("Blended Burger", "Ground Beef"))
        ),

        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Table", plotOutput("HVM_Table")),
                        tabPanel("Visualize the Data", tableOutput("HVM_Plot")))
        )
    )
))
