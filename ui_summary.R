library(shiny)

cuts <- cut_weight$cut
sample_value <- sample(0:3000,6)

cutinput <- function(x) {
    column(6,
              selectInput(inputId = paste0("cut_",x),
                          label = paste0("Cut ", LETTERS[x],":"),
                          choices = cuts, selected = cuts[x], selectize = TRUE),
              sliderInput(inputId = paste0("quantity_",x),
                          label = p("Pounds"),
                          min = 0, max = 3000, value = sample_value[x], post = "lb")
      )
}

shinyUI(fluidPage(
    
    titlePanel("Carcass Calculator"),
    
    sidebarLayout(
      sidebarPanel(
        fluidRow(cutinput(1), cutinput(2)), hr(),
        fluidRow(cutinput(3), cutinput(4)), hr(),
        fluidRow(cutinput(5), cutinput(6))
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Summary", 
                             plotlyOutput("plot_cut_package"),
                             fluidRow(
                               column(3, 
                                 h3("Total Heads:"),
                                 tableOutput(outputId = "total_heads")
                               ),
                               column(3,offset = 1 ,
                                 h3("Resource Use:"),
                                 tableOutput(outputId = "resource_use")
                               ),
                               column(3, offset = 1,
                                 h3("Gas Emission:"),
                                 tableOutput(outputId = "gas_emission")
                               )
                             ),
                             fluidRow(
                               DT::dataTableOutput("table_cut_package")
                             ),
                             
                             tabPanel("Comparison"),
                             tabPanel("Overview")
      )
    ))
    )))
    
        