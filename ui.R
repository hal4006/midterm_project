library(shiny)
library(shinythemes)
library(plotly)
library(shinyWidgets)

cuts <- cut_weight$cut

shinyUI(fluidPage(
#set handwriting-style font
theme ="bootstraphand.css",

#set manhanttan background
setBackgroundImage(
  src = "https://picsum.photos/id/430/5302/3534"
),

#add logo
#tags$h1 etc. change the size of titles
titlePanel(
  fluidRow(
    column(4, h1(strong("Happy  Valley  Meat  Advisor"))), 
    column(8, img(height = 60, width = 290, src = "black.png"))
  )
),

hr( color="black"),

  fluidRow(
    column(8,
           selectInput(
             inputId = "cuts", label = h1("Cuts of Meat"), width = "100%", 
             cuts, selected = cuts[sample(1:36,6)], multiple = T
           ))
  ),

  fluidRow(
    column(8, 
           
           #actionButton("all", "Select All")
           #actionButton("clear", "Clear")
           tags$br(),
           actionBttn(
             inputId = "all",
             label = "Select All",
             color = "primary",
             style = "stretch",
             size = "lg"
           ) ,
          
           actionBttn(
             inputId = "clear",
             label = "Clear All",
             color = "success",
             style = "stretch",
             size = "lg"
           ) 
           
  ),
  
  hr(),
  
  fluidRow(
    column(6,
           numericInput(
             inputId = "pounds", label = h2("Pounds of Meat (lbs)"),
             max = 10000, min = 0, value = 1000, step = 10
           ))
  ),
  
  tabsetPanel(
    type = "tabs",
    tabPanel(
      h3("Overview"),
      plotlyOutput("HVM_bar"),
      plotOutput("HVM_wordcloud"),
      dataTableOutput("HVM_table")
    ),
    tabPanel(
      h3("Advisor"),
      sidebarLayout(
        sidebarPanel(
          fluidRow(
   #actionbutton customize         
            tags$br(),
            actionBttn(
              inputId = "return0_A",
              label = "Reset A",
              color = "success",
              style = "stretch",
              size = "lg"
            ),

            tags$br(),
            actionBttn(
              inputId = "return0_B",
              label = "Reset B",
              color = "success",
              style = "stretch",
              size ="lg"
            )

          ),
   
          hr(),
   
          fluidRow(
            column(6, sliderInput("qty_A1", "cut_A1", post = "lbs",
                                  value = sample(0:5000, 1), min = 0, max = 5000)),
            column(6, sliderInput("qty_B1", "cut_B1", post = "lbs",
                                  value = 0, min = 0, max = 5000))
          ),
          fluidRow(
            column(6, sliderInput("qty_A2", "cut_A2", post = "lbs",
                                  value = sample(0:5000, 1), min = 0, max = 5000)),
            column(6, sliderInput("qty_B2", "cut_B2", post = "lbs",
                                  value = 0, min = 0, max = 5000))
          ),
          fluidRow(
            column(6, sliderInput("qty_A3", "cut_A3", post = "lbs",
                                  value = sample(0:5000, 1), min = 0, max = 5000)),
            column(6, sliderInput("qty_B3", "cut_B3", post = "lbs",
                                  value = 0, min = 0, max = 5000))
          ),
          fluidRow(
            column(6, sliderInput("qty_A4", "cut_A4", post = "lbs",
                                  value = sample(0:5000, 1), min = 0, max = 5000)),
            column(6, sliderInput("qty_B4", "cut_B4", post = "lbs",
                                  value = 0, min = 0, max = 5000))
          ),
          fluidRow(
            column(6, sliderInput("qty_A5", "cut_A5", post = "lbs",
                                  value = sample(0:5000, 1), min = 0, max = 5000)),
            column(6, sliderInput("qty_B5", "cut_B5", post = "lbs",
                                  value = 0, min = 0, max = 5000))
          ),
          fluidRow(
            column(6, sliderInput("qty_A6", "cut_A6", post = "lbs",
                                  value = sample(0:5000, 1), min = 0, max = 5000)),
            column(6, sliderInput("qty_B6", "cut_B6", post = "lbs",
                                  value = 0, min = 0, max = 5000))
          )
        ),
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Visualization",
              plotlyOutput("cp_radar"),
              dataTableOutput("cp_summary"),
              h3("Interpretation of Water Use"),
              fluidRow(
                column(4, plotlyOutput("ei_wateruse_0")),
                column(4, plotlyOutput("ei_wateruse_A")),
                column(4, plotlyOutput("ei_wateruse_B"))
              ),
              hr(),
              h3("Interpretation of Land Use"),
              plotlyOutput("ei_landuse"),
              hr(),
              h3("Interpretation of Gas Emission"),
              plotlyOutput("ei_gase")
    
              ),
            
            tabPanel(
              "Table",
              h3("Datatable for Set with Same Quantity"),
              dataTableOutput("cp0_table"), hr(),
              h3("Datatable for Set A"),
              dataTableOutput("cpA_table"), hr(),
              h3("Datatable for Set B"),
              dataTableOutput("cpB_table")

            )
          )
        )
      )
    )
  )
)))