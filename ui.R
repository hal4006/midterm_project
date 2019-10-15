library(shiny)
library(shinythemes)
library(plotly)
library(shinyWidgets)

cuts <- cut_weight$cut

shinyUI(fluidPage(
    #set handwriting-style font
    chooseSliderSkin("Flat"),
    
    #set manhanttan background
    setBackgroundImage(
        src = "https://picsum.photos/id/430/5302/3534"
    ),
    
    #add logo
    #tags$h1 etc. change the size of titles
    titlePanel(
        fluidRow(
            column(12, h1(strong("Happy  Valley  Meat  Advisor"))), 
            column(8, img(height = 60, width = 290, src = "black.png")),
        )
    ),
    
    hr( color="black"),
    
    fluidRow(
        column(6,
               selectInput(
                   inputId = "cuts", label = h4("Cuts of Meat:"), width = "100%", 
                   cuts, selected = cuts[sample(1:36,6)], multiple = T
               )),
        column(3,
               
               numericInput(
                   inputId = "pounds", label = h4("Pounds of Meat (lbs):"), width = "100%",
                   max = 10000, min = 0, value = 1000, step = 10
               ))
    ),
    
    fluidRow(
        column(8, 
               
               #actionButton("all", "Select All")
               #actionButton("clear", "Clear")
               
               actionBttn(
                   inputId = "all",
                   label = "Select All",
                   style = "material-flat",
                   size = "sm"
               ) ,
               
               actionBttn(
                   inputId = "clear",
                   label = "Clear All",
                   style = "material-flat",
                   color = "danger",
                   size = "sm"
               ) 
               
        ),
        
        hr(),
    
 
        
        tabsetPanel(
            type = "tabs",
            tabPanel(
                h4(em("Overview")),
                fluidRow(
                    column(8,plotlyOutput("HVM_bar")),
                    column(4,plotOutput("HVM_wordcloud"))
                ),
                dataTableOutput("HVM_table")
            ),
            tabPanel(
                h4(em("Advisor")),
                sidebarLayout(
                    sidebarPanel(
                        fluidRow(
                            #actionbutton customize         
                            column(6,
                                   
                                   actionBttn(
                                       inputId = "return0_A",
                                       label = "Reset A",
                                       color = "danger",
                                       style = "bordered",
                                       size = "xs"
                                   )),
                            column(6,
                                   
                                   actionBttn(
                                       inputId = "return0_B",
                                       label = "Reset B",
                                       color = "danger",
                                       style = "bordered",
                                       size ="xs"
                                   ))
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
                                em("Visualization"),  icon = icon("globe", lib = "font-awesome"),
                                plotlyOutput("cp_radar"),
                                dataTableOutput("cp_summary"),
                                br(),
                                h4("Interpretation of Water Use"),
                                h5(em("Do you know?")),
                                p(" - The water use of one cow can cover the annual water use of 58 famlies of three."),
                                plotlyOutput("ei_wateruse"),
                                hr(),
                                h4("Interpretation of Land Use"),
                                h5(em("Do you know?")),
                                p(" - The land use of 11 cows can make up a central park."),
                                plotlyOutput("ei_landuse"),
                                hr(),
                                h4("Interpretation of Gas Emission"),
                                h5(em("Do you know?")),
                                p(" - The CO2 emission of one cow can support a typical vehicle to run 115.7 thousand miles."),
                                plotlyOutput("ei_gase")
                                
                            ),
                            
                            tabPanel(
                                em("Table"), icon = icon("table", lib = "font-awesome"),
                                h4("Environmental Impact Interpretations"),
                                dataTableOutput("ei_summary"), hr(),
                                h4("Datatable for Set with Same Quantity"),
                                dataTableOutput("cp0_table"), hr(),
                                h4("Datatable for Set A"),
                                dataTableOutput("cpA_table"), hr(),
                                h4("Datatable for Set B"),
                                dataTableOutput("cpB_table")
                                
                            )
                        )
                    )
                )
            )
        )
    )))