library(shiny)
library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(DT)
library(plotly)
library(scales)
library(wordcloud)
library(RColorBrewer)
library(shinythemes)
library(shinyWidgets)

#setwd('C:/Users/16462/OneDrive/Documents/Weill Cornell/Data Science I/Midterm/midterm_project-master_101219H/midterm_project-master_101519/HVMC_CullConserve')

#cut_weight <- read_excel("C:/Users/lhj98/Desktop/DataSci1/Midterm_Project/Datasets/carcass_calculator_data.xlsx")
#cut_weight <-read_excel('C:/Users/16462/OneDrive/Documents/Weill Cornell/Data Science I/Midterm/midterm_project-master_101219H/carcass_calculator_data.xlsx')

cut_weight <-readxl::read_excel("carcass_calculator_data.xlsx") #reading in excel file
cuts<- cut_weight$cut #subsetting cut

# Calculation for environmental impact
#http://www.fao.org/gleam/resources/en/
#http://gleami.org/
#GLEAM is an agriculture modeling tool provided by the United Nations 
water.rate <- 6.355078 #(million gallons) 
land.rate <- 77 #(acres)
## Calculate CH4 and NO2 emission using data according to GLEAM Model by FAO/UN
total.co2 <- 2119809499787 #kg/year
total.ch4 <- 199979479648 #kg/year
total.no2 <- 14908328599 #kg/year
co2.e <- 102.959 #(thousand lbs)
ch4.e <- total.ch4 / total.co2 * co2.e #scaling ch4 emissions from GLEAM model to co2 emissions units 
no2.e <- total.no2 / total.co2 * co2.e #scaling no2 emissions from GLEAM model to co2 emission units
## Set a function to add environment impact to data frame
numeric_format <- function(x){ #numeric format function
    as.numeric(round(x,2))
}
environment_impact <- function(df, var) { #creating a function for environmental impact * # of cows required per lbs of cuts
    df <- df %>%
        mutate(Wateruse = numeric_format(water.rate * var),
               Landuse = numeric_format(land.rate * var),
               CO2e = numeric_format(co2.e * var),
               CH4e = numeric_format(ch4.e * var),
               NO2e = numeric_format(no2.e * var))
    return(as.data.frame(df))
}

# Interpretation for environmental impact
## Wateruse
### Estimates vary, but each person uses about 80-100 gallons of water per day.
### The average number of family members in the US is 3.14 in 2018.
#-- The water use volumn for an average family in a whole year is 114,610 gallons.
water.i <- 0.11 #(million gallons)

## Landuse
### The area of central park is 1.317 mi2.
### 1 mi2 equals to 640 acres.
#-- The area of central park is 842.88 acres.
land.i <- 842.88 #(acres)

## Gasemission
### The average gasoline vehicle on the road today has a fuel economy of about 22.0 miles per gallon.
### Every gallon of gasoline burned creates about 8,887 grams of CO2.
#-- A typical vehicle emits 0.89 Klbs CO2 per thousand miles.
gase.i <- 0.89 #(Klbs/Kmi)

# Set a function to calculate the number of cows according to inputs
heads_calculator <- function(vec1, vec2) {
    N <- length(vec1)
    Category <- NULL
    Weight_per_Head <- NULL
    Quantity <- NULL
    for(i in 1:N) {
        Category = c(Category, vec1[i])
        Weight_per_Head = c(Weight_per_Head, cut_weight$total_weight[which(cut_weight$cut==vec1[i])])
        Quantity = c(Quantity, vec2[i])
    }
    heads_summary <- data.frame(Category, Weight_per_Head, Quantity)
    heads_summary <- heads_summary %>% mutate(Heads = ceiling(Quantity/Weight_per_Head))
    return(heads_summary)
}

sub6 <- function(x) {
    n <- length(x)
    y <- if(n >= 6) {x[1:6]} else {x}
    return(y)
}

shinyServer(function(input, output, clientData, session){
    
    observeEvent(input$all,{ #creating user update event
        updateSelectInput(session, inputId = "cuts", label = "Cuts of Meat",
                          cuts, selected = cuts)
    })
    observeEvent(input$clear,{ #creating another user update event
        updateSelectInput(session, inputId = "cuts", label = "Cuts of Meat",
                          cuts, selected = NULL)
    })
    
    HVM <- reactive({ #creating reactive expressions for dynamic updates
        HVM <- cut_weight %>%
            filter(cut %in% input$cuts) %>% #filtering based on user input of cuts
            mutate(number.cows = ceiling(input$pounds / total_weight)) #rounding up to the next cow
        environment_impact(HVM, HVM$number.cows) #total environmental impact based on number of cows
    })
    
    output$HVM_bar <- renderPlotly({ #plotly plot for bar chart 
        bar <-  HVM() %>%
            ggplot(aes(x = cut, y = number.cows, fill = cut)) + #x axis is the cuts selected, y axis is the number of cows
            theme(axis.text = element_text(size=12),
                  axis.title = element_text(size=12),
                  plot.title = element_text(size = 12),
                  legend.position = "none",
                  panel.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'), #removing panel background
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            scale_y_continuous(breaks = pretty_breaks()) +
            ylab('Number of Cows') +
            xlab('Cuts of Cow') +
            ggtitle('How Many Cows Are You Killing?') +
            geom_bar(stat = "identity") +
            coord_flip() #coordinate flip for visuals
            
        
        ggplotly(bar)
    })
    
    pal <- brewer.pal(12,"Paired") #creating ColorBrewer wordcloud
    output$HVM_wordcloud <- renderPlot({
        wordcloud(words = HVM()$cut, freq = HVM()$Landuse,
                  min.freq = 1, rot.per=.15, random.order = FALSE, colors = pal)
    })
    
    
    output$HVM_table <- DT::renderDataTable({ #datatable to show environmental impact based on user defined cuts and lbs. 
        datatable(HVM(), options = list(pageLength = 10, lengthMenu = c(10, 30, 60)),
                  colnames = c('Cut', 'Weight per Cow (lbs)', 'Number of Cows', 'Water Use (milgal)', 'Land Use (acres)', 'CO2 (Klbs)', 'CH4 (Klbs)', 'NO2 (Klbs)'))
    })
    
    observeEvent(input$return0_A, {
        for(i in 1:6) {
            updateSliderInput(session, inputId = paste0("qty_A",i), value = 0) #slider for Purchase A
        }
    })
    
    observeEvent(input$return0_B, {
        for(i in 1:6) {
            updateSliderInput(session, inputId = paste0("qty_B",i), value = 0) #slider for Purchase B
        }
    })
    
    observe({
        x <- input$cuts
        y <- input$pounds
        range <- round(y*.5):round(y*1.5)
        if(length(x) >= 6) {
            for(i in 1:6){ #defining maximum amount of cuts allowed for selection
                updateSliderInput(session, inputId = paste0("qty_A", i), #Purchase A
                                  label = x[i], value = sample(range, 1))
                updateSliderInput(session, inputId = paste0("qty_B", i), #Purchase B
                                  label = x[i], value = 0)
            }
        } else if(length(x) > 0) {
            for(i in 1:length(x)){
                updateSliderInput(session, inputId = paste0("qty_A", i),
                                  label = x[i], value = sample(range, 1))
                updateSliderInput(session, inputId = paste0("qty_B", i),
                                  label = x[i], value = 0)
            }
            for(i in (length(x)+1):6){
                updateSliderInput(session, inputId = paste0("qty_A", i),
                                  label = "Not Selected", value = 0)
                updateSliderInput(session, inputId = paste0("qty_B", i),
                                  label = "Not Selected", value = 0)
            }
        } else {
            for(i in 1:6){
                updateSliderInput(session, inputId = paste0("qty_A", i),
                                  label = "Not Selected", value = 0)
                updateSliderInput(session, inputId = paste0("qty_B", i),
                                  label = "Not Selected", value = 0)
            }
        }
    })
    
    CP <- reactive({ #reactive expression based on user-defined cuts and lbs. 
        Set_cut <- sub6(input$cuts)
        len <- length(Set_cut)
        Set0_qty <- rep(input$pounds, length(Set_cut))
        Set0 <- heads_calculator(Set_cut, Set0_qty)
        
        SetA_qty <- c(input$qty_A1,input$qty_A2,input$qty_A3,input$qty_A4,input$qty_A5,input$qty_A6)[1:len] #Total Purchase A quantity
        SetA <- heads_calculator(Set_cut, SetA_qty)
        
        SetB_qty <- c(input$qty_B1,input$qty_B2,input$qty_B3,input$qty_B4,input$qty_B5,input$qty_B6)[1:len] #Total Purchase B quantity
        SetB <- heads_calculator(Set_cut, SetB_qty)
        
        Set_all <- as.data.frame(cbind(Purchases = c(rep("Original Set (same quantity (lbs)) for each cut", len), rep("Purchase A", len), rep("Purchase B", len)),
                                       rbind(Set0, SetA, SetB))) #labels for each group of user-definied cuts and lbs
        environment_impact(Set_all, Set_all[,5])
    })
    NCuts <- reactive({length(input$cuts)}) #reactive expression based on user-defeined cuts
    SelectedCuts <- reactive({sub6(input$cuts)})
    Set_Len <- reactive({length(sub6(input$cuts))})
    output$cp_radar <- renderPlotly({ #radar chart to show "balancing" of number of cows required for purchases
        if(NCuts() > 2){
            plot_ly(
                type = "scatterpolar",
                fill = "none",
                linetype = I("twodash")
            ) %>%
                add_trace(
                    r = CP()[1:Set_Len(),5],
                    theta = SelectedCuts(),
                    name = "Original Set (same quantity (lbs)) for each cut" #original, static lb selection for all cuts
                ) %>%
                add_trace(
                    r = CP()[(Set_Len()+1):(2*Set_Len()),5],
                    theta = SelectedCuts(),
                    name = "Purchase A" #dynamic lb selection per cut
                ) %>%
                add_trace(
                    r = CP()[(2*Set_Len()+1):(3*Set_Len()),5],
                    theta = SelectedCuts(),
                    name = "Purchase B" #dynamic lb selection per cut
                ) %>%
                layout(
                    polar = list(
                        radialaxis = list(
                            visible = T,
                            range = c(0, max(CP()[,5]))
                        )
                    ),
                    showlegend = T
                )
        } else {
            bar <- CP() %>% #plotting plotly bar chart based on all "sets" of purchases
                ggplot(aes(x = Category, y = Heads, fill = factor(Purchases, levels = c("Original Set (same quantity (lbs)) for each cut", "Purchase A", "Purchase B")))) +
                geom_bar(stat = "identity", position = "dodge") +
                theme(axis.text = element_text(size=12),
                      axis.title = element_text(size=12),
                      plot.title = element_text(size = 12),
                      panel.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank()) +
                scale_y_continuous(breaks = pretty_breaks())
            
            ggplotly(bar, tooltip = c("x","y"))
        }
    })
    
    CP_0 <- reactive({
        Set_cut <- sub6(input$cuts)
        Set0_qty <- rep(input$pounds, length(Set_cut))
        Set0 <- heads_calculator(Set_cut, Set0_qty)
        environment_impact(Set0, Set0[,4])
    })
    
    CP_A <- reactive({
        Set_cut <- sub6(input$cuts)
        SetA_qty <- c(input$qty_A1,input$qty_A2,input$qty_A3,input$qty_A4,input$qty_A5,input$qty_A6)
        SetA <- heads_calculator(Set_cut, SetA_qty)
        environment_impact(SetA, SetA[,4])
    })
    
    CP_B <- reactive({
        Set_cut <- sub6(input$cuts)
        SetB_qty <- c(input$qty_B1,input$qty_B2,input$qty_B3,input$qty_B4,input$qty_B5,input$qty_B6)
        SetB <- heads_calculator(Set_cut, SetB_qty)
        environment_impact(SetB, SetB[,4])
    })
    
    output$cp0_table <- DT::renderDataTable({ #datatable for environmental impact of original, static lbs of cuts
        datatable(CP_0()[-2], options = list(paging = F, searching = F),
                  colnames = c('Cut', 'Quantity (lbs)', 'Number of Cows', 'Water Use (milgal)', 'Land Use (acres)', 'CO2 (Klbs)', 'CH4 (Klbs)', 'NO2 (Klbs)'))
    })
    output$cpA_table <- DT::renderDataTable({ #datatable for environmental impact of Purchase A
        datatable(CP_A()[-2], options = list(paging = F, searching = F),
                  colnames = c('Cut', 'Quantity (lbs)', 'Number of Cows', 'Water Use (milgal)', 'Land Use (acres)', 'CO2 (Klbs)', 'CH4 (Klbs)', 'NO2 (Klbs)'))
    })
    output$cpB_table <- DT::renderDataTable({ #datatable for environmental impact of Purchase B
        datatable(CP_B()[-2], options = list(paging = F, searching = F),
                  colnames = c('Cut', 'Quantity (lbs)', 'Number of Cows', 'Water Use (milgal)', 'Land Use (acres)', 'CO2 (Klbs)', 'CH4 (Klbs)', 'NO2 (Klbs)'))
    })
    
    CP_summary <- reactive({ #reactive expression summarizing user inputs
        Set_cut <- sub6(input$cuts)
        Set0_qty <- rep(input$pounds, length(Set_cut))
        Set0 <- heads_calculator(Set_cut, Set0_qty)
        summary_0 <- c(sum(Set0[,3]), sum(Set0[,4]))
        
        SetA_qty <- c(input$qty_A1,input$qty_A2,input$qty_A3,input$qty_A4,input$qty_A5,input$qty_A6)
        SetA <- heads_calculator(Set_cut, SetA_qty)
        summary_A <- c(sum(SetA[,3]), sum(SetA[,4]))
        
        SetB_qty <- c(input$qty_B1,input$qty_B2,input$qty_B3,input$qty_B4,input$qty_B5,input$qty_B6)
        SetB <- heads_calculator(Set_cut, SetB_qty)
        summary_B <- c(sum(SetB[,3]), sum(SetB[,4]))
        
        summary_all <- data.frame(rbind(summary_0, summary_A, summary_B))
        environment_impact(summary_all, summary_all[,2])
    })
    
    output$cp_summary <- DT::renderDataTable({ #summary table render
        datatable(CP_summary(),
                  colnames = c('Total Quantity (lbs)', 'Total Number of Cows', 'Water Use (milgal)', 'Land Use (acres)', 'CO2 (Klbs)', 'CH4 (Klbs)', 'NO2 (Klbs)'),
                  rownames = c("Datatable for Original Set (same quantity (lbs)) for each cut", "Purchase A", "Purchase B"),
                  options = list(paging = F, searching = F))
    })
    
    output$ei_summary <- DT::renderDataTable({ #datatable showing static units in which all calculations were made from
        ei_summary <- data.frame(
            EI = c("Water Use","Land Use","Gas Emission"),
            Cow.rate = c(6.36, 77, 102.96),
            Refer = c("Annual Water Use of a US Family of 3",
                      "Area of Central Park",
                      "CO2 Emisson per Thousand Miles for a Typical Vehicle"),
            Value = c(0.11, 842.88, 0.89),
            Unit = c("Million Gallon", "Acres", "Thousand Lbs"),
            Interpretation = c("1 Cow for 58 Famlies",
                               "11 Cows for Central Park",
                               "1 Cow for 115.7 Thousand Miles Ride")
        )
        datatable(ei_summary, options = list(paging = F, searching = F),
                  colnames = c("Environmental Impact", "Use/Emission per Cow", "Reference", "Value", "Unit", "Interpretation"))
    })
    
    output$ei_wateruse <- renderPlotly({ #plotly plot of water usage. Units are in "number of families' annual water usage" based on cows required for purchase
        wateruse <- CP() %>%
            ggplot(aes(x = Purchases, y = Wateruse/water.i, fill = Category)) +
            geom_bar(stat = "identity", position = "stack") +
            theme(axis.text = element_text(size=12),
                  axis.title = element_text(size=12),
                  plot.title = element_text(size = 12),
                  panel.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            ylab("Number of Families' Annual Water Usage") +
            ggtitle("Number of Families Your Purchase Can Provide Water For!") +
            scale_y_continuous(breaks = pretty_breaks())+
            scale_fill_brewer(palette = "Blues")
        ggplotly(wateruse)
    })
    
    output$ei_landuse <- renderPlotly({ ##plotly plot of land usage. Units are in "number of Central Park equivalents" based on cows required for purchase and land acreage requirements
        landuse <- CP() %>%
            ggplot(aes(x = Purchases, y = Landuse/land.i, fill = Category)) +
            geom_bar(stat = "identity", position = "stack") +
            theme(axis.text = element_text(size=12),
                  axis.title = element_text(size=12),
                  plot.title = element_text(size = 12),
                  panel.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            ylab("Number of Central Parks") +
            ggtitle("Number of Central Park Equivalents Your Purchase Requires!") +
            scale_y_continuous(breaks = pretty_breaks()) +
            scale_fill_brewer(palette = "Oranges")
        ggplotly(landuse)
    })
    
    output$ei_gase <- renderPlotly(({ #plotly plot of "equivalent car miles (k miles)" of CO2 emissions based on number of cows required for purchase
        gase <- CP() %>%
            ggplot(aes(x = Purchases, y = CO2e/gase.i, fill = Category)) +
            geom_bar(stat = "identity", position = "stack") +
            theme(axis.text = element_text(size=12),
                  axis.title = element_text(size=12),
                  plot.title = element_text(size = 12),
                  panel.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            ylab("Equivalent Car Miles (k miles)") +
            ggtitle("Number of Car Miles (k miles) Your Purchase Is Equivalent To!") +
            scale_y_continuous(breaks = pretty_breaks()) +
            scale_fill_brewer(palette = "Purples")
        ggplotly(gase)
    }))
})

Sys.setlocale('LC_ALL','C')
options(DT.fillContainer = FALSE) 
options(DT.autoHideNavigation = FALSE) 

