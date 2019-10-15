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

cut_weight <- read_excel("C:/Users/lhj98/Desktop/DataSci1/Midterm_Project/Datasets/carcass_calculator_data.xlsx")
#cut_weight <-read_excel('C:/Users/16462/OneDrive/Documents/Weill Cornell/Data Science I/Midterm/midterm_project-master_101219H/carcass_calculator_data.xlsx')


# Calculation for environmental impact
water.rate <- 6.355078 #(million gallons)
land.rate <- 77 #(acres)
## Calculate CH4 and NO2 emission using data according to Gleam Model by FAO
total.co2 <- 2119809499787 #kg/year
total.ch4 <- 199979479648 #kg/year
total.no2 <- 14908328599 #kg/year
co2.e <- 102.959 #(thousand lbs)
ch4.e <- total.ch4 / total.co2 * co2.e
no2.e <- total.no2 / total.co2 * co2.e
## Set a function to add environment impact to data frame
numeric_format <- function(x) {
    as.numeric(round(x,2))
}
environment_impact <- function(df, var) {
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
### The population of NYC in 2017 is 8.623 million.
#-- The water use volumn for an average family in a whole year is 114,610 gallons.
#-- The water use volumn for the whole NYC in a day is 862.3 million gallons.
water.inter <- 0.11461

## Landuse
### The area of central park is 1.317 mi2, NYC is 468 mi2.
### 1 mi2 equals to 640 acres.
#-- The area of central park is 842.88 acres, NYC is 299520.

## Gasemission
### A typical passenger vehicle emits about 4.6 metric tons of carbon dioxide per year.
### Average vehicle age in the U.S. is projected to increase to 11.8 years by 2019.
### The average gasoline vehicle on the road today has a fuel economy of about 22.0 miles per gallon.
### Every gallon of gasoline burned creates about 8,887 grams of CO2.
### There are 6,074 miles of road in NYC.
#-- A typical passenger vehicle emits about 10.14 thousand pounds of carbon dioxide per year.
#-- Emits about 119.652 thousand pounds of CO2 in the years of use.
#-- The estimated CO2 emission for a typical vehicle to cover the roads of NYC is 5.409 thousand pounds.


# Set a function to calculate the heads according to inputs
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
    
    observeEvent(input$all,{
        updateSelectInput(session, inputId = "cuts", label = "Cuts of Meat",
                          cuts, selected = cuts)
    })
    observeEvent(input$clear,{
        updateSelectInput(session, inputId = "cuts", label = "Cuts of Meat",
                          cuts, selected = NULL)
    })
    
    HVM <- reactive({
        HVM <- cut_weight %>%
            filter(cut %in% input$cuts) %>%
            mutate(number.cows = ceiling(input$pounds / total_weight))
        environment_impact(HVM, HVM$number.cows)
    })
    
    output$HVM_bar <- renderPlotly({
        bar <-  HVM() %>%
            ggplot(aes(x = cut, y = number.cows, fill = cut)) +
            theme(axis.text = element_text(size=12),
                  axis.title = element_text(size=12),
                  plot.title = element_text(size = 12),
                  legend.position = "none",
                  panel.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            scale_y_continuous(breaks = pretty_breaks()) +
            ylab('Number of Cows') +
            xlab('Cuts of Cow') +
            ggtitle('How Many Cows Are You Killing?') +
            geom_bar(stat = "identity") +
            coord_flip()
        
        ggplotly(bar)
    })
    
    pal <- brewer.pal(12,"Paired")
    output$HVM_wordcloud <- renderPlot({
        wordcloud(words = HVM()$cut, freq = HVM()$Landuse,
                  min.freq = 1, rot.per=.15, random.order = FALSE, colors = pal)
    })
    
    
    output$HVM_table <- renderDataTable({
        datatable(HVM(), options = list(pageLength = 10, lengthMenu = c(10, 30, 60)))
    })
    
    observeEvent(input$return0_A, {
        for(i in 1:6) {
            updateSliderInput(session, inputId = paste0("qty_A",i), value = 0)
        }
    })
    
    observeEvent(input$return0_B, {
        for(i in 1:6) {
            updateSliderInput(session, inputId = paste0("qty_B",i), value = 0)
        }
    })
    
    observe({
        x <- input$cuts
        y <- input$pounds
        range <- round(y*.5):round(y*1.5)
        if(length(x) >= 6) {
            for(i in 1:6){
                updateSliderInput(session, inputId = paste0("qty_A", i),
                                  label = x[i], value = sample(range, 1))
                updateSliderInput(session, inputId = paste0("qty_B", i),
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
    
    CP <- reactive({
        Set_cut <- sub6(input$cuts)
        len <- length(Set_cut)
        Set0_qty <- rep(input$pounds, length(Set_cut))
        Set0 <- heads_calculator(Set_cut, Set0_qty)
        
        SetA_qty <- c(input$qty_A1,input$qty_A2,input$qty_A3,input$qty_A4,input$qty_A5,input$qty_A6)[1:len]
        SetA <- heads_calculator(Set_cut, SetA_qty)
        
        SetB_qty <- c(input$qty_B1,input$qty_B2,input$qty_B3,input$qty_B4,input$qty_B5,input$qty_B6)[1:len]
        SetB <- heads_calculator(Set_cut, SetB_qty)
        
        Set_all <- as.data.frame(cbind(Set = c(rep("Set with same quantity", len), rep("Set A", len), rep("Set B", len)),
                            rbind(Set0, SetA, SetB)))
        environment_impact(Set_all, Set_all[,5])
    })
    NCuts <- reactive({length(input$cuts)})
    SelectedCuts <- reactive({sub6(input$cuts)})
    Set_Len <- reactive({length(sub6(input$cuts))})
    output$cp_radar <- renderPlotly({
        if(NCuts() > 2){
            plot_ly(
                type = "scatterpolar",
                fill = "none",
                linetype = I("twodash")
            ) %>%
                add_trace(
                    r = CP()[1:Set_Len(),5],
                    theta = SelectedCuts(),
                    name = "Set with same quantity"
                ) %>%
                add_trace(
                    r = CP()[(Set_Len()+1):(2*Set_Len()),5],
                    theta = SelectedCuts(),
                    name = "Set A"
                ) %>%
                add_trace(
                    r = CP()[(2*Set_Len()+1):(3*Set_Len()),5],
                    theta = SelectedCuts(),
                    name = "Set B"
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
            bar <- CP() %>%
                ggplot(aes(x = Category, y = Heads, fill = factor(Set, levels = c("Set with same quantity", "Set A", "Set B")))) +
                geom_bar(stat = "identity", position = "dodge") +
                theme(axis.text = element_text(size=12),
                      axis.title = element_text(size=12),
                      plot.title = element_text(size = 12),
                      panel.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank()) +
                scale_fill_discrete(name = "Sets") +
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
    
    output$cp0_table <- renderDataTable({
        datatable(CP_0()[-2], options = list(paging = F, searching = F),
                  colnames = c('Cut', 'Quantity (lbs)', 'Number of Cows', 'Water Use (mil gal)', 'Land Use (acres)', 'CO2 (k lbs)', 'CH4 (k lbs)', 'NO2 (k lbs)'))
    })
    output$cpA_table <- renderDataTable({
        datatable(CP_A()[-2], options = list(paging = F, searching = F),
                  colnames = c('Cut', 'Quantity (lbs)', 'Number of Cows', 'Water Use (mil gal)', 'Land Use (acres)', 'CO2 (k lbs)', 'CH4 (k lbs)', 'NO2 (k lbs)'))
    })
    output$cpB_table <- renderDataTable({
        datatable(CP_B()[-2], options = list(paging = F, searching = F),
                  colnames = c('Cut', 'Quantity (lbs)', 'Number of Cows', 'Water Use (mil gal)', 'Land Use (acres)', 'CO2 (k lbs)', 'CH4 (k lbs)', 'NO2 (k lbs)'))
    })
    
    CP_summary <- reactive({
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
    
    output$cp_summary <- renderDataTable({
        datatable(CP_summary(),
                  colnames = c('Total Quantity (lbs)', 'Total Number of Cows', 'Water Use (mil gal)', 'Land Use (acres)', 'CO2 (k lbs)', 'CH4 (k lbs)', 'NO2 (k lbs)'),
                  rownames = c("Set with same quantity", "Set A", "Set B"),
                  options = list(paging = F, searching = F))
    })
})




