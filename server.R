library(shiny)
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(fmsb)

cut_weight <- read_excel("C:/Users/lhj98/Desktop/DataSci1/Midterm_Project/Datasets/carcass_calculator_data.xlsx")
HVM_trans <- read_csv("C:/Users/lhj98/Desktop/DataSci1/Midterm_Project/Datasets/HVM_transaction_data.csv")
item_cut_dict <- read_excel("C:/Users/lhj98/Desktop/DataSci1/Midterm_Project/Datasets/item_cut_dictionary.xlsx")

#HVM <- read_csv("C:/Users/16462/OneDrive/Documents/Weill Cornell/Data Science I/Midterm/carcass_calculator_data.csv")

# Filter the beef transaction by three-digit item code "1xx"
HVM_beef_trans <- HVM_trans %>%
    mutate(Item.Code = as.numeric(substring(Item, 1,3))) %>%
    filter(Item.Code >= 100 & Item.Code < 200)

# Set the items to the cut categories
item_cut_dict <- item_cut_dict %>%
    mutate(Item.Code = as.numeric(substring(Item, 1,3)))
HVM <- HVM_beef_trans %>%
    inner_join(item_cut_dict, by = "Item.Code")
HVM <- HVM %>%
    inner_join(cut_weight, by = c("Cut" = "cut")) %>%
    rename(Item.Name = Item.x,
           Cut.Weight = total_weight) %>%
    select(Name, Item.Code, Item.Name, Cut, Cut.Weight, `Price $/lb`, `Quantity (in lbs)`, `Amount (in $)`)



# Calculation for environmental impact
water.rate <- 6.355078 #(million gallons)
land.rate <- 77 #(acres)
## Calculate CH4 and NO2 emission using data according to Gleam Model by FAO
total.co2 <- 2119809499787
total.ch4 <- 199979479648
total.no2 <- 14908328599
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

# Set a function to calculate the heads according to inputs
heads_calculator <- function(vec1, vec2) {
    Order <- NULL
    Category <- NULL
    Weight_per_Head <- NULL
    Quantity <- NULL
    for(i in 1:6) {
        #if(vec2[i] == 0) {next}
        Order = c(Order, LETTERS[i])
        Category = c(Category, vec1[i])
        Weight_per_Head = c(Weight_per_Head, cut_weight$total_weight[which(cut_weight$cut==vec1[i])])
        Quantity = c(Quantity, vec2[i])
    }
    heads_summary <- data.frame(Order, Category, Weight_per_Head, Quantity)
    heads_summary <- heads_summary %>% mutate(Heads = ceiling(Quantity/Weight_per_Head))
    return(heads_summary)
}

shinyServer(function(input, output) {
    
    HVM_Overview <-  reactive({
        cut_weight %>%
            filter(cut %in% input$cut) %>%
            mutate(number.cows = ceiling(input$pounds_of_meat / total_weight),
                   water.use = water.rate * number.cows,
                   land.use = land.rate * number.cows,
                   co2e = co2.emission * number.cows,
                   ch4e = ch4.emission * number.cows,
                   no2e = no2.emission * number.cows)
    })
    
    output$HVM_Plot <- renderPlotly({
        bar <-  HVM_Overview() %>%
            ggplot(aes(x = cut, y = number.cows, fill = cut)) +
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=12),
                  plot.title=element_text(size = 12),
                  panel.background = element_rect(fill = 'ghostwhite',
                                                  color = 'ghostwhite'),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            scale_y_continuous(breaks = pretty_breaks()) +
            ylab('Number of Cows') +
            xlab('Cuts of Cow') +
            ggtitle('How Many Cows Are You Killing?') +
            geom_bar(stat = "identity")
        
        ggplotly(bar)
    })
    
    pack_summary <- reactive({
        pack_cuts <- c(input$cut_1, input$cut_2,
                       input$cut_3, input$cut_4,
                       input$cut_5, input$cut_6)
        pack_quantities <- c(input$quantity_1, input$quantity_2, 
                             input$quantity_3, input$quantity_4, 
                             input$quantity_5, input$quantity_6)
        pack_heads <- heads_calculator(pack_cuts, pack_quantities)
        environment_impact(pack_heads, pack_heads[,5])
    })
    

    output$total_heads <- renderTable({
        sum_head <- sum(as.numeric(pack_summary()[,5]))
        max_head <- max(as.numeric(pack_summary()[,5]))
        data.frame(Name = c("Minimum Cows in Need", "Minimum Cows in Need"),
                   Value = c(sum_head, max_head))
    })
    output$resource_use <- renderTable({
        wateruse <- max(pack_summary()[,6])
        landuse <- max(pack_summary()[,7])
        data.frame(Resource = c("Water", "Land"),
                   Amount = c(wateruse, landuse),
                   Unit = c("mil gal", "acres"))
    })
    output$gas_emission <- renderTable({
        co2e <- max(pack_summary()[,8])
        ch4e <- max(pack_summary()[,9])
        no2e <- max(pack_summary()[,10])
        data.frame(Gas = c("CO2", "CH4", "NO2"),
                   Amount = c(co2e, ch4e, no2e),
                   Unit = "klbs")
    })
    output$table_cut_package <- DT::renderDataTable({
        DT::datatable(pack_summary()[-c(1,3)],
                  options = list(paging = F))
    })
    output$plot_cut_package <- renderPlotly({
        max_head <- max(as.numeric(pack_summary()[,5]))
    
        radar_chart <- plot_ly(
            type = 'scatterpolar',
            r = pack_summary()[,5],
            theta = pack_summary()[,2],
            fill = 'toself'
        ) %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,max_head)
                    )
                ),
                showlegend = F
            )
        
    })
    
})