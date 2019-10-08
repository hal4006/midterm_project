library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)

HVM <- read_csv("C:/Users/lhj98/Downloads/carcass_calculator_data.csv")

# constants
water.rate <- 6.355078 #(million gallons)
land.rate <- 77 #(acres)

## calculate CH4 and NO2 emission using data according to Gleam Model by FAO
total.co2 <- 2119809499787
total.ch4 <- 199979479648
total.no2 <- 14908328599
co2.emission <- 102.959 #(thousand lbs)
ch4.emission <- total.ch4 / total.co2 * co2.emission
no2.emission <- total.no2 / total.co2 * co2.emission



shinyServer(function(input, output) {
    Carcass <- reactive({
        HVM %>%
            filter(cut %in% input$cut) %>%
            mutate(number.cows = ceiling(input$pounds_of_meat / total_weight),
                   water.use = water.rate * number.cows,
                   land.use = land.rate * number.cows,
                   co2e = co2.emission * number.cows,
                   ch4e = ch4.emission * number.cows,
                   no2e = no2.emission * number.cows)
    })
    

    output$HVM_Table <- renderTable({
         Carcass()
    })
    
    output$HVM_Plot <- renderPlot({
        Carcass() %>%
        ggplot(aes(x = cut, y = number.cows)) +
            geom_bar(stat = "identity")
    })

})
