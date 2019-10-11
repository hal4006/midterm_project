library(shiny)
library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)

##Enter your carcass_calculator_data.csv path here:

#HVM <- read_csv("C:/Users/lhj98/Downloads/carcass_calculator_data.csv")

HVM <- read_csv("C:/Users/16462/OneDrive/Documents/Weill Cornell/Data Science I/Midterm/carcass_calculator_data.csv")


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

    output$HVM_Plot <- renderPlotly({

        bar <-  Carcass() %>%
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

    output$HVM_Water <- renderPlotly({

        barwater <- Carcass() %>%
            ggplot(aes(x = cut, y = water.use, fill = cut)) +
            theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=12),
                  plot.title=element_text(size = 12),
                  panel.background = element_rect(fill = 'ghostwhite',
                                                  color = 'ghostwhite'),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            scale_y_continuous(breaks = pretty_breaks()) +
            ylab('Water Use') +
            xlab('Cuts of Cow') +
            ggtitle('Water Usage by Cut') +
            geom_bar(stat = "identity")

        ggplotly(barwater)
    })

})
