

shinyServer(function(input, output){

  # heat map tab #################################
    # show map using googleVis 2013 (youth)
    output$Y_map2013 <- renderGvis({
        gvisGeoChart(y_heatmap_2013, "LocationDesc", input$Y_selectedMap2015,
                     options=list(region="US", displayMode="regions", 
                                  resolution="provinces",
                                  width="auto", height="auto"))
    })
    # show map using googleVis 2013 (adult)
    output$A_map2013 <- renderGvis({
      gvisGeoChart(a_heatmap_2013, "LocationDesc", input$A_selectedMap2015,
                   options=list(region="US", displayMode="regions", 
                                resolution="provinces",
                                width="auto", height="auto"))
    })
  
    
  # linear reg model tab #################################
    # lin reg general trend of Youth from state data across years
    # youth state data over 2007, 2009, 2011, 2013, 2015
    output$Y_trend_fru <- renderPlot(
      ggplot(y_spread, aes(x=`Fruit consumption`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +   # obesity vs fruit
        labs(x= "Fruit consumption population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$Y_trend_veg <- renderPlot(
      ggplot(y_spread, aes(x=`Vegetables consumption`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +   # obesity vs veggie
        labs(x= "Vegetables consumption population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$Y_trend_pa1 <- renderPlot(
      ggplot(y_spread, aes(x=`>1 hr daily physical activity`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +  # obesity vs PA
        labs(x= ">1 hr daily physical activity population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$Y_trend_pa2 <- renderPlot(
      ggplot(y_spread, aes(x=`Daily PE class`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +  # obesity vs PA
        labs(x= "Daily PE class population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    
    # lin reg general trend of Youth and Adult from state data across years
    # adult state data over 2011, 2013, 2015
    output$A_trend_fru <- renderPlot(
      ggplot(a_spread, aes(x=`Fruit consumption`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +  # obesity vs fruit
        labs(x= "Fruit consumption population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$A_trend_veg <- renderPlot(
      ggplot(a_spread, aes(x=`Vegetables consumption`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +  # obesity vs veggie
        labs(x= "Vegetables consumption population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$A_trend_pa1 <- renderPlot(
      ggplot(a_spread, aes(x=`Short duration aerobic`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +   # obesity vs PA
        labs(x= "Short duration aerobic population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$A_trend_pa2 <- renderPlot(
      ggplot(a_spread, aes(x=`Short duration aerobic and strengthening`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +  # obesity vs PA
        labs(x= "Short duration aerobic and strengthening population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$A_trend_pa3 <- renderPlot(
      ggplot(a_spread, aes(x=`Long duration aerobic`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +  # obesity vs PA
        labs(x= "Long duration aerobic population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$A_trend_pa4 <- renderPlot(
      ggplot(a_spread, aes(x=`Strengthening`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +  # obesity vs PA
        labs(x= "Strengthening population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$A_trend_pa5 <- renderPlot(
      ggplot(a_spread, aes(x=`No physical activity`, y=Obesity)) + geom_point() + geom_smooth(method = 'lm') +   # obesity vs PA
        labs(x= "No physical activity population (%)", y="Obese population (%)") +
        theme(axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    
    
  # demographic tab #################################
    # show plot obesity vs demographic (youth) from state/national data across years 
    output$Y_ob_stra <- renderPlot(
      ay_combine %>%
        filter(!Stratification == "Total", Question == "Obesity", Data == "youth", 
               LocationDesc == input$Y_Selected_LocationDesc, StratificationCategory  == input$Y_Selected_cat) %>%
        ggplot(aes(x=Year, y=Data_Value)) + labs(y="Obese population (%)") + 
        scale_y_continuous(limits = c(0, 40)) + scale_x_continuous(breaks = seq(2001, 2015, by = 2)) +
        geom_bar(stat ="identity", position = "dodge", fill="tomato1") + facet_grid(~ Stratification) +
        theme(strip.text = element_text(size=15), axis.title = element_text(size=13),
              axis.text.x = element_text(size=14, angle=90), axis.text.y = element_text(size=14))
    )
    # show plot obesity vs demographic (adult) from state/national data across years 
    output$A_ob_stra <- renderPlot(
        if (input$A_Selected_cat == "Education"){
        edu %>%
        filter(Question == "Obesity") %>%
        ggplot(aes(x=Year, y=Data_Value)) + labs(y="Obese population (%)") + 
        scale_y_continuous(limits = c(0, 40)) + scale_x_continuous(breaks = seq(2011, 2016, by = 1)) +
        geom_bar(stat ="identity", position = "dodge", fill="tomato1") + facet_grid(~ Stratification) +
        theme(strip.text = element_text(size=15), axis.title = element_text(size=13),
              axis.text.x = element_text(size=14, angle=90), axis.text.y = element_text(size=14))
        }else{
        ay_combine %>%
        filter(!Stratification == "Total", Question == "Obesity", Data == "adult",
               LocationDesc == input$A_Selected_LocationDesc, StratificationCategory == input$A_Selected_cat) %>%
        ggplot(aes(x=Year, y=Data_Value)) + labs(y="Obese population (%)") + 
        scale_y_continuous(limits = c(0, 40)) + scale_x_continuous(breaks = seq(2011, 2016, by = 1)) +
        geom_bar(stat ="identity", position = "dodge", fill="tomato1") + facet_grid(~ Stratification) +
        theme(strip.text = element_text(size=15), axis.title = element_text(size=13),
              axis.text.x = element_text(size=14, angle=90), axis.text.y = element_text(size=14))
        }
    )
    
    
  # comparison tab #################################
    # box plot Obesity across age (Youth and Adult) from state data across years   
    output$AY_ob_age <- renderPlot(
      ay_combine %>% 
        filter(!Stratification == "Total", !LocationDesc == "National", Year == 2015,
               StratificationCategory == "Age", Question == "Obesity") %>%   #filter obesity, age data
        ggplot(aes(x=Stratification, y=Data_Value)) + labs(x= "Age", y="Obese population (%)") + 
        geom_boxplot() + geom_point(alpha = 0.15) +
        theme(strip.text = element_text(size=15), axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    # box plot Youth and Adult Behavior Factor (Food) from state data across years   
    output$AY_ob_food <- renderPlot(
     ay_combine %>% 
        filter(!Stratification == "Total", !LocationDesc == "National", Year == 2015,
               StratificationCategory == "Age", Question == input$AY_Selected_food) %>%
        ggplot(aes(x=Stratification, y=Data_Value)) + labs(x= "Age", y="Population (%)") + 
        geom_boxplot() + geom_point(alpha = 0.15) +
        theme(strip.text = element_text(size=15), axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    ) 
    # box plot Youth and Adult Behavior Factor (PA) from state data across years   
    output$Y_ob_pa <- renderPlot(
      ay_combine %>%
        filter(!Stratification == "Total", !LocationDesc == "National", Year == 2015,
               StratificationCategory == "Age", Question == input$Y_Selected_pa) %>%
        ggplot(aes(x=Stratification, y=Data_Value)) + labs(x= "Age", y="Population (%)") + scale_y_continuous(limits = c(10, 75)) + 
        geom_boxplot() + geom_point(alpha = 0.05) +
        theme(strip.text = element_text(size=15), axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
    output$A_ob_pa <- renderPlot(
      ay_combine %>%
        filter(!Stratification == "Total", !LocationDesc == "National", Year == 2015,
               StratificationCategory == "Age", Question == input$A_Selected_pa) %>%
        ggplot(aes(x=Stratification, y=Data_Value)) + labs(x= "Age", y="Population (%)") + scale_y_continuous(limits = c(10, 75)) +
        geom_boxplot() + geom_point(alpha = 0.05) +
        theme(strip.text = element_text(size=15), axis.title = element_text(size=13),
              axis.text.x = element_text(size=14), axis.text.y = element_text(size=14))
    )
})
    