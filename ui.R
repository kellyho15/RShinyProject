# library(DT)
library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "US Health Trend"),
    dashboardSidebar(
        
        sidebarUserPanel("Kelly Ho",
                         image = "https://yt3.ggpht.com/-04uuTMHfDz4/AAAAAAAAAAI/AAAAAAAAAAA/Kjeupp-eNNg/s100-c-k-no-rj-c0xffffff/photo.jpg"),
        sidebarMenu(
            menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
            menuItem("Map", tabName = "map", icon = icon("map")),
            menuItem("Youth General Trend", tabName = "y_gen_trend", icon = icon("signal")),
            menuItem("Adult General Trend", tabName = "a_gen_trend", icon = icon("signal")),
            menuItem("Demographic", tabName = "ob_stra", icon = icon("database")),
            menuItem("Diet/Physical Activity", tabName = "diet_pa", icon = icon("user"))

        )
       
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
          tabItem (tabName = "intro",
                   fluidRow(column(1), box(tags$img(src = "www.rstudio.com", width = "100px", height = "100px"), width = 9)),
                   fluidRow(column(1), box(tags$h2("Background"), 
                                           tags$h4("• Almost half of the world’s adult population could be overweight or obese by 2030"), 
                                           tags$h4("• Obesity is often related to conditions include heart disease, stroke,
                                                   type 2 diabetes and certain types of cancer"), br(),
                                           tags$h2("Goal"),
                                           tags$h4("• Identify distinguishable segmentation within the US obese population"),
                                           tags$h4("• Market segmentation could allow for more effective targeting of products/marketing campaign 
                                                   to move Americans towards healthier weights"), br(),
                                           tags$h2("Dataset"),  
                                           tags$h4("•	The dataset for analysis provides national and state specific data on youth and adult's diet,
                                                   physical activity, and weight status across demographics"),
                                           tags$h4("•	Youth data were collected every two years from 2011 to 2015", 
                                                   tags$a(href="https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-youth-risk-behavior-surveillance-system-bf946", "Click here")), 
                                           tags$h4("•	Adult data were collected every year from 2011 to 2016", 
                                                   tags$a(href="https://catalog.data.gov/dataset/nutrition-physical-activity-and-obesity-behavioral-risk-factor-surveillance-system-f645f", "Click here")), width = 9))),
         
          tabItem(tabName = "map",
                  fluidRow(box(title = "Youth (age 14-17)", "Obesity, Diet, and Physical Activity 2013", htmlOutput("Y_map2013"), 
                               "*Note: No data collected for states with no color; dark green color for states with missing value", height = 450, width = 6),
                           box(title = "Adult (age >18)", "Obesity, Diet, and Physical Activity 2013", htmlOutput("A_map2013"), height = 450, width = 6)),
                  fluidRow(box(title = "Youth", selectizeInput("Y_selectedMap2015", "Select Input", y_choice2013), width = 6),
                           box(title = "Adult", selectizeInput("A_selectedMap2015", "Selected Input", a_choice2013), width = 6))),
          
          tabItem(tabName = "y_gen_trend",
                  fluidRow(box(title = "Youth Obesity vs Diet", "US States data across year (2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015)", width = 10)),
                  fluidRow(box(plotOutput("Y_trend_fru"), width = 6),
                           box(plotOutput("Y_trend_veg"), width = 6)),
                  br(),
                  fluidRow(box(title = "Youth Obesity vs Physical Activity", "US States data across years (2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015)", width = 10)),
                  fluidRow(box(plotOutput("Y_trend_pa1"), width = 6),
                           box(plotOutput("Y_trend_pa2"), width = 6))),
          tabItem(tabName = "a_gen_trend",
                  fluidRow(box(title = "Adult Obesity vs Diet", "US States data across years (2011, 2012, 2013, 2014, 2015, 2016)", width = 10)),
                  fluidRow(box(plotOutput("A_trend_fru"), width = 6),
                           box(plotOutput("A_trend_veg"), width = 6)),
                  br(),
                  fluidRow(box(title = "Adult Obesity vs Physical Activity", "US States data across years (2011, 2012, 2013, 2014, 2015, 2016)", width = 10)),
                  fluidRow(box(plotOutput("A_trend_pa1"), width = 6),
                           box(plotOutput("A_trend_pa2"), width = 6)),
                  fluidRow(box(plotOutput("A_trend_pa3"), width = 6),
                           box(plotOutput("A_trend_pa4"), width = 6)),
                  fluidRow(box(plotOutput("A_trend_pa5"), width = 6))),
          
          tabItem(tabName = "ob_stra",
                  fluidRow(box(title = "Youth Obesity Demographic", plotOutput("Y_ob_stra"), width = 8),
                           box(title = "Youth", selectizeInput("Y_Selected_LocationDesc", "Select Location", y_choice_states), width = 2),  # selection for demographic 
                           box(title = "Youth", selectizeInput("Y_Selected_cat", "Select Demographic", y_choice_cat), width = 2)),
                
                  fluidRow(box(title = "Adult Obesity Demographic", plotOutput("A_ob_stra"), width = 8),
                           box(title = "Adult", selectizeInput("A_Selected_LocationDesc", "Select Location", a_choice_states), width = 2),  
                           box(title = "Adult",selectizeInput("A_Selected_cat", "Select Demographic", a_choice_cat), width = 2))),
          
          tabItem(tabName = "diet_pa",
                  fluidRow(column(2), box(title = "Youth and Adult Obesity", "US States data 2015", plotOutput("AY_ob_age"), width = 8)),     # selection for comparison between adult and youth
                  fluidRow(column(2), box(title = "Youth and Adult Diet Consumption", "US States data 2015 ", plotOutput("AY_ob_food"), width = 8),
                           box(title = "Diet",selectizeInput("AY_Selected_food", "Select Diet", ay_choice_food), width = 2)),
                  fluidRow(box(title = "Youth", selectizeInput("Y_Selected_pa", "Select Physical Activity", y_choice_pa), width = 2),
                           box(title = "Youth Physical Activity", "US States data 2015", plotOutput("Y_ob_pa"), width = 3),
                           box(title = "Adult Physical Activity", "US States data 2015", plotOutput("A_ob_pa"), width = 5),
                           box(title = "Adult", selectizeInput("A_Selected_pa", "Select Physical Activity", a_choice_pa), width = 2)))
         
          )
        )
))