
# Final Project Shiny App

# load libraries ---------------------------------------------------------------

library(htmlwidgets)
library(leaflet)
library(plotly)
library(readr)
library(sf)
library(readtext)
library(readxl)
library(bit64)
library(stopwords)
library(data.table)
library(hash)
library(shinyWidgets)
library(dplyr)
library(tidyverse)
library(ggtext)
library(kableExtra)
library(gridExtra)
library(ggrepel)
library(countrycode)
library(ggthemes)
library(rsconnect)
library(shinythemes)
library(gganimate)
library(ggplot2)
library(maps)

# Clear environment
rm(list = ls())

# Data setup -------------------------------------------------------------------

# Read in Data
women <- read.csv("Project data/women_violence.csv")
children <- read.csv("Project data/children.csv")
opinion <- read.csv("Project data/opinion_on_violence.csv")
income <- read.csv("Project data/country_income.csv")

# Clean data

# Abuse against women
old_long_name = colnames(women)[4]
women <- women %>%
  rename("Country" = "Entity", "Proportion_Abused" = all_of(old_long_name)) %>%
  mutate(Continent = countrycode(sourcevar = Country,
                                 origin = "country.name",
                                 destination = "continent")) %>%
  select(!c("Code"))

# Create wide version of women table to be able to get average for each country
women_average <- women %>%
  pivot_wider(names_from = "Year", values_from = "Proportion_Abused") %>%
  mutate("Average_Proportion" = (rowMeans(across(where(is.numeric)), dims = 1))) %>%
  select(Country, Continent, Average_Proportion) %>%
  rename("Proportion_Abused" = "Average_Proportion")

# Abuse against children
old_long_name_2 = colnames(children)[4]
children <- children %>%
  rename("Country" = "Entity", "Proportion_Abused" = all_of(old_long_name_2)) %>%
  mutate(Continent = countrycode(sourcevar = Country,
                                 origin = "country.name",
                                 destination = "continent")) %>%
  select(c(Country, Continent, Proportion_Abused))

# Opinions on abuse
demo_q = colnames(opinion)[4]
demo_r = colnames(opinion)[5]
year = colnames(opinion)[7]
opinion <- opinion %>%
  rename("Proportion" = "Value", "Demographics_Q" = all_of(demo_q), 
         "Demographics_R" = all_of(demo_r), "Long_Year" = all_of(year)) %>%
  mutate(Female = 1*(Gender == "F")) %>%
  mutate(Year = as.integer(substring(Long_Year, first = 7))) %>%
  mutate(Continent = countrycode(sourcevar = Country,
                                 origin = "country.name",
                                 destination = "continent")) %>%
  select(c(Country, Continent, Year, Demographics_Q, Demographics_R, Female, 
           Question, Proportion))

# Get average opinion for each country
avg_opinion <- opinion %>%
  group_by(Country, Continent) %>%
  summarize(Avg_Opinion = mean(Proportion, na.rm = TRUE))

# Income
income <- income %>%
  select(!c(medianIncome)) %>%
  rename("Country" = "country", "Mean_Income" = "meanIncome", 
         "GDP" = "gdpPerCapitaPPP", "Population" = "pop2022") %>%
  mutate(population = str_replace(Population, "\\.", ""))

# Women and Income merged
women_average_inc <- women_average %>%
  inner_join(income, by = "Country") %>%
  filter(Continent != "NA")

# Children and Income merged
children_inc <- children %>%
  inner_join(income, by = "Country") %>%
  filter(Continent != "NA")


# UI ---------------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("flatly"),
  # Create a navigation bar layout with different tabs              
  navbarPage("Abuse Against Women Around the World",

             tabPanel("Home", icon = icon("house-user"),
                      mainPanel(h2(style="text-align:center", strong("Home")),
                                br(),
                                h4("This visualization presents several figures about abuse on women and children around the world, including how people feel about abuse.
                                   It highlights how much work there is still to be done to improve conditions in many countries, and provides some key trends on 
                                   where our time and resources should be focused."),
                                h4("The data presented is from several sources. The Institute for Health Metrics and Evaluation collected data on domestic violence against women
                                   over time, UNICEF collected data on violence against children, and the Demographic and Health Surveys collected data on people's attitudes on abuse.
                                   Finally, there is also data from the world bank for average incomes."),
                                h4("By visiting each of the following pages, you can get a summary of this data and its important implications."),
                                br(),
                                h3(style = "text-align:justify", strong("Abuse in Specific Countries:")),
                                h4(style = "text-align:justify", 
                                "This tab shows the top countries where abuse occurs as well as the correlation between
                                abuse and average income."), 
                                br(),
                                h3(style = "text-align:justify", strong("Abuse over Time:")),
                                h4(style = "text-align:justify", 
                                  "This tab shows how abuse decreases over time after legislation against domestic violence is passed."), 
                                br(),
                                h3(style = "text-align:justify", strong("Abuse and Opinion Correlation:")),
                                h4(style = "text-align: justify",
                                   "This tab shows how the percentage of abused women and the percentage of people 
                                    who justify abuse are correlated in many countries."),
                                br(),
                                h3(style = "text-align:justify", strong("Opinions on Abuse:")),
                                h4(style = "text-align:justify", 
                                  "This tab shows some figures on how people view abuse around the world,
                                  including how it differs for certain demographics, reasons for abuse, and location."), 
                                br(),
                                width = 12)),
             
             tabPanel("Abuse in Specific Countries", icon = icon("globe"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = 'abuse_dataset',
                                      label = 'Choose dataset to view - abuse of women or abuse of children',
                                      choices = c('Women', 'Children'),
                                      selected = 'Women'),
                          radioButtons(inputId = 'filter_options',
                                       label = 'Choice options to filter and highlight key countries',
                                       choices = c('None', 'Show Top N Countries', 'Filter for Specific Countries'),
                                       selected = 'None'),
                          conditionalPanel(
                            "input.filter_options == 'Filter for Specific Countries'",
                            uiOutput('abuse_countries')),
                          conditionalPanel(
                            "input.filter_options == 'Show Top N Countries'",
                            numericInput(inputId = 'num_countries',
                                         label = "Number of Countries",
                                         value = 5,
                                         min = 1,
                                         step = 1)), width = 2),
                        
                        mainPanel(h3("Proportion Abused in Top Countries"),
                                  fluidRow(splitLayout(cellWidths = c("45%", "55%"), 
                                                       tableOutput("abused_table"),                       
                                                       plotlyOutput("income_plot"))),
                                  h4("As shown from the plot above, the lower the average income of a country, 
                                     the higher percentage of people there are that are abused."),
                                  br(), 
                                  width = 10))),
             
             tabPanel("Abuse over Time", icon = icon("calendar"),
                      mainPanel(
                        h2("Proportion of Women Abused Over Time"),
                        h4("Once a law is passed (indicated by the blue line) 
                           against domestic violence, the proportion of women 
                           abused drops more significantly."),
                        img(src="over_time.gif", height='700px',width='900px'),
                        br(),
                        width = 12)),
             
             tabPanel("Abuse and Opinion Correlation", icon = icon("chart-line"),
                      h2("Proportion Abused and Who Justify Abuse Around the World"),
                      h4("Many Countries Have Similar Levels of Abuse as Those Who Justify it"),
                      fluidRow(
                        column(6, align = "center", plotOutput('abuse_map', height = "500px")),
                        column(6, align = "center", plotOutput('opinion_map', height = "500px"))),
                      
                      hr(),
                      
                      fluidRow(
                        column(6, align="center", offset = 3,
                               plotlyOutput('abuse_opinion', height = "500px"))),
                      br()),
             
             tabPanel("Opinions on Abuse", icon = icon("comments"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = 'demographics',
                                      label = "Choose Demographic Aspect to Aggregate On",
                                      choices = c("Age", "Urbanicity", "Marital Status",
                                                  "Employment", "Education")),
                          checkboxInput(inputId = 'by_continent',
                                        label = "View by Continent",
                                        value = TRUE), 
                          width = 3),
                          
                        mainPanel(
                          plotlyOutput('opinion_bar'), width = 8)),
                      
                      hr(),
                      h4(style="text-align:center", "In this study, people were presented with scenarios of when abuse might be justified."),
                      br(),
                      plotOutput('q_opinion_bar'),
                      br())))

# Server -----------------------------------------------------------------------

server <- function(input, output){
  
  # Additional input: filter for countries input
  output$abuse_countries <- renderUI({
    if(input$abuse_dataset == "Women"){
      countries <- women_average_inc$Country
    }else{
      countries <- children_inc$Country
    }
    selectizeInput(inputId = 'abuse_countries', 
                   label = 'Filter by Country',
                   choices = countries,
                   selected = NULL,
                   multiple = TRUE)
  })
  
  # Abuse in Specific Countries Page
  
  # Create base dataset for table and plot based on choice of women or children
  abuse_data <- reactive({
    if(input$abuse_dataset == "Women"){
      women_average %>%
        inner_join(income, by = "Country") %>%
        filter(Continent != "NA")
    }else{
      children %>%
        inner_join(income, by = "Country") %>%
        filter(Continent != "NA")
    }
  })
  
  # Filter dataset further depending on choice of top N countries or specific countries
  filtered <- reactive({
    if(input$filter_options == 'Show Top N Countries'){
      abuse_data() %>%
        arrange(desc(Proportion_Abused)) %>%
        slice_head(n = input$num_countries) %>%
        select(Country, Continent, Proportion_Abused, Mean_Income)
    }else if(input$filter_options == 'Filter for Specific Countries'){
      abuse_data() %>%
        arrange(desc(Proportion_Abused)) %>%
        filter(Country %in% input$abuse_countries) %>%
        select(Country, Continent, Proportion_Abused, Mean_Income)
    }else{
      abuse_data() %>%
        slice_head(n = 5) %>%
        select(Country, Continent, Proportion_Abused, Mean_Income)
    }
  })
  
  # Display table 
  output$abused_table <- renderTable({
    filtered()
  })
  
  # Create conditional y limits for plot depending on women or children data
  lim <- reactive({
    if(input$abuse_dataset == 'Women'){
      c(500, 50000)
    }else{
      c(500, 15000)
    }
  })
  
  # Plot abuse against income
  output$income_plot <- renderPlotly({
    if(input$filter_options == 'None'){
      p1 <- abuse_data() %>%
        ggplot(aes(x = Proportion_Abused, y = Mean_Income)) +
        geom_point(aes(color = Continent, text = paste0("Country: ", Country, "\n", 
                                                        "Continent: ", Continent, "\n",
                                                        "Proportion Abused: ", round(Proportion_Abused, 2), "\n",
                                                        "Income: ", Mean_Income, "\n",
                                                        "Population: ", round(Population, 0)))) +
        scale_color_brewer(palette = "Set1") + 
        scale_y_log10(label = scales::dollar_format(accuracy = 1L), limits = lim()) +
        scale_x_continuous(labels = function(x) paste0(x, '%')) +
        labs(x = paste("Proportion of", input$abuse_dataset, "Abused"), 
             y = "Mean Income (PPP, Current Int$)") +
        theme_classic()
    }else{
      p1 <- abuse_data() %>%
        ggplot(aes(x = Proportion_Abused, y = Mean_Income)) +
        geom_point(aes(color = Continent, text = paste0("Country: ", Country, "\n", 
                                                        "Continent: ", Continent, "\n",
                                                        "Proportion Abused: ", round(Proportion_Abused, 2), "\n",
                                                        "Income: ", Mean_Income, "\n",
                                                        "Population: ", round(Population, 0))),
                   alpha = 0.5, size = 1) +
        geom_point(data = filtered(), aes(x = Proportion_Abused, y = Mean_Income, color = Continent),
                   size = 2.5) +
        scale_color_brewer(palette = "Set1") + 
        scale_y_log10(label = scales::dollar_format(accuracy = 1L), limits = lim()) +
        scale_x_continuous(labels = function(x) paste0(x, '%')) +
        labs(x = paste("Proportion of", input$abuse_dataset, "Abused"), 
             y = "Mean Income (PPP, Current Int$)") +
        theme_classic()      
    }
    
    # Plot using plotly
    ggplotly(p1, tooltip = c("text")) %>%
      # Subtitles don't work regularly with plotly - need to add in layout
      layout(title = list(text = paste0('Correlation of Proportion of Abused ', input$abuse_dataset, '\n', ' and Average Income',
                                         '<br>',
                                         '<sup>',
                                         'Lower Income Countries Have Increased Abuse',
                                         '</sup>'), y = 0.96))
  })
  
  # Opinions on Abuse Page
  
  # Create base dataset depending on demographics choice
  opinion_data_pt_1 <- reactive({
    if(input$demographics == 'Urbanicity'){
      opinion %>%
        filter(Demographics_Q == "Residence") %>%
        group_by(Demographics_R, Continent) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }else if (input$demographics == 'Marital Status'){
      opinion %>%
        filter(Demographics_Q == "Marital status") %>%
        group_by(Demographics_R, Continent) %>%
        mutate(Demographics_R = factor(Demographics_R, 
                                       levels = c("Never married", "Widowed, divorced, separated", 
                                                  "Married or living together"))) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }else if (input$demographics == 'Age'){
      opinion %>%
        filter(Demographics_Q == "Age") %>%
        group_by(Demographics_R, Continent) %>%
        mutate(Demographics_R = factor(Demographics_R, 
                                       levels = c("35-49", "25-34", "15-24"))) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }else if (input$demographics == 'Education'){
      opinion %>%
        filter(Demographics_Q == "Education") %>%
        group_by(Demographics_R, Continent) %>%
        mutate(Demographics_R = factor(Demographics_R, 
                                       levels = c("Higher", "Secondary", "Primary", "No education"))) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }else{
      opinion %>%
        filter(Demographics_Q == input$demographics) %>%
        group_by(Demographics_R, Continent) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))      
    }
  })
  
  # Get average proportion just by demographics if user unchecks view by continent
  opinion_data <- reactive({
    if(input$by_continent == FALSE){
      opinion_data_pt_1() %>%
        group_by(Demographics_R) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))   
    }else{
      opinion_data_pt_1()
    }
  })
 
  # Plot opinion bar graph by demographics
  output$opinion_bar <- renderPlotly({
    if (input$by_continent == FALSE){
      p2 <- ggplot(opinion_data(), aes(x = Proportion, 
                                       y = Demographics_R)) +
        geom_bar(stat = "identity", fill = "darkblue") +
        scale_fill_brewer(palette = "Set1") + 
        scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, '%')) +
        labs(x = "Proportion of the Population", y = input$demographics, 
             title = "Proportion of People Who Believe Abuse is Acceptable") +
        theme_classic()
    }else{
      p2 <- ggplot(opinion_data(), aes(x = Proportion,
                                       y = Demographics_R,
                                       fill = Continent)) +
        geom_bar(stat = "identity", position = 'dodge') +
        scale_fill_brewer(palette = "Set1") + 
        scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, '%')) +
        labs(x = "Proportion of the Population", y = input$demographics, 
             title = "Proportion of People Who Believe Abuse is Acceptable") +
        theme_classic()    
    }
    
    ggplotly(p2, tooltip = c("Proportion")) %>%
      layout(title = list(text = paste0('Proportion of People Who Believe Abuse is Acceptable',
                                        '<br>',
                                        '<sup>',
                                        'Africa and Asia Have Higher Amounts',
                                        '</sup>')))
  })
  
  # Plot opinion for each reason
  output$q_opinion_bar <- renderPlot({
    opinion %>%
      filter(Question != "... for at least one specific reason") %>%
      group_by(Question) %>%
      summarize(Proportion = mean(Proportion, na.rm = TRUE)) %>%
      ggplot(aes(x = Proportion, y = reorder(Question, Proportion))) +
      geom_bar(stat = 'identity', fill = '#057676') +
      geom_text(aes(label = paste0(round(Proportion,0), "%")), hjust = 1, color = 'white', size = 7) +
      labs(x = "Proportion of the Population", y = "", title = "Proportion Who Think Abuse is Acceptable Under Circumstances",
           subtitle = "Neglecting the Children is a Top Reason") +
      theme_classic() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank()) +
      theme(axis.title = element_text(size = 15)) +
      theme(axis.text.y = element_text(size = 15)) +  
      theme(plot.title = element_text(size = 20)) + 
      theme(plot.subtitle = element_text(size = 15))
  })
  
  # Maps of Abuse Statistics Page
  
  # Map proportion of women abused in each country
  output$abuse_map <- renderPlot({
    map_data(map = "world") %>%
      left_join(women_average, by = c("region" = "Country")) %>%
      ggplot(aes(fill = Proportion_Abused)) +
      geom_polygon(aes(long, lat, group = group), color = "black") +
      scale_fill_distiller(palette="Blues", direction = 1, labels = function(x) paste0(x, '%')) +
      labs(fill = 'Proportion of Women Abused') +
      theme_void() +
      theme(legend.position="bottom") +
      theme(legend.key.size = unit(1, 'cm')) +
      theme(legend.title = element_text(size=18)) +
      theme(legend.text = element_text(size=12))
  })
  
  # Map proportion who think abuse is ok in each country
  output$opinion_map <- renderPlot({
    map_data(map = "world") %>%
      left_join(avg_opinion, by = c("region" = "Country")) %>%
      ggplot(aes(fill = Avg_Opinion)) +
      geom_polygon(aes(long, lat, group = group), color = "black") +
      scale_fill_distiller(palette="Blues", direction = 1, labels = function(x) paste0(x, '%')) +
      labs(fill = 'Proportion Who Justify Abuse') +
      theme_void() +
      theme(legend.position="bottom") +
      theme(legend.key.size = unit(1, 'cm')) +
      theme(legend.title = element_text(size=18)) +
      theme(legend.text = element_text(size=12))
  })
  
  # Plot abuse against opinion
  output$abuse_opinion <- renderPlotly({
    p3 <- avg_opinion %>%
      left_join(women_average %>% select(!c(Continent)), by = "Country") %>%
      ggplot(aes(y = Avg_Opinion, x = Proportion_Abused)) +
      geom_point(aes(color = Continent, text = paste0("Country: ", Country, "\n", 
                                                      "Continent: ", Continent, "\n",
                                                      "Proportion Abused: ", round(Proportion_Abused, 2), "\n",
                                                      "Proportion Who Justify Abuse: ", round(Avg_Opinion, 2)))) +
      geom_smooth(method = 'lm', size = 0.7) +
      scale_color_brewer(palette = "Set1") + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,55), labels = function(x) paste0(x, '%')) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,55), labels = function(x) paste0(x, '%')) +
      labs(x = "Proportion of Women Abused", y = "Proportion Who Justify Abuse", 
           title = "Correlation of Abused Women and Those Who Justify Abuse", 
           subtitle = "Countries With More Abused Women Also Have More Who Justify Abuse") +
      theme_classic()
    
    ggplotly(p3, tooltip = c("text")) %>%
      layout(title = list(text = paste0('Correlation of Abused Women and Those Who Justify Abuse',
                                        '<br>',
                                        '<sup>',
                                        'Countries With More Abused Women Also Have More Who Justify Abuse',
                                        '</sup>')))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
