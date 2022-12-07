# Colette's Final Project

# load libraries ---------------------------------------------------------------

library(htmlwidgets)
library(leaflet)
library(plotly)
library(readr)
library(sf)
library(readtext)
library(readxl)
library(quanteda)
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


rm(list = ls())

#setwd("/Users/coletteyeager/Documents/Documents /Georgetown/Fall 2022/Data Viz/Final App")
# Data setup -------------------------------------------------------------------
# Read in Data
women <- read.csv("Project data/women_violence.csv")
multi_year <- read.csv("Project data/multi_year_violence.csv")
children <- read.csv("Project data/children.csv")
opinion <- read.csv("Project data/opinion_on_violence.csv")
income <- read.csv("Project data/country_income.csv")

# Clean data

# Women
old_long_name = colnames(women)[4]
women <- women %>%
  rename("Country" = "Entity", "Proportion_Violence" = all_of(old_long_name)) %>%
  select(!c("Code")) %>%
  group_by(Country) %>%
  summarize(Proportion_Violence = mean(Proportion_Violence, na.rm = TRUE)) %>%
  mutate(Continent = countrycode(sourcevar = Country,
                                 origin = "country.name",
                                 destination = "continent"))

# Multi-Year Women
old_long_name_2 = colnames(multi_year)[4]
multi_year <- multi_year%>%
  rename("Country" = "Entity", "Proportion_Violence" = all_of(old_long_name_2)) %>%
  mutate(Continent = countrycode(sourcevar = Country,
                                 origin = "country.name",
                                 destination = "continent")) %>%
  select(!c("Code"))

wide_table <- multi_year %>%
  pivot_wider(names_from = "Year", values_from = "Proportion_Violence") %>%
  mutate("Average_Proportion" = (rowMeans(across(where(is.numeric)), dims = 1)))

multiyear_average <- wide_table %>%
  select(Country, Continent, Average_Proportion) %>%
  rename("Proportion_Violence" = "Average_Proportion")

# Children
old_long_name_3 = colnames(children)[4]
children <- children %>%
  rename("Country" = "Entity", "Proportion_Violence" = all_of(old_long_name_3)) %>%
  mutate(Continent = countrycode(sourcevar = Country,
                                 origin = "country.name",
                                 destination = "continent")) %>%
  select(c(Country, Continent, Proportion_Violence))

# Opinion
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

opinion_countries = unique(opinion$Country)

avg_opinion <- opinion %>%
  group_by(Country, Continent) %>%
  summarize(Avg_Opinion = mean(Proportion, na.rm = TRUE))

# Income
income <- income %>%
  select(!c(medianIncome)) %>%
  rename("Country" = "country", "Mean_Income" = "meanIncome", 
         "GDP" = "gdpPerCapitaPPP", "Population" = "pop2022") %>%
  mutate(population = str_replace(Population, "\\.", ""))

# Combined df for all avg numbers (way of getting overlapping countries)
combined_df <- avg_opinion %>%
  inner_join(children %>% select(!c(Continent)), by = "Country") %>%
  inner_join(women %>% select(!c(Continent)), by = "Country", 
             suffix = c("_children", "_women")) %>%
  inner_join(multiyear_average %>% select(!c(Continent)), by = "Country")

overlapping_countries = combined_df$Country

Country <- c('Antigua and Barbuda', 'Argentina', 'Bahamas', 'Barbados', 'Belize', 
             'Bolivia', 'Brazil', 'Chile', 'Colombia', 'Costa Rica', 'Dominica', 
             'Dominican Republic', 'Ecuador', 'El Salvador', 'Guatemala', 'Guyana', 
             'Honduras', 'Jamaica', 'Mexico', 'Nicaragua', 'Panema', 'Paraguay', 
             'Peru', 'Puerto Rico', 'Saint Lucia', 'Saint Vincent and the Grenadines', 
             'Trinidad and Tobago', 'Uruguay')
year_passed <- c(1999, 1994, 1991, 1992, 1992, 1995, 1995, 1994, 1996, 1996, 
                 1996, 1997, 1995, 1996, 1999, 1996, 1997, 1996, 1996, 1996, 
                 1999, 2000, 1993, 1989, 1995, 1984, 1999, 1995)

laws_passed <- data.frame(Country, year_passed)

# Make the animation and save as a gif
# p2 <- multi_year %>%
#   inner_join(laws_passed, by = "Country") %>%
#   filter(Country %in% c('Bahamas', 'Dominican Republic', 'El Salvador', 
#                         'Honduras', 'Nicaragua', 'Peru')) %>%
#   ggplot(aes(x = Year, y = Proportion_Violence)) +
#   geom_line() +
#   facet_wrap(~Country, scales = 'free_y', nrow = 3) +
#   geom_vline(data = sample, aes(xintercept = year_passed), color = 'blue') +
#   labs(x = "", y = "Proportion of Women Abused") +
#   scale_y_continuous(labels = function(x) paste0(x, '%')) +
#   theme_classic() +
#   theme(strip.background = element_blank()) +
#   theme(strip.text = element_text(size = 15)) +
#   geom_point() +
#   transition_reveal(Year)
# 
# anim_save("over_time.gif", animate(p2, fps = 15, end_pause = 25)) 


# UI ----------------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage("Abuse Against Women Around the World",
             
             tabPanel("Home", icon = icon("house-user"),
                      mainPanel(p("*Note that more will be added to this page, talking about the project 
                                  and the implications of the figures"),
                                h2(style="text-align:center", strong("Home")),
                                br(),
                                h3(style = "text-align:justify", strong("Abuse in Specific Countries:")),
                                p(style = "text-align:justify", 
                                "This tab will show the top countries where abuse occurs as well as the correlation between
                                abuse and average income."), 
                                br(),
                                h3(style = "text-align:justify", strong("Abuse over Time:")),
                                p(style = "text-align:justify", 
                                  "This tab will show how abuse decreases over time after a law is passed."), 
                                br(),
                                h3(style = "text-align:justify", strong("Opinions on Abuse:")),
                                p(style = "text-align:justify", 
                                  "This tab shows some figures on how people view abuse around the world,
                                  including how it differs for certain demographics, reasons for abuse, and location."), 
                                width = 12)),
             
             tabPanel("Abuse in Specific Countries", icon = icon("globe"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = 'w_or_c_data',
                                      label = "Choose a dataset",
                                      choices = c("Women", 'Children')),
                          numericInput(inputId = 'num_countries',
                                       label = "Number of Top Countries",
                                       value = 5,
                                       min = 1,
                                       step = 1), width = 2),
                        mainPanel(h3("Proportion Abused in Top Countries"),
                                  fluidRow(
                                    splitLayout(cellWidths = c("45%", "55%"), 
                                                tableOutput("abused_table"),
                                                plotlyOutput("income_plot"))),
                                  h4("As shown from the plot above, the lower the average income of a country, 
                                     the higher percentage of people there are that are abused.")
                                  , width = 9))),
             
             tabPanel("Abuse over Time", icon = icon("calendar"),
                      sidebarLayout(
                        sidebarPanel(h4("Once a law is passed (indicated by the blue line) against domestic violence,
                                      the proportion of women abused drops more significantly.")),
                        mainPanel(
                          #imageOutput("time_plot")
                          img(src="over_time.gif", height='600px',width='800px')
                          ))),
             
             tabPanel("Opinions on Abuse", icon = icon("comments"),
                      # Sidebar 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = 'choice',
                                      label = "Choose Aspects to Aggregate On",
                                      choices = c("Demographics", "Reason for Abuse", "Location", 
                                                  "Demographics and Reason", "Demographics and Location",
                                                  "Reason and Location"),
                                      ),
                          conditionalPanel(condition = "input.choice == 'Demographics' || input.choice == 'Demographics and Reason' || input.choice == 'Demographics and Location'",
                                           selectInput(inputId = 'demo',
                                                       label = "Choose a demographic aspect",
                                                       choices = c("Age", "Residence", 
                                                                   "Marital status",
                                                                   "Employment", "Education"))),
                          # selectInput(inputId = 'question',
                          #             label = "Choose a reason for abuse",
                          #             choices = c("All (See an aggregated summary)", 
                          #                         "... if she burns the food",
                          #                         "... if she argues with him",
                          #                         "... if she goes out without telling him",
                          #                         "... if she neglects the children",
                          #                         "... if she refuses to have sex with him",
                          #                         "... for at least one specific reason"),
                          #             selected = "All (See an aggregated summary)"
                          #             ),
                          checkboxInput(inputId = 'country_filter',
                                        label = "Filter by Country",
                                        value = FALSE),
                          conditionalPanel(condition = "input.country_filter",
                                           pickerInput(inputId = 'countries',
                                                       label = "Filter by Country",
                                                       choices = opinion_countries,
                                                       multiple = TRUE,
                                                       options = list(`actions-box` = TRUE, 
                                                                      size = 10,
                                                                      `selected-text-format` = "count > 4")))),
                        mainPanel(
                          plotlyOutput("plotly_test")
                        )
                      )
             )
  )
)

# Server ------------------------------------------------------------------------

server <- function(input, output){
  
  # Numbers data/plotting
  abuse_data <- reactive({
    if(input$w_or_c_data == "Women"){
      multiyear_average %>%
        inner_join(income, by = "Country") %>%
        filter(Continent != "NA")
    }else{
      children %>%
        inner_join(income, by = "Country") %>%
        filter(Continent != "NA")
    }
  })
  
  output$abused_table <- renderTable({
    abuse_data() %>%
      arrange(desc(Proportion_Violence)) %>%
      slice_head(n = input$num_countries) %>%
      select(Country, Continent, Proportion_Violence)
  })
  
  output$correlation_text <- renderText({
    paste("Correlation of Proportion of Abused", input$w_or_c_data, "and Average Country Income")
  })
  
  output$income_plot <- renderPlotly({
    p1 <- abuse_data() %>%
      ggplot(aes(x = Proportion_Violence, y = Mean_Income)) +
      geom_point(aes(color = Continent, text = paste0("Country: ", Country, "\n", 
                                                      "Continent: ", Continent, "\n",
                                                      "Proportion Abused: ", round(Proportion_Violence, 2), "\n",
                                                      "Income: ", Mean_Income, "\n",
                                                      "Population: ", round(Population, 0)))) +
      scale_color_brewer(palette = "Set1") + 
      scale_y_log10(label = scales::dollar_format(accuracy = 1L)) +
      scale_x_continuous(labels = function(x) paste0(x, '%')) +
      labs(x = paste("Proportion of", input$w_or_c_data, "Abused"), y = "Mean Income") +
      theme_classic()
    
    ggplotly(p1, tooltip = c("text"))
  })
  
  # Time Plot
  # 
  # output$time_plot <- renderImage({
  #   # A temp file to save the output.
  #   # This file will be removed later by renderImage
  #   outfile <- tempfile(fileext='.gif')
  # 
  #     
  #   # Return a list containing the filename
  #   list(src = "outfile.gif",
  #        contentType = 'image/gif'
  #        ,width = 700
  #        ,height = 600
  #        # alt = "This is alternate text"
  #   )}, deleteFile = TRUE)
  # 
  # Opinions data/plotting
  
  opinion_data_pt_1 <- reactive({
    if(input$country_filter == TRUE){
      opinion %>%
        filter(Country %in% input$countries)
    }else{
      opinion
    }
  })
  
  opinion_data <- reactive({
    if(input$choice == 'Demographics'){
      opinion_data_pt_1() %>%
        filter(Demographics_Q == input$demo) %>%
        group_by(Demographics_R) %>%
        summarise(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }else if(input$choice == "Reason for Abuse"){
      opinion_data_pt_1() %>%
        group_by(Question) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }else if(input$choice == "Location"){
      opinion_data_pt_1() %>%
        group_by(Continent) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }else if(input$choice == "Demographics and Reason"){
      opinion_data_pt_1() %>%
        filter(Demographics_Q == input$demo) %>%
        group_by(Demographics_R, Question) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }else if(input$choice == "Demographics and Location"){
      opinion_data_pt_1() %>%
        filter(Demographics_Q == input$demo) %>%
        group_by(Demographics_R, Continent) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }else if(input$choice == "Reason and Location"){
      opinion_data_pt_1() %>%
        group_by(Question, Continent) %>%
        summarize(Proportion = round(mean(Proportion, na.rm = TRUE), 2))
    }
  })
  
  y_var <- reactive({
    if(input$choice %in% c("Demographics", "Demographics and Reason", "Demographics and Location")){
      "Demographics_R"
    }else if(input$choice %in% c("Reason for Abuse", "Reason and Location")){
      "Question"
    }else if(input$choice == "Location"){
      "Continent"
    }
  })
  
  fill_var <- reactive({
    if(input$choice %in% c("Demographics and Location", "Reason and Location")){
      "Continent"
    }else if(input$choice == "Demographics and Reason"){
      "Question"
    }
  })
  
  output$plotly_test <- renderPlotly({
    if (input$choice %in% c("Demographics", "Reason for Abuse", "Location")){
      p3 <- ggplot(opinion_data(), aes_string(x = "Proportion",
                                              y = y_var())) +
        geom_bar(stat = "identity") +
        labs(x = "Proportion of Population", y = "", 
             title = "Proportion of People Who Believe Abuse is Acceptable") +
        xlim(0, 100)
    }else{
      p3 <- ggplot(opinion_data(), aes_string(x = "Proportion",
                                              y = y_var(),
                                              fill = fill_var())) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Proportion of Population", y = "", 
             title = "Proportion of People Who Believe Abuse is Acceptable") +
        xlim(0, 100)
    }
    
    ggplotly(p3, tooltip = c("Proportion"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)