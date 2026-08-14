#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(readr)
library(shiny)
library(lubridate)
library(data.table)
library(plotly)
library(htmltools)
library(DT)

'%!in%' <- function(x,y)!('%in%'(x,y))

us_stats <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv?_sm_au_=iVVWPQ545LD7stpRGqtBJK3R1GQBC") %>%
  filter(date == max(as_date(date)))

cases_deaths <- read_csv("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv") %>%
  select(date, county = area, dof_pop = population, cases_epdate = cases, deaths_DOD = deaths)

tDatDOF  <- cases_deaths %>% 
  distinct(county, dof_pop)

county_alpha <- select(tDatDOF, county) %>% arrange(county) %>% pull()

ui <- fluidPage(theme = shinytheme("united"),
                
                navbarPage("California Covid-19 Dashboard",
                           tabPanel("",
                                    column(3,
                                           wellPanel(selectInput("select_county", label = "Select County",
                                                                          choices = county_alpha, selected = "California"),
                                                     sliderInput("daterange",
                                                                 "Dates:",
                                                                 min = as.Date("2020-01-01","%Y-%m-%d"),
                                                                 max = as.Date("2022-12-01","%Y-%m-%d"),
                                                                 value=c(as.Date("2020-01-01"), as.Date("2022-12-01")),
                                                                 timeFormat="%b %Y")
                                                     )
                                    ),
                                    column(9,
                                           wellPanel(uiOutput("plot_title"),
                                                     plotlyOutput(outputId = "cases_plot", height = "300px")
                                           ),
                                           wellPanel(uiOutput("second_title"),
                                                     plotlyOutput(outputId = "deaths_plot", height = "300px")
                                           )
                                           
                                    ) #end column
                           ), #end tabPanel
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  selected_county_text <- reactive({
    if(input$select_county != "California") {
      paste0(input$select_county, " county")
    } else {
      "Statewide"
    }
  })
  
  plot_data <- reactive({
    print(as_date(input$daterange[[1]]))
    if (input$select_county != "California") {
      cases_deaths %>%
        filter(county == input$select_county & between(date, as_date(input$daterange[[1]]), as_date(input$daterange[[2]]))) %>%
        select(date, cases = cases_epdate, deaths = deaths_DOD) %>%
        mutate(roll_mean_cases = frollmean(cases, 7), roll_mean_deaths = frollmean(deaths, 7)) %>%
        filter(date >= as_date("2020-03-01"))
    } else {
      cases_deaths %>%
        filter(between(date, as_date(input$daterange[[1]]), as_date(input$daterange[[2]]))) %>%
        group_by(date) %>%
        summarize(cases = sum(cases_epdate, na.rm = T), deaths = sum(deaths_DOD, na.rm = T)) %>%
        mutate(roll_mean_cases = frollmean(cases, 7), roll_mean_deaths = frollmean(deaths, 7)) %>%
        filter(date >= as_date("2020-03-01"))
    }
  })

  output$plot_title <- renderUI({
    
    div(h4("Cases by Episode Date"),
        h5(selected_county_text())
    )
    
  })
  
  output$second_title <- renderUI({
    
    div(h4("Deaths by Date of Death"),
        h5(selected_county_text())
    )
    
  })
  
  #selected_county_text <- function() {"Statewide"}

  output$cases_plot<- renderPlotly({
    
    fig <- plot_ly(data = plot_data()) %>%
      add_trace(x = ~date, y = ~cases, type = 'bar', name = 'Cases per day',
                marker = list(color = 'rgb(187, 216, 228)'),
                hoverinfo = "text",
                text = ~paste(cases, ' cases')) %>%
      add_trace(x = ~date, y = ~roll_mean_cases, type = 'scatter', mode = 'lines', name = 'Average Cases', 
                line = list(color = '#345B6B'),
                hoverinfo = "text",
                text = ~paste(roll_mean_cases, ' average cases')
      ) %>% 
      layout(legend = list(x = 0.1, y = 0.9))
    
    
  })
  
  output$deaths_plot<- renderPlotly({
    
    
    fig <- plot_ly(data = plot_data()) %>%
      add_trace(x = ~date, y = ~deaths, type = 'bar', name = 'Deaths per day',
                marker = list(color = '#C6C9CA'),
                hoverinfo = "text",
                text = ~paste(deaths, ' deaths')) %>%
      add_trace(x = ~date, y = ~roll_mean_deaths, type = 'scatter', mode = 'lines', name = 'Average Deaths', 
                line = list(color = '#43494B'),
                hoverinfo = "text",
                text = ~paste(roll_mean_deaths, ' average deaths')
      ) %>%
      layout(legend = list(x = 0.1, y = 0.9))
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
