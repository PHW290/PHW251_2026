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
library(shiny)
library(dplyr)
library(readr)
library(shiny)
library(lubridate)
library(data.table)
library(plotly)
library(htmltools)
library(DT)

us_stats <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv?_sm_au_=iVVWPQ545LD7stpRGqtBJK3R1GQBC") %>%
  filter(date == max(as_date(date)))

cases_deaths <- read_csv("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv") %>%
  select(date, county = area, dof_pop = population, cases_epdate = cases, deaths_DOD = deaths)

coLink    <- unique(cases_deaths$county)

tDatDOF  <- cases_deaths %>% 
  distinct(county, dof_pop)

max_date <- max(as_date(cases_deaths$date), na.rm = T)

unit.scale <- function(x) round((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)), 6)

rep_cases_deaths <- cases_deaths %>%
  mutate(recent_cases_ep = ifelse(date > as_date(max_date) - 14 & date <= as_date(max_date) - 7, cases_epdate / 7, 0),
         recent_deaths_dd = ifelse(date > as_date(max_date) - 14 & date <= as_date(max_date) - 7, deaths_DOD / 7, 0)
  ) %>%
  group_by(county, dof_pop) %>%
  summarize(across(where(is.numeric), ~sum(., na.rm = T))) %>%
  ungroup()


county_alpha <- select(tDatDOF, county) %>% arrange(county) %>% pull()

ui <- fluidPage(theme = shinytheme("united"),
                
                navbarPage("California Covid-19 Dashboard",
                           tabPanel("Cases",
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
                                           wellPanel(
                                             uiOutput("stats_box")
                                           ),
                                           wellPanel(uiOutput("plot_title"),
                                                     plotlyOutput(outputId = "cases_plot", height = "300px")
                                           ),
                                           wellPanel(uiOutput("second_title"),
                                                     plotlyOutput(outputId = "deaths_plot", height = "300px")
                                           )
                                           
                                    ) #end column
                           ), #end tabPanel
                           tabPanel("Testing",
                                    fluidRow(h3("Still working on it", style = "color: green; font-weight:bold;" ))
                           ),
                           tabPanel("Demographics",
                                    fluidRow(h3("Still working on it", style = "color: orange; font-weight:bold;"))
                           )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  

  selected_county <- reactive({
    county_alpha[input$select_county]
  })
  
  selected_county_text <- reactive({
    if(input$select_county != "California") {
      paste0(input$select_county, " county")
    } else {
      "Statewide"
    }
  })
  
  summary_data <- reactive({
    if(input$select_county != "California"){
      rep_cases_deaths %>%
        filter(county == input$select_county) %>%
        select(cases = cases_epdate, deaths = deaths_DOD, recent_cases_ep, recent_deaths_dd, dof_pop) %>%
        mutate(across(c(recent_cases_ep, recent_deaths_dd), ~round((.x /  dof_pop) * 100000, 3), .names = "{.col}_rate"))
    } else {
      rep_cases_deaths %>%
        summarize(across(c(cases_epdate, deaths_DOD, recent_cases_ep, recent_deaths_dd, dof_pop), ~sum(.x, na.rm = T) )) %>%
        select(cases = cases_epdate, deaths = deaths_DOD, recent_cases_ep, recent_deaths_dd, dof_pop) %>%
        mutate(across(c(recent_cases_ep, recent_deaths_dd), ~round((.x /  dof_pop) * 100000, 3), .names = "{.col}_rate"))
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
  case_style <- c(bold = "color: #58ABCC; font-weight:bold;", notbold = "color: #58ABCC;")
  death_style <- c(bold = "color: #8A8D8E; font-weight:bold;", notbold = "color: #8A8D8E;")
  
  output$stats_box <- renderUI({
    
    case_change <- round((sum(summary_data()$recent_cases_ep, na.rm = T) / (sum(summary_data()$cases, na.rm = T) - sum(summary_data()$recent_cases_ep, na.rm = T)) * 100), 1)
    death_change <- round((sum(summary_data()$recent_deaths_dd, na.rm = T) / (sum(summary_data()$deaths, na.rm = T) - sum(summary_data()$recent_deaths_dd, na.rm = T)) * 100), 1)
    
    stats <- c(tot_cases = formatC(sum(summary_data()$cases, na.rm = T), format="f", big.mark=",", digits=0),
               today_cases = paste0(formatC(sum(summary_data()$recent_cases_ep, na.rm = T)), " (", ifelse(case_change >= 0, "+", ""), case_change, "%)"),
               case_rate = summary_data()$recent_cases_ep_rate,
               us_cases = formatC(us_stats$cases, format="f", big.mark=",", digits=0),
               tot_deaths = formatC(sum(summary_data()$deaths, na.rm = T), format="f", big.mark=",", digits=0),
               today_deaths = paste0(formatC(sum(summary_data()$recent_deaths_dd, na.rm = T)), " (", ifelse(death_change >= 0, "+", ""), death_change, "%)"),
               death_rate = summary_data()$recent_deaths_dd_rate,
               us_deaths = formatC(us_stats$deaths, format="f", big.mark=",", digits=0)
    )
    
    
    
    tagList(
      fluidRow(
        column(6,
               h5(paste0("Cases (", selected_county_text(), ")"), style = case_style[["bold"]]),
               hr(style = "color: #1F2121; font-weight:bold;"),
               column(6,
                      div(
                        h2(stats[["tot_cases"]], style = case_style[["bold"]]),
                        h5("Total Confirmed Cases", style = case_style[["notbold"]]),
                        br(),
                        h3(stats[["today_cases"]], style = case_style[["bold"]]),
                        h5(paste0("Average per day between ", max_date - 7, " and ", max_date - 14), style = case_style[["notbold"]])
                      )
               ),
               column(6,
                      div(
                        h2(stats[["case_rate"]], style = case_style[["bold"]]),
                        h5(" Recent cases per 100k (7 day average)", style = case_style[["notbold"]]),
                        br(),
                        h4(stats[["us_cases"]], style = case_style[["notbold"]]),
                        h5("US Total Cases", style = case_style[["notbold"]])
                      )
               )
        ),
        column(6,
               h5(paste0("Deaths (", selected_county_text(), ")"), style = death_style[["bold"]]),
               hr(style = "color: #1F2121; font-weight:bold;"),
               column(6,
                      div(
                        h2(stats[["tot_deaths"]], style = death_style[["bold"]]),
                        h5("Total Confirmed Deaths", style = death_style[["notbold"]]),
                        br(),
                        h3(stats[["today_deaths"]], style = death_style[["bold"]]),
                        h5(paste0("Average per day between ", max_date - 7, " and ", max_date - 28), style = death_style[["notbold"]])
                      )
               ),
               column(6,
                      div(
                        h2(stats[["death_rate"]], style = death_style[["bold"]]),
                        h5("Recent deaths per 100k (7-day average)", style = death_style[["notbold"]]),
                        br(),
                        h4(stats[["us_deaths"]], style = death_style[["notbold"]]),
                        h5("US Total Deaths", style = death_style[["notbold"]])
                      )
               )
        )
      )# end fluidRow
    ) # end tagList
    
    
    
  })
  
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
