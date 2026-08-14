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
library(purrr)

options(scipen=999)

'^M.*csv'
'%!in%' <- function(x,y)!('%in%'(x,y))

cases_deaths <- read_csv("https://data.chhs.ca.gov/dataset/f333528b-4d38-4814-bebb-12db1f10f535/resource/046cdd2b-31e5-4d34-9ed3-b48cdbc4be7a/download/covid19cases_test.csv") %>%
  select(date, county = area, dof_pop = population, cases_epdate = cases, deaths_DOD = deaths) %>%
  filter(county %!in% c("California", "Out of state", "Unknown"))


us_stats <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv?_sm_au_=iVVWPQ545LD7stpRGqtBJK3R1GQBC") %>%
  filter(date == max(as_date(date)))


left_col_attr <- list(`Confirmed Recent Daily Cases` = c("recent_cases_ep", "cases_epdate_chart"),
                      `Cases per 100k 7 day avg` = c("recent_cases_ep_rate", "recent_cases_ep_rate_chart"),
                      `Confirmed Recent Daily Deaths` = c("recent_deaths_dd", "deaths_DOD_chart"),
                      `Deaths per 100k 7 day avg` = c("recent_deaths_dd_rate", "recent_deaths_dd_rate_chart")
)

ui <- fluidPage(theme = shinytheme("united"),
                
                navbarPage("California Covid-19 Dashboard",
                           tabPanel("Cases",
                                    # fluidRow(radioGroupButtons(
                                    #     inputId = "source",
                                    #     label = "Choose Data",
                                    #     choices = c("Cases", "Testing", "Demographics"),
                                    #     individual = TRUE,
                                    #     checkIcon = list(
                                    #         yes = tags$i(class = "fa fa-circle", 
                                    #                      style = "color: steelblue"),
                                    #         no = tags$i(class = "fa fa-circle-o", 
                                    #                     style = "color: steelblue"))
                                    # )
                                    #),
                                    column(3,
                                           wellPanel(
                                             selectInput("select_metric", label = "Select metric", choices = names(left_col_attr), selected = "Recent Cases"),
                                             sliderInput("daterange",
                                                         "Epi curves date range:",
                                                         min = as.Date("2020-01-01","%Y-%m-%d"),
                                                         max = as.Date(max(as_date(cases_deaths$date), na.rm = T),"%Y-%m-%d"),
                                                         value=c(as.Date("2020-01-01"), max(as_date(cases_deaths$date), na.rm = T)),
                                                         timeFormat="%b %Y"),
                                             dataTableOutput("county_table"), style = "height:1000px; overflow-y: scroll;overflow-x: scroll;")
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
  
  

  tDatDOF  <- cases_deaths %>% 
    distinct(county, dof_pop) 
  
  max_date <- max(as_date(cases_deaths$date), na.rm = T)
  
  county_alpha <- select(tDatDOF, county) %>% arrange(county) %>% pull()
  
  unit.scale <- function(x) round((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)), 6)
  
  chart_left <- function(bar_color = "lightblue", prop_value) {
    paste0("<span style=\"display: inline-block; direction: rtl; unicode-bidi: plaintext; border-radius: 4px; padding-right: 2px; font-size: 0px; background-color: lightblue; width: ", unit.scale(prop_value) * 100, "%\">", prop_value, "</span>")
  }
   # input <- list()
   # input$daterange[1] <- as_date("2021-01-01")
   # input$daterange[2] <- as_date(max(cases_deaths$date, na.rm = T))
   # input$county_table_rows_selected <- 13

  output$county_table <- DT::renderDataTable({
    
    # Setting the table 
    display_date <- as_date(input$daterange[2]) - 7
    rep_cases_deaths <- cases_deaths %>%
      group_by(county) %>%
      mutate(cumulative_cases = cumsum(cases_epdate),
             cumulative_deaths = cumsum(deaths_DOD),
             recent_cases_ep = round(frollsum(cases_epdate, 14) / 14, 1), 
             recent_deaths_dd = round(frollsum(deaths_DOD, 28) / 28, 1),
             across(c(recent_cases_ep, recent_deaths_dd), ~round((.x /  dof_pop) * 100000, 3), .names = "{.col}_rate"),
             across(!!unname(unlist(map(left_col_attr, 1))), 
                    ~chart_left(prop_value = round(.x, 0)), .names = "{.col}_chart")) %>%
      ungroup() %>%
      filter(date == display_date - 7) 

    
    rep_cases_deaths_tbl <- rep_cases_deaths %>%
      select(county, left_col_attr[[input$select_metric]][1])
    
    DT::datatable(data = rep_cases_deaths_tbl, selection=list(mode="single", target="row"), rownames = T,
                  options = list(dom = "t", 
                                 paging = FALSE,
                                 order = list(list(2, 'desc')),
                                 columnDefs = list(list(visible=FALSE, targets=c(0))),
                                 headerCallback = JS(
                                   "function(thead, data, start, end, display){",
                                   "  $(thead).remove();",
                                   "}")
                  )) %>%
      formatStyle(left_col_attr[[input$select_metric]][1],
                  background = styleColorBar(rep_cases_deaths[[left_col_attr[[input$select_metric]][1]]], 'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'right')
    
  }, server = TRUE)
  
  
  selected_county <- reactive({
    county_alpha[input$county_table_rows_selected]
  })
  
  selected_county_text <- reactive({
    if (length(input$county_table_rows_selected) != 0){
      paste0(county_alpha[input$county_table_rows_selected], " county")
    } else {
      "Statewide"
    }
  })
  
  summary_data <- reactive({
    
    display_date <- as_date(input$daterange[2]) - 7
  
    if (length(input$county_table_rows_selected) == 1){
    cases_deaths %>%
        group_by(county) %>%
        mutate(cumulative_cases = cumsum(cases_epdate),
               cumulative_deaths = cumsum(deaths_DOD),
               recent_cases_ep = round(frollsum(cases_epdate, 14) / 14, 1), 
               recent_deaths_dd = round(frollsum(deaths_DOD, 28) / 28, 1),
               across(c(recent_cases_ep, recent_deaths_dd), ~round((.x /  dof_pop) * 100000, 3), .names = "{.col}_rate")) %>%
        ungroup() %>%
        filter(county == county_alpha[input$county_table_rows_selected] & (date == display_date | date == display_date - 7)) %>%
        select(cases = cumulative_cases, deaths = cumulative_deaths, recent_cases_ep, recent_deaths_dd, recent_cases_ep_rate, recent_deaths_dd_rate, dof_pop)

    } else {
      cases_deaths %>%
        group_by(date) %>%
        summarize(across(c(cases_epdate, deaths_DOD, dof_pop), ~sum(.x, na.rm = T) )) %>%
        mutate(cumulative_cases = cumsum(cases_epdate),
               cumulative_deaths = cumsum(deaths_DOD),
               recent_cases_ep = round(frollsum(cases_epdate, 14) / 14, 1), 
               recent_deaths_dd = round(frollsum(deaths_DOD, 28) / 28, 1),
               across(c(recent_cases_ep, recent_deaths_dd), ~round((.x /  dof_pop) * 100000, 3), .names = "{.col}_rate")) %>%
        filter(date == display_date | date == display_date - 7) %>%
        select(cases = cumulative_cases, deaths = cumulative_deaths, recent_cases_ep, recent_deaths_dd, recent_cases_ep_rate, recent_deaths_dd_rate, dof_pop)
    }
    

  })
  
  plot_data <- reactive({
    display_date <- as_date(input$daterange[2]) - 7
    if (length(input$county_table_rows_selected) == 1){
      cases_deaths %>%
        filter(county == county_alpha[input$county_table_rows_selected] & between(date, as_date(input$daterange[[1]]), as_date(input$daterange[[2]]))) %>%
        select(date, cases = cases_epdate, deaths = deaths_DOD) %>%
        mutate(roll_mean_cases = frollmean(cases, 7), roll_mean_deaths = frollmean(deaths, 7)) %>%
        filter(between(date, as_date("2020-03-01"), display_date))
    } else {
      cases_deaths %>%
        filter(between(date, as_date(input$daterange[[1]]), as_date(input$daterange[[2]]))) %>%
        group_by(date) %>%
        summarize(cases = sum(cases_epdate), deaths = sum(deaths_DOD)) %>%
        mutate(roll_mean_cases = frollmean(cases, 7), roll_mean_deaths = frollmean(deaths, 7)) %>%
        filter(between(date, as_date("2020-03-01"), display_date))
    }
  })
  # alpha <- c(petunia = "today_cases_rep")
  # select(rep_cases_deaths, county, today_cases_rep) %>%
  #     arrange(desc(.data[[alpha[["petunia"]]]]))
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
    
    display_date <- as_date(input$daterange[2]) - 7
    
    recent <- unlist(summary_data()[1,])
    previous <- unlist(summary_data()[2,])
    
    print(recent)
    print(previous)
    case_change <- round(((recent[["recent_cases_ep"]] - previous[["recent_cases_ep"]]) / previous[["recent_cases_ep"]]) * 100, 1)
    death_change <- round(((recent[["recent_deaths_dd"]] - previous[["recent_deaths_dd"]]) / previous[["recent_deaths_dd"]]) * 100, 1)
    
    stats <- c(tot_cases = formatC(recent[["cases"]], format="f", big.mark=",", digits=0),
               today_cases = paste0(formatC(recent[["recent_cases_ep"]], format="f", big.mark=",", digits=0), " (", ifelse(case_change >= 0, "+", ""), case_change, "%)"),
               case_rate = recent[["recent_cases_ep_rate"]],
               us_cases = formatC(us_stats$cases, format="f", big.mark=",", digits=0),
               tot_deaths = formatC(recent[["deaths"]], format="f", big.mark=",", digits=0),
               today_deaths = paste0(formatC(recent[["recent_deaths_dd"]], format="f", big.mark=",", digits=0), " (", ifelse(death_change >= 0, "+", ""), death_change, "%)"),
               death_rate = recent[["recent_deaths_dd_rate"]],
               us_deaths = formatC(us_stats$deaths, format="f", big.mark=",", digits=0)
    
    )
    
    #print(stats)
    
    
    tagList(
      fluidRow(
        column(6,
               h5(paste0("Cases (", selected_county_text(), ")"), style = case_style[["bold"]]),
               hr(style = "color: #1F2121; font-weight:bold;"),
               column(6,
                      div(
                        h2(stats[["today_cases"]], style = case_style[["bold"]]),
                        h6(paste0("Average per day between ", display_date - 7, " and ", display_date), style = case_style[["notbold"]]),
                        br(),
                        h3(stats[["tot_cases"]], style = case_style[["bold"]]),
                        h5("Total Confirmed Cases", style = case_style[["notbold"]])
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
                        h2(stats[["today_deaths"]], style = death_style[["bold"]]),
                        h5(paste0("Average per day between ", display_date - 28, " and ", display_date), style = death_style[["notbold"]]),
                        br(),
                        h3(stats[["tot_deaths"]], style = death_style[["bold"]]),
                        h5("Total Confirmed Deaths", style = death_style[["notbold"]])
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
