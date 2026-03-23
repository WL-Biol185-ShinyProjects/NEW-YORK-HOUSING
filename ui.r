library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(tigris)

# --- UI ---
ui <- page_navbar(
  title     = "New York Housing Analysis",
  id        = "page",
  underline = FALSE,
  header = tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=DM+Serif+Display&display=swap",
      rel  = "stylesheet"
    ),
    tags$style(HTML("
      body, .navbar, .sidebar, p, h1, h2, h3, h4, h5, h6,
      .value-box, table, .shiny-text-output {
        font-family: 'DM Serif Display', serif !important;
       }
    "))
  ),
  
  nav_panel("Home",
            div(
              style = paste0(
                "position: relative; height: calc(100vh - 54px); overflow: hidden;",
                "display: flex; align-items: center; justify-content: center;"
              ),
              tags$img(
                src   = "https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/Southwest_corner_of_Central_Park%2C_looking_east%2C_NYC.jpg/1280px-Southwest_corner_of_Central_Park%2C_looking_east%2C_NYC.jpg",
                style = paste0(
                  "position: absolute; top: 0; left: 0;",
                  "width: 100%; height: 100%;",
                  "object-fit: cover; object-position: center;",
                  "filter: brightness(0.45);"
                )
              ),
              div(
                style = paste0(
                  "position: relative; z-index: 1;",
                  "text-align: center; color: white; padding: 20px;"
                ),
                tags$h1(
                  "NYC METRO HOUSING",
                  style = paste0(
                    "font-size: 64px; font-weight: 900;",
                    "letter-spacing: 4px; margin-bottom: 16px;",
                    "text-shadow: 2px 2px 8px rgba(0,0,0,0.8);"
                  )
                ),
                tags$p(
                  "Exploring the NYC Metro Area Housing Market",
                  style = paste0(
                    "font-size: 24px; font-weight: 300;",
                    "letter-spacing: 2px;",
                    "text-shadow: 1px 1px 4px rgba(0,0,0,0.8);"
                  )
                )
              )
            )
  ),
  
  nav_panel("Market Overview",
            layout_sidebar(
              sidebar = sidebar(
                title = "Market Controls",
                selectInput("market_region", "Select Region:",
                            choices = c("All Regions", "Nassau County", "New York City", "Westchester County")
                ),
                sliderInput("market_years", "Year Range:",
                            min = 2016, max = 2025, value = c(2016, 2025), sep = ""
                ),
                selectInput("market_metric", "Select Metric:",
                            choices = c(
                              "Median Sale Price"      = "Median.Sale.Price",
                              "Homes Sold"             = "Homes.Sold",
                              "New Listings"           = "New.Listings",
                              "Inventory (MoM/YoY)"   = "Inventory",
                              "Days on Market"         = "Days.on.Market",
                              "Avg Sale to List Ratio" = "Average.Sale.To.List"
                            )
                ),
                selectInput("market_change", "View Change As:",
                            choices = c(
                              "Actual Value"     = "actual",
                              "Month-over-Month" = "MoM",
                              "Year-over-Year"   = "YoY"
                            )
                ),
                radioButtons("market_chart", "Chart Type:",
                             choices  = c("Line Graph" = "line", "Bar Graph" = "bar"),
                             selected = "line"
                ),
                hr(),
                helpText("Inventory has no absolute value — use MoM or YoY for inventory trends."),
                helpText("Summary statistics reflect the most recent month in the selected range.")
              ),
              
              layout_columns(
                value_box(
                  title = "Current Value",
                  value = textOutput("card_price"),
                  style = "background-color: #A8C8E8; color: white;"
                ),
                value_box(
                  title = "Month-over-Month",
                  value = textOutput("card_mom"),
                  style = "background-color: #7BA7D1; color: white;"
                ),
                value_box(
                  title = "Year-over-Year",
                  value = textOutput("card_yoy"),
                  style = "background-color: #4F86BA; color: white;"
                ),
                value_box(
                  title = "All-Time High",
                  value = textOutput("card_high"),
                  style = "background-color: #2E6699; color: white;"
                ),
                value_box(
                  title = "All-Time Low",
                  value = textOutput("card_low"),
                  style = "background-color: #1A4670; color: white;"
                )
              ),
              
              plotOutput("market_plot", height = "50vh")
            )
  ),
  
  nav_panel("Region Overview",
            layout_sidebar(
              fill = FALSE,
              sidebar = sidebar(
                title  = "Region Controls",
                width  = 250,
                selectInput("region_metric", "Select Metric:",
                            choices = c(
                              "Median Sale Price"      = "Median.Sale.Price",
                              "Homes Sold"             = "Homes.Sold",
                              "New Listings"           = "New.Listings",
                              "Inventory"              = "Inventory",
                              "Days on Market"         = "Days.on.Market",
                              "Avg Sale to List Ratio" = "Average.Sale.To.List"
                            )
                ),
                selectInput("region_change", "View Change As:",
                            choices = c(
                              "Actual Value"     = "actual",
                              "Month-over-Month" = "MoM",
                              "Year-over-Year"   = "YoY"
                            )
                ),
                sliderInput("region_years", "Year Range:",
                            min = 2016, max = 2025, value = c(2016, 2025), sep = ""
                ),
                hr(),
                helpText("Compare all regions side by side. Use the ranking chart to see which region leads each metric.")
              ),
              h4("Regional Scorecards — Latest Month"),
              layout_columns(
                col_widths = c(4, 4, 4),
                fill       = FALSE,
                uiOutput("scorecard_nassau"),
                uiOutput("scorecard_nyc"),
                uiOutput("scorecard_westchester")
              ),
              hr(),
              h4("All Regions Over Time"),
              plotOutput("region_line_plot", height = "500px"),
              hr(),
              h4("Region Ranking — Most Recent Month"),
              plotOutput("region_rank_plot", height = "350px"),
              hr(),
              h4("Month-over-Month & Year-over-Year Changes"),
              tableOutput("region_change_table")
            )
  ),
  
  nav_panel("Map",
            layout_sidebar(
              fill = FALSE,
              sidebar = sidebar(
                title = "Map Controls",
                width = 250,
                selectInput("map_metric", "Select Metric:",
                            choices = c(
                              "Median Sale Price"      = "Median.Sale.Price",
                              "Homes Sold"             = "Homes.Sold",
                              "New Listings"           = "New.Listings",
                              "Days on Market"         = "Days.on.Market",
                              "Avg Sale to List Ratio" = "Average.Sale.To.List"
                            )
                ),
                selectInput("map_change", "View Change As:",
                            choices = c(
                              "Actual Value"     = "actual",
                              "Month-over-Month" = "MoM",
                              "Year-over-Year"   = "YoY"
                            )
                ),
                hr(),
                helpText("Hover over a county to see the latest housing statistics for that region.")
              ),
              leafletOutput("map_plot", height = "850px")
            )
  ),
  
  nav_panel("Prediction Model",
            layout_sidebar(
              sidebar = sidebar(
                title = "Prediction Controls",
                selectInput("pred_region", "Select Region:",
                            choices = c("Nassau County", "New York City", "Westchester County")
                ),
                numericInput("pred_year", "Forecast Through Year:",
                             value = 2026, min = 2016, max = 2035
                ),
                hr(),
                helpText("The line graph shows actual prices and the model fitted and forecasted trend.")
              ),
              plotOutput("prediction_plot", height = "75vh")
            )
  )
)