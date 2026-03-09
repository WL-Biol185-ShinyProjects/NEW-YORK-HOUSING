library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

# --- Load Data ---
nychousing <- read.csv("nychousing.csv")

# --- Assign Regions ---
nychousing$Region <- c(
  rep("Nassau County,NY",             times = 168),
  rep("Nassau County, NY metro area", times = 168),
  rep("New York, NY",                 times = 128),
  rep("New York, NY metro area",      times = 128),
  rep("Westchester County, NY",       times = 128)
)

# --- Data Prep ---
nychousing <- nychousing %>%
  mutate(
    Median.Sale.Price = as.numeric(gsub("\\$|K", "", Median.Sale.Price)) * 1000,
    date       = as.Date(paste("01", Month.of.Period.End), format = "%d %B %Y"),
    time_index = year(date) * 12 + month(date),
    month_num  = month(date),
    year_num   = year(date),
    Region     = as.factor(Region)
  )

# --- Split into Three Regional Datasets ---
nassau_df <- nychousing %>%
  filter(Region %in% c("Nassau County,NY", "Nassau County, NY metro area")) %>%
  filter(!is.na(Median.Sale.Price)) %>%
  mutate(Region = droplevels(Region))

nyc_df <- nychousing %>%
  filter(Region %in% c("New York, NY", "New York, NY metro area")) %>%
  filter(!is.na(Median.Sale.Price)) %>%
  mutate(Region = droplevels(Region))

westchester_df <- nychousing %>%
  filter(Region %in% c("Westchester County, NY")) %>%
  filter(!is.na(Median.Sale.Price)) %>%
  mutate(Region = droplevels(Region))

# --- Build Models ---
nassau_model      <- lm(Median.Sale.Price ~ time_index + month_num + Region, data = nassau_df)
nyc_model         <- lm(Median.Sale.Price ~ time_index + month_num + Region, data = nyc_df)
westchester_model <- lm(Median.Sale.Price ~ time_index + month_num,          data = westchester_df)

# --- UI ---
ui <- page_navbar(
  title = "New York Housing Analysis",
  id    = "page",
  
  nav_panel("Home", "NYC metro area including data from NYC, Westchester, and Nassau County"),
  
  nav_panel("Market Overview",
            layout_sidebar(
              sidebar = sidebar(
                title = "Market Controls",
                selectInput("market_region", "Select Region:",
                            choices = c("All Regions", "Nassau County", "New York City", "Westchester County")
                ),
                sliderInput("market_years", "Year Range:",
                            min = 2012, max = 2024, value = c(2012, 2024), sep = ""
                ),
                hr(),
                helpText("Summary statistics reflect the most recent month in the selected range.")
              ),
              # --- Summary Cards ---
              layout_columns(
                value_box(
                  title   = "Current Median Price",
                  value   = textOutput("card_price"),
                  showcase = bsicons::bs_icon("house-fill"),
                  theme   = "primary"
                ),
                value_box(
                  title   = "Month-over-Month",
                  value   = textOutput("card_mom"),
                  showcase = bsicons::bs_icon("arrow-left-right"),
                  theme   = "success"
                ),
                value_box(
                  title   = "Year-over-Year",
                  value   = textOutput("card_yoy"),
                  showcase = bsicons::bs_icon("calendar"),
                  theme   = "info"
                ),
                value_box(
                  title   = "All-Time High",
                  value   = textOutput("card_high"),
                  showcase = bsicons::bs_icon("graph-up-arrow"),
                  theme   = "warning"
                ),
                value_box(
                  title   = "All-Time Low",
                  value   = textOutput("card_low"),
                  showcase = bsicons::bs_icon("graph-down-arrow"),
                  theme   = "danger"
                )
              ),
              # --- Line Chart ---
              plotOutput("market_plot", height = "50vh")
            )
  ),
  
  nav_panel("Region Overview", "Page B content"),
  nav_panel("Map", "Page C content"),
  
  nav_panel("Prediction Model",
            layout_sidebar(
              sidebar = sidebar(
                title = "Prediction Controls",
                selectInput("pred_region", "Select Region:",
                            choices = c("Nassau County", "New York City", "Westchester County")
                ),
                numericInput("pred_year", "Forecast Through Year:",
                             value = 2026, min = 2012, max = 2035),
                hr(),
                helpText("The line graph shows actual prices and the model fitted and forecasted trend.")
              ),
              plotOutput("prediction_plot", height = "75vh")
            )
  )
)