---
title: "server.r"
output: shinyapp
date: "2026-02-16"
---



library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

# --- Load Data ---
nychousing <- read.csv("nychousing.csv")

# --- Assign Regions ---
nychousing$Region <- c(
  rep("Nassau County,NY",              times = 168),
  rep("Nassau County, NY metro area",  times = 168),
  rep("New York, NY",                  times = 128),
  rep("New York, NY metro area",       times = 128),
  rep("Westchester County, NY",        times = 128)
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
  nav_panel("Market Overview", "line graphs, average household pricing (monthly and yearly)"),
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

# --- Server ---
server <- function(input, output) {

  output$prediction_plot <- renderPlot({

    if (input$pred_region == "Nassau County") {
      plot_df <- nassau_df
      model   <- nassau_model
      plot_df <- plot_df %>%
        group_by(date, time_index, month_num) %>%
        summarise(Median.Sale.Price = mean(Median.Sale.Price, na.rm = TRUE), .groups = "drop") %>%
        mutate(Region = factor("Nassau County,NY"))

    } else if (input$pred_region == "New York City") {
      plot_df <- nyc_df
      model   <- nyc_model
      plot_df <- plot_df %>%
        group_by(date, time_index, month_num) %>%
        summarise(Median.Sale.Price = mean(Median.Sale.Price, na.rm = TRUE), .groups = "drop") %>%
        mutate(Region = factor("New York, NY"))

    } else {
      plot_df <- westchester_df
      model   <- westchester_model
      plot_df <- plot_df %>%
        group_by(date, time_index, month_num) %>%
        summarise(Median.Sale.Price = mean(Median.Sale.Price, na.rm = TRUE), .groups = "drop")
    }

    # --- Build forecast dates ---
    last_date    <- max(plot_df$date)
    forecast_end <- as.Date(paste0(input$pred_year, "-12-01"))

    forecast_df <- data.frame(
      date = seq.Date(last_date %m+% months(1), forecast_end, by = "month")
    ) %>%
      mutate(
        time_index = year(date) * 12 + month(date),
        month_num  = month(date)
      )

    if (input$pred_region == "Nassau County") {
      forecast_df$Region <- factor("Nassau County,NY")
    } else if (input$pred_region == "New York City") {
      forecast_df$Region <- factor("New York, NY")
    }

    # --- Fitted and forecast values ---
    plot_df$fitted             <- predict(model, newdata = plot_df)
    forecast_df$Median.Sale.Price <- predict(model, newdata = forecast_df)

    # --- Plot ---
    ggplot() +
      geom_line(data = plot_df,
                aes(x = date, y = Median.Sale.Price, color = "Actual"),
                linewidth = 1) +
      geom_line(data = plot_df,
                aes(x = date, y = fitted, color = "Model Fit"),
                linewidth = 0.8, linetype = "dashed") +
      geom_line(data = forecast_df,
                aes(x = date, y = Median.Sale.Price, color = "Forecast"),
                linewidth = 1, linetype = "dotted") +
      scale_color_manual(
        name   = "",
        values = c("Actual" = "#2C3E50", "Model Fit" = "#E63946", "Forecast" = "#457B9D")
      ) +
      scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
      labs(
        title    = paste(input$pred_region, "Median Sale Price"),
        subtitle = paste("Actual data with model fit and forecast through", input$pred_year),
        x = "Date",
        y = "Median Sale Price"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title      = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
}

shinyApp(ui = ui, server = server)

library(shiny) 
library(ggplot2)
library(tidyverse)


## R Markdown
#Nassau 1-336
#Ny, NY 337-464
#Ny Metro 465-592 
# Westchester 593-720
#c


nychousing <- read.csv("nychousing.csv")
nychousing$Region <- c(rep("Nassua County, NY", times = 168), 
              rep("Nassua County, NY metro area", times= 168), 
              rep("New York, NY", times= 128), 
              rep("New York, NY metro area", times = 128), 
              rep("Westchester County, NY", times = 128), 
              recursive = TRUE
              )






)

