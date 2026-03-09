---
title: "ui.r"
output: html_document
date: "2026-02-16"
---

  
```{r}
library(shiny)
```


```{r}
library(dplyr)
library(lubridate)

# --- Data Prep ---
nychousing <- nychousing %>%
  mutate(
    # Fix price: remove "$" and "K", convert to numeric (multiply by 1000)
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
nassau_model <- lm(Median.Sale.Price ~ time_index + month_num + Region,
                   data = nassau_df)

nyc_model <- lm(Median.Sale.Price ~ time_index + month_num + Region,
                data = nyc_df)

westchester_model <- lm(Median.Sale.Price ~ time_index + month_num,
                        data = westchester_df)

# --- Summaries ---
summary(nassau_model)
summary(nyc_model)
summary(westchester_model)


```

```{r}
library(shiny)
library(bslib)

ui <- page_navbar( 
  nav_panel("Home", "NYC metro area including data from NYC, Westchester, and Nassau County"),
  nav_panel("Market Overview", "line graphs, average household pricing (monthly and yearly), "), 
  nav_panel("Region Overview", "Page B content"), 
  nav_panel("Map","Page C content"),
  nav_panel("Prediction Model", 
            layout_sidebar(
                   sidebar = sidebar(
                    title = "Prediction Controls",
                    selectInput("pred_region", "Select Region:",
                      choices = c("Nassau County", "New York City", "Westchester County")
                    ),
                    numericInput("pred_year", "Forecast Through Year:",
                                 value = 2027, min = 2012, max = 2035),
                    hr(),
                    helpText("The line graph shows actual prices and the model fitted and forecasted trend.")
                  ),
                  plotOutput("prediction_plot", height = "75vh")
                ))
)

server <- function(input, output) {

}

shinyApp(ui = ui, server = server)
```