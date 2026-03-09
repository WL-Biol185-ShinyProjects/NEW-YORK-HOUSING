server <- function(input, output) {
  
  # --- Filtered data for Market Overview ---
  market_df <- reactive({
    df <- nychousing %>% filter(!is.na(Median.Sale.Price))
    
    # Filter by region
    if (input$market_region == "Nassau County") {
      df <- df %>% filter(Region %in% c("Nassau County,NY", "Nassau County, NY metro area"))
    } else if (input$market_region == "New York City") {
      df <- df %>% filter(Region %in% c("New York, NY", "New York, NY metro area"))
    } else if (input$market_region == "Westchester County") {
      df <- df %>% filter(Region == "Westchester County, NY")
    }
    
    # Filter by year range
    df %>% filter(year_num >= input$market_years[1] & year_num <= input$market_years[2])
  })
  
  # --- Most recent month for summary cards ---
  latest <- reactive({
    market_df() %>%
      filter(date == max(date)) %>%
      summarise(
        price = mean(Median.Sale.Price,       na.rm = TRUE),
        mom   = mean(Median.Sale.Price.MoM,   na.rm = TRUE),
        yoy   = mean(Median.Sale.Price.YoY,   na.rm = TRUE)
      )
  })
  
  # --- Summary Cards ---
  output$card_price <- renderText({
    paste0("$", formatC(latest()$price / 1000, format = "f", digits = 0), "K")
  })
  
  output$card_mom <- renderText({
    val <- latest()$mom
    paste0(ifelse(val >= 0, "+", ""), round(val, 1), "%")
  })
  
  output$card_yoy <- renderText({
    val <- latest()$yoy
    paste0(ifelse(val >= 0, "+", ""), round(val, 1), "%")
  })
  
  output$card_high <- renderText({
    high <- market_df() %>%
      filter(Median.Sale.Price == max(Median.Sale.Price, na.rm = TRUE)) %>%
      slice(1)
    paste0("$", formatC(high$Median.Sale.Price / 1000, format = "f", digits = 0), "K — ", format(high$date, "%b %Y"))
  })
  
  output$card_low <- renderText({
    low <- market_df() %>%
      filter(Median.Sale.Price == min(Median.Sale.Price, na.rm = TRUE)) %>%
      slice(1)
    paste0("$", formatC(low$Median.Sale.Price / 1000, format = "f", digits = 0), "K — ", format(low$date, "%b %Y"))
  })
  
  # --- Market Overview Line Chart ---
  output$market_plot <- renderPlot({
    df <- market_df() %>%
      group_by(date, Region) %>%
      summarise(Median.Sale.Price = mean(Median.Sale.Price, na.rm = TRUE), .groups = "drop")
    
    # Mark all-time high point
    high_point <- df %>% filter(Median.Sale.Price == max(Median.Sale.Price, na.rm = TRUE)) %>% slice(1)
    
    ggplot(df, aes(x = date, y = Median.Sale.Price, color = Region)) +
      geom_line(linewidth = 1) +
      geom_point(data = high_point, aes(x = date, y = Median.Sale.Price),
                 color = "red", size = 4, shape = 18) +
      geom_label(data = high_point,
                 aes(x = date, y = Median.Sale.Price,
                     label = paste0("High: $", formatC(Median.Sale.Price / 1000, format = "f", digits = 0), "K")),
                 nudge_y = 20000, size = 3.5, color = "red", inherit.aes = FALSE) +
      # COVID marker
      geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "gray40") +
      annotate("text", x = as.Date("2020-03-01"), y = Inf,
               label = "COVID-19", vjust = 2, hjust = -0.1, size = 3, color = "gray40") +
      scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
      scale_color_manual(values = c(
        "Nassau County,NY"             = "#E63946",
        "Nassau County, NY metro area" = "#f4a261",
        "New York, NY"                 = "#457B9D",
        "New York, NY metro area"      = "#a8dadc",
        "Westchester County, NY"       = "#2A9D8F"
      )) +
      labs(
        title    = paste(input$market_region, "Median Sale Price Over Time"),
        subtitle = paste(input$market_years[1], "to", input$market_years[2]),
        x = "Date", y = "Median Sale Price", color = "Region"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  })
  
  # --- Prediction Model Plot ---
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
    
    plot_df$fitted                <- predict(model, newdata = plot_df)
    forecast_df$Median.Sale.Price <- predict(model, newdata = forecast_df)
    
    ggplot() +
      geom_line(data = plot_df,
                aes(x = date, y = Median.Sale.Price, color = "Actual"), linewidth = 1) +
      geom_line(data = plot_df,
                aes(x = date, y = fitted, color = "Model Fit"), linewidth = 0.8, linetype = "dashed") +
      geom_line(data = forecast_df,
                aes(x = date, y = Median.Sale.Price, color = "Forecast"), linewidth = 1, linetype = "dotted") +
      scale_color_manual(
        name   = "",
        values = c("Actual" = "#2C3E50", "Model Fit" = "#E63946", "Forecast" = "#457B9D")
      ) +
      scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K")) +
      labs(
        title    = paste(input$pred_region, "Median Sale Price"),
        subtitle = paste("Actual data with model fit and forecast through", input$pred_year),
        x = "Date", y = "Median Sale Price"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  })
}

shinyApp(ui = ui, server = server)