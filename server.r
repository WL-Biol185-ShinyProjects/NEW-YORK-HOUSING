server <- function(input, output) {
  
  # --- Filtered data for Market Overview ---
  market_df <- reactive({
    df <- nychousing %>% 
      filter(!is.na(Median.Sale.Price)) %>%
      mutate(
        Median.Sale.Price = as.numeric(gsub("\\$|K", "", Median.Sale.Price)) * 1000,
        date      = as.Date(paste("01", Month.of.Period.End), format = "%d %B %Y"),
        year_num  = year(date),
        month_num = month(date)
      )
    
    if (input$market_region == "Nassau County") {
      df <- df %>% filter(Region %in% c("Nassau County,NY", "Nassau County, NY metro area"))
    } else if (input$market_region == "New York City") {
      df <- df %>% filter(Region %in% c("New York, NY", "New York, NY metro area"))
    } else if (input$market_region == "Westchester County") {
      df <- df %>% filter(Region == "Westchester County, NY")
    }
    
    df %>% filter(year_num >= input$market_years[1] & year_num <= input$market_years[2])
  })
  
  # --- Determine which column to plot based on metric + change toggle ---
  plot_col <- reactive({
    if (input$market_change == "actual") {
      input$market_metric
    } else {
      paste0(input$market_metric, ".", input$market_change)
    }
  })
  
  # --- Clean the selected column to numeric ---
  clean_col <- reactive({
    col  <- plot_col()
    df   <- market_df()
    vals <- df[[col]]
    # Strip % signs if present
    as.numeric(gsub("%", "", vals))
  })
  
  # --- Most recent month for summary cards ---
  latest <- reactive({
    df  <- market_df()
    col <- input$market_metric
    
    mom_col <- paste0(col, ".MoM")
    yoy_col <- paste0(col, ".YoY")
    
    df %>%
      filter(date == max(date)) %>%
      summarise(
        val = mean(as.numeric(gsub("%", "", .data[[col]])),     na.rm = TRUE),
        mom = mean(as.numeric(gsub("%", "", .data[[mom_col]])), na.rm = TRUE),
        yoy = mean(as.numeric(gsub("%", "", .data[[yoy_col]])), na.rm = TRUE)
      )
  })
  
  # --- Summary Cards ---
  output$card_price <- renderText({
    val <- latest()$val
    if (input$market_metric == "Median.Sale.Price") {
      paste0("$", formatC(val / 1000, format = "f", digits = 0), "K")
    } else if (input$market_metric == "Average.Sale.To.List") {
      paste0(round(val, 1), "%")
    } else {
      formatC(val, format = "d", big.mark = ",")
    }
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
    df  <- market_df()
    col <- input$market_metric
    vals <- as.numeric(gsub("%", "", df[[col]]))
    high <- df[which.max(vals), ]
    val  <- max(vals, na.rm = TRUE)
    if (input$market_metric == "Median.Sale.Price") {
      paste0("$", formatC(val / 1000, format = "f", digits = 0), "K — ", format(high$date, "%b %Y"))
    } else {
      paste0(formatC(val, format = "f", digits = 1), " — ", format(high$date, "%b %Y"))
    }
  })
  
  output$card_low <- renderText({
    df  <- market_df()
    col <- input$market_metric
    vals <- as.numeric(gsub("%", "", df[[col]]))
    low  <- df[which.min(vals), ]
    val  <- min(vals, na.rm = TRUE)
    if (input$market_metric == "Median.Sale.Price") {
      paste0("$", formatC(val / 1000, format = "f", digits = 0), "K — ", format(low$date, "%b %Y"))
    } else {
      paste0(formatC(val, format = "f", digits = 1), " — ", format(low$date, "%b %Y"))
    }
  })
  
  # --- Market Overview Chart ---
  output$market_plot <- renderPlot({
    df  <- market_df()
    col <- plot_col()
    
    df <- df %>%
      mutate(plot_val = as.numeric(gsub("%", "", .data[[col]]))) %>%
      filter(!is.na(plot_val)) %>%
      group_by(date, Region) %>%
      summarise(plot_val = mean(plot_val, na.rm = TRUE), .groups = "drop")
    
    # Y axis label
    y_label <- names(which(c(
      "Median Sale Price"      = "Median.Sale.Price",
      "Homes Sold"             = "Homes.Sold",
      "New Listings"           = "New.Listings",
      "Inventory"              = "Inventory",
      "Days on Market"         = "Days.on.Market",
      "Avg Sale to List Ratio" = "Average.Sale.To.List"
    ) == input$market_metric))
    
    if (input$market_change == "MoM") y_label <- paste(y_label, "MoM (%)")
    if (input$market_change == "YoY") y_label <- paste(y_label, "YoY (%)")
    
    region_colors <- c(
      "Nassau County,NY"             = "#E63946",
      "Nassau County, NY metro area" = "#f4a261",
      "New York, NY"                 = "#457B9D",
      "New York, NY metro area"      = "#a8dadc",
      "Westchester County, NY"       = "#2A9D8F"
    )
    
    # High point annotation (only for actual value view)
    high_point <- df %>%
      filter(plot_val == max(plot_val, na.rm = TRUE)) %>%
      slice(1)
    
    p <- ggplot(df, aes(x = date, y = plot_val, color = Region, fill = Region))
    
    if (input$market_chart == "line") {
      p <- p +
        geom_line(linewidth = 1) +
        geom_point(data = high_point,
                   aes(x = date, y = plot_val),
                   color = "red", size = 4, shape = 18, inherit.aes = FALSE) +
        geom_label(data = high_point,
                   aes(x = date, y = plot_val,
                       label = paste0("High: ", round(plot_val, 1))),
                   nudge_y = max(df$plot_val, na.rm = TRUE) * 0.03,
                   size = 3.5, color = "red", inherit.aes = FALSE)
    } else {
      p <- p +
        geom_col(position = "dodge", alpha = 0.85)
    }
    
    # Add COVID line for actual value view
    if (input$market_change == "actual") {
      p <- p +
        geom_vline(xintercept = as.Date("2020-03-01"),
                   linetype = "dashed", color = "gray40") +
        annotate("text", x = as.Date("2020-03-01"), y = Inf,
                 label = "COVID-19", vjust = 2, hjust = -0.1,
                 size = 3, color = "gray40")
    }
    
    # Add zero line for MoM/YoY views
    if (input$market_change %in% c("MoM", "YoY")) {
      p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
    }
    
    p +
      scale_color_manual(values = region_colors) +
      scale_fill_manual(values  = region_colors) +
      labs(
        title    = paste(input$market_region, "-", y_label),
        subtitle = paste(input$market_years[1], "to", input$market_years[2]),
        x = "Date", y = y_label, color = "Region", fill = "Region"
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
        x = "Date", y = "Median Sale Price"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  })
}
