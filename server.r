server <- function(input, output) {
  
  # --- Clean helper: strips $, K, %, commas, and empty strings ---
  clean_numeric <- function(x) {
    x[x == ""] <- NA
    as.numeric(gsub("\\$|K|%|,", "", x))
  }
  
  # --- Filtered and cleaned data ---
  market_df <- reactive({
    nychousing %>%
      mutate(
        Median.Sale.Price            = clean_numeric(Median.Sale.Price) * 1000,
        Median.Sale.Price.MoM        = clean_numeric(Median.Sale.Price.MoM),
        Median.Sale.Price.YoY        = clean_numeric(Median.Sale.Price.YoY),
        Homes.Sold                   = clean_numeric(Homes.Sold),
        Homes.Sold.MoM               = clean_numeric(Homes.Sold.MoM),
        Homes.Sold.YoY               = clean_numeric(Homes.Sold.YoY),
        New.Listings                 = clean_numeric(New.Listings),
        New.Listings.MoM             = clean_numeric(New.Listings.MoM),
        New.Listings.YoY             = clean_numeric(New.Listings.YoY),
        Inventory                    = clean_numeric(Inventory),
        Inventory.MoM                = clean_numeric(Inventory.MoM),
        Inventory.YoY                = clean_numeric(Inventory.YoY),
        Days.on.Market               = clean_numeric(Days.on.Market),
        Days.on.Market.MoM           = clean_numeric(Days.on.Market.MoM),
        Days.on.Market.YoY           = clean_numeric(Days.on.Market.YoY),
        Average.Sale.To.List         = clean_numeric(Average.Sale.To.List),
        Average.Sale.To.List.MoM     = clean_numeric(Average.Sale.To.List.MoM),
        Average.Sale.To.List.YoY     = clean_numeric(Average.Sale.To.List.YoY),
        date      = as.Date(paste("01", Month.of.Period.End), format = "%d %B %Y"),
        year_num  = year(date),
        month_num = month(date),
        Region.Group = case_when(
          Region %in% c("Nassau County,NY", "Nassau County, NY metro area") ~ "Nassau County",
          Region %in% c("New York, NY", "New York, NY metro area")          ~ "New York City",
          Region == "Westchester County, NY"                                 ~ "Westchester",
          TRUE ~ "Other"
        )
      ) %>%
      filter(!is.na(Median.Sale.Price)) %>%
      { if (input$market_region != "All Regions")
        filter(., Region.Group == input$market_region)
        else . } %>%
      filter(year_num >= input$market_years[1] & year_num <= input$market_years[2])
  })
  
  # --- Determine which column to plot ---
  plot_col <- reactive({
    metric <- input$market_metric
    change <- input$market_change
    if (metric == "Inventory" && change == "actual") return("Inventory.MoM")
    if (change == "actual") metric else paste0(metric, ".", change)
  })
  
  # --- Latest month summary ---
  latest <- reactive({
    df  <- market_df()
    col     <- input$market_metric
    mom_col <- paste0(col, ".MoM")
    yoy_col <- paste0(col, ".YoY")
    
    df %>%
      group_by(date) %>%
      summarise(
        val = mean(.data[[col]],     na.rm = TRUE),
        mom = mean(.data[[mom_col]], na.rm = TRUE),
        yoy = mean(.data[[yoy_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(date == max(date, na.rm = TRUE))
  })
  
  # --- Format helper ---
  format_val <- function(val, metric) {
    if (is.na(val)) return("N/A")
    if (metric == "Median.Sale.Price") {
      paste0("$", formatC(val / 1000, format = "f", digits = 0), "K")
    } else if (metric %in% c("Average.Sale.To.List", "Inventory")) {
      paste0(round(val, 1), "%")
    } else {
      formatC(round(val, 0), format = "d", big.mark = ",")
    }
  }
  
  # --- Summary Cards ---
  output$card_price <- renderText({
    metric <- input$market_metric
    if (metric == "Inventory") return("See MoM/YoY")
    format_val(latest()$val, metric)
  })
  
  output$card_mom <- renderText({
    val <- latest()$mom
    if (is.na(val)) return("N/A")
    paste0(ifelse(val >= 0, "+", ""), round(val, 1), "%")
  })
  
  output$card_yoy <- renderText({
    val <- latest()$yoy
    if (is.na(val)) return("N/A")
    paste0(ifelse(val >= 0, "+", ""), round(val, 1), "%")
  })
  
  output$card_high <- renderText({
    metric <- input$market_metric
    if (metric == "Inventory") return("N/A")
    df   <- market_df()
    vals <- df[[metric]]
    if (all(is.na(vals))) return("N/A")
    idx  <- which.max(vals)
    paste0(format_val(vals[idx], metric), " — ", format(df$date[idx], "%b %Y"))
  })
  
  output$card_low <- renderText({
    metric <- input$market_metric
    if (metric == "Inventory") return("N/A")
    df   <- market_df()
    vals <- df[[metric]]
    if (all(is.na(vals))) return("N/A")
    idx  <- which.min(vals)
    paste0(format_val(vals[idx], metric), " — ", format(df$date[idx], "%b %Y"))
  })
  
  # --- Market Overview Chart ---
  output$market_plot <- renderPlot({
    df  <- market_df()
    col <- plot_col()
    
    df <- df %>%
      mutate(plot_val = .data[[col]]) %>%
      filter(!is.na(plot_val)) %>%
      group_by(date, Region.Group) %>%
      summarise(plot_val = mean(plot_val, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, label = "No data available for this selection",
                        size = 6, color = "gray50") +
               theme_void())
    }
    
    y_label <- names(which(c(
      "Median Sale Price"      = "Median.Sale.Price",
      "Homes Sold"             = "Homes.Sold",
      "New Listings"           = "New.Listings",
      "Inventory (MoM/YoY)"   = "Inventory",
      "Days on Market"         = "Days.on.Market",
      "Avg Sale to List Ratio" = "Average.Sale.To.List"
    ) == input$market_metric))
    
    if (input$market_change == "MoM") y_label <- paste(y_label, "MoM (%)")
    if (input$market_change == "YoY") y_label <- paste(y_label, "YoY (%)")
    
    region_colors <- c(
      "Nassau County" = "#E63946",
      "New York City" = "#457B9D",
      "Westchester"   = "#2A9D8F"
    )
    
    high_point <- df %>%
      filter(plot_val == max(plot_val, na.rm = TRUE)) %>%
      slice(1)
    
    p <- ggplot(df, aes(x = date, y = plot_val, color = Region.Group, fill = Region.Group))
    
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
      p <- p + geom_col(position = "dodge", alpha = 0.85)
    }
    
    if (input$market_change == "actual" && input$market_metric != "Inventory") {
      p <- p +
        geom_vline(xintercept = as.Date("2020-03-01"),
                   linetype = "dashed", color = "gray40") +
        annotate("text", x = as.Date("2020-03-01"), y = Inf,
                 label = "COVID-19", vjust = 2, hjust = -0.1,
                 size = 3, color = "gray40")
    }
    
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
    
    # Only forecast if pred_year is beyond the last date in the data
    if (forecast_end <= last_date) {
      forecast_df <- data.frame(date = as.Date(character(0))) %>%
        mutate(time_index = integer(0), month_num = integer(0))
    } else {
      forecast_df <- data.frame(
        date = seq.Date(last_date %m+% months(1), forecast_end, by = "month")
      ) %>%
        mutate(
          time_index = year(date) * 12 + month(date),
          month_num  = month(date)
        )
    }
    
    if (input$pred_region == "Nassau County") {
      forecast_df$Region <- factor("Nassau County,NY")
    } else if (input$pred_region == "New York City") {
      forecast_df$Region <- factor("New York, NY")
    }
    
    plot_df$fitted <- predict(model, newdata = plot_df)
    
    if (nrow(forecast_df) > 0) {
      forecast_df$Median.Sale.Price <- predict(model, newdata = forecast_df)
    }
    
    p <- ggplot() +
      geom_line(data = plot_df,
                aes(x = date, y = Median.Sale.Price, color = "Actual"),
                linewidth = 1) +
      geom_line(data = plot_df,
                aes(x = date, y = fitted, color = "Model Fit"),
                linewidth = 0.8, linetype = "dashed")
    
    if (nrow(forecast_df) > 0) {
      p <- p + geom_line(data = forecast_df,
                         aes(x = date, y = Median.Sale.Price, color = "Forecast"),
                         linewidth = 1, linetype = "dotted")
    }
    
    p +
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