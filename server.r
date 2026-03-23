library(leaflet)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# --- Load county shapefiles (runs once at startup) ---
ny_counties <- counties(state = "NY", cb = TRUE, resolution = "5m") %>%
  filter(COUNTYFP %in% c(
    "059",  # Nassau
    "061",  # New York (Manhattan)
    "047",  # Kings (Brooklyn)
    "081",  # Queens
    "005",  # Bronx
    "085",  # Richmond (Staten Island)
    "119"   # Westchester
  )) %>%
  mutate(Region.Group = case_when(
    COUNTYFP == "119" ~ "Westchester",
    COUNTYFP == "059" ~ "Nassau County",
    TRUE              ~ "New York City"
  ))

function(input, output) {
  
  # --- Load raw data once inside server ---
  nychousing <- read.csv("nychousing.csv")
  
  nychousing$Region <- c(
    rep("Nassau County,NY",             times = 168),
    rep("Nassau County, NY metro area", times = 168),
    rep("New York, NY",                 times = 128),
    rep("New York, NY metro area",      times = 128),
    rep("Westchester County, NY",       times = 128)
  )
  
  # --- Clean helper ---
  clean_numeric <- function(x) {
    x[x == ""] <- NA
    as.numeric(gsub("\\$|K|%|,", "", x))
  }
  
  # --- Region lookup ---
  region_map <- c(
    "Nassau County"      = "Nassau County",
    "New York City"      = "New York City",
    "Westchester County" = "Westchester"
  )
  
  # --- Shared cleaned + grouped dataset ---
  clean_df <- reactive({
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
        date       = as.Date(paste("01", Month.of.Period.End), format = "%d %B %Y"),
        year_num   = year(date),
        month_num  = month(date),
        time_index = year(date) * 12 + month(date),
        Region.Group = case_when(
          Region %in% c("Nassau County,NY", "Nassau County, NY metro area") ~ "Nassau County",
          Region %in% c("New York, NY", "New York, NY metro area")          ~ "New York City",
          Region == "Westchester County, NY"                                 ~ "Westchester",
          TRUE ~ "Other"
        )
      )
  })
  
  # ============================================================
  # MARKET OVERVIEW
  # ============================================================
  
  market_df <- reactive({
    df <- clean_df() %>% filter(!is.na(Median.Sale.Price))
    if (input$market_region != "All Regions") {
      df <- df %>% filter(Region.Group == region_map[input$market_region])
    }
    df %>% filter(year_num >= input$market_years[1] & year_num <= input$market_years[2])
  })
  
  plot_col <- reactive({
    metric <- input$market_metric
    change <- input$market_change
    if (metric == "Inventory" && change == "actual") return("Inventory.MoM")
    if (change == "actual") metric else paste0(metric, ".", change)
  })
  
  latest <- reactive({
    df      <- market_df()
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
  
  output$market_plot <- renderPlot({
    req(input$market_region, input$market_metric, input$market_change)
    df  <- market_df()
    col <- plot_col()
    
    df <- df %>%
      mutate(plot_val = .data[[col]]) %>%
      filter(!is.na(plot_val)) %>%
      group_by(date, Region.Group) %>%
      summarise(plot_val = mean(plot_val, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = "No data available for this selection",
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
      "Nassau County" = "#0A1929",
      "New York City" = "#1565C0",
      "Westchester"   = "#A8C8E8"
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
  
  # ============================================================
  # REGION OVERVIEW
  # ============================================================
  
  region_colors <- c(
    "Nassau County" = "#0A1929",
    "New York City" = "#1565C0",
    "Westchester"   = "#A8C8E8"
  )
  
  region_col <- reactive({
    metric <- input$region_metric
    change <- input$region_change
    if (change == "actual") metric else paste0(metric, ".", change)
  })
  
  region_df <- reactive({
    col <- region_col()
    clean_df() %>%
      filter(!is.na(.data[[col]])) %>%
      filter(year_num >= input$region_years[1],
             year_num <= input$region_years[2]) %>%
      group_by(date, Region.Group) %>%
      summarise(value = mean(.data[[col]], na.rm = TRUE), .groups = "drop")
  })
  
  region_latest <- reactive({
    region_df() %>%
      filter(date == max(date)) %>%
      arrange(desc(value))
  })
  
  format_region_val <- function(val, metric, change) {
    if (is.na(val)) return("N/A")
    if (change %in% c("MoM", "YoY")) {
      paste0(ifelse(val >= 0, "+", ""), round(val, 1), "%")
    } else if (metric == "Median.Sale.Price") {
      paste0("$", formatC(val / 1000, format = "f", digits = 0), "K")
    } else if (metric == "Average.Sale.To.List") {
      paste0(round(val * 100, 1), "%")
    } else {
      formatC(round(val, 0), format = "d", big.mark = ",")
    }
  }
  
  make_scorecard <- function(region_name, display_name, color) {
    renderUI({
      metric  <- input$region_metric
      change  <- input$region_change
      col     <- region_col()
      mom_col <- paste0(metric, ".MoM")
      yoy_col <- paste0(metric, ".YoY")
      
      base <- clean_df() %>% filter(Region.Group == region_name)
      
      latest_val <- base %>%
        filter(!is.na(.data[[col]]), date == max(date[!is.na(.data[[col]])])) %>%
        summarise(v = mean(.data[[col]], na.rm = TRUE)) %>%
        pull(v)
      
      mom_val <- base %>%
        filter(!is.na(.data[[mom_col]]), date == max(date[!is.na(.data[[mom_col]])])) %>%
        summarise(v = mean(.data[[mom_col]], na.rm = TRUE)) %>%
        pull(v)
      
      yoy_val <- base %>%
        filter(!is.na(.data[[yoy_col]]), date == max(date[!is.na(.data[[yoy_col]])])) %>%
        summarise(v = mean(.data[[yoy_col]], na.rm = TRUE)) %>%
        pull(v)
      
      val_fmt <- format_region_val(if (length(latest_val)) latest_val else NA, metric, change)
      mom_fmt <- if (length(mom_val) && !is.na(mom_val))
        paste0(ifelse(mom_val >= 0, "+", ""), round(mom_val, 1), "% MoM") else ""
      yoy_fmt <- if (length(yoy_val) && !is.na(yoy_val))
        paste0(ifelse(yoy_val >= 0, "+", ""), round(yoy_val, 1), "% YoY") else ""
      
      div(
        style = paste0(
          "background-color:", color, "; color: white; border-radius: 10px;",
          "padding: 20px 15px; text-align: center; min-height: 160px;",
          "display: flex; flex-direction: column; justify-content: center;"
        ),
        tags$strong(style = "font-size: 14px; opacity: 0.85;", display_name),
        tags$div(style = "font-size: 26px; font-weight: bold; margin: 8px 0;", val_fmt),
        tags$div(style = "font-size: 13px; opacity: 0.85;", mom_fmt),
        tags$div(style = "font-size: 13px; opacity: 0.85;", yoy_fmt)
      )
    })
  }
  
  output$scorecard_nassau      <- make_scorecard("Nassau County", "Nassau County", "#0A1929")
  output$scorecard_nyc         <- make_scorecard("New York City", "New York City", "#1565C0")
  output$scorecard_westchester <- make_scorecard("Westchester",   "Westchester",   "#A8C8E8")
  
  output$region_line_plot <- renderPlot({
    df <- region_df()
    
    y_label <- case_when(
      input$region_change == "MoM" ~ paste(input$region_metric, "MoM (%)"),
      input$region_change == "YoY" ~ paste(input$region_metric, "YoY (%)"),
      input$region_metric == "Median.Sale.Price" ~ "Median Sale Price",
      TRUE ~ input$region_metric
    )
    
    y_scale <- if (input$region_change %in% c("MoM", "YoY")) {
      scale_y_continuous(labels = function(x) paste0(x, "%"))
    } else if (input$region_metric == "Median.Sale.Price") {
      scale_y_continuous(labels = scales::dollar_format(scale = 0.001, suffix = "K"))
    } else if (input$region_metric == "Average.Sale.To.List") {
      scale_y_continuous(labels = scales::percent_format(scale = 100))
    } else {
      scale_y_continuous(labels = scales::comma)
    }
    
    ggplot(df, aes(x = date, y = value, color = Region.Group)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = region_colors) +
      y_scale +
      labs(title = paste("All Regions —", y_label),
           x = "Date", y = y_label, color = "Region") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  })
  
  output$region_rank_plot <- renderPlot({
    metric <- input$region_metric
    change <- input$region_change
    df     <- region_latest()
    
    bar_label <- if (change %in% c("MoM", "YoY")) {
      function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 1), "%")
    } else if (metric == "Median.Sale.Price") {
      function(x) paste0("$", round(x / 1000, 0), "K")
    } else {
      function(x) formatC(round(x, 0), format = "d", big.mark = ",")
    }
    
    ggplot(df, aes(x = reorder(Region.Group, value), y = value, fill = Region.Group)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = bar_label(value)), hjust = -0.1, size = 3.5) +
      coord_flip() +
      scale_fill_manual(values = region_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(title = paste("Region Ranking —", metric), x = NULL, y = NULL) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  output$region_change_table <- renderTable({
    metric  <- input$region_metric
    mom_col <- paste0(metric, ".MoM")
    yoy_col <- paste0(metric, ".YoY")
    
    latest_date <- max(clean_df()$date, na.rm = TRUE)
    
    clean_df() %>%
      filter(date == latest_date) %>%
      group_by(Region.Group) %>%
      summarise(
        `Latest Value` = mean(.data[[metric]], na.rm = TRUE),
        `MoM Change`   = mean(.data[[mom_col]], na.rm = TRUE),
        `YoY Change`   = mean(.data[[yoy_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        `Latest Value` = if (metric == "Median.Sale.Price")
          paste0("$", formatC(`Latest Value` / 1000, format = "f", digits = 0), "K")
        else if (metric == "Average.Sale.To.List")
          paste0(round(`Latest Value` * 100, 1), "%")
        else
          formatC(round(`Latest Value`, 0), format = "d", big.mark = ","),
        `MoM Change` = paste0(ifelse(`MoM Change` >= 0, "+", ""), round(`MoM Change`, 1), "%"),
        `YoY Change` = paste0(ifelse(`YoY Change` >= 0, "+", ""), round(`YoY Change`, 1), "%")
      ) %>%
      rename(Region = Region.Group)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ============================================================
  # MAP
  # ============================================================
  
  map_data <- reactive({
    metric <- input$map_metric
    change <- input$map_change
    col    <- if (change == "actual") metric else paste0(metric, ".", change)
    
    latest_date <- max(clean_df()$date, na.rm = TRUE)
    
    clean_df() %>%
      filter(date == latest_date) %>%
      group_by(Region.Group) %>%
      summarise(
        value                 = mean(.data[[col]],             na.rm = TRUE),
        Median.Sale.Price     = mean(Median.Sale.Price,        na.rm = TRUE),
        Median.Sale.Price.MoM = mean(Median.Sale.Price.MoM,    na.rm = TRUE),
        Median.Sale.Price.YoY = mean(Median.Sale.Price.YoY,    na.rm = TRUE),
        Homes.Sold            = mean(Homes.Sold,               na.rm = TRUE),
        Days.on.Market        = mean(Days.on.Market,           na.rm = TRUE),
        New.Listings          = mean(New.Listings,             na.rm = TRUE),
        Average.Sale.To.List  = mean(Average.Sale.To.List,     na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  output$map_plot <- renderLeaflet({
    df     <- map_data()
    metric <- input$map_metric
    change <- input$map_change
    
    map_sf <- ny_counties %>%
      left_join(df, by = "Region.Group")
    
    pal <- colorNumeric(
      palette  = "YlOrRd",
      domain   = map_sf$value,
      na.color = "#cccccc"
    )
    
    val_label <- function(val) {
      if (is.na(val)) return("N/A")
      if (change %in% c("MoM", "YoY")) {
        paste0(ifelse(val >= 0, "+", ""), round(val, 1), "%")
      } else if (metric == "Median.Sale.Price") {
        paste0("$", formatC(val / 1000, format = "f", digits = 0), "K")
      } else if (metric == "Average.Sale.To.List") {
        paste0(round(val * 100, 1), "%")
      } else {
        formatC(round(val, 0), format = "d", big.mark = ",")
      }
    }
    
    metric_name <- switch(metric,
                          "Median.Sale.Price"    = "Median Sale Price",
                          "Homes.Sold"           = "Homes Sold",
                          "New.Listings"         = "New Listings",
                          "Days.on.Market"       = "Days on Market",
                          "Average.Sale.To.List" = "Avg Sale-to-List"
    )
    change_name <- switch(change,
                          "actual" = "",
                          "MoM"    = " (MoM)",
                          "YoY"    = " (YoY)"
    )
    
    labels <- lapply(seq_len(nrow(map_sf)), function(i) {
      row       <- map_sf[i, ]
      rg        <- row$Region.Group
      price_fmt <- paste0("$", formatC(row$Median.Sale.Price / 1000, format = "f", digits = 0), "K")
      mom_fmt   <- paste0(ifelse(row$Median.Sale.Price.MoM >= 0, "+", ""),
                          round(row$Median.Sale.Price.MoM, 1), "%")
      yoy_fmt   <- paste0(ifelse(row$Median.Sale.Price.YoY >= 0, "+", ""),
                          round(row$Median.Sale.Price.YoY, 1), "%")
      sold_fmt  <- formatC(round(row$Homes.Sold, 0), format = "d", big.mark = ",")
      dom_fmt   <- round(row$Days.on.Market, 0)
      list_fmt  <- formatC(round(row$New.Listings, 0), format = "d", big.mark = ",")
      stl_fmt   <- paste0(round(row$Average.Sale.To.List * 100, 1), "%")
      sel_fmt   <- val_label(row$value)
      
      htmltools::HTML(paste0(
        "<div style='font-family: sans-serif; min-width: 200px;'>",
        "<div style='background:#2C3E50; color:white; padding:8px 12px; border-radius:6px 6px 0 0;'>",
        "<strong style='font-size:14px;'>", rg, "</strong></div>",
        "<div style='padding:10px 12px; border:1px solid #ddd; border-top:none; border-radius:0 0 6px 6px;'>",
        "<table style='width:100%; font-size:12px; border-collapse:collapse;'>",
        "<tr style='background:#f9f9f9;'><td style='padding:4px 6px; color:#555;'>",
        metric_name, change_name,
        "</td><td style='padding:4px 6px; font-weight:bold; text-align:right;'>", sel_fmt, "</td></tr>",
        "<tr><td style='padding:4px 6px; color:#555;'>Median Sale Price</td>",
        "<td style='padding:4px 6px; text-align:right;'>", price_fmt, "</td></tr>",
        "<tr style='background:#f9f9f9;'><td style='padding:4px 6px; color:#555;'>MoM Change</td>",
        "<td style='padding:4px 6px; text-align:right;'>", mom_fmt, "</td></tr>",
        "<tr><td style='padding:4px 6px; color:#555;'>YoY Change</td>",
        "<td style='padding:4px 6px; text-align:right;'>", yoy_fmt, "</td></tr>",
        "<tr style='background:#f9f9f9;'><td style='padding:4px 6px; color:#555;'>Homes Sold</td>",
        "<td style='padding:4px 6px; text-align:right;'>", sold_fmt, "</td></tr>",
        "<tr><td style='padding:4px 6px; color:#555;'>New Listings</td>",
        "<td style='padding:4px 6px; text-align:right;'>", list_fmt, "</td></tr>",
        "<tr style='background:#f9f9f9;'><td style='padding:4px 6px; color:#555;'>Days on Market</td>",
        "<td style='padding:4px 6px; text-align:right;'>", dom_fmt, "</td></tr>",
        "<tr><td style='padding:4px 6px; color:#555;'>Sale-to-List Ratio</td>",
        "<td style='padding:4px 6px; text-align:right;'>", stl_fmt, "</td></tr>",
        "</table></div></div>"
      ))
    })
    
    leaflet(map_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(value),
        fillOpacity = 0.75,
        color       = "white",
        weight      = 2,
        highlight   = highlightOptions(
          weight       = 3,
          color        = "#2C3E50",
          fillOpacity  = 0.9,
          bringToFront = TRUE
        ),
        label        = lapply(labels, htmltools::HTML),
        labelOptions = labelOptions(
          style     = list("box-shadow" = "3px 3px 6px rgba(0,0,0,0.2)"),
          textsize  = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal      = pal,
        values   = ~value,
        title    = paste0(metric_name, change_name),
        position = "bottomright",
        labFormat = if (change %in% c("MoM", "YoY")) {
          labelFormat(suffix = "%")
        } else if (metric == "Median.Sale.Price") {
          labelFormat(prefix = "$", suffix = "K",
                      transform = function(x) round(x / 1000, 0))
        } else {
          labelFormat()
        }
      ) %>%
      setView(lng = -73.7, lat = 40.85, zoom = 10) %>%
      fitBounds(lng1 = -74.05, lat1 = 40.45, lng2 = -73.1, lat2 = 41.2)
  })
  
  # ============================================================
  # PREDICTION MODEL
  # ============================================================
  
  output$prediction_plot <- renderPlot({
    req(input$pred_region, input$pred_year)
    
    base <- clean_df() %>% filter(!is.na(Median.Sale.Price))
    
    model_df <- if (input$pred_region == "Nassau County") {
      base %>% filter(Region.Group == "Nassau County")
    } else if (input$pred_region == "New York City") {
      base %>% filter(Region.Group == "New York City")
    } else {
      base %>% filter(Region.Group == "Westchester")
    }
    
    model <- lm(Median.Sale.Price ~ time_index + month_num, data = model_df)
    
    plot_df <- model_df %>%
      group_by(date, time_index, month_num) %>%
      summarise(Median.Sale.Price = mean(Median.Sale.Price, na.rm = TRUE), .groups = "drop")
    
    last_date    <- max(plot_df$date, na.rm = TRUE)
    forecast_end <- as.Date(paste0(input$pred_year, "-12-01"))
    
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