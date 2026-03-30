library(leaflet)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# --- Load county shapefiles (runs once at startup) ---
ny_counties <- suppressWarnings(
  counties(state = "NY", cb = TRUE, resolution = "5m") %>%
    filter(COUNTYFP %in% c("059","061","047","081","005","085","119")) %>%
    mutate(Region.Group = case_when(
      COUNTYFP == "119" ~ "Westchester",
      COUNTYFP == "059" ~ "Nassau County",
      TRUE              ~ "New York City"
    ))
)

# --- Region-specific images with price tier fallback ---
home_image <- function(price, region_name = NULL) {
  if (!is.null(region_name)) {
    if (region_name == "Nassau County") {
      if (price < 600000)
        return("https://images.unsplash.com/photo-1570129477492-45c003edd2be?w=600&q=80")
      else
        return("https://images.unsplash.com/photo-1600585154340-be6161a56a0c?w=600&q=80")
    } else if (region_name == "New York City") {
      if (price < 700000)
        return("https://images.unsplash.com/photo-1555636222-cae831e670b3?w=600&q=80")
      else
        return("https://images.unsplash.com/photo-1486325212027-8081e485255e?w=600&q=80")
    } else if (region_name == "Westchester") {
      if (price < 700000)
        return("https://images.unsplash.com/photo-1568605114967-8130f3a36994?w=600&q=80")
      else
        return("https://images.unsplash.com/photo-1600596542815-ffad4c1539a9?w=600&q=80")
    }
  }
  # Price tier fallback
  if (price < 400000)       "https://images.unsplash.com/photo-1568605114967-8130f3a36994?w=600&q=80"
  else if (price < 700000)  "https://images.unsplash.com/photo-1570129477492-45c003edd2be?w=600&q=80"
  else if (price < 1000000) "https://images.unsplash.com/photo-1600596542815-ffad4c1539a9?w=600&q=80"
  else                      "https://images.unsplash.com/photo-1613977257363-707ba9348227?w=600&q=80"
}

home_tier_label <- function(price) {
  if (price < 400000)       "Starter Home"
  else if (price < 700000)  "Mid-Range Home"
  else if (price < 1000000) "Upper Mid-Range Home"
  else                      "Luxury Home"
}

function(input, output) {
  
  nychousing <- read.csv("nychousing.csv")
  
  nychousing$Region <- c(
    rep("Nassau County,NY",             times = 168),
    rep("Nassau County, NY metro area", times = 168),
    rep("New York, NY",                 times = 128),
    rep("New York, NY metro area",      times = 128),
    rep("Westchester County, NY",       times = 128)
  )
  
  clean_numeric <- function(x) {
    x[x == ""] <- NA
    as.numeric(gsub("\\$|K|%|,", "", x))
  }
  
  region_map <- c(
    "Nassau County"      = "Nassau County",
    "New York City"      = "New York City",
    "Westchester County" = "Westchester"
  )
  
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
    if (metric == "Median.Sale.Price") paste0("$", formatC(val / 1000, format = "f", digits = 0), "K")
    else if (metric %in% c("Average.Sale.To.List", "Inventory")) paste0(round(val, 1), "%")
    else formatC(round(val, 0), format = "d", big.mark = ",")
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
    df <- market_df(); vals <- df[[metric]]
    if (all(is.na(vals))) return("N/A")
    idx <- which.max(vals)
    paste0(format_val(vals[idx], metric), " — ", format(df$date[idx], "%b %Y"))
  })
  output$card_low <- renderText({
    metric <- input$market_metric
    if (metric == "Inventory") return("N/A")
    df <- market_df(); vals <- df[[metric]]
    if (all(is.na(vals))) return("N/A")
    idx <- which.min(vals)
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
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="No data available", size=6, color="gray50") + theme_void())
    }
    y_label <- names(which(c(
      "Median Sale Price"="Median.Sale.Price","Homes Sold"="Homes.Sold",
      "New Listings"="New.Listings","Inventory (MoM/YoY)"="Inventory",
      "Days on Market"="Days.on.Market","Avg Sale to List Ratio"="Average.Sale.To.List"
    ) == input$market_metric))
    if (input$market_change == "MoM") y_label <- paste(y_label, "MoM (%)")
    if (input$market_change == "YoY") y_label <- paste(y_label, "YoY (%)")
    region_colors <- c("Nassau County"="#0A1929","New York City"="#1565C0","Westchester"="#A8C8E8")
    high_point <- df %>% filter(plot_val == max(plot_val, na.rm=TRUE)) %>% slice(1)
    p <- ggplot(df, aes(x=date, y=plot_val, color=Region.Group, fill=Region.Group))
    if (input$market_chart == "line") {
      p <- p + geom_line(linewidth=1) +
        geom_point(data=high_point, aes(x=date,y=plot_val), color="red", size=4, shape=18, inherit.aes=FALSE) +
        geom_label(data=high_point, aes(x=date,y=plot_val,label=paste0("High: ",round(plot_val,1))),
                   nudge_y=max(df$plot_val,na.rm=TRUE)*0.03, size=3.5, color="red", inherit.aes=FALSE)
    } else {
      p <- p + geom_col(position="dodge", alpha=0.85)
    }
    if (input$market_change == "actual" && input$market_metric != "Inventory") {
      p <- p + geom_vline(xintercept=as.Date("2020-03-01"), linetype="dashed", color="gray40") +
        annotate("text", x=as.Date("2020-03-01"), y=Inf, label="COVID-19", vjust=2, hjust=-0.1, size=3, color="gray40")
    }
    if (input$market_change %in% c("MoM","YoY")) p <- p + geom_hline(yintercept=0, linetype="dashed", color="gray40")
    p + scale_color_manual(values=region_colors) + scale_fill_manual(values=region_colors) +
      labs(title=paste(input$market_region,"-",y_label), subtitle=paste(input$market_years[1],"to",input$market_years[2]),
           x="Date", y=y_label, color="Region", fill="Region") +
      theme_minimal(base_size=14) + theme(plot.title=element_text(face="bold"), legend.position="bottom")
  })
  
  # ============================================================
  # REGION OVERVIEW
  # ============================================================
  
  region_colors <- c("Nassau County"="#0A1929","New York City"="#1565C0","Westchester"="#A8C8E8")
  
  region_col <- reactive({
    metric <- input$region_metric; change <- input$region_change
    if (change == "actual") metric else paste0(metric, ".", change)
  })
  
  region_df <- reactive({
    col <- region_col()
    clean_df() %>%
      filter(!is.na(.data[[col]]), year_num >= input$region_years[1], year_num <= input$region_years[2]) %>%
      group_by(date, Region.Group) %>%
      summarise(value = mean(.data[[col]], na.rm=TRUE), .groups="drop")
  })
  
  region_latest <- reactive({
    region_df() %>% filter(date == max(date)) %>% arrange(desc(value))
  })
  
  format_region_val <- function(val, metric, change) {
    if (is.na(val)) return("N/A")
    if (change %in% c("MoM","YoY")) paste0(ifelse(val>=0,"+",""), round(val,1), "%")
    else if (metric == "Median.Sale.Price") paste0("$", formatC(val/1000, format="f", digits=0), "K")
    else if (metric == "Average.Sale.To.List") paste0(round(val*100,1), "%")
    else formatC(round(val,0), format="d", big.mark=",")
  }
  
  make_scorecard <- function(region_name, display_name, color) {
    renderUI({
      metric <- input$region_metric; change <- input$region_change
      col <- region_col(); mom_col <- paste0(metric,".MoM"); yoy_col <- paste0(metric,".YoY")
      base <- clean_df() %>% filter(Region.Group == region_name)
      latest_val <- base %>% filter(!is.na(.data[[col]]), date==max(date[!is.na(.data[[col]])])) %>%
        summarise(v=mean(.data[[col]], na.rm=TRUE)) %>% pull(v)
      mom_val <- base %>% filter(!is.na(.data[[mom_col]]), date==max(date[!is.na(.data[[mom_col]])])) %>%
        summarise(v=mean(.data[[mom_col]], na.rm=TRUE)) %>% pull(v)
      yoy_val <- base %>% filter(!is.na(.data[[yoy_col]]), date==max(date[!is.na(.data[[yoy_col]])])) %>%
        summarise(v=mean(.data[[yoy_col]], na.rm=TRUE)) %>% pull(v)
      val_fmt <- format_region_val(if(length(latest_val)) latest_val else NA, metric, change)
      mom_fmt <- if(length(mom_val)&&!is.na(mom_val)) paste0(ifelse(mom_val>=0,"+",""),round(mom_val,1),"% MoM") else ""
      yoy_fmt <- if(length(yoy_val)&&!is.na(yoy_val)) paste0(ifelse(yoy_val>=0,"+",""),round(yoy_val,1),"% YoY") else ""
      div(style=paste0("background-color:",color,"; color:white; border-radius:10px; padding:20px 15px; text-align:center; min-height:160px; display:flex; flex-direction:column; justify-content:center;"),
          tags$strong(style="font-size:14px; opacity:0.85;", display_name),
          tags$div(style="font-size:26px; font-weight:bold; margin:8px 0;", val_fmt),
          tags$div(style="font-size:13px; opacity:0.85;", mom_fmt),
          tags$div(style="font-size:13px; opacity:0.85;", yoy_fmt))
    })
  }
  
  output$scorecard_nassau      <- make_scorecard("Nassau County","Nassau County","#0A1929")
  output$scorecard_nyc         <- make_scorecard("New York City","New York City","#1565C0")
  output$scorecard_westchester <- make_scorecard("Westchester","Westchester","#A8C8E8")
  
  output$region_line_plot <- renderPlot({
    df <- region_df()
    y_label <- case_when(
      input$region_change=="MoM" ~ paste(input$region_metric,"MoM (%)"),
      input$region_change=="YoY" ~ paste(input$region_metric,"YoY (%)"),
      input$region_metric=="Median.Sale.Price" ~ "Median Sale Price",
      TRUE ~ input$region_metric)
    y_scale <- if(input$region_change %in% c("MoM","YoY")) scale_y_continuous(labels=function(x) paste0(x,"%"))
    else if(input$region_metric=="Median.Sale.Price") scale_y_continuous(labels=scales::dollar_format(scale=0.001,suffix="K"))
    else if(input$region_metric=="Average.Sale.To.List") scale_y_continuous(labels=scales::percent_format(scale=100))
    else scale_y_continuous(labels=scales::comma)
    ggplot(df, aes(x=date, y=value, color=Region.Group)) + geom_line(linewidth=1) +
      scale_color_manual(values=region_colors) + y_scale +
      labs(title=paste("All Regions —",y_label), x="Date", y=y_label, color="Region") +
      theme_minimal(base_size=13) + theme(plot.title=element_text(face="bold"), legend.position="bottom")
  })
  
  output$region_rank_plot <- renderPlot({
    metric <- input$region_metric; change <- input$region_change; df <- region_latest()
    bar_label <- if(change %in% c("MoM","YoY")) function(x) paste0(ifelse(x>=0,"+",""),round(x,1),"%")
    else if(metric=="Median.Sale.Price") function(x) paste0("$",round(x/1000,0),"K")
    else function(x) formatC(round(x,0), format="d", big.mark=",")
    ggplot(df, aes(x=reorder(Region.Group,value), y=value, fill=Region.Group)) +
      geom_col(show.legend=FALSE) + geom_text(aes(label=bar_label(value)), hjust=-0.1, size=3.5) +
      coord_flip() + scale_fill_manual(values=region_colors) +
      scale_y_continuous(expand=expansion(mult=c(0,0.15))) +
      labs(title=paste("Region Ranking —",metric), x=NULL, y=NULL) +
      theme_minimal(base_size=13) + theme(plot.title=element_text(face="bold"))
  })
  
  output$region_change_table <- renderTable({
    metric <- input$region_metric; mom_col <- paste0(metric,".MoM"); yoy_col <- paste0(metric,".YoY")
    latest_date <- max(clean_df()$date, na.rm=TRUE)
    clean_df() %>% filter(date==latest_date) %>% group_by(Region.Group) %>%
      summarise(`Latest Value`=mean(.data[[metric]],na.rm=TRUE),
                `MoM Change`=mean(.data[[mom_col]],na.rm=TRUE),
                `YoY Change`=mean(.data[[yoy_col]],na.rm=TRUE), .groups="drop") %>%
      mutate(
        `Latest Value` = if(metric=="Median.Sale.Price") paste0("$",formatC(`Latest Value`/1000,format="f",digits=0),"K")
        else if(metric=="Average.Sale.To.List") paste0(round(`Latest Value`*100,1),"%")
        else formatC(round(`Latest Value`,0),format="d",big.mark=","),
        `MoM Change` = paste0(ifelse(`MoM Change`>=0,"+",""),round(`MoM Change`,1),"%"),
        `YoY Change` = paste0(ifelse(`YoY Change`>=0,"+",""),round(`YoY Change`,1),"%")
      ) %>% rename(Region=Region.Group)
  }, striped=TRUE, hover=TRUE, bordered=TRUE)
  
  # ============================================================
  # MAP
  # ============================================================
  
  map_data <- reactive({
    metric <- input$map_metric; change <- input$map_change
    col <- if(change=="actual") metric else paste0(metric,".",change)
    latest_date <- max(clean_df()$date, na.rm=TRUE)
    clean_df() %>% filter(date==latest_date) %>% group_by(Region.Group) %>%
      summarise(value=mean(.data[[col]],na.rm=TRUE),
                Median.Sale.Price=mean(Median.Sale.Price,na.rm=TRUE),
                Median.Sale.Price.MoM=mean(Median.Sale.Price.MoM,na.rm=TRUE),
                Median.Sale.Price.YoY=mean(Median.Sale.Price.YoY,na.rm=TRUE),
                Homes.Sold=mean(Homes.Sold,na.rm=TRUE),
                Days.on.Market=mean(Days.on.Market,na.rm=TRUE),
                New.Listings=mean(New.Listings,na.rm=TRUE),
                Average.Sale.To.List=mean(Average.Sale.To.List,na.rm=TRUE), .groups="drop")
  })
  
  output$map_plot <- renderLeaflet({
    df <- map_data(); metric <- input$map_metric; change <- input$map_change
    map_sf <- ny_counties %>% left_join(df, by="Region.Group")
    pal <- colorNumeric(palette="YlOrRd", domain=map_sf$value, na.color="#cccccc")
    val_label <- function(val) {
      if(is.na(val)) return("N/A")
      if(change %in% c("MoM","YoY")) paste0(ifelse(val>=0,"+",""),round(val,1),"%")
      else if(metric=="Median.Sale.Price") paste0("$",formatC(val/1000,format="f",digits=0),"K")
      else if(metric=="Average.Sale.To.List") paste0(round(val*100,1),"%")
      else formatC(round(val,0),format="d",big.mark=",")
    }
    metric_name <- switch(metric,"Median.Sale.Price"="Median Sale Price","Homes.Sold"="Homes Sold",
                          "New.Listings"="New Listings","Days.on.Market"="Days on Market","Average.Sale.To.List"="Avg Sale-to-List")
    change_name <- switch(change,"actual"="","MoM"=" (MoM)","YoY"=" (YoY)")
    labels <- lapply(seq_len(nrow(map_sf)), function(i) {
      row <- map_sf[i,]; rg <- row$Region.Group
      htmltools::HTML(paste0(
        "<div style='font-family:sans-serif; min-width:200px;'>",
        "<div style='background:#2C3E50; color:white; padding:8px 12px; border-radius:6px 6px 0 0;'>",
        "<strong style='font-size:14px;'>",rg,"</strong></div>",
        "<div style='padding:10px 12px; border:1px solid #ddd; border-top:none; border-radius:0 0 6px 6px;'>",
        "<table style='width:100%; font-size:12px; border-collapse:collapse;'>",
        "<tr style='background:#f9f9f9;'><td style='padding:4px 6px; color:#555;'>",metric_name,change_name,
        "</td><td style='padding:4px 6px; font-weight:bold; text-align:right;'>",val_label(row$value),"</td></tr>",
        "<tr><td style='padding:4px 6px; color:#555;'>Median Sale Price</td><td style='padding:4px 6px; text-align:right;'>",
        paste0("$",formatC(row$Median.Sale.Price/1000,format="f",digits=0),"K"),"</td></tr>",
        "<tr style='background:#f9f9f9;'><td style='padding:4px 6px; color:#555;'>MoM Change</td><td style='padding:4px 6px; text-align:right;'>",
        paste0(ifelse(row$Median.Sale.Price.MoM>=0,"+",""),round(row$Median.Sale.Price.MoM,1),"%"),"</td></tr>",
        "<tr><td style='padding:4px 6px; color:#555;'>YoY Change</td><td style='padding:4px 6px; text-align:right;'>",
        paste0(ifelse(row$Median.Sale.Price.YoY>=0,"+",""),round(row$Median.Sale.Price.YoY,1),"%"),"</td></tr>",
        "<tr style='background:#f9f9f9;'><td style='padding:4px 6px; color:#555;'>Homes Sold</td><td style='padding:4px 6px; text-align:right;'>",
        formatC(round(row$Homes.Sold,0),format="d",big.mark=","),"</td></tr>",
        "<tr><td style='padding:4px 6px; color:#555;'>New Listings</td><td style='padding:4px 6px; text-align:right;'>",
        formatC(round(row$New.Listings,0),format="d",big.mark=","),"</td></tr>",
        "<tr style='background:#f9f9f9;'><td style='padding:4px 6px; color:#555;'>Days on Market</td><td style='padding:4px 6px; text-align:right;'>",
        round(row$Days.on.Market,0),"</td></tr>",
        "<tr><td style='padding:4px 6px; color:#555;'>Sale-to-List Ratio</td><td style='padding:4px 6px; text-align:right;'>",
        paste0(round(row$Average.Sale.To.List*100,1),"%"),"</td></tr>",
        "</table></div></div>"
      ))
    })
    leaflet(map_sf) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor=~pal(value), fillOpacity=0.75, color="white", weight=2,
                  highlight=highlightOptions(weight=3,color="#2C3E50",fillOpacity=0.9,bringToFront=TRUE),
                  label=lapply(labels,htmltools::HTML),
                  labelOptions=labelOptions(style=list("box-shadow"="3px 3px 6px rgba(0,0,0,0.2)"),textsize="13px",direction="auto")) %>%
      addLegend(pal=pal, values=~value, title=paste0(metric_name,change_name), position="bottomright",
                labFormat=if(change %in% c("MoM","YoY")) labelFormat(suffix="%")
                else if(metric=="Median.Sale.Price") labelFormat(prefix="$",suffix="K",transform=function(x) round(x/1000,0))
                else labelFormat()) %>%
      setView(lng=-73.7, lat=40.85, zoom=10) %>%
      fitBounds(lng1=-74.05, lat1=40.45, lng2=-73.1, lat2=41.2)
  })
  
  # ============================================================
  # AFFORDABILITY CALCULATOR (IMPROVED ALGORITHM)
  # ============================================================
  
  aff_calc <- reactive({
    income  <- input$aff_income
    down    <- input$aff_downpayment
    rate    <- input$aff_rate / 100 / 12
    term    <- as.numeric(input$aff_term) * 12
    tax_ins <- input$aff_tax_insurance
    max_monthly  <- (income / 12) * 0.28
    max_mortgage <- max(max_monthly - tax_ins, 0)
    max_loan <- if (rate == 0) max_mortgage * term else
      max_mortgage * (1 - (1 + rate)^(-term)) / rate
    max_price <- max_loan + down
    list(max_price=max_price, max_loan=max_loan, max_monthly=max_monthly,
         monthly_pmt=max_mortgage, down=down)
  })
  
  output$aff_max_price <- renderText({
    val <- aff_calc()$max_price
    if (val <= 0) return("N/A")
    paste0("$", formatC(round(val/1000,0), format="d", big.mark=","), "K")
  })
  output$aff_monthly <- renderText({
    val <- aff_calc()$monthly_pmt
    if (val <= 0) return("N/A")
    paste0("$", formatC(round(val,0), format="d", big.mark=","), "/mo")
  })
  output$aff_loan <- renderText({
    val <- aff_calc()$max_loan
    if (val <= 0) return("N/A")
    paste0("$", formatC(round(val/1000,0), format="d", big.mark=","), "K")
  })
  output$aff_budget <- renderText({
    paste0("$", formatC(round(aff_calc()$max_monthly,0), format="d", big.mark=","), "/mo")
  })
  
  # --- Enriched region prices: pulls market health metrics for scoring ---
  region_prices <- reactive({
    latest_date <- max(clean_df()$date, na.rm=TRUE)
    clean_df() %>%
      filter(date == latest_date, Region.Group != "Other") %>%
      group_by(Region.Group) %>%
      summarise(
        price             = mean(Median.Sale.Price, na.rm=TRUE),
        yoy_change        = mean(Median.Sale.Price.YoY, na.rm=TRUE),
        days_on_market    = mean(Days.on.Market, na.rm=TRUE),
        sale_to_list      = mean(Average.Sale.To.List, na.rm=TRUE),
        homes_sold        = mean(Homes.Sold, na.rm=TRUE),
        new_listings      = mean(New.Listings, na.rm=TRUE),
        inventory_mom     = mean(Inventory.MoM, na.rm=TRUE),
        .groups="drop"
      )
  })
  
  # --- IMPROVED: Composite scoring for best region ---
  # Scores each affordable region on 4 dimensions (0-100 total)
  # so different user inputs produce different recommendations
  best_region_scored <- reactive({
    max_price <- aff_calc()$max_price
    prices    <- region_prices()
    
    # Score ALL regions (even unaffordable ones get scored for the chart)
    scored <- prices %>%
      mutate(
        is_affordable = price <= max_price,
        
        # 1. Budget Fit Score (0-30 pts)
        #    Rewards regions that USE your budget well (85-100% of max)
        #    instead of always picking the cheapest
        budget_ratio   = price / max_price,
        headroom_score = case_when(
          !is_affordable                          ~ 0,
          budget_ratio >= 0.85 & budget_ratio <= 1.0 ~ 30,  # Sweet spot
          budget_ratio >= 0.70 & budget_ratio < 0.85 ~ 22,  # Good
          budget_ratio >= 0.50 & budget_ratio < 0.70 ~ 14,  # Lots of room
          TRUE ~ 8                                            # Very cheap vs budget
        ),
        
        # 2. Market Stability Score (0-25 pts)
        #    Prefers moderate/stable YoY growth; penalizes overheating
        momentum_score = case_when(
          yoy_change >= -2 & yoy_change <= 3   ~ 25,  # Stable
          yoy_change > 3  & yoy_change <= 6    ~ 20,  # Moderate growth
          yoy_change > 6  & yoy_change <= 10   ~ 12,  # Hot
          yoy_change > 10                       ~ 5,   # Overheating
          yoy_change < -2 & yoy_change >= -5   ~ 18,  # Slight dip = opportunity
          TRUE ~ 8                                      # Big decline
        ),
        
        # 3. Buyer Competitiveness Score (0-25 pts)
        #    More days on market + sale-to-list <= 100% = better for buyers
        compete_score = case_when(
          days_on_market >= 60 & sale_to_list <= 98 ~ 25,
          days_on_market >= 40 & sale_to_list <= 100 ~ 20,
          days_on_market >= 25 & sale_to_list <= 102 ~ 14,
          days_on_market >= 15 & sale_to_list <= 104 ~ 8,
          TRUE ~ 4
        ),
        
        # 4. Inventory Health Score (0-20 pts)
        #    Growing inventory = more options for buyers
        inventory_score = case_when(
          inventory_mom > 5   ~ 20,
          inventory_mom > 2   ~ 16,
          inventory_mom > 0   ~ 12,
          inventory_mom > -3  ~ 8,
          TRUE ~ 4
        ),
        
        # Total composite score (affordable regions only get full score)
        total_score = ifelse(is_affordable,
                             headroom_score + momentum_score + compete_score + inventory_score,
                             momentum_score + compete_score + inventory_score)  # partial score for chart
      ) %>%
      arrange(desc(is_affordable), desc(total_score))
    
    scored
  })
  
  # --- Helper: generate human-readable reason for recommendation ---
  score_reason <- function(row) {
    reasons <- c()
    if (row$headroom_score >= 22) reasons <- c(reasons, "Great use of your budget")
    else if (row$headroom_score >= 14) reasons <- c(reasons, "Comfortable budget fit")
    else if (row$headroom_score > 0)   reasons <- c(reasons, "Well below your budget")
    
    if (row$momentum_score >= 20) reasons <- c(reasons, "Stable price trends")
    else if (row$momentum_score <= 12) reasons <- c(reasons, "Rapidly rising prices")
    
    if (row$compete_score >= 20)  reasons <- c(reasons, "Buyer-friendly market")
    else if (row$compete_score <= 8) reasons <- c(reasons, "Competitive market")
    
    if (row$inventory_score >= 16) reasons <- c(reasons, "Growing inventory")
    else if (row$inventory_score <= 8) reasons <- c(reasons, "Tight inventory")
    
    if (length(reasons) == 0) reasons <- c("Balanced across all factors")
    paste(reasons, collapse = " · ")
  }
  
  # --- Best Region Recommendation Card ---
  output$aff_best_region <- renderUI({
    max_price <- aff_calc()$max_price
    prices    <- region_prices()
    scored    <- best_region_scored()
    
    region_desc <- list(
      "Nassau County" = "Nassau County offers suburban communities with good schools and easy access to NYC via commuter rail.",
      "New York City" = "New York City offers urban living with world-class amenities, transit, and diverse neighborhoods.",
      "Westchester"   = "Westchester offers a mix of suburban towns and villages with a quieter lifestyle north of the city."
    )
    
    affordable <- scored %>% filter(is_affordable)
    
    if (nrow(affordable) == 0) {
      # No affordable regions — show closest option
      closest <- scored %>% arrange(price) %>% slice(1)
      gap     <- closest$price - max_price
      div(style="background-color:#B71C1C; color:white; border-radius:10px; padding:20px; margin-bottom:10px;",
          tags$strong(style="font-size:16px;", paste0(
            "\u26A0 No regions are currently within your budget")),
          tags$p(style="margin:8px 0 0;",
                 paste0("Your closest option is ", closest$Region.Group, " — you are $",
                        formatC(round(gap/1000,0), format="d", big.mark=","),
                        "K short of the median price. Consider increasing your down payment or income.")))
    } else {
      best      <- affordable %>% slice(1)
      best_rg   <- best$Region.Group
      best_img  <- home_image(best$price, best_rg)
      best_tier <- home_tier_label(best$price)
      best_desc <- region_desc[[best_rg]]
      why_text  <- score_reason(best)
      
      score_detail <- paste0(
        "Score: ", round(best$total_score), "/100 — ",
        "Budget Fit (", best$headroom_score, "/30) · ",
        "Stability (", best$momentum_score, "/25) · ",
        "Competitiveness (", best$compete_score, "/25) · ",
        "Inventory (", best$inventory_score, "/20)"
      )
      
      runners <- affordable %>% slice(-1)
      
      div(
        style = "border-radius:10px; overflow:hidden; border:2px solid #1565C0; margin-bottom:10px;",
        div(style="background-color:#1565C0; color:white; padding:14px 20px;",
            tags$strong(style="font-size:18px;", paste0(
              "\u2B50 Best Match: ", best_rg)),
            tags$span(style="font-size:13px; opacity:0.85; margin-left:12px;",
                      paste0("Median: $", formatC(round(best$price/1000,0), format="d", big.mark=","), "K — within your budget"))
        ),
        div(style="display:flex; background:white;",
            tags$img(src=best_img,
                     style="width:280px; height:180px; object-fit:cover; flex-shrink:0;",
                     onerror="this.style.display='none'"),
            div(style="padding:16px; flex:1;",
                tags$p(style="margin:0 0 8px; color:#333; font-size:14px;", best_desc),
                tags$p(style="margin:0 0 6px; color:#1565C0; font-size:13px; font-weight:bold;",
                       paste0("Why this region: ", why_text)),
                tags$p(style="margin:0 0 6px; color:#555; font-size:12px; font-style:italic;",
                       score_detail),
                tags$p(style="margin:0; color:#555; font-size:13px;",
                       paste0("At the current median price, you'd be looking at a ", best_tier, " in this area. ",
                              "Your budget of $", formatC(round(max_price/1000,0), format="d", big.mark=","),
                              "K gives you ", ifelse(max_price >= best$price,
                                                     paste0("$", formatC(round((max_price - best$price)/1000,0), format="d", big.mark=","), "K of headroom."),
                                                     "limited options at the median price.")))
            )
        ),
        # Runner-up regions
        if (nrow(runners) > 0) {
          div(style="background:#f8f9fa; padding:12px 20px; border-top:1px solid #ddd;",
              tags$strong(style="font-size:13px; color:#333;", "Also affordable:"),
              lapply(seq_len(nrow(runners)), function(i) {
                r <- runners[i,]
                tags$div(style="font-size:12px; color:#555; margin-top:4px; margin-left:12px;",
                         paste0(r$Region.Group,
                                " — Score: ", round(r$total_score), "/100",
                                " · Median: $", formatC(round(r$price/1000,0), format="d", big.mark=","), "K",
                                " · ", score_reason(r)))
              })
          )
        }
      )
    }
  })
  
  # --- Score Breakdown Bar Chart ---
  output$aff_score_plot <- renderPlot({
    scored <- best_region_scored()
    if (is.null(scored) || nrow(scored) == 0) {
      return(ggplot() + annotate("text", x=0.5, y=0.5, label="Adjust inputs to see scores", size=6, color="gray50") + theme_void())
    }
    
    plot_data <- scored %>%
      select(Region.Group, is_affordable, headroom_score, momentum_score, compete_score, inventory_score) %>%
      pivot_longer(cols = c(headroom_score, momentum_score, compete_score, inventory_score),
                   names_to = "category", values_to = "score") %>%
      mutate(
        category = case_when(
          category == "headroom_score"  ~ "Budget Fit (30)",
          category == "momentum_score"  ~ "Stability (25)",
          category == "compete_score"   ~ "Competitiveness (25)",
          category == "inventory_score" ~ "Inventory (20)"
        ),
        category = factor(category, levels = c("Inventory (20)", "Competitiveness (25)",
                                               "Stability (25)", "Budget Fit (30)")),
        label = ifelse(is_affordable, Region.Group, paste0(Region.Group, " *"))
      )
    
    score_colors <- c("Budget Fit (30)" = "#1565C0", "Stability (25)" = "#2E6699",
                      "Competitiveness (25)" = "#4F86BA", "Inventory (20)" = "#A8C8E8")
    
    totals <- scored %>%
      mutate(label = ifelse(is_affordable, Region.Group, paste0(Region.Group, " *"))) %>%
      select(label, total_score)
    
    ggplot(plot_data, aes(x = reorder(label, -score, FUN = sum), y = score, fill = category)) +
      geom_col(width = 0.6) +
      geom_text(data = totals, aes(x = label, y = total_score, label = paste0(round(total_score), "/100"), fill = NULL),
                vjust = -0.5, size = 4, fontface = "bold", color = "#333") +
      scale_fill_manual(values = score_colors) +
      scale_y_continuous(limits = c(0, 110), expand = expansion(mult = c(0, 0.05))) +
      labs(title = "Region Scores — Why We Recommend What We Do",
           subtitle = "Higher score = better fit for you as a buyer (* = not affordable)",
           x = NULL, y = "Score", fill = "Category") +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"),
            legend.position = "bottom",
            panel.grid.major.x = element_blank())
  })
  
  # --- Per-Region Affordability Cards ---
  make_aff_card <- function(region_name, display_name) {
    renderUI({
      max_price    <- aff_calc()$max_price
      prices       <- region_prices()
      scored       <- best_region_scored()
      region_price <- prices %>% filter(Region.Group == region_name) %>% pull(price)
      if (length(region_price) == 0 || is.na(region_price)) return(div("No data available"))
      
      affordable  <- max_price >= region_price
      diff        <- abs(max_price - region_price)
      diff_fmt    <- paste0("$", formatC(round(diff/1000,0), format="d", big.mark=","), "K")
      price_fmt   <- paste0("$", formatC(round(region_price/1000,0), format="d", big.mark=","), "K")
      max_fmt     <- paste0("$", formatC(round(max_price/1000,0), format="d", big.mark=","), "K")
      pct_diff    <- round(abs(max_price - region_price) / region_price * 100, 1)
      bg_color    <- if (affordable) "#1B5E20" else "#B71C1C"
      status_icon <- if (affordable) "\u2713" else "\u2717"
      status_text <- if (affordable)
        paste0("Within budget by ", diff_fmt, " (", pct_diff, "% below median)")
      else
        paste0("Over budget by ", diff_fmt, " (", pct_diff, "% above median)")
      
      # Pull score for this region
      region_score <- scored %>% filter(Region.Group == region_name)
      score_text <- if (nrow(region_score) > 0) paste0("Score: ", round(region_score$total_score[1]), "/100") else ""
      
      img_url  <- home_image(region_price, region_name)
      tier_lbl <- home_tier_label(region_price)
      
      div(
        style = paste0("border-radius:10px; overflow:hidden; border:2px solid ", bg_color, ";"),
        tags$img(src=img_url,
                 style="width:100%; height:160px; object-fit:cover; display:block;",
                 onerror="this.style.display='none'"),
        div(style=paste0("background-color:", bg_color, "; color:white; padding:14px; text-align:center;"),
            tags$strong(style="font-size:15px;", display_name),
            tags$div(style="font-size:22px; font-weight:bold; margin:6px 0;",
                     paste0(status_icon, " ", if(affordable) "Affordable" else "Not Affordable")),
            tags$div(style="font-size:12px; opacity:0.9; margin:3px 0;", paste0("Median: ", price_fmt)),
            tags$div(style="font-size:12px; opacity:0.9; margin:3px 0;", paste0("Your Max: ", max_fmt)),
            tags$div(style="font-size:11px; opacity:0.8; margin-top:6px;", status_text),
            if (nchar(score_text) > 0) tags$div(style="font-size:11px; opacity:0.85; margin-top:4px; font-weight:bold;", score_text),
            tags$div(style="font-size:11px; opacity:0.75; margin-top:4px; font-style:italic;",
                     paste0("Typical home at median: ", tier_lbl))
        )
      )
    })
  }
  
  output$aff_card_nassau      <- make_aff_card("Nassau County", "Nassau County")
  output$aff_card_nyc         <- make_aff_card("New York City", "New York City")
  output$aff_card_westchester <- make_aff_card("Westchester",   "Westchester County")
  
  # --- Historical Affordability Plot ---
  output$aff_history_plot <- renderPlot({
    req(aff_calc()$max_price > 0)
    calc      <- aff_calc()
    max_price <- calc$max_price
    df <- clean_df() %>%
      filter(!is.na(Median.Sale.Price)) %>%
      group_by(date, Region.Group) %>%
      summarise(Median.Sale.Price = mean(Median.Sale.Price, na.rm=TRUE), .groups="drop") %>%
      filter(Region.Group != "Other")
    aff_colors <- c("Nassau County"="#0A1929","New York City"="#1565C0","Westchester"="#A8C8E8")
    date_min <- min(df$date)
    date_max <- max(df$date)
    y_max    <- max(max(df$Median.Sale.Price, na.rm=TRUE), max_price) * 1.1
    ggplot(df, aes(x=date, y=Median.Sale.Price, color=Region.Group)) +
      annotate("rect", xmin=date_min, xmax=date_max, ymin=0, ymax=max_price,
               fill="#1B5E20", alpha=0.10) +
      geom_hline(yintercept=max_price, linetype="dashed", color="#1B5E20", linewidth=1) +
      annotate("text", x=date_min+30, y=max_price*1.04,
               label=paste0("Your Max: $", formatC(round(max_price/1000,0), format="d", big.mark=","), "K"),
               color="#1B5E20", size=3.5, hjust=0) +
      geom_line(linewidth=1) +
      scale_color_manual(values=aff_colors) +
      scale_y_continuous(labels=scales::dollar_format(scale=0.001, suffix="K"), limits=c(0, y_max)) +
      labs(title="Your Affordability Threshold vs. Regional Median Prices",
           subtitle="Green shaded area = homes within your budget",
           x="Date", y="Price", color="Region") +
      theme_minimal(base_size=13) +
      theme(plot.title=element_text(face="bold"), legend.position="bottom")
  })
  
  # --- Sensitivity Plot ---
  output$aff_sensitivity_plot <- renderPlot({
    req(aff_calc()$max_price > 0)
    rate    <- input$aff_rate / 100 / 12
    term    <- as.numeric(input$aff_term) * 12
    tax_ins <- input$aff_tax_insurance
    income  <- input$aff_income
    max_monthly  <- (income / 12) * 0.28
    max_mortgage <- max(max_monthly - tax_ins, 0)
    max_loan     <- if (rate == 0) max_mortgage * term else
      max_mortgage * (1 - (1 + rate)^(-term)) / rate
    down_seq   <- seq(0, 500000, by=10000)
    max_prices <- down_seq + max_loan
    sens_df    <- data.frame(down=down_seq, max_price=max_prices)
    prices     <- region_prices()
    aff_colors <- c("Nassau County"="#0A1929","New York City"="#1565C0","Westchester"="#A8C8E8")
    p <- ggplot(sens_df, aes(x=down, y=max_price)) +
      geom_line(color="#1565C0", linewidth=1.5) +
      geom_vline(xintercept=input$aff_downpayment, linetype="dashed", color="gray40") +
      annotate("text", x=input$aff_downpayment, y=max(sens_df$max_price)*0.98,
               label=paste0("Current: $", formatC(round(input$aff_downpayment/1000,0), format="d", big.mark=","), "K down"),
               hjust=-0.1, size=3.5, color="gray40")
    for (i in seq_len(nrow(prices))) {
      rg <- prices$Region.Group[i]; price <- prices$price[i]; col <- aff_colors[rg]
      p <- p +
        geom_hline(yintercept=price, linetype="dotted", color=col, linewidth=0.8) +
        annotate("text", x=max(down_seq)*0.98, y=price*1.02, label=rg, hjust=1, size=3, color=col)
    }
    p + scale_x_continuous(labels=scales::dollar_format(scale=0.001, suffix="K")) +
      scale_y_continuous(labels=scales::dollar_format(scale=0.001, suffix="K")) +
      labs(title="How Your Down Payment Affects Maximum Home Price",
           subtitle="Dotted lines show current median price per region",
           x="Down Payment", y="Max Affordable Home Price") +
      theme_minimal(base_size=13) + theme(plot.title=element_text(face="bold"))
  })
  
  # ============================================================
  # PREDICTION MODEL
  # ============================================================
  
  output$prediction_plot <- renderPlot({
    req(input$pred_region, input$pred_year)
    base <- clean_df() %>% filter(!is.na(Median.Sale.Price))
    model_df <- if (input$pred_region=="Nassau County") base %>% filter(Region.Group=="Nassau County")
    else if (input$pred_region=="New York City") base %>% filter(Region.Group=="New York City")
    else base %>% filter(Region.Group=="Westchester")
    model <- lm(Median.Sale.Price ~ time_index + month_num, data=model_df)
    plot_df <- model_df %>%
      group_by(date, time_index, month_num) %>%
      summarise(Median.Sale.Price=mean(Median.Sale.Price,na.rm=TRUE), .groups="drop")
    last_date    <- max(plot_df$date, na.rm=TRUE)
    forecast_end <- as.Date(paste0(input$pred_year,"-12-01"))
    if (forecast_end <= last_date) {
      forecast_df <- data.frame(date=as.Date(character(0))) %>% mutate(time_index=integer(0), month_num=integer(0))
    } else {
      forecast_df <- data.frame(date=seq.Date(last_date %m+% months(1), forecast_end, by="month")) %>%
        mutate(time_index=year(date)*12+month(date), month_num=month(date))
    }
    plot_df$fitted <- predict(model, newdata=plot_df)
    if (nrow(forecast_df)>0) forecast_df$Median.Sale.Price <- predict(model, newdata=forecast_df)
    p <- ggplot() +
      geom_line(data=plot_df, aes(x=date,y=Median.Sale.Price,color="Actual"), linewidth=1) +
      geom_line(data=plot_df, aes(x=date,y=fitted,color="Model Fit"), linewidth=0.8, linetype="dashed")
    if (nrow(forecast_df)>0)
      p <- p + geom_line(data=forecast_df, aes(x=date,y=Median.Sale.Price,color="Forecast"), linewidth=1, linetype="dotted")
    p + scale_color_manual(name="", values=c("Actual"="#2C3E50","Model Fit"="#E63946","Forecast"="#457B9D")) +
      scale_y_continuous(labels=scales::dollar_format(scale=0.001,suffix="K")) +
      labs(title=paste(input$pred_region,"Median Sale Price"),
           subtitle=paste("Actual data with model fit and forecast through",input$pred_year),
           x="Date", y="Median Sale Price") +
      theme_minimal(base_size=14) + theme(plot.title=element_text(face="bold"), legend.position="bottom")
  })
}
