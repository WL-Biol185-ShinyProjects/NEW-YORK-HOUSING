library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(tigris)
library(shinyjs)

# --- UI ---
ui <- page_navbar(
  title     = "New York Housing Analysis",
  id        = "page",
  underline = FALSE,
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=DM+Serif+Display&display=swap",
        rel  = "stylesheet"
      ),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('hide_profile_modal', function(msg) {
          document.getElementById('profile-modal').style.display = 'none';
        });
        Shiny.addCustomMessageHandler('show_profile_modal', function(msg) {
          document.getElementById('profile-modal').style.display = 'flex';
        });
        Shiny.addCustomMessageHandler('toggle_modal_error', function(msg) {
          var el = document.getElementById('profile_error');
          el.style.display = msg.show ? 'block' : 'none';
        });
      ")),
      tags$style(HTML("
    body, .navbar, .sidebar, p, h1, h2, h3, h4, h5, h6,
    .value-box, table, .shiny-text-output {
      font-family: 'DM Serif Display', serif !important;
    }

    /* Navbar */
    .navbar {
      background-color: #0A1929 !important;
      border-bottom: 1px solid #1565C0 !important;
    }

    /* Navbar text and links */
    .navbar-nav .nav-link, .navbar-brand {
      color: white !important;
    }

    .navbar-nav .nav-link:hover, .navbar-nav .nav-link.active {
      color: #A8C8E8 !important;
      background-color: #1565C0 !important;
      border-radius: 6px;
    }

    /* Sidebar */
    .sidebar, .bslib-sidebar-panel {
      background-color: #0A1929 !important;
      color: white !important;
    }

    /* Sidebar labels and text */
    .sidebar label, .sidebar .help-block, .sidebar p,
    .sidebar .control-label, .sidebar h4, .sidebar h5 {
      color: white !important;
    }

    /* Modal overlay — hidden by default, JS sets display:flex to show */
    #profile-modal {
      display: none;
      position: fixed;
      top: 0; left: 0;
      width: 100%; height: 100%;
      background: rgba(10,25,41,0.88);
      z-index: 9999;
      align-items: center;
      justify-content: center;
    }

    #profile-modal-box {
      background: white;
      border-radius: 14px;
      padding: 36px 40px;
      max-width: 540px;
      width: 92%;
      max-height: 90vh;
      overflow-y: auto;
      box-shadow: 0 8px 40px rgba(0,0,0,0.5);
    }

    #profile-modal-box .radio label {
      color: #333 !important;
      font-size: 14px;
    }

    #profile-modal-box label {
      color: #0A1929 !important;
      font-size: 15px;
      font-weight: bold;
      margin-top: 14px;
      display: block;
    }

    #profile_submit {
      background-color: #1565C0 !important;
      color: white !important;
      border: none !important;
      padding: 12px 36px !important;
      font-size: 16px !important;
      border-radius: 8px !important;
      font-family: 'DM Serif Display', serif !important;
      cursor: pointer !important;
      width: 100% !important;
      margin-top: 10px !important;
    }

    #profile_submit:hover {
      background-color: #0d47a1 !important;
    }

    .profile-summary-box {
      background: #0d2137;
      border-radius: 8px;
      padding: 10px 14px;
      margin-bottom: 6px;
      font-size: 12px;
      color: #A8C8E8 !important;
    }

    .profile-summary-box span {
      color: white !important;
      font-weight: bold;
    }
      "))
    )
  ),
  
  # ============================================================
  # HOME TAB
  # ============================================================
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
  
  # ============================================================
  # MARKET OVERVIEW TAB
  # ============================================================
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
                value_box(title = "Current Value",      value = textOutput("card_price"), style = "background-color: #A8C8E8; color: white;"),
                value_box(title = "Month-over-Month",   value = textOutput("card_mom"),   style = "background-color: #7BA7D1; color: white;"),
                value_box(title = "Year-over-Year",     value = textOutput("card_yoy"),   style = "background-color: #4F86BA; color: white;"),
                value_box(title = "All-Time High",      value = textOutput("card_high"),  style = "background-color: #2E6699; color: white;"),
                value_box(title = "All-Time Low",       value = textOutput("card_low"),   style = "background-color: #1A4670; color: white;")
              ),
              plotOutput("market_plot", height = "50vh")
            )
  ),
  
  # ============================================================
  # REGION OVERVIEW TAB
  # ============================================================
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
                col_widths = c(4, 4, 4), fill = FALSE,
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
  
  # ============================================================
  # MAP TAB
  # ============================================================
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
  
  # ============================================================
  # AFFORDABILITY CALCULATOR TAB
  # ============================================================
  nav_panel("Affordability Calculator",
            
            # --- Profile Modal ---
            div(id = "profile-modal",
                div(id = "profile-modal-box",
                    tags$h3("Tell Us About Yourself",
                            style = "margin-top:0; color:#0A1929; font-size:22px; margin-bottom:4px;"),
                    tags$p("Answer a few quick questions so we can personalize your region recommendation.",
                           style = "color:#666; font-size:13px; margin-bottom:20px; border-bottom:1px solid #eee; padding-bottom:16px;"),
                    
                    tags$label("Which best describes your life stage?"),
                    radioButtons("profile_lifestage", label = NULL,
                                 choices = c(
                                   "Young Professional — single or couple, career-focused, no kids" = "young_pro",
                                   "Growing Family — kids or planning to have them soon"             = "family",
                                   "Established / Empty Nester — 50s+, kids grown or leaving"       = "established",
                                   "Retiree — 60s+, looking for comfort, quiet, and convenience"    = "retiree"
                                 ),
                                 selected = character(0)
                    ),
                    
                    tags$label("What kind of environment do you prefer?"),
                    radioButtons("profile_urban", label = NULL,
                                 choices = c(
                                   "Strongly urban — walkable, dense, city energy"   = "urban",
                                   "Mostly suburban — quieter streets, more space"   = "suburban",
                                   "Flexible — open to either"                       = "flexible"
                                 ),
                                 selected = character(0)
                    ),
                    
                    tags$label("How important are school district ratings?"),
                    radioButtons("profile_schools", label = NULL,
                                 choices = c(
                                   "Very important — a top priority for my household" = "high",
                                   "Somewhat important — nice to have"                = "medium",
                                   "Not important right now"                          = "low"
                                 ),
                                 selected = character(0)
                    ),
                    
                    tags$label("How much do nightlife, dining, and entertainment matter?"),
                    radioButtons("profile_nightlife", label = NULL,
                                 choices = c(
                                   "Essential — I want it right at my doorstep" = "high",
                                   "Nice to have, but not a dealbreaker"        = "medium",
                                   "Not a priority for me"                      = "low"
                                 ),
                                 selected = character(0)
                    ),
                    
                    tags$label("How reliant are you on public transit?"),
                    radioButtons("profile_transit", label = NULL,
                                 choices = c(
                                   "Heavily — I don't own or want a car"  = "high",
                                   "Somewhat — I like having the option"  = "medium",
                                   "Rarely — I drive everywhere"          = "low"
                                 ),
                                 selected = character(0)
                    ),
                    
                    div(style = "margin-top:20px; border-top:1px solid #eee; padding-top:16px;",
                        tags$p(id = "profile_error",
                               style = "color:#B71C1C; font-size:13px; margin-bottom:8px; display:none;",
                               "\u26A0 Please answer all questions before continuing."
                        ),
                        actionButton("profile_submit", "See My Results \u2192")
                    )
                )
            ),
            
            # --- Main Calculator UI ---
            layout_sidebar(
              fill = FALSE,
              sidebar = sidebar(
                title = "Your Budget",
                width = 280,
                numericInput("aff_income", "Annual Household Income ($):",
                             value = 100000, min = 10000, max = 2000000, step = 5000),
                numericInput("aff_downpayment", "Down Payment ($):",
                             value = 50000, min = 0, max = 2000000, step = 5000),
                sliderInput("aff_rate", "Interest Rate (%):",
                            min = 2, max = 12, value = 7.0, step = 0.1),
                radioButtons("aff_term", "Loan Term:",
                             choices = c("30 Years" = 30, "15 Years" = 15), selected = 30),
                sliderInput("aff_tax_insurance", "Monthly Tax & Insurance ($):",
                            min = 0, max = 3000, value = 500, step = 50),
                hr(),
                uiOutput("profile_summary_sidebar"),
                uiOutput("profile_edit_btn_ui"),
                hr(),
                helpText("Based on the 28% rule: monthly mortgage should not exceed 28% of gross monthly income."),
                helpText("Comparison uses the most recent median sale price for each region.")
              ),
              
              h4("Your Affordability Summary"),
              layout_columns(
                col_widths = c(3, 3, 3, 3), fill = FALSE,
                value_box(title = "Max Home Price",     value = textOutput("aff_max_price"), style = "background-color: #1565C0; color: white;"),
                value_box(title = "Monthly Payment",    value = textOutput("aff_monthly"),   style = "background-color: #0A1929; color: white;"),
                value_box(title = "Loan Amount",        value = textOutput("aff_loan"),      style = "background-color: #2E6699; color: white;"),
                value_box(title = "Max Monthly Budget", value = textOutput("aff_budget"),    style = "background-color: #4F86BA; color: white;")
              ),
              hr(),
              h4("Best Region For You"),
              uiOutput("aff_best_region"),
              hr(),
              h4("Region Score Breakdown"),
              helpText("Composite score considers budget fit, market stability, competitiveness, inventory health, and your lifestyle profile."),
              plotOutput("aff_score_plot", height = "350px"),
              hr(),
              h4("Can You Afford Each Region?"),
              layout_columns(
                col_widths = c(4, 4, 4), fill = FALSE,
                uiOutput("aff_card_nassau"),
                uiOutput("aff_card_nyc"),
                uiOutput("aff_card_westchester")
              ),
              hr(),
              h4("Historical Affordability Over Time"),
              helpText("The green shaded area shows your maximum affordable price. Lines show each region's median sale price."),
              plotOutput("aff_history_plot", height = "500px"),
              hr(),
              h4("How Does Changing Your Down Payment Affect Buying Power?"),
              plotOutput("aff_sensitivity_plot", height = "400px")
            )
  ),
  
  # ============================================================
  # PREDICTION MODEL TAB
  # ============================================================
  nav_panel("Prediction Model",
            layout_sidebar(
              sidebar = sidebar(
                title = "Prediction Controls",
                selectInput("pred_region", "Select Region:",
                            choices = c("Nassau County", "New York City", "Westchester County")),
                numericInput("pred_year", "Forecast Through Year:",
                             value = 2026, min = 2016, max = 2035),
                hr(),
                helpText("The line graph shows actual prices and the model fitted and forecasted trend.")
              ),
              plotOutput("prediction_plot", height = "75vh")
            )
  )
)