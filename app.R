library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(lubridate)
library(sf)
library(leaflet)
library(scales)
library(DT)

source("R_codes/chart9_scatter.R")
source("R_codes/chart18_bubble.R")
source("R_codes/chart5_circular_area.R")
source("R_codes/mapping_data.R")

# Transform sf object to WGS84 (EPSG:4326) for Leaflet compatibility
dfw_map_leaflet <- sf::st_transform(dfw_map, 4326)

# Combine county-level snapshot datasets (unchanged logic)
dashboard_data <- dfw_county_adoption %>%
  left_join(
    dfw_ev_stations_counts,
    by = "County"
  ) %>%
  left_join(
    dfw_ev_ports_counts,
    by = "County"
  ) %>%
  left_join(
    dfw_counties_income,
    by = "County"
  )

# Summary table (unchanged logic)
dashboard_table <- dashboard_data %>%
  transmute(
    County,
    `EV Registrations` = `Electric Vehicles`,
    `Total Vehicles` = `All Vehicles`,
    `Adoption Rate` = `Adoption Rate`,
    `Charging Stations` = charging_stations,
    `Charging Ports` = charging_ports
  ) %>%
  arrange(County)

# Historical charging stations opened over time from AFDC CSV
stations_history_raw <- readr::read_csv(
  "data/electric_fuel_stations_historical_day_20260101.csv",
  show_col_types = FALSE
) %>%
  mutate(
    open_date = suppressWarnings(as.Date(`Open Date`)),
    open_year = year(open_date)
  ) %>%
  filter(!is.na(open_date), open_year >= 2005, open_year <= 2026)

stations_yearly <- stations_history_raw %>%
  count(open_year, name = "new_stations") %>%
  arrange(open_year) %>%
  mutate(cumulative_stations = cumsum(new_stations))

# Map variable choices
map_metric_choices <- c(
  "EV Adoption Rate" = "adoption_rate",
  "EV Registrations" = "electric_vehicles",
  "Charging Stations" = "charging_stations",
  "Charging Ports" = "charging_ports",
  "Median Household Income" = "median_household_income"
)

format_metric_values <- function(x, metric) {
  switch(
    metric,
    adoption_rate = paste0(format(round(x * 100, 1), nsmall = 1), "%"),
    median_household_income = paste0("$", format(round(x, 0), big.mark = ",")),
    format(round(x, 0), big.mark = ",")
  )
}

get_metric_label <- function(metric) {
  names(map_metric_choices)[match(metric, map_metric_choices)]
}

# bslib theme using your Quattrocento typography and purple palette
ev_bs_theme <- bs_theme(
  version = 5,
  base_font = font_google("Quattrocento"),
  heading_font = font_google("Quattrocento"),
  bg = "#ffffff",
  fg = "#262626",
  primary = "#551A8B",
  secondary = "#9A32CD"
)

ui <- page_navbar(
  title = "DFW Electric Vehicle Dashboard",
  theme = ev_bs_theme,
  fillable = FALSE,
  header = tags$head(
    tags$style(HTML("
      .chart-title {
        text-align: center;
        font-size: 26px;
        font-weight: bold;
        margin-top: 12px;
      }
      .chart-subtitle {
        text-align: center;
        font-size: 15px;
        color: dimgray;
        margin-bottom: 12px;
      }
      .chart-caption {
        text-align: left;
        font-size: 13px;
        color: dimgray;
        margin-top: 8px;
      }
    "))
  ),
  sidebar = sidebar(
    width = 300,
    title = "Filter DFW Counties",
    selectizeInput(
      inputId = "county_filter",
      label = "Select Counties:",
      choices = sort(unique(dashboard_data$County)),
      selected = character(0),
      multiple = TRUE,
      options = list(plugins = list("remove_button")),
      width = "100%"
    ),
    actionLink(
      inputId = "reset_counties",
      label = "Reset to All Counties"
    ),
    hr(),
    sliderInput(
      inputId = "income_filter",
      label = "Median Household Income (USD):",
      min = 60000,
      max = 130000,
      value = c(60000, 130000),
      step = 5000,
      pre = "$",
      sep = ",",
      ticks = FALSE,
      width = "100%"
    ),
    hr(),
    radioButtons(
      inputId = "timeseries_metric",
      label = "Charging Stations Over Time:",
      choices = c(
        "Cumulative Stations" = "cumulative_stations",
        "New Stations Opened / Year" = "new_stations"
      ),
      selected = "cumulative_stations"
    )
  ),
  
  nav_panel(
    title = "Overview & Charts",
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4),
      value_box(
        title = "Total EV Registrations",
        value = textOutput("vb_total_evs"),
        showcase = bs_icon("ev-front-fill"),
        theme = "primary"
      ),
      value_box(
        title = "Average Adoption Rate",
        value = textOutput("vb_avg_adoption"),
        showcase = bs_icon("speedometer2"),
        theme = "purple"
      ),
      value_box(
        title = "Total Charging Stations",
        value = textOutput("vb_total_stations"),
        showcase = bs_icon("lightning-charge-fill"),
        theme = "secondary"
      )
    ),
    br(),
    navset_card_tab(
      full_screen = TRUE,
      nav_panel(
        title = "Chart 9",
        h3(
          "EV Adoption Rates vs. EV Charging Availability",
          class = "chart-title"
        ),
        p(
          "Across the selected DFW counties, as of January 1, 2026",
          class = "chart-subtitle"
        ),
        plotlyOutput("chart9_output", height = "500px"),
        p(
          "Sources: DFW Clean Cities EV Registration Report; Alternative Fuels Data Center (AFDC), January 1, 2026.",
          class = "chart-caption"
        )
      ),
      nav_panel(
        title = "Chart 18",
        h3(
          "EV Adoption Rates vs. Median Household Income",
          class = "chart-title"
        ),
        p(
          "Bubble size represents the number of public EV charging stations across the selected DFW counties.",
          class = "chart-subtitle"
        ),
        plotlyOutput("chart18_output", height = "500px"),
        p(
          "Sources: U.S. Census Bureau (2020–2024 ACS); DFW Clean Cities EV Registration Report and AFDC (January 1, 2026).",
          class = "chart-caption"
        )
      ),
      nav_panel(
        title = "Chart 5",
        div(
          style = "max-width: 900px; margin: 0 auto;",
          plotOutput(
            "chart5_output",
            width = "100%",
            height = "670px"
          )
        )
      )
    ),
    br(),
    card(
      card_header("Statistics on EV Registrations & Public Charging Infrastructure Across DFW Counties"),
      DTOutput("county_table")
    )
  ),
  
  nav_panel(
    title = "Map",
    layout_columns(
      fill = FALSE,
      col_widths = c(6, 6),
      selectInput(
        inputId = "map_metric",
        label = "Map Measure:",
        choices = map_metric_choices,
        selected = "adoption_rate",
        width = "100%"
      ),
      selectizeInput(
        inputId = "map_counties",
        label = "Filter Map Counties (leave blank for all):",
        choices = sort(unique(dfw_map_leaflet$county)),
        selected = character(0),
        multiple = TRUE,
        options = list(plugins = list("remove_button")),
        width = "100%"
      )
    ),
    layout_columns(
      fill = FALSE,
      col_widths = c(6, 6),
      value_box(
        title = "Counties Shown",
        value = textOutput("vb_map_counties"),
        showcase = bs_icon("geo-alt-fill"),
        theme = "primary"
      ),
      value_box(
        title = "Median Selected Measure",
        value = textOutput("vb_map_median"),
        showcase = bs_icon("bar-chart-line-fill"),
        theme = "secondary"
      )
    ),
    br(),
    layout_columns(
      fill = FALSE,
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Interactive DFW County Choropleth Map"),
        leafletOutput("dfw_leaflet_map", height = "500px"),
        p(
          "Hover or click any county for details. Boundaries: U.S. Census Bureau (tigris); EV & AFDC data as of January 1, 2026.",
          class = "chart-caption"
        )
      ),
      card(
        full_screen = TRUE,
        card_header("Top 10 DFW Counties"),
        plotlyOutput("map_top10_chart", height = "500px"),
        p(
          "Ranked among counties currently shown in the map view.",
          class = "chart-caption"
        )
      )
    )
  ),
  
  nav_panel(
    title = "Charging Over Time",
    card(
      full_screen = TRUE,
      card_header("Historical EV Charging Station Openings Over Time"),
      p(
        "Tracking station opening dates from the AFDC historical dataset (2005–2026).",
        class = "chart-subtitle"
      ),
      plotlyOutput("timeseries_output", height = "500px"),
      p(
        "Source: Alternative Fuels Data Center (AFDC) Historical Station Dataset (`Open Date`).",
        class = "chart-caption"
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$reset_counties, {
    updateSelectizeInput(
      session,
      "county_filter",
      selected = character(0)
    )
    updateSliderInput(
      session,
      "income_filter",
      value = c(60000, 130000)
    )
  })
  
  filtered_data <- reactive({
    req(input$income_filter)
    data <- dashboard_data
    if (length(input$county_filter) > 0) {
      data <- data %>%
        filter(County %in% input$county_filter)
    }
    data <- data %>%
      filter(
        median_household_income >= input$income_filter[1],
        median_household_income <= input$income_filter[2]
      )
    data
  })
  
  # Shared reactive spatial dataset for Map tab
  filtered_map_sf <- reactive({
    req(input$map_metric, input$income_filter)
    sf_obj <- dfw_map_leaflet %>%
      filter(
        median_household_income >= input$income_filter[1],
        median_household_income <= input$income_filter[2]
      )
    
    if (length(input$map_counties) > 0) {
      sf_obj <- sf_obj %>% filter(county %in% input$map_counties)
    } else if (length(input$county_filter) > 0) {
      sf_obj <- sf_obj %>% filter(county %in% input$county_filter)
    }
    
    sf_obj
  })
  
  # Dynamic Value Boxes (Overview tab)
  output$vb_total_evs <- renderText({
    format(sum(filtered_data()$`Electric Vehicles`, na.rm = TRUE), big.mark = ",")
  })
  
  output$vb_avg_adoption <- renderText({
    avg_rate <- mean(filtered_data()$`Adoption Rate`, na.rm = TRUE)
    if (is.nan(avg_rate)) return("0.0%")
    paste0(format(round(avg_rate * 100, 1), nsmall = 1), "%")
  })
  
  output$vb_total_stations <- renderText({
    format(sum(filtered_data()$charging_stations, na.rm = TRUE), big.mark = ",")
  })
  
  # Dynamic Value Boxes (Map tab)
  output$vb_map_counties <- renderText({
    nrow(filtered_map_sf())
  })
  
  output$vb_map_median <- renderText({
    sf_obj <- filtered_map_sf()
    if (nrow(sf_obj) == 0) return("N/A")
    vals <- sf::st_drop_geometry(sf_obj)[[input$map_metric]]
    med_val <- stats::median(vals, na.rm = TRUE)
    format_metric_values(med_val, input$map_metric)
  })
  
  # Interactive Leaflet Map
  output$dfw_leaflet_map <- renderLeaflet({
    sf_obj <- filtered_map_sf()
    metric <- input$map_metric
    metric_label <- get_metric_label(metric)
    vals <- sf::st_drop_geometry(sf_obj)[[metric]]
    
    pal <- colorNumeric(
      palette = "Purples",
      domain = vals,
      na.color = "#eeeeee"
    )
    
    label_format_fn <- switch(
      metric,
      adoption_rate = label_percent(accuracy = 0.1),
      median_household_income = label_dollar(accuracy = 1),
      label_comma(accuracy = 1)
    )
    
    popups <- sprintf(
      "<strong>%s County</strong><br/>%s: %s<br/>EV Registrations: %s<br/>Adoption Rate: %s<br/>Charging Stations: %s<br/>Median Income: %s",
      sf_obj$county,
      metric_label,
      format_metric_values(vals, metric),
      format(sf_obj$electric_vehicles, big.mark = ","),
      paste0(format(round(sf_obj$adoption_rate * 100, 1), nsmall = 1), "%"),
      format(sf_obj$charging_stations, big.mark = ","),
      paste0("$", format(round(sf_obj$median_household_income, 0), big.mark = ","))
    )
    
    leaflet(sf_obj) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(vals),
        weight = 1.5,
        opacity = 1,
        color = "#4A235A",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#262626",
          fillOpacity = 0.92,
          bringToFront = TRUE
        ),
        popup = popups,
        label = ~paste0(county, ": ", format_metric_values(vals, metric))
      ) %>%
      addLegend(
        pal = pal,
        values = ~vals,
        opacity = 0.85,
        title = metric_label,
        position = "bottomright",
        labFormat = labelFormat(
          transform = function(x) {
            if (identical(metric, "adoption_rate")) round(x * 100, 1) else x
          },
          suffix = if (identical(metric, "adoption_rate")) "%" else ""
        )
      )
  })
  
  # Companion Top-10 Bar Chart on Map Tab
  output$map_top10_chart <- renderPlotly({
    sf_obj <- filtered_map_sf()
    metric <- input$map_metric
    metric_label <- get_metric_label(metric)
    
    top_df <- sf::st_drop_geometry(sf_obj) %>%
      mutate(metric_val = .data[[metric]]) %>%
      arrange(desc(metric_val)) %>%
      slice_head(n = 10) %>%
      mutate(
        county_ord = factor(county, levels = rev(county)),
        hover_text = paste0(
          county, " County<br>",
          metric_label, ": ", format_metric_values(metric_val, metric)
        )
      )
    
    p_top <- ggplot(
      top_df,
      aes(x = metric_val, y = county_ord, text = hover_text)
    ) +
      geom_col(fill = "#551A8B") +
      labs(
        x = metric_label,
        y = NULL
      ) +
      theme_minimal(base_family = "Quattrocento", base_size = 14)
    
    ggplotly(p_top, tooltip = "text") %>%
      plotly::layout(
        font = list(family = "Quattrocento", size = 12),
        xaxis = list(title = list(font = list(size = 13))),
        yaxis = list(title = list(font = list(size = 13)))
      )
  })
  
  # Chart 9
  output$chart9_output <- renderPlotly({
    chart9_filtered <- chart9 %+% filtered_data() +
      labs(
        title = NULL,
        subtitle = NULL,
        caption = NULL
      )
    ggplotly(
      chart9_filtered,
      tooltip = "text"
    ) %>%
      plotly::layout(
        font = list(
          family = "Quattrocento",
          size = 13
        ),
        xaxis = list(
          title = list(font = list(size = 15)),
          tickfont = list(size = 12)
        ),
        yaxis = list(
          title = list(font = list(size = 15)),
          tickfont = list(size = 12)
        )
      )
  })
  
  # Chart 18
  output$chart18_output <- renderPlotly({
    chart18_filtered <- chart18 %+% filtered_data() +
      labs(
        title = NULL,
        subtitle = NULL,
        caption = NULL
      )
    ggplotly(
      chart18_filtered,
      tooltip = "text"
    ) %>%
      plotly::layout(
        font = list(
          family = "Quattrocento",
          size = 13
        ),
        xaxis = list(
          title = list(font = list(size = 15)),
          tickfont = list(size = 12)
        ),
        yaxis = list(
          title = list(font = list(size = 15)),
          tickfont = list(size = 12)
        )
      )
  })
  
  # Chart 5
  output$chart5_output <- renderPlot({
    circular_area_chart
  }, res = 96)
  
  # Time-Series Chart (Open Date from AFDC CSV)
  output$timeseries_output <- renderPlotly({
    metric_col <- input$timeseries_metric
    y_label <- if (identical(metric_col, "cumulative_stations")) {
      "Cumulative Charging Stations"
    } else {
      "New Stations Opened"
    }
    
    p_ts <- ggplot(
      stations_yearly,
      aes(
        x = open_year,
        y = .data[[metric_col]],
        text = paste0(
          "Year: ", open_year,
          "<br>", y_label, ": ", format(.data[[metric_col]], big.mark = ",")
        )
      )
    ) +
      geom_line(color = "#551A8B", linewidth = 1.2) +
      geom_point(color = "#9A32CD", size = 2.5) +
      labs(
        x = "Year Opened",
        y = y_label
      ) +
      theme_minimal(base_family = "Quattrocento", base_size = 15)
    
    ggplotly(p_ts, tooltip = "text") %>%
      plotly::layout(
        font = list(family = "Quattrocento", size = 13),
        xaxis = list(title = list(font = list(size = 15)), tickfont = list(size = 12)),
        yaxis = list(title = list(font = list(size = 15)), tickfont = list(size = 12))
      )
  })
  
  # Table
  output$county_table <- renderDT({
    datatable(
      dashboard_table,
      rownames = FALSE,
      options = list(
        pageLength = 16,
        lengthChange = FALSE,
        ordering = TRUE,
        searching = TRUE,
        scrollX = TRUE,
        dom = "ft"
      )
    ) %>%
      formatRound(
        columns = c(
          "EV Registrations",
          "Total Vehicles",
          "Charging Stations",
          "Charging Ports"
        ),
        digits = 0
      ) %>%
      formatPercentage(
        columns = "Adoption Rate",
        digits = 1
      )
  }, server = FALSE)
}

shinyApp(
  ui = ui,
  server = server
)