library(shiny)
library(tidyverse)
library(TDA)
library(quantmod)

# 1. Define your embedding helper function (from your current script)
embed_time_series <- function(series, dimension = 3, delay = 1) {
  n <- length(series)
  max_idx <- n - (dimension - 1) * delay
  if (max_idx <= 0) return(NULL)
  
  embedded <- matrix(0, nrow = max_idx, ncol = dimension)
  for (i in 1:dimension) {
    idx <- (1:max_idx) + (i - 1) * delay
    embedded[, i] <- series[idx]
  }
  return(embedded)
}

# 2. UI Layout
ui <- fluidPage(
  titlePanel("Live PLTR.US Topological H1 Persistence Tracker"),
  sidebarLayout(
    sidebarPanel(
      helpText("Streaming live market data and calculating rolling TDA features."),
      verbatimTextOutput("status_text")
    ),
    mainPanel(
      plotOutput("live_tda_plot")
    )
  )
)

# 3. Server Logic & Streaming Loop
server <- function(input, output, session) {
  
  # 1. Pre-load historical daily data on startup so the plot isn't empty
  initial_data <- read_csv("../pltr_us_d.csv")
  init_prices <- initial_data$Close
  init_dates <- initial_data$Date
  
  # Compute initial rolling history from historical data
  init_dates_vec <- c()
  init_pers_vec <- c()
  
  window_size <- 15
  for (i in 1:(length(init_prices) - window_size + 1)) {
    end_idx <- i + window_size - 1
    sub_prices <- init_prices[i:end_idx]
    pc <- embed_time_series(sub_prices, dimension = 3, delay = 1)
    
    if (!is.null(pc) && nrow(pc) > 3) {
      rips_out <- ripsDiag(X = pc, maxdimension = 1, maxscale = max(dist(pc)), library = "GUDHI")
      diag_mat <- unclass(rips_out$diagram)
      pd_df <- data.frame(dimension = diag_mat[, 1], birth = diag_mat[, 2], death = diag_mat[, 3])
      h1_features <- pd_df |> filter(dimension == 1 & !is.infinite(death))
      max_val <- if (nrow(h1_features) > 0) max(h1_features$death - h1_features$birth) else 0
      
      init_dates_vec <- c(init_dates_vec, init_dates[end_idx])
      init_pers_vec <- c(init_pers_vec, max_val)
    }
  }
  
  # Reactive values initialized with historical data
  values <- reactiveValues(
    dates = as.Date(init_dates_vec),
    persistence = init_pers_vec
  )
  
  # Timer to check for live updates (e.g., every 60 seconds)
  timer <- reactiveTimer(60000) 
  
  observe({
    timer() 
    
    tryCatch({
      getSymbols("PLTR", src = "yahoo", auto.assign = TRUE, warnings = FALSE)
      latest_data <- tail(Cl(PLTR), 30)
      prices_live <- as.numeric(latest_data)
      dates_live <- index(tail(PLTR, 30))
      
      if (length(prices_live) >= window_size) {
        sub_prices <- tail(prices_live, window_size)
        sub_date <- tail(dates_live, 1)
        
        pc <- embed_time_series(sub_prices, dimension = 3, delay = 1)
        if (!is.null(pc) && nrow(pc) > 3) {
          rips_out <- ripsDiag(X = pc, maxdimension = 1, maxscale = max(dist(pc)), library = "GUDHI")
          diag_mat <- unclass(rips_out$diagram)
          pd_df <- data.frame(dimension = diag_mat[, 1], birth = diag_mat[, 2], death = diag_mat[, 3])
          h1_features <- pd_df |> filter(dimension == 1 & !is.infinite(death))
          max_val <- if (nrow(h1_features) > 0) max(h1_features$death - h1_features$birth) else 0
          
          # Only append if this date isn't already the latest one
          if (sub_date != tail(values$dates, 1)) {
            values$dates <- c(values$dates, sub_date)
            values$persistence <- c(values$persistence, max_val)
          }
        }
      }
    }, error = function(e) {
      message("Live poll notice: ", e$message)
    })
  })
  
  output$live_tda_plot <- renderPlot({
    req(length(values$dates) > 0)
    df <- tibble(Date = as.Date(values$dates), Persistence = values$persistence)
    
    p <- ggplot(df, aes(x = Date, y = Persistence)) +
      geom_point(color = "darkorchid4", size = 2) +
      labs(
        title = "Real-Time PLTR.US H1 Topological Persistence",
        x = "Date",
        y = "Max H1 Persistence"
      ) +
      theme_minimal(base_size = 14)
    
    # Only add geom_line if we have more than 1 observation
    if (nrow(df) > 1) {
      p <- p + geom_line(color = "purple", linewidth = 1.2)
    }
    
    print(p)
  })
  
  output$status_text <- renderText({
    paste("Status: Active\nLast checked:", Sys.time(), "\nTotal rolling windows tracked:", length(values$dates))
  })
}

shinyApp(ui = ui, server = server)