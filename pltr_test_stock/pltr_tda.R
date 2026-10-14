library(TDA)
library(tidyverse)

#Analyzing the market structure of PLTR.US over the 6 month period (Aug2025-Feb2026)

pltr_data <- read_csv("pltr_us_w.csv")
filtered_pltr_data <- pltr_data |>
filter(Date >= ("2025-08-14") & Date <= ("2026-02-14") )

#create time series
embed_time_series <- function(series, dimension = 3, delay = 1) {
  n <- length(series)
  max_idx <- n - (dimension - 1) * delay
  
  if (max_idx <= 0) stop("Series is too short for these embedding parameters.")
  
  embedded <- matrix(0, nrow = max_idx, ncol = dimension)
  for (i in 1:dimension) {
    idx <- (1:max_idx) + (i - 1) * delay
    embedded[, i] <- series[idx]
  }
  return(embedded)
}

prices <- filtered_pltr_data |>
pull(Close)

#generate point cloud
point_cloud <- embed_time_series(prices, dimension=3, delay=1)

#componenets (0) and loops(1)
rips_output <- ripsDiag(
  X = point_cloud, 
  maxdimension = 1, 
  maxscale = max(dist(point_cloud)), 
  library = "GUDHI"
)

diag_matrix <- unclass(rips_output$diagram)

# 2. Build the data frame explicitly by column index
pd_results <- data.frame(
  dimension = diag_matrix[, 1],
  birth     = diag_matrix[, 2],
  death     = diag_matrix[, 3]
)

persistence_plot <- pd_results |>
ggplot(aes(x= birth, y = death, colour = as.factor(dimension))) +
    geom_point(size=2.5, alpha = 0.8)+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("steelblue", "firebrick"), name = "Dimension") +
  labs(
    title = "PLTR.US Vietoris-Rips Persistence Diagram",
    x = "Birth Radius",
    y = "Death Radius"
  ) +
  theme_minimal(base_size = 14)

  #Shows that the H_1 features (loops + cycles --> stock rising/pulling back) have generally short life cycles and indictae cyclical 
  # patterns and flucuations

  #Next i want to look at a rolling window frame over the 6 months (i.e how do these features change as opposed to a global snapsht as above)

  

