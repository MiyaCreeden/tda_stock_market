library(tidyverse)


pltr_data <- read_csv("pltr_us_w.csv")
pltr_data

pltr_plot <- pltr_data |>
ggplot(aes(x= Date, y = Close)) +
    geom_line(colour = "steelblue", linewidth = 0.9) +
    labs(title = "3-Year PLTR.US Price Trend (Weekly)",
        subtitle= "Weekly Close Prices",
        x = "Date (6 month intervals)",
        y= "Closing Price") +
    scale_x_date(
    date_breaks = "6 months", 
    date_labels = "%b %Y") + 
    theme(text = element_text(size=20)) 

filtered_pltr_data <- pltr_data |>
filter(Date >= ("2025-08-14") & Date <= ("2026-02-14") )
filtered_pltr_data

ranged_pltr_plot <- filtered_pltr_data |>
ggplot(aes(x= Date, y = Close)) +
    geom_line(colour = "steelblue", linewidth = 0.9) +
    labs(title = "PLTR.US Price Trend (Aug 2025- Feb 2026)",
        subtitle= "Weekly Close Prices",
        x = "Date (Monthly)",
        y= "Closing Price") +
    theme(text = element_text(size=20)) 

