library(tidyverse)



pltr_data_w <- read_csv("pltr_us_w.csv")
pltr_data_w

pltr_plot_w <- pltr_data_w |>
  ggplot(aes(x = Date, y = Close)) +
    geom_line(colour = "steelblue", linewidth = 0.9) +
    labs(title = "3-Year PLTR.US Price Trend (Weekly)",
        subtitle= "Weekly Close Prices",
        x = "Date (6 month intervals)",
        y= "Closing Price") +
    scale_x_date(
    date_breaks = "6 months", 
    date_labels = "%b %Y") + 
    theme(text = element_text(size=20)) 

print(pltr_plot_w)

filtered_pltr_data <- pltr_data_w |>
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


##the above data was for weekly prices. Below is Oct 1-15 2026 daily data

pltr_data_d <- read_csv("pltr_us_d.csv")

pltr_plot_d <- pltr_data_d |>
ggplot(aes(x= Date, y = Close)) +
    geom_line(colour = "steelblue", linewidth = 0.9) +
    labs(title = "15 Day PLTR.US Price Trend (Daily)",
        subtitle= "Daily Close Prices",
        x = "Date (Oct 1 -15, 2026)",
        y= "Closing Price") +
    theme(text = element_text(size=20)) 

print(pltr_plot_d)
