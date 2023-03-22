library(rnaturalearth)
library(sf)
library(mapview)
library(tmap)
library(ggplot2)
library(viridis)
library(coronavirus)
library(dplyr)
library(plotly)

data("covid19_vaccine")

head(covid19_vaccine)

plot_covid19_vaccine <- function(data, country) {
  data %>%
    filter(country_region == country) %>%
    select(date, doses_admin, total = people_at_least_one_dose) %>%
    arrange(date) %>%
    plot_ly() %>%
    add_lines(x = ~ date, y = ~ doses_admin, name = "Total Doses") %>%
    add_lines(x = ~ date, y = ~ total, name = "Received Vaccine with One/Two Doses") %>%
    layout(title = paste0(country, " Cumulative Number of Doses by Type"),
           legend = list(x = 0.05, y = 0.95),
           margin = list(l = 50, r = 50, b = 50, t = 90),
           yaxis = list(title = "Number of Doses"),
           xaxis = list(title = ""))
}

plot_covid19_vaccine(covid19_vaccine, "Nigeria")
plot_covid19_vaccine(covid19_vaccine, "United Kingdom")
plot_covid19_vaccine(covid19_vaccine, "US")


