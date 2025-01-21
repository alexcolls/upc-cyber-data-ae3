# Instalar y cargar paquetes necesarios
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("lubridate")) install.packages("lubridate")
if (!require("stringr")) install.packages("stringr")

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

# Cargar los datos desde el archivo
raw_data <- read_lines("epa-http.csv")

# Dividir las columnas usando una expresión regular ajustada
epa_http <- raw_data %>%
  str_match('^(\\S+) \\[(\\d+):(\\d+):(\\d+):(\\d+)\\] "([A-Z]+)\\s+([^"]+)\\s+(HTTP/\\d\\.\\d)" (\\d{3}) (\\d+|-)') %>%
  as.data.frame() %>%
  select(-V1) %>%
  setNames(c("host", "day", "hour", "minute", "second", "method", "resource", "protocol", "status", "bytes"))

# Convertir las columnas relevantes
epa_http <- epa_http %>%
  mutate(
    day = as.integer(day),
    hour = as.integer(hour),
    minute = as.integer(minute),
    second = as.integer(second),
    status = as.integer(status),
    bytes = na_if(bytes, "-") %>% as.integer(),
    timestamp = make_datetime(
      year = 2025, month = 1, day = day, hour = hour, min = minute, sec = second
    )
  )

# Crear columnas derivadas
epa_http <- epa_http %>%
  mutate(day_hour = floor_date(timestamp, unit = "hour"))

# Contar peticiones por hora
requests_by_time <- epa_http %>%
  group_by(day_hour) %>%
  summarize(request_count = n(), .groups = "drop")

# Graficar el número de peticiones a lo largo del tiempo
ggplot(requests_by_time, aes(x = day_hour, y = request_count)) +
  geom_line(color = "blue") +
  labs(
    title = "Número de peticiones servidas a lo largo del tiempo",
    x = "Tiempo (Hora)",
    y = "Número de Peticiones"
  ) +
  theme_minimal()

