# Instalar y cargar paquetes necesarios
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("DT")) install.packages("DT")

library(tidyverse)
library(lubridate)
library(DT)

# Leer los datos del archivo
raw_data <- read_lines("epa-http.csv")

# Dividir las columnas usando una expresión regular
epa_http <- raw_data %>%
  str_match('^(\\S+) \\[(\\d+):(\\d+):(\\d+):(\\d+)\\] "([A-Z]+)\\s+([^"]+)\\s+(HTTP/\\d\\.\\d)" (\\d{3}) (\\d+|-)') %>%
  as.data.frame() %>%
  select(-V1) %>%
  setNames(c("host", "day", "hour", "minute", "second", "method", "resource", "protocol", "status", "bytes"))

# Limpieza y codificación de las columnas
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
    ),
    method = as.factor(method),
    protocol = as.factor(protocol),
    resource = str_trim(resource) # Eliminar espacios extra en la columna 'resource'
  ) %>%
  select(timestamp, host, method, resource, protocol, status, bytes) %>% # Seleccionar columnas relevantes
  filter(!is.na(timestamp)) # Eliminar filas con timestamp inválido

# Explorar el DataFrame en una pestaña interactiva
datatable(epa_http, options = list(pageLength = 25))
