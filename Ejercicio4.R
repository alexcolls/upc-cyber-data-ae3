# Instalar y cargar paquetes necesarios
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")

library(dplyr)
library(readr)

# Cargar los datos
raw_data <- read_lines("epa-http.csv")

# Dividir las columnas usando una expresión regular
epa_http <- raw_data %>%
  str_match('^(\\S+) \\[(\\d+:\\d+:\\d+:\\d+)\\] "([A-Z]+)\\s+([^"]+)\\s+(HTTP/\\d\\.\\d)" (\\d{3}) (\\d+|-)') %>%
  as.data.frame() %>%
  select(-V1) %>%
  setNames(c("host", "timestamp", "method", "resource", "protocol", "status", "bytes"))

# Limpiar y convertir las columnas
epa_http <- epa_http %>%
  mutate(
    status = as.integer(status),
    bytes = na_if(bytes, "-") %>% as.integer()
  )

# Identificar usuarios únicos por tipo de error
error_analysis <- epa_http %>%
  mutate(
    error_type = case_when(
      status >= 400 & status < 500 ~ "Client Error (4xx)",
      status >= 500 ~ "Server Error (5xx)",
      TRUE ~ "No Error"
    )
  ) %>%
  group_by(error_type) %>%
  summarize(unique_users = n_distinct(host), .groups = "drop")

# Contar usuarios únicos con y sin errores
summary_analysis <- epa_http %>%
  mutate(has_error = status >= 400) %>%
  group_by(has_error) %>%
  summarize(unique_users = n_distinct(host), .groups = "drop") %>%
  mutate(error_status = ifelse(has_error, "With Errors", "No Errors"))

# Combinar resultados
final_results <- bind_rows(
  error_analysis,
  summary_analysis %>% select(-has_error) %>% rename(error_type = error_status)
)

# Mostrar resultados
print(final_results)
