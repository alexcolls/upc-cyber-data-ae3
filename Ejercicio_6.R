# Librerías necesarias
library(dplyr)
library(ggplot2)

# Verificación: asegurar que epa_http tiene datos válidos
if (!exists("epa_http") || nrow(epa_http) == 0) {
  stop("Error: el dataset 'epa_http' no está cargado o está vacío.")
}

# --- PARTE 1: Frecuencia de códigos de estado HTTP ---

# Calcular la frecuencia de códigos de estado HTTP
frecuencia_status <- epa_http %>%
  filter(!is.na(status)) %>%  # Excluir filas con NA en status
  count(status, name = "frecuencia")  # Contar ocurrencias por cada código de estado

# Mostrar la tabla de frecuencias
print("Tabla de Frecuencia de Códigos de Estado HTTP:")
print(frecuencia_status)

# Crear el gráfico
grafico_frecuencia_status <- ggplot(frecuencia_status, aes(x = status, y = frecuencia, fill = as.factor(status))) +
  geom_bar(stat = "identity") +
  labs(
    title = "Frecuencia de códigos de estado HTTP",
    x = "Código de estado HTTP",
    y = "Frecuencia"
  ) +
  theme_minimal()

# Mostrar el gráfico
print(grafico_frecuencia_status)

# Guardar el gráfico como PDF
ggsave("frecuencia_status.pdf", plot = grafico_frecuencia_status, width = 8, height = 6)

# --- PARTE 2: Volumen de datos transmitidos por método HTTP ---

# Calcular el volumen de datos transmitidos por método HTTP
volumen_por_metodo <- epa_http %>%
  filter(!is.na(method) & !is.na(bytes)) %>%  # Excluir filas con NA en method o bytes
  group_by(method) %>%
  summarize(total_bytes = sum(bytes, na.rm = TRUE))  # Sumar bytes transmitidos por método

# Mostrar la tabla de volumen por método
print("Tabla de Volumen de Datos Transmitidos por Método HTTP:")
print(volumen_por_metodo)

# Crear el gráfico
grafico_volumen_metodo <- ggplot(volumen_por_metodo, aes(x = method, y = total_bytes, fill = method)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Volumen de datos transmitidos por método HTTP",
    x = "Método HTTP",
    y = "Total de bytes transmitidos"
  ) +
  theme_minimal()

# Mostrar el gráfico
print(grafico_volumen_metodo)

# Guardar el gráfico como PDF
ggsave("volumen_por_metodo.pdf", plot = grafico_volumen_metodo, width = 8, height = 6)

# --- CONFIRMAR UBICACIÓN DE ARCHIVOS GUARDADOS ---

# Verificar el directorio de trabajo
print(paste("Los gráficos han sido guardados en:", getwd()))




