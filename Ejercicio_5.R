# Cargar los paquetes necesarios
if (!require("readr")) install.packages("readr")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

library(readr)   # Para read_lines
library(stringr) # Para str_match
library(dplyr)   # Para manipulación de datos
library(ggplot2) # Para gráficos

# Cargar los datos crudos como líneas de texto
raw_data <- read_lines("epa-http.csv")

# Separar las líneas de texto en columnas utilizando un delimitador adecuado, como el espacio
# Aquí se asume que las columnas están separadas por espacios y que las columnas relevantes son:
# - IP: Dirección IP
# - Timestamp: Hora de la solicitud
# - Method: Método HTTP (GET, POST, etc.)
# - URL: El recurso solicitado
# - HTTP version: Versión HTTP (HTTP/1.0, HTTP/1.1, etc.)
# - Status: Código de estado de la respuesta
# - Size: Tamaño de la respuesta

# Usamos str_split para separar cada línea en componentes
data_split <- str_split(raw_data, pattern = "\\s+", simplify = TRUE)

# Convertir el dataframe a un formato adecuado
data <- data.frame(
  ip = data_split[, 1],
  timestamp = data_split[, 2],
  method = str_replace_all(data_split[, 3], "\"", ""),  # Eliminar las comillas
  url = data_split[, 4],
  status = data_split[, 6],
  size = data_split[, 7],
  stringsAsFactors = FALSE
)

# Ver las primeras filas para comprobar la estructura de los datos
head(data)

# Paso 1: Analizar la frecuencia de los tipos de peticiones HTTP (GET, POST, PUT, DELETE)
peticiones <- data %>%
  group_by(method) %>%
  summarise(frecuencia = n())

# Mostrar las frecuencias de las peticiones
print(peticiones)

# Paso 2: Filtrar las peticiones que corresponden a recursos de tipo imagen
imagenes <- data %>%
  filter(grepl("\\.jpg$|\\.png$|\\.gif$|\\.jpeg$", url, ignore.case = TRUE))

# Paso 3: Contar la frecuencia de cada tipo de petición HTTP para imágenes
peticiones_imagenes <- imagenes %>%
  group_by(method) %>%
  summarise(frecuencia = n())

# Mostrar las frecuencias de las peticiones para imágenes
print(peticiones_imagenes)

# Paso 4: Visualizar los resultados con gráficos de barras

# Gráfico de barras para la frecuencia de peticiones HTTP generales
ggplot(peticiones, aes(x = method, y = frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frecuencia de peticiones HTTP", x = "Método HTTP", y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras para la frecuencia de peticiones HTTP para imágenes
ggplot(peticiones_imagenes, aes(x = method, y = frecuencia)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Frecuencia de peticiones HTTP para imágenes", x = "Método HTTP", y = "Frecuencia") +
  theme_minimal()

