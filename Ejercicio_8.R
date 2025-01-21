# Cargar las librerías necesarias
if (!require("readr")) install.packages("readr")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("mltools")) install.packages("mltools")
if (!require("data.table")) install.packages("data.table")
if (!require("ggplot2")) install.packages("ggplot2")

library(readr)
library(stringr)
library(dplyr)
library(mltools)
library(data.table)
library(ggplot2)

# Cargar los datos
raw_data <- read_lines("epa-http.csv")

# Parsear las columnas desde las líneas
epa_http <- tibble(
  ip = str_match(raw_data, "^(\\S+)")[, 1],
  timestamp = str_match(raw_data, "\\[(.*?)\\]")[, 2],
  request = str_match(raw_data, "\"(.*?)\"")[, 2],
  status = as.numeric(str_match(raw_data, "\\s(\\d{3})\\s")[, 2]),
  bytes = as.numeric(str_match(raw_data, "\\s(\\d+)$")[, 2])
)

# Limpiar los datos
epa_http <- epa_http %>%
  filter(!is.na(request)) %>% 
  mutate(
    method = str_match(request, "^(GET|POST|PUT|DELETE|HEAD)")[, 1],
    resource = str_match(request, "^\\w+\\s(.*?)\\sHTTP")[, 2]
  ) %>%
  select(-request)

# Reemplazar los NA en la columna `bytes` con 0 donde el status es 404
epa_http <- epa_http %>%
  mutate(bytes = ifelse(is.na(bytes) & status == 404, 0, bytes))

# Agregar columna numérica derivada del recurso servido
epa_http <- epa_http %>%
  mutate(resource_length = nchar(resource))

# Aplicar one-hot encoding
epa_http_one_hot <- one_hot(as.data.table(epa_http), sparsifyNAs = TRUE)

# Seleccionar solo las columnas numéricas para k-means
data_numeric <- epa_http_one_hot %>%
  select(where(is.numeric))

# Imputar valores NA con la mediana de la columna correspondiente
for (col in names(data_numeric)) {
  if (any(is.na(data_numeric[[col]]))) {
    data_numeric[[col]][is.na(data_numeric[[col]])] <- median(data_numeric[[col]], na.rm = TRUE)
  }
}

# Aplicar k-means con diferentes valores de k
set.seed(42)
k_values <- c(2, 3, 4, 5)  # Valores de k a probar
k_results <- lapply(k_values, function(k) kmeans(data_numeric, centers = k, nstart = 25))

# Mostrar los centros de los clústeres para cada valor de k
for (i in seq_along(k_values)) {
  cat(paste("\nResultados con k =", k_values[i], "\n"))
  print(k_results[[i]]$centers)
}

# Visualización con PCA para las dos primeras componentes principales
pca_data <- prcomp(data_numeric, scale. = TRUE)
plot_data <- as.data.frame(pca_data$x[, 1:2])

# Crear gráficos para cada k
for (i in seq_along(k_values)) {
  plot_data <- plot_data %>%
    mutate(Cluster = factor(k_results[[i]]$cluster))
  
  p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point() +
    ggtitle(paste("Clustering con k =", k_values[i])) +
    theme_minimal()
  
  print(p)  # Usar print() explícito dentro del bucle
}
x