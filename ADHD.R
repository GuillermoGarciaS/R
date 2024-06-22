
# Instalamos los paquetes necesarios, en caso de ya estar instalados, no se volvera a realizar
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("viridis", quietly = TRUE)) {
  install.packages("viridis")
}
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  install.packages("RColorBrewer")
}
if (!requireNamespace("colorspace", quietly = TRUE)) {
  install.packages("colorspace")
}

# Cargamos las librerias
library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)
library(RColorBrewer)
library(colorspace)

# Enlistamos todos los archivos dentro de la carpeta basado en el patron de .csv
file_paths <- list.files("C:/Users/eiven/Documents/Data/adhd1", pattern = ".csv", full.names = TRUE)

# Funcion que lee y le da nombre a los datos
read_and_label <- function(file) {
  df <- read.csv(file)
  state_name <- tools::file_path_sans_ext(basename(file))
  df$State <- state_name
  return(df)
}

# Leemos los datos y les damos un nombre
data_list <- lapply(file_paths, read_and_label)

# Combinamos todos los datos en un solo frame
combined_data <- bind_rows(data_list)

# En caso de ocuparse, imprimimos los nombres de las columnas
# print(colnames(combined_data))

# Checamos y limpiamos la columna 'Prevalence Year'
combined_data <- combined_data %>%
  mutate(`Prevalence.Year` = as.character(`Prevalence.Year`)) %>%
  separate_rows(`Prevalence.Year`, sep = "-") %>%
  mutate(`Prevalence.Year` = as.numeric(`Prevalence.Year`))

# Resumimos los datos
summary_data <- combined_data %>%
  group_by(State, `Prevalence.Year`) %>%
  summarise(Total_Cases = sum(`Prevalence.Percentage`, na.rm = TRUE))

# Dividimos los datos en grupos de 11 (los que sobren iran a una ultima division)
states <- unique(combined_data$State)
states <- sort(states)  # Sort states alphabetically
state_groups <- split(states, ceiling(seq_along(states)/11))

# Fucnion para graficar el % de cada estado
create_plot <- function(state_group, index) {
  filtered_data <- summary_data %>% filter(State %in% state_group)
  
  # Generatmos una paleta de colores para representar cada estado
  colors <- qualitative_hcl(length(state_group), palette = "Set3")
  
  plot <- ggplot(filtered_data, aes(x = `Prevalence.Year`, y = Total_Cases, group = State, color = State)) +
    geom_line() +
    labs(x = "Year", y = "Total Cases of ADHD", title = paste("Total ADHD Cases by Year and State - Group", index)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = brewer.pal(11, "Set3"))
  
  # Guardamos las gráficas
  ggsave(paste("total_adhd_cases_by_year_and_state_group", index, ".png", sep = ""), plot = plot, width = 10, height = 6, dpi = 300)
}

# Finalmente ajendamos una gráfica usando un ciclo
for (i in seq_along(state_groups)) {
  create_plot(state_groups[[i]], i)
}
