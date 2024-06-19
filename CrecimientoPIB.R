# Instalamos los paquetes en caso de no haber sido instalados
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)
library(readxl)

# Leemos el archivo
file_path <- "C:/Users/eiven/Documents/Data/imf-dm-export-20240619.xls"
imf_data <- read_excel(file_path)

# Cambiamos el formato del archivo para poder manipularlo
imf_data_long <- imf_data %>%
  pivot_longer(
    cols = -Real GDP growth (Annual percent change),  
    names_to = "Año",                                  
    values_to = "Crecimiento_PIB",                     
    names_transform = list(Año = as.integer)           
  ) %>%
  mutate(Crecimiento_PIB = as.numeric(Crecimiento_PIB))           

# Filtramos los datos por país
country_data <- imf_data_long %>%
  
  # Por pais ####

filter(Real GDP growth (Annual percent change) == "Chile") #Se puede escoger otro país

# Creamos una columna para indicar los periodos de crecimiento y decrecimiento
country_data <- country_data %>%
  mutate(Growth_Indicator = ifelse(Crecimiento_PIB > 0, "Crecimiento", "Decrecimiento"))

# Creamos la gráfica para ver el crecimiento del PIB a través del tiempo
ggplot(country_data, aes(x = Año, y = Crecimiento_PIB)) +
  geom_rect(data = country_data %>% filter(Growth_Indicator == "Crecimiento"),
            aes(xmin = Año - 0.5, xmax = Año + 0.5, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.1) +
  geom_rect(data = country_data %>% filter(Growth_Indicator == "Decrecimiento"),
            aes(xmin = Año - 0.5, xmax = Año + 0.5, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.1) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Crecimiento del PIB a través del tiempo",
       x = "Año",
       y = "Crecimiento del PIB (%)") +
  theme_minimal()


# Para el mundo en general ####

# Agregamos el PIB de todos los paises
world_data <- imf_data_long %>%
  group_by(Año) %>%
  summarise(Total_Growth = sum(Crecimiento_PIB, na.rm = TRUE)) %>%
  ungroup()

# Graficamos los resultaddos

ggplot(country_data, aes(x = Año, y = Crecimiento_PIB)) +
  geom_rect(data = country_data %>% filter(Growth_Indicator == "Crecimiento"),
            aes(xmin = Año - 0.5, xmax = Año + 0.5, ymin = -Inf, ymax = Inf),
            fill = "green", alpha = 0.1) +
  geom_rect(data = country_data %>% filter(Growth_Indicator == "Decrecimiento"),
            aes(xmin = Año - 0.5, xmax = Año + 0.5, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.1) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Crecimiento del PIB a través del tiempo",
       x = "Año",
       y = "Crecimiento del PIB (%)") +
  theme_minimal()