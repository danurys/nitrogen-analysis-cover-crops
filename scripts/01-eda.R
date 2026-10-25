# Análisis exploratorio de datos
# Dataset N-Data
install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(ggplot2)

# Importar los datos
datos_n <- read_excel("datos/N-Data.xlsx", sheet = "Hoja2")

# Explorar la estructura de los datos
glimpse(datos_n)

# Resumen estadístico
summary(datos_n)

# Comprobar datos ausentes
sum(is.na(datos_n))


# Renombrar las variables numéricas
names(datos_n)[5:7] <- c("amonio", "nitrato", "lixiviacion")

names(datos_n)

# Resumen estadístico de la lixiviación de nitrato
summary(datos_n$lixiviacion)

# Lixiviación promedio de nitrato según cultivo

lixiviacion_cultivo <- datos_n %>%
  group_by(Crop) %>%
  summarise(
    promedio_lixiviacion = mean(lixiviacion, na.rm = TRUE)
  )

lixiviacion_cultivo

# Gráfico exploratorio de la lixiviación de nitrato según cultivo

ggplot(datos_n, aes(x = Crop, y = lixiviacion)) +
  geom_boxplot() +
  labs(
    title = "Lixiviación de nitrato según cultivo",
    subtitle = "Análisis exploratorio de los datos",
    x = "Cultivo",
    y = "Lixiviación de nitrato (kg N/ha)"
  ) +
  theme_minimal()
ggplot(datos_n, aes(x = Crop, y = lixiviacion)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c(
      "Fallow" = "Barbecho",
      "Multispecies" = "Multiespecies",
      "Oat" = "Avena"
    )
  ) +
  labs(
    x = "Cultivo",
    y = "Lixiviación de nitrato (kg N/ha)"
  ) +
  theme_minimal()
