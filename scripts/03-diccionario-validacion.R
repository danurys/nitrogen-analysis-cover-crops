# Diccionario de datos
library(tibble)
library(dplyr)
diccionario <- tribble(
  ~variable,      ~tipo,      ~valores_esperados,       ~Admite_NA,
  "amonio",       "numérica", "> 0 y <= 20 mg/kg",      "Sí",
  "nitrato",      "numérica", "> 0 y <= 30 mg/kg",      "Sí",
  "lixiviacion",  "numérica", "> 0 y <= 50 kg N/ha",    "Sí"
)

View(diccionario)

# Validación de datos
validar_datos <- function(datos_n) {
  
  datos_n %>%
    mutate(
      validar_amonio = is.na(amonio) |
        (amonio > 0 & amonio <= 20),
      
      validar_nitrato = is.na(nitrato) |
        (nitrato > 0 & nitrato <= 30),
      
      validar_lixiviacion = is.na(lixiviacion) |
        (lixiviacion > 0 & lixiviacion <= 50)
    )
}
datos_validados <- validar_datos(datos_n)
View(datos_validados)
resumen_validacion <- datos_validados %>%
  summarise(
    total_observaciones = n(),
    
    amonio_validos = sum(validar_amonio),
    amonio_invalidos = sum(!validar_amonio),
    
    nitrato_validos = sum(validar_nitrato),
    nitrato_invalidos = sum(!validar_nitrato),
    
    lixiviacion_validos = sum(validar_lixiviacion),
    lixiviacion_invalidos = sum(!validar_lixiviacion)
  )
View(resumen_validacion)