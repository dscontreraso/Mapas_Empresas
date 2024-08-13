# Mapas_Empresas
Mapeo de empresas según ingresos


library(dplyr)
library(readxl)
library(leaflet)
library(viridis)

# Cargar los datos desde el archivo Excel
datos <- read_excel("C:/Users/danie/Downloads/unifiicada.xlsx")

# Convertir la columna Ingreso_Operativo2022 a numérico después de eliminar caracteres no numéricos
datos <- datos %>%
  mutate(Ingreso_Operativo2022 = as.numeric(gsub("[^0-9.-]", "", Ingreso_Operativo2022)))

# Verificar los valores después de la conversión
print(summary(datos$Ingreso_Operativo2022))

# Calcular los quintiles de Ingreso_Operativo2022 y asignarlos a una nueva variable
datos <- datos %>%
  mutate(
    quintil = case_when(
      is.na(Ingreso_Operativo2022) ~ "Sin información",
      TRUE ~ cut(Ingreso_Operativo2022, 
                 breaks = quantile(Ingreso_Operativo2022, probs = seq(0, 1, by = 0.2), na.rm = TRUE), 
                 include.lowest = TRUE, 
                 labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
    )
  )

# Asignar colores según la categoría de quintiles y escala Víridis
color_pal <- colorFactor(
  palette = c(viridis(5), "grey"), 
  domain = c("Q1", "Q2", "Q3", "Q4", "Q5", "Sin información")
)

# Función para ajustar el tamaño de los puntos según el quintil
size_function <- function(quintil) {
  case_when(
    quintil == "Sin información" ~ 5,
    quintil == "Q1" ~ 8,
    quintil == "Q2" ~ 10,
    quintil == "Q3" ~ 12,
    quintil == "Q4" ~ 14,
    quintil == "Q5" ~ 16
  )
}

# Generar el mapa con leaflet
leaflet(datos) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitud, 
    lat = ~Latitud,
    radius = ~size_function(quintil),
    color = ~color_pal(quintil),
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>Compañía:</b> ", Compañía, "<br>",
      "<b>CIIURev4AC:</b> ", CIIURev4AC, "<br>",
      "<b>Ingreso Operativo 2022:</b> $", formatC(Ingreso_Operativo2022, format = "f", big.mark = ",", digits = 2)
    )
  ) %>%
  addLegend("bottomleft", 
            pal = color_pal, 
            values = ~quintil, 
            title = "Categoría (Quintiles)",
            opacity = 1) %>%
  addLegend("bottomleft",
            pal = colorNumeric("viridis", domain = c(0, max(datos$Ingreso_Operativo2022, na.rm = TRUE))),
            values = ~Ingreso_Operativo2022,
            title = "Nivel de ingreso",
            labFormat = labelFormat(transform = function(x) x * max(datos$Ingreso_Operativo2022, na.rm = TRUE)),
            opacity = 1)
