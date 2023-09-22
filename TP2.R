## TRABAJO PRÁCTICO N° 2 ---- 

# El presente trabajo continuará con el tratamiento de datos del TP1:
# Departamentos en venta del 2020, según la información disponible en BA Data.

### ACTIVACIÓN DE LIBRERÍAS ----

#En primer lugar, se activarán todas las librerías necesarias:

library(tidyverse)
library(skimr)
library(ggplot2)
library(sf)

### IMPORTACIÓN Y ANÁLISIS DE LOS DATOS BASE ----

# Se procede a importar los datos de los Departamentos en venta del 2020 en CABA:

(datos <- read_csv(file = "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/secretaria-de-desarrollo-urbano/departamentos-venta/departamentos-en-venta-2020.csv"))

# Observaciones iniciales al revisar rápidamente el contenido de la base de datos:

skimr::skim(datos)
summary(datos)

# La base de datos cuenta con 156.258 filas y 11 columnas (3 de tipo 'character' y 8 de tipo 'numeric').
# La propiedad más barata cuesta 7.200 USD y la más cara 17.000.000 USD.
# El m2 más económico cuesta 147 USD/m2 y el más caro 72.917 USD/m2.
# El número máximo de ambientes es 8; pero al observar la cantidad mínima se detecta un error, 
# puesto que el 'summary' indica -7.

### 1. TIPO DE PROPIEDADES EN VENTA. Cantidad según total de ambientes ----

# Continuando con el TP1, se plantea realizar un gráfico que evidencie la cantidad de propiedades
# en venta para cada tipo de propiedad, según el número de ambientes.

# En primer lugar, limpiamos la base de datos descartando las columnas que no poseen relevancia para el análisis,
# y aquellas filas que indican que la propiedad posee -7 ambientes.

dptos_venta <- datos %>% 
  select(-PropiedadS,-Cotizacion,-Trimestre) %>% 
  filter(Ambientes >= 0)

# Verificamos que la limpieza se haya efectuado correctamente

names(dptos_venta)
unique(dptos_venta$Ambientes)

# Luego, generamos una base de datos con la cantidad de propiedades en venta según cada tipología (s/ ambientes)

tipo_dpto <- dptos_venta %>% 
  group_by(Ambientes) %>% 
  summarise(Cantidad = n())

# Por último, realizamos un gráfico de barras que nos permita visualizar fácilmente esta información,
# corroborando (al igual que en el TP1) que las propiedades de dos ambientes son las más abundantes,
# seguidas de las de 3, 1 y 4 ambientes. 

ggplot(tipo_dpto, aes(x = Ambientes, y = Cantidad)) +
  geom_bar(stat = "identity", fill = "#4c92b5") +
  labs(title = "TIPOS DE PROPIEDADES EN VENTA",
       subtitle = "Cantidad según total de ambientes",
       x = "Ambientes de la propiedad", y = "Cantidad de propiedades en venta",
       caption = "FUENTE: BA Data") +
  theme_gray()
  
### 2.  PROPIEDADES EN VENTA POR COMUNA. Categorización por cantidad de ambientes ----

# El gráfico anterior podría enriquecerse al incorporar la variable 'Comunas' para observar su 
# distribución espacial sobre el territorio de CABA.

# Para ello, se procede a transformar la base de datos, agrupando la información según la cantidad
# de ambientes (conservando las propiedades más representativas: 1 - 2 - 3 - 4 ambientes) y las 
# comunas a las que pertenecen. A su vez, se calcula la cantidad de propiedades para cada caso:

(dptos_venta_com_amb <- dptos_venta %>% 
   group_by(Ambientes, Comunas) %>%
   filter(Comunas != "0") %>%
   filter(Ambientes %in% c("1", "2", "3", "4")) %>% 
   summarise(Cantidad = n ()))

# Dicha información es utilizada para efectuar el siguiente gráfico, donde la cantidad de propiedades
# en venta se encuentra faceteada según las comunas, indicando el número de departamentos en venta 
# para cada tipología: 1 - 2 - 3 - 4 ambientes:

ggplot(dptos_venta_com_amb) +
  geom_bar(aes(x = Ambientes, weight = Cantidad), width = 0.3, fill = "#125466") +
  labs(title = "PROPIEDADES EN VENTA POR COMUNA",
       subtitle = "Categorización por cantidad de ambientes",
       x = "Cantidad de Ambientes",
       y = "Propiedades en Venta",
       caption = "FUENTE: BA Data") +
  facet_wrap(~Comunas, scales = "free_x") +
  theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"), #ajusta los margenes del gráfico
        panel.background = element_rect(fill = "gray100", colour = "gray100", linewidth = 2, linetype = "solid"), #fondo del gráfico
        panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", colour = "gray80"), #lineas del gráfico
        panel.grid.minor = element_line(linewidth = 0.25, linetype = "dashed", colour = "gray90"), #líneas auxiliares
        title=element_text(size=12, face = "bold"), #título del gráfico
        plot.caption=element_text(face = "italic", colour = "gray35",size=5)) #nota al pie

# De este modo, se observa rápidamente que la Comuna 8 posee la menor oferta de propiedades en venta; las comunas
# 1 - 12 - 13 - 14 - 15 son las más diversificadas, seguidas por 2 - 3 - 5 - 6 - 7, y en menor medida por las comunas
# 4 - 9 - 10 - 11.


### 3. MAPA CABA PRECIO M2 ----

# Nuevamente, si retomamos el TP1 es posible plasmar en un mapa de CABA cuál es la 
# variación del precio por m2 en las diversas comunas de la ciudad. 

# Para ello, en primer lugar se importa la base de datos de las Comunas de CABA desde BA Data:

(comunas <- st_read("data_comunas/comunas_wgs84.shp",
                    stringsAsFactors = TRUE,
                    options = "ENCODING=UTF-8"))
  
# Posteriormente, es necesario trabajar sobre la base de datos para poder unificar ambas bases (dptos_ventas y comunas)
# a partir de una columna en común, para obtener un único dataframe que permita su visualización.

# Por tanto, se procede a determinar la cantidad de propiedades en venta por comuna, junto a su 
# precio promedio en dólares por m2. Asimismo, para contar con una columna en común de iguales propiedades,
# se procede a descartar la 'comuna 0' y renombrar la columna "Comunas" como "COMUNAS" para que coincida con el otro dataframe.

(dptos_venta_comuna <- dptos_venta %>% 
  group_by(Comunas) %>%
    filter(Comunas != "0") %>% 
  summarise(cantidad = n (),
            promedio_dolarM2 = mean(DolaresM2)) %>% 
  rename(COMUNAS = Comunas))

# En cuanto a la base de datos de las comunas, se procede a conservar sólamente dos columnas:
# comunas y geometría 

(comunas_geometria <- comunas %>% 
  select(COMUNAS, geometry))

# De este modo, es posible unir ambas base de datos y transformarlas a formato sf:

(dptos_venta_mapa1 <- left_join(dptos_venta_comuna, comunas_geometria, by = "COMUNAS") %>% 
  st_as_sf())

class(dptos_venta_mapa1) # se verifica la transformación a 'sf'

# Finalmente, se confecciona el Mapa de CABA indicando la variación del precio por m2 según cada comuna:

ggplot(dptos_venta_mapa1) +
  geom_sf(aes (fill = promedio_dolarM2)) +
  geom_sf_label(aes(label = COMUNAS),size=2)+
  scale_fill_viridis_c() + 
  labs(title = "VARIACIÓN DEL PRECIO POR M2",
       subtitle = "USD/m2",
       fill = "Precio promedio (USD/m2)",
       caption = "FUENTE: BA Data") +
    scale_fill_viridis_c()+
  guides(fill=guide_legend(title.position = "top", ncol=2))+ #posición titulo leyenda y columnas datos
  theme(plot.margin = margin(0.25, 1, 0.25, 0.1, "cm"), #margenes del gráfico
        panel.background = element_rect(fill = "gray100", colour = "gray100", linewidth = 2, linetype = "solid"), #fondo del gráfico
        panel.grid.major = element_line(linewidth = 0.5, linetype = "dashed", colour = "gray80"), #lineas del gráfico
        panel.grid.minor = element_line(linewidth = 0.25, linetype = "dashed", colour = "gray90"), #líneas auxiliares
        title=element_text(size=12, face = "bold"), #titulo del gráfico
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia 
        legend.position="bottom", #ubicacion de leyenda
        legend.direction = "horizontal", #dirección de la leyenda
        legend.title=element_text(size=8, face = "bold"), #tamaño de titulo de leyenda
        legend.text=element_text(size=5), #tamaño de texto de leyenda
        plot.caption=element_text(face = "italic", colour = "gray35",size=5)) #nota al pie
      
# De esta manera, se puede observar que la Comuna 14 posee el precio por m2 promedio más elevado, 
# seguidas por la Comuna 13 y 2.
