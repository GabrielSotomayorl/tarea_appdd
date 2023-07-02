#Tarea 1 APPDD 

#Limpiar el ambiente
gc()
rm(list=ls())

if (!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse,
               readxl, 
               chilemapas,
               sjmisc,
               summarytools,
               htmlTable)

#Descarga de datos de ingreso

# Verifica si el archivo existe
if (!file.exists("input/data/original/Indicadores_Ingreso_Anual_2021.xlsx")) {
  download.file("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/rraa/2021/Indicadores_Ingreso_Anual_2021.xlsx",
                "input/data/original/Indicadores_Ingreso_Anual_2021.xlsx", mode = "wb")
  print("Archivo descargado.")
} else {
  print("El archivo ya existe, no se descargará de nuevo.")
}
#Carga de datos a R
ingreso<-read_excel("input/data/original/Indicadores_Ingreso_Anual_2021.xlsx", 
                    sheet = "1",range = "A3:E3270")

#Calculo de brecha nacional
media_hombres<- as.numeric(ingreso$"2021"[ingreso$Desagregación1=="Masculino"]) 
media_mujeres <- as.numeric(ingreso$"2021"[ingreso$Desagregación1=="Femenino"]) 
brecha_nacional <- (media_hombres - media_mujeres) / media_hombres * 100

#separar la varaible según hombres y mujeres 
ingresoh<-ingreso %>% filter(Nivel=="comuna_sexo" & Desagregación2 =="Masculino")  %>%
  dplyr::select(comuna= Desagregación1, ing_prom_hombre = "2021")
ingresom<-ingreso %>% filter(Nivel=="comuna_sexo" & Desagregación2 =="Femenino")%>%
  dplyr::select(comuna= Desagregación1, ing_prom_mujer = "2021")

#creación variable de brecha de género por comuna y eliminación de los carácteres esepcial de los nombres de comuna
ingresos<-ingresoh %>% 
  left_join(ingresom, by="comuna") %>% 
  mutate_at(vars(ing_prom_hombre, ing_prom_mujer), as.numeric) %>%
  mutate(brecha=(ing_prom_hombre-ing_prom_mujer)/ing_prom_hombre*100,
         comuna= tolower(iconv(comuna,to = "ASCII//TRANSLIT" )))

#estadísticos descritpivos
view(dfSummary(ingresos[,-1], headings = FALSE,method ="browser"))

# Armonizar nombres de comunas
ingresos$comuna <- case_when(
  ingresos$comuna=="aysen" ~ "aisen",
  ingresos$comuna=="o higgins" ~ "o'higgins",
  ingresos$comuna=="coyhaique" ~ "coihaique",
  TRUE ~ ingresos$comuna
)

# Perdidos de casos de comuna con valores extremos con muy pocos habitantes
ingresos$brecha[ingresos$comuna %in% c("laguna blanca","lago verde","la higuera")] <- NA

# Histograma de distribución de la brecha salarial de género por comuna
hist_plot <- ggplot(ingresos, aes(x=brecha)) +
  geom_histogram(fill="#35b779", color="black") +
  theme_minimal() +
  labs(x="Brecha salarial de género bruta", y="Frecuencia", title="Distribución de la brecha salarial de género por comuna")

# Guardar el gráfico
ggsave(filename="output/graphs/histograma.jpeg", plot = hist_plot, device = "jpeg", width = 2000, height = 2000, units = "px")

# Tabla por tramos
# Crear variable tramos de la brecha
ingresos <- ingresos %>%
  mutate(brecha_tramos = cut(brecha,c(min(brecha,na.rm=T)-0.1,0,12,24,max(brecha,na.rm=T))))

ingresos[!is.na(ingresos$brecha)&is.na(ingresos$brecha_tramos),]

# Tabla de frencuencia por tramos
tabla <- frq(ingresos$brecha_tramos)[[1]][-2]
names(tabla) <- c("Valores", 
                  "Frecuencia absoluta",
                  "Prc %",
                  "Prc % válido",
                  "Prc % acumulado")
tabla_html <- htmlTable(tabla,
                        caption = "Distribución de la brecha salarial de género media bruta comunal según tramos",
                        css.cell = "text-align: center;",
                        tfoot = "Fuente: Elaboración propia a partir de registros administrativos del Ministerio de Desarrollo Social")

writeLines(tabla_html, "output/tables/tablarangos.html")

# Integrar código de comuna para merge con base de datos de información geográfica 
codigo_comuna <- read_excel("input/data/original/codigo_comuna.xlsx") %>%
  mutate(comuna= tolower(iconv(comuna,to = "ASCII//TRANSLIT" )))
ingresos <- left_join(codigo_comuna,ingresos,by="comuna")

# Unir datos de brecha con información geográfica del paquete chile_mapas
mapa_comunas$codigo_comunan <- as.numeric(mapa_comunas$codigo_comuna) 
datamapa <- left_join(mapa_comunas, select(ingresos, c(cod_comuna, brecha)), by = c("codigo_comunan" = "cod_comuna"))

# Crear mapa
map_plot <- ggplot(datamapa[!datamapa$codigo_comuna %in% c("05104","05201"),]) + 
  geom_sf(aes(fill = brecha, geometry = geometry))  +
  scale_fill_gradient2(low = "#fde725", mid = "#35b779", high = "#440154", midpoint = 0, name = "brecha") +
  theme_minimal(base_size = 13)+
  labs(title="Figura 1: Distribución geográfica de la brecha 
      salarial de género comunal", caption= "Fuente: Elaboración propia a partir de registros 
       administrativos del Ministerio de Desarrollo Social.")

# Guardar mapa
ggsave(filename="output/graphs/mapa.jpeg", plot = map_plot, device = "jpeg", width = 3800, height = 7000, units = "px",
       dpi=800)

