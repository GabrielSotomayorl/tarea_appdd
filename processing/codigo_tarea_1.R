#Tarea 1 APPDD 


#Limpiar el ambiente
gc()
rm(list=ls())

if (!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse,
               readxl, 
               chilemapas,
               sjmisc)

#Descarga de datos de ingreso
download.file("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/rraa/2021/Indicadores_Ingreso_Anual_2021.xlsx",
              "input/data/original/Indicadores_Ingreso_Anual_2021.xlsx",mode = "wb")

#Carga de datos a R
ingreso<-read_excel("input/data/original/Indicadores_Ingreso_Anual_2021.xlsx", skip = 2,
                    sheet = "1")

#separar la varaible según hombres y mujeres 
ingresoh<-ingreso %>% filter(Nivel=="comuna_sexo" & Desagregación2 =="Masculino")  %>%
  dplyr::select(comuna= Desagregación1, ing_prom_hombre = "2021")
ingresom<-ingreso %>% filter(Nivel=="comuna_sexo" & Desagregación2 =="Femenino")%>%
  dplyr::select(comuna= Desagregación1, ing_prom_mujer = "2021")

#creación variable de brecha de género por comuna y eliminación de los carácteres esepcial de los nombres de comuna
ingresos<-merge(ingresoh,ingresom, by="comuna") %>% 
  mutate(ing_prom_hombre=as.numeric(ing_prom_hombre),
         ing_prom_mujer=as.numeric(ing_prom_mujer),
    brecha=(ing_prom_hombre-ing_prom_mujer)/ing_prom_hombre*100,
    comuna= tolower(iconv(comuna,to = "ASCII//TRANSLIT" )))

#armonizar nombres de comunas
ingresos$comuna[ingresos$comuna=="aysen"] <- "aisen"
ingresos$comuna[ingresos$comuna=="o higgins"] <- "o'higgins"
ingresos$comuna[ingresos$comuna=="coyhaique"] <- "coihaique"

#perdidos de casos de comuina con valores extremos con muy pocos habitantes
ingresos$brecha[ingresos$comuna%in% c("laguna blanca","lago verde",
                                      "la higuera")]<-NA

#histograma de distribución de la brecha salarial de género por comuna
ggplot(ingresos, aes(x=brecha)) +
  geom_histogram(fill="#35b779", color="black") +
  theme_minimal() +
  labs(x="Brecha salarial de género bruta", y="Frecuencia", title="Distribución de la brecha salarial de género por comuna")

#guardar el gráfico
ggsave(filename="output/graphs/histograma.jpeg",device = "jpeg",
       width = 2000,
       height = 2000,
       units = "px")

#tabla por tramos
#Crear variable tramos de la brecha
ingresos$brecha_tramos<-cut(ingresos$brecha,c(min(ingresos$brecha,na.rm=T),0,12,24,
                                              max(ingresos$brecha,na.rm=T)))
#ver tabla en el viewer
frq(ingresos$brecha_tramos,out = "viewer")

#guardar tabla
frq(ingresos$brecha_tramos,out = "browser",
    file = "output/tables/tablarangos.html")

#intregrar código de comuna para merge con base de datos de información geográfica 
codigo_comuna <- read_excel("input/data/original/codigo_comuna.xlsx") %>% 
  mutate(comuna= tolower(iconv(comuna,to = "ASCII//TRANSLIT" )))
ingresos<-merge(codigo_comuna,ingresos,by="comuna")

#Unir datos de brecha con información geográfica del paquete chile_mapas
mapa_comunas$codigo_comunan<-as.numeric(mapa_comunas$codigo_comuna) 
datamapa<-merge(mapa_comunas,ingresos[,c("cod_comuna","brecha")],by.x = "codigo_comunan",by.y = "cod_comuna")

#Crear mapa
ggplot(datamapa) + 
  geom_sf(aes(fill = brecha, geometry = geometry))  +
  scale_fill_gradient2(low = "#fde725", mid = "#35b779", high = "#440154", midpoint = 0, name = "brecha") +
  theme_minimal(base_size = 13)

#guardar mapa
ggsave(filename="output/graphs/mapa.jpeg",device = "jpeg",
       width = 5000,
       height = 5000,
       units = "px")

