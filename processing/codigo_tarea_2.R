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
               htmlTable,
               httr,
               texreg,
               chilemapas,
               knitr,
               kableExtra,
               gridExtra,
               stringr)

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


#población 
if (!file.exists("input/data/original/estimaciones-y-proyecciones-2002-2035-comunas.xlsx")) {
  GET("https://www.ine.gob.cl/docs/default-source/proyecciones-de-poblacion/cuadros-estadisticos/base-2017/estimaciones-y-proyecciones-2002-2035-comunas.xlsx?sfvrsn=8c87fc3f_3", 
      write_disk("input/data/original/estimaciones-y-proyecciones-2002-2035-comunas.xlsx", overwrite = TRUE))
  print("Archivo descargado.")
} else {
  print("El archivo ya existe, no se descargará de nuevo.")
}

#Carga de datos a R
poblacion <- read_excel("input/data/original/estimaciones-y-proyecciones-2002-2035-comunas.xlsx")

# Población total para el año 2021 por cada comuna
poblacion_total_2021 <- poblacion %>%
  filter(`Poblacion 2021` >= 0) %>% 
  group_by(Comuna, `Nombre Comuna`) %>%
  summarise(Total_Poblacion_2021 = sum(`Poblacion 2021`), .groups = "drop")

# Población de 0 a 17 años para el año 2021 por cada comuna
poblacion_0_17_2021 <- poblacion %>%
  filter(`Poblacion 2021` >= 0, Edad >= 0, Edad <= 17) %>% 
  group_by(Comuna, `Nombre Comuna`) %>%
  summarise(Poblacion_0_17_2021 = sum(`Poblacion 2021`), .groups = "drop")

# Unir las dos tablas para tener una sola base de datos por comuna
poblacion_final <- merge(poblacion_total_2021, poblacion_0_17_2021, by = c("Comuna", "Nombre Comuna"))
poblacion_final$propnna<-poblacion_final$Poblacion_0_17_2021/poblacion_final$Total_Poblacion_2021*100

#Educacion
if (!file.exists("input/data/original/Indicadores_Educacion_Anual_2021.xlsx")) {
  GET("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/rraa/2021/Indicadores_Educacion_Anual_2021.xlsx", 
      write_disk("input/data/original/Indicadores_Educacion_Anual_2021.xlsx", overwrite = TRUE))
  print("Archivo descargado.")
} else {
  print("El archivo ya existe, no se descargará de nuevo.")
}


educacion <- read_excel("input/data/original/Indicadores_Educacion_Anual_2021.xlsx", sheet = "3", skip = 2)

educacion<-educacion %>% filter(Nivel=="Comuna" )%>%
  dplyr::select(comuna= Desagregación1, tasa_matricula_parvularia_neta ="2021")


#ocupacion
if (!file.exists("input/data/original/Indicadores_Ocupacion_Anual_2021.xlsx")) {
  GET("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/rraa/2021/Indicadores_Ocupacion_Anual_2021.xlsx", 
      write_disk("input/data/original/Indicadores_Ocupacion_Anual_2021.xlsx", overwrite = TRUE))
  print("Archivo descargado.")
} else {
  print("El archivo ya existe, no se descargará de nuevo.")
}

ocupacion <- read_excel("input/data/original/Indicadores_Ocupacion_Anual_2021.xlsx", sheet = "1", skip = 2)

ocupacion<-ocupacion %>% filter(Nivel=="comuna" )%>%
  dplyr::select(comuna= Desagregación1, prop_ocup="2021")

# datos tributarios----------------


estadísticas_sii<- read_excel("input/data/original/PUB_COMU_RUBR.xlsx", skip = 4)

estadísticas_sii<- estadísticas_sii %>% janitor::clean_names()

b<- estadísticas_sii %>% filter(ano_comercial==2021 
                                # & rubro_economico ==  "B - Explotación de minas y canteras"
)

b$comuna_del_domicilio_o_casa_matriz <- tolower(b$comuna_del_domicilio_o_casa_matriz)

# Remove special characters
b$comuna_del_domicilio_o_casa_matriz <- iconv(b$comuna_del_domicilio_o_casa_matriz, "UTF-8", "ASCII//TRANSLIT")


name_map <- data.frame(
  from = c("titil", "la calera", "marchigue", "coihaique", "aisen", "ohiggins", "paihuano", "trehuaco"), 
  to = c("tiltil", "calera", "marchihue", "coyhaique", "aysen", "o'higgins", "paiguano", "treguaco")
)

# Adjust municipality names in b$comuna_del_domicilio_o_casa_matriz to match a$com_str
b <- b %>%
  mutate(comuna_del_domicilio_o_casa_matriz = case_when(
    comuna_del_domicilio_o_casa_matriz %in% name_map$from ~ name_map$to[match(comuna_del_domicilio_o_casa_matriz, name_map$from)],
    TRUE ~ comuna_del_domicilio_o_casa_matriz
  ))

b<-b[, c("comuna_del_domicilio_o_casa_matriz", "rubro_economico"
          ,"numero_de_empresas")]


b$numero_de_empresas <- as.numeric(b$numero_de_empresas)


b_wide <- b %>%
  pivot_wider(names_from = rubro_economico, values_from = numero_de_empresas, values_fill = 0)


var_names <- names(b_wide )[(ncol(b_wide ) - 21):ncol(b_wide)]

# Duplicar columnas
b_wide  <- b_wide %>% 
  mutate(across(all_of(var_names), list(pc = ~ .), .names = "{.col}_{.fn}"))


# crear proporciones 
b_wide  <-b_wide %>% mutate(across(var_names, 
                         ~ . / rowSums(b_wide[,var_names], na.rm = TRUE) * 100, 
                         .names = "{.col}_prop"))


censo17 <- readRDS("input/data/original/censo17.rds")

# Armonizar nombres de comunas
ingresos$comuna <- case_when(
  ingresos$comuna=="aysen" ~ "aisen",
  ingresos$comuna=="o higgins" ~ "o'higgins",
  ingresos$comuna=="coyhaique" ~ "coihaique",
  TRUE ~ ingresos$comuna
)

educacion$comuna= tolower(iconv(educacion$comuna,to = "ASCII//TRANSLIT" ))
educacion$comuna <- case_when(
  educacion$comuna=="aysen" ~ "aisen",
  educacion$comuna=="o higgins" ~ "o'higgins",
  educacion$comuna=="coyhaique" ~ "coihaique",
  TRUE ~ educacion$comuna
)

ocupacion$comuna<-tolower(iconv(ocupacion$comuna,to = "ASCII//TRANSLIT" ))
ocupacion$comuna <- case_when(
  ocupacion$comuna=="aysen" ~ "aisen",
  ocupacion$comuna=="o higgins" ~ "o'higgins",
  ocupacion$comuna=="coyhaique" ~ "coihaique",
  TRUE ~ ocupacion$comuna
)

b_wide$comuna_del_domicilio_o_casa_matriz <- case_when(
  b_wide$comuna_del_domicilio_o_casa_matriz=="aysen" ~ "aisen",
  b_wide$comuna_del_domicilio_o_casa_matriz=="coyhaique" ~ "coihaique",
  TRUE ~ b_wide$comuna_del_domicilio_o_casa_matriz
)

poblacion_final$`Nombre Comuna`= tolower(iconv(poblacion_final$`Nombre Comuna`,to = "ASCII//TRANSLIT" ))
poblacion_final$`Nombre Comuna`<- case_when(
  poblacion_final$`Nombre Comuna`=="aysen" ~ "aisen",
  poblacion_final$`Nombre Comuna`=="coyhaique" ~ "coihaique",
  TRUE ~ poblacion_final$`Nombre Comuna`
)


censo17$com_str = tolower(iconv(censo17$com_str,to = "ASCII//TRANSLIT" ))
censo17$com_str<- case_when(
  censo17$com_str=="aysen" ~ "aisen",
  censo17$com_str=="coyhaique" ~ "coihaique",
  TRUE ~ censo17$com_str
)

# Perdidos de casos de comuna con valores extremos con muy pocos habitantes
ingresos$brecha[ingresos$comuna %in% c("laguna blanca","lago verde","la higuera")] <- NA

df_final <- ingresos %>%
  dplyr::left_join(educacion, by = "comuna") %>%
  dplyr::left_join(ocupacion, by = "comuna") %>% 
  dplyr::left_join(b_wide, by = c("comuna" = "comuna_del_domicilio_o_casa_matriz")) %>% 
  dplyr::left_join(poblacion_final, by = c("comuna" = "Nombre Comuna")) %>% 
  dplyr::left_join(censo17, by = c("comuna" = "com_str")) 

df_final$prop_poblacion_pueblos_originarios_ley_chile_2017<-df_final$prop_poblacion_pueblos_originarios_ley_chile_2017*100
df_final$prop_rural_2020<-df_final$prop_rural_2020*100


#imputacion por region comuna 

df_final <-  df_final %>% 
  mutate(region = as.numeric(ifelse(comuna.y < 10000, substr(comuna.y, 1, 1), substr(comuna.y, 1, 2))),
         provincia = as.numeric(ifelse(comuna.y < 10000, substr(comuna.y, 1, 2), substr(comuna.y, 1, 3))),
         prop_ocup = as.numeric(prop_ocup),
         tasa_matricula_parvularia_neta = as.numeric(tasa_matricula_parvularia_neta)) %>% 
  filter(comuna != "antartica"& !is.na(brecha))


# ** imputación de datos faltantes (provincia - región) -------------------


# función de imputación: cambio por la media ------------------------------

reemplaza_mean <- function(x){
  if(!is.numeric(x)) return(x)
  if(any(is.na(x))){
    pos <- which(is.na(x))
    x[pos] <- mean(x, na.rm = T)
  }
  return(x)
}

summary(df_final)
###--- Imputación por provincia ---###

df_final <- df_final %>% group_by(provincia) %>% 
  mutate_all(reemplaza_mean) %>% ungroup()

###--- Imputación por región ---###

df_final <- df_final %>% group_by(region) %>% 
  mutate_all(reemplaza_mean) %>% ungroup() %>% 
  janitor::clean_names()


library(corrplot)

vars<-df_final[,c("brecha","prop_rural_2020","prop_poblacion_pueblos_originarios_ley_chile_2017","promedio_anios_escolaridad25_2017","propnna",
                  "tasa_matricula_parvularia_neta","prop_ocup","g_comercio_al_por_mayor_y_al_por_menor_reparacion_de_vehiculos_automotores_y_motocicletas_prop","i_actividades_de_alojamiento_y_de_servicio_de_comidas_prop",
                  "j_informacion_y_comunicaciones_prop","d_suministro_de_electricidad_gas_vapor_y_aire_acondicionado_prop","c_industria_manufacturera_prop","b_explotacion_de_minas_y_canteras_prop",
                  "e_suministro_de_agua_evacuacion_de_aguas_residuales_gestion_de_desechos_y_descontaminacion_prop","o_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria_prop",
                  "l_actividades_inmobiliarias_prop")]
colnames(vars)<- c("brecha","prop_rural_2020", "prop_pueblos_originarios", "promedio_anios_escolaridad",
                          "Proporcion NNA", "tasa_matricula_parvularia_neta", "prop_ocup", 
                          "g_comercio_prop", 
                          "i_alojamiento_y_comidas_prop", "j_informacion_y_com_prop",
                          "d_electricidad_gas_prop", "c_industria_manufacturera_prop",
                          "b_explotacion_de_minas_prop", "e_suministro_de_agua_prop", 
                          "o_adm_publica_y_defensa_prop", 
                          "l_actividades_inmobiliarias_prop")

corre<-cor(vars)
corrplot(corre, method = "number")


library(PerformanceAnalytics)
chart.Correlation(vars, histogram = TRUE)

# Fig.5
corrplot(corre,
         method = "circle",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.6)
#Regresión lineal múltiple-----------------------------

models <- list(
  lm(brecha ~ prop_rural_2020 + prop_poblacion_pueblos_originarios_ley_chile_2017 + 
       promedio_anios_escolaridad25_2017 + propnna  + tasa_matricula_parvularia_neta+ prop_ocup ,data=df_final),
  lm(brecha ~        e_suministro_de_agua_evacuacion_de_aguas_residuales_gestion_de_desechos_y_descontaminacion_prop + 
       o_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria_prop +
       d_suministro_de_electricidad_gas_vapor_y_aire_acondicionado_prop + 
       b_explotacion_de_minas_y_canteras_prop + 
       g_comercio_al_por_mayor_y_al_por_menor_reparacion_de_vehiculos_automotores_y_motocicletas_prop + 
       i_actividades_de_alojamiento_y_de_servicio_de_comidas_prop + 
       l_actividades_inmobiliarias_prop +
       c_industria_manufacturera_prop + 
       j_informacion_y_comunicaciones_prop ,data=df_final),
  lm(brecha ~ prop_rural_2020 + prop_poblacion_pueblos_originarios_ley_chile_2017 + 
       promedio_anios_escolaridad25_2017 + propnna  + tasa_matricula_parvularia_neta+ prop_ocup + 
       e_suministro_de_agua_evacuacion_de_aguas_residuales_gestion_de_desechos_y_descontaminacion_prop + 
       o_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria_prop +
       d_suministro_de_electricidad_gas_vapor_y_aire_acondicionado_prop + 
       b_explotacion_de_minas_y_canteras_prop + 
       g_comercio_al_por_mayor_y_al_por_menor_reparacion_de_vehiculos_automotores_y_motocicletas_prop + 
       i_actividades_de_alojamiento_y_de_servicio_de_comidas_prop + 
       l_actividades_inmobiliarias_prop +
       c_industria_manufacturera_prop + 
       j_informacion_y_comunicaciones_prop ,data=df_final) 
  
)





coefnames<-c("Intercepto","Proporción Rural", "Proporción pueblos orginarios", "Promedio años de escolaridad",
             "Porcentaje de NNA","Tasa de matricula parvularia","Proporción de ocupados",
             "Prop. Suministro de agua",
             "Prop. Administración publica y defensa",
             "Prop. Suministro de electricidad, gas, vapor y aire acondicionado",
             "Prop. Minas y Canteras",
             "Prop. Comercio, reparación autos",
             "Prop. Alojamiento y comidas",
             "Prop. Actividades inmobiliarias",
             "Prop Manofactura",
             "Prop. información y comunicaciones"
             
            )

screenreg(models, custom.coef.names = coefnames)

reg<-htmlreg(models,custom.coef.names = coefnames, single.row = T)

write_lines(reg,"output/tables/regresion.html")
browseURL("output/tables/regresion.html")

# mapa ----------------


# Integrar código de comuna para merge con base de datos de información geográfica 
codigo_comuna <- read_excel("input/data/original/codigo_comuna.xlsx") %>%
  mutate(comuna= tolower(iconv(comuna,to = "ASCII//TRANSLIT" )))
df_finalm <- left_join(codigo_comuna,df_final,by="comuna")

# Unir datos de brecha con información geográfica del paquete chile_mapas
mapa_comunas$codigo_comunan <- as.numeric(mapa_comunas$codigo_comuna) 
datamapa <- left_join(mapa_comunas, select(df_finalm, c(comuna,cod_comuna, brecha)), by = c("codigo_comunan" = "cod_comuna"))

saveRDS(datamapa, "input/data/proc/datamapa.rds")
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


# Tabla de descriptivos-------------

#estadísticos descritpivos
summary <- dfSummary(df_final[,c("ing_prom_hombre","ing_prom_mujer","brecha")], headings = FALSE)
summary_html <- print(summary, method = "render", omit.headings = TRUE, omit.style = TRUE, omit.body = FALSE)



writeLines(summary_html$children[[2]][[1]], "output/tables/vardep.html")
# Crear la carpeta 'output' si no 

# Tabla por tramos----------------------
# Crear variable tramos de la brecha
df_final <- df_final  %>%
  mutate(brecha_tramos = cut(brecha,c(min(brecha,na.rm=T)-0.1,0,12,24,max(brecha,na.rm=T))))

# Tabla de frencuencia por tramos
tabla <- frq(df_final$brecha_tramos)[[1]][-2]
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

# descritivos de las variables del modelo ---------------

df_model <- df_final[,c("brecha","prop_rural_2020","prop_poblacion_pueblos_originarios_ley_chile_2017","promedio_anios_escolaridad25_2017","propnna",
                        "tasa_matricula_parvularia_neta","prop_ocup","g_comercio_al_por_mayor_y_al_por_menor_reparacion_de_vehiculos_automotores_y_motocicletas_prop","i_actividades_de_alojamiento_y_de_servicio_de_comidas_prop",
                        "j_informacion_y_comunicaciones_prop","d_suministro_de_electricidad_gas_vapor_y_aire_acondicionado_prop","c_industria_manufacturera_prop","b_explotacion_de_minas_y_canteras_prop",
                        "e_suministro_de_agua_evacuacion_de_aguas_residuales_gestion_de_desechos_y_descontaminacion_prop","o_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria_prop",
                        "l_actividades_inmobiliarias_prop")]


# Cálculo de la media y la desviación estándar
means <- sapply(df_model, mean, na.rm = TRUE)
sds <- sapply(df_model, sd, na.rm = TRUE)
mins <- sapply(df_model, min, na.rm = TRUE)
maxs <- sapply(df_model, max, na.rm = TRUE)

# Convertir los resultados en un dataframe
df_summary <- data.frame(
  Variable = names(df_model),
  Mean = means,
  SD = sds,
  Min = mins,
  Max = maxs
)

df_summary$Variable <- c("Brecha salarial",
                         "Proporción Rural 2020",
                         "Proporción de pueblos originarios Ley Chile 2017",
                         "Promedio de años de escolaridad 2017",
                         "Proporción de NNA",
                         "Tasa de matrícula parvularia neta",
                         "Proporción de ocupados/as formales sobre la población comunal",
                         "Proporción de comercio al por mayor y menor, reparación de vehículos automotores y motocicletas",
                         "Proporción de actividades de alojamiento y de servicio de comidas",
                         "Proporción de información y comunicaciones",
                         "Proporción de suministro de electricidad, gas, vapor y aire acondicionado",
                         "Proporción de industria manufacturera",
                         "Proporción de explotación de minas y canteras",
                         "Proporción de suministro de agua, evacuación de aguas residuales, gestión de desechos y descontaminación",
                         "Proporción de administración pública y defensa, planes de seguridad social de afiliación obligatoria",
                         "Proporción de actividades inmobiliarias")

# Utilizar el paquete kable de knitr para convertir el dataframe en una tabla HTML
tab_desc<-kable(df_summary, format = "html", row.names = FALSE, digits = 2) %>%
  kable_styling(full_width = F)

writeLines(tab_desc,"output/tables/descriptivos_regresión.html")

# Variables en el modelo
variables <- c("prop_rural_2020", "prop_poblacion_pueblos_originarios_ley_chile_2017", "promedio_anios_escolaridad25_2017",
               "propnna", "tasa_matricula_parvularia_neta", "prop_ocup", 
               "g_comercio_al_por_mayor_y_al_por_menor_reparacion_de_vehiculos_automotores_y_motocicletas_prop", 
               "i_actividades_de_alojamiento_y_de_servicio_de_comidas_prop", "j_informacion_y_comunicaciones_prop",
               "d_suministro_de_electricidad_gas_vapor_y_aire_acondicionado_prop", "c_industria_manufacturera_prop",
               "b_explotacion_de_minas_y_canteras_prop", "e_suministro_de_agua_evacuacion_de_aguas_residuales_gestion_de_desechos_y_descontaminacion_prop", 
               "o_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria_prop", 
               "l_actividades_inmobiliarias_prop")

xlabs<-c("prop_rural_2020", "prop_pueblos_originarios", "promedio_anios_escolaridad",
         "proporción NNA", "tasa_matricula_parvularia_neta", "prop_ocup", 
         "g_comercio_prop", 
         "i_alojamiento_y_comidas_prop", "j_informacion_y_com_prop",
         "d_electricidad_gas_prop", "c_industria_manufacturera_prop",
         "b_explotacion_de_minas_prop", "e_suministro_de_agua_prop", 
         "o_adm_publica_y_defensa_prop", 
         "l_actividades_inmobiliarias_prop")

# Crear una lista para almacenar los gráficos
plots <- list()

# Bucle for para crear scatterplots
for (i in seq_along(variables)) {
  
  # Subset de los datos para excluir NA's
  df_sub <- na.omit(df_final[,c(variables[i], 'brecha')])
  
  r <- cor(df_sub[[1]], df_sub$brecha)  # Calcula el coeficiente de correlación
  r_label <- paste("r =", round(r, 2))  # Crea una etiqueta para 'r'
  
  plots[[i]] <- ggplot(df_sub, aes_string(x = variables[i], y = 'brecha')) + 
    geom_point(alpha = 0.6, color = 'darkblue') +
    geom_smooth(method = lm, se = FALSE, color = 'red') +
    theme_minimal() +
    labs(x = paste(xlabs[i],r_label),  # Divide las etiquetas en dos líneas
         y = "Brecha salarial")
}

# Crear un grid de gráficos
cor_plot<-gridExtra::grid.arrange(grobs = plots, ncol = 4)

# Guardar mapa
ggsave(filename="output/graphs/cor.jpeg", plot = cor_plot, device = "jpeg", width = 10000, height =7000 , units = "px",
       dpi=800)

saveRDS(df_final, "input/data/proc/datafinal.rds")
