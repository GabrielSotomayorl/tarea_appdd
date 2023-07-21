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
               texreg)

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

###--- Imputación por provincia ---###

df_final <- df_final %>% group_by(provincia) %>% 
  mutate_all(reemplaza_mean) %>% ungroup()

###--- Imputación por región ---###

df_final <- df_final %>% group_by(region) %>% 
  mutate_all(reemplaza_mean) %>% ungroup()

screenreg(list(lm(brecha ~ prop_rural_2020 + prop_poblacion_pueblos_originarios_ley_chile_2017 + 
                  promedio_anios_escolaridad25_2017 + I(Poblacion_0_17_2021/Total_Poblacion_2021 * 
                                                          100) + tasa_matricula_parvularia_neta+ prop_ocup ,data=df_final),
             lm(brecha ~              `G - Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas_prop` + 
                  `I - Actividades de alojamiento y de servicio de comidas_prop` + 
                  `J - Información y comunicaciones_prop` + `D - Suministro de electricidad, gas, vapor y aire acondicionado_prop` + 
                  `C - Industria manufacturera_prop` + `B - Explotación de minas y canteras_prop` + 
                  `E - Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación_prop` + 
                  `O - Administración pública y defensa; planes de seguridad social de afiliación obligatoria_prop` + 
                  `L - Actividades inmobiliarias_prop`,data=df_final),
             lm(brecha ~ prop_rural_2020 + prop_poblacion_pueblos_originarios_ley_chile_2017 + 
                  promedio_anios_escolaridad25_2017 + I(Poblacion_0_17_2021/Total_Poblacion_2021 * 
                                                          100) + tasa_matricula_parvularia_neta+ prop_ocup + 
                  `G - Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas_prop` + 
                  `I - Actividades de alojamiento y de servicio de comidas_prop` + 
                  `J - Información y comunicaciones_prop` + `D - Suministro de electricidad, gas, vapor y aire acondicionado_prop` + 
                  `C - Industria manufacturera_prop` + `B - Explotación de minas y canteras_prop` + 
                  `E - Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación_prop` + 
                  `O - Administración pública y defensa; planes de seguridad social de afiliación obligatoria_prop` + 
                  `L - Actividades inmobiliarias_prop`,data=df_final)),
        custom.coef.names = c("Intercepto","Proporción Rural", "Proporción pueblos orginarios", "Promedio años de escolaridad",
                              "Porcentaje de NNA","Tasa de matricula parvularia","Proporción de ocupados","Prop. Comercio, reparación autos", "Prop. Alojamiento y comidas","Prop. información y comunicaciones", "Prop. Suministro de electricidad, gas, vapor y aire acondicionado", "Prop Manofactura","Prop. Minas y Canteras","Prop. Suministro de agua","Prop. Administración publica y defensa",
                              "Prop. Actividades inmobiliarias"))

reg<-htmlreg(list(lm(brecha ~ prop_rural_2020 + prop_poblacion_pueblos_originarios_ley_chile_2017 + 
                    promedio_anios_escolaridad25_2017 + I(Poblacion_0_17_2021/Total_Poblacion_2021 * 
                                                            100) + tasa_matricula_parvularia_neta+ prop_ocup ,data=df_final),
               lm(brecha ~              `G - Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas_prop` + 
                    `I - Actividades de alojamiento y de servicio de comidas_prop` + 
                    `J - Información y comunicaciones_prop` + `D - Suministro de electricidad, gas, vapor y aire acondicionado_prop` + 
                    `C - Industria manufacturera_prop` + `B - Explotación de minas y canteras_prop` + 
                    `E - Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación_prop` + 
                    `O - Administración pública y defensa; planes de seguridad social de afiliación obligatoria_prop` + 
                    `L - Actividades inmobiliarias_prop`,data=df_final),
               lm(brecha ~ prop_rural_2020 + prop_poblacion_pueblos_originarios_ley_chile_2017 + 
                    promedio_anios_escolaridad25_2017 + I(Poblacion_0_17_2021/Total_Poblacion_2021 * 
                                                            100) + tasa_matricula_parvularia_neta+ prop_ocup + 
                    `G - Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas_prop` + 
                    `I - Actividades de alojamiento y de servicio de comidas_prop` + 
                    `J - Información y comunicaciones_prop` + `D - Suministro de electricidad, gas, vapor y aire acondicionado_prop` + 
                    `C - Industria manufacturera_prop` + `B - Explotación de minas y canteras_prop` + 
                    `E - Suministro de agua; evacuación de aguas residuales, gestión de desechos y descontaminación_prop` + 
                    `O - Administración pública y defensa; planes de seguridad social de afiliación obligatoria_prop` + 
                    `L - Actividades inmobiliarias_prop`,data=df_final)),
          custom.coef.names = c("Intercepto","Proporción Rural", "Proporción pueblos orginarios", "Promedio años de escolaridad",
                                "Porcentaje de NNA","Tasa de matricula parvularia","Proporción de ocupados","Prop. Comercio, reparación autos", "Prop. Alojamiento y comidas","Prop. información y comunicaciones", "Prop. Suministro de electricidad, gas, vapor y aire acondicionado", "Prop Manofactura","Prop. Minas y Canteras","Prop. Suministro de agua","Prop. Administración publica y defensa",
                                "Prop. Actividades inmobiliarias"))

write_lines(reg,"output/tables/regresion.html")
browseURL("output/tables/regresion.html")
