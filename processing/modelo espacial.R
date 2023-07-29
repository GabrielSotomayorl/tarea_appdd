dfi<-select(df_finalm, c(comuna, cod_comuna, brecha ,prop_rural_2020 ,prop_poblacion_pueblos_originarios_ley_chile_2017 , promedio_anios_escolaridad25_2017 , propnna  , tasa_matricula_parvularia_neta, prop_ocup, g_comercio_al_por_mayor_y_al_por_menor_reparacion_de_vehiculos_automotores_y_motocicletas_prop ,                                                 i_actividades_de_alojamiento_y_de_servicio_de_comidas_prop ,                                                 j_informacion_y_comunicaciones_prop , d_suministro_de_electricidad_gas_vapor_y_aire_acondicionado_prop ,  c_industria_manufacturera_prop ,b_explotacion_de_minas_y_canteras_prop ,                                                 e_suministro_de_agua_evacuacion_de_aguas_residuales_gestion_de_desechos_y_descontaminacion_prop ,                                             o_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria_prop ,  l_actividades_inmobiliarias_prop))


datamapa1 <- left_join(mapa_comunas, dfi, by = c("codigo_comunan" = "cod_comuna"))
datamapa1<-datamapa1[!datamapa1$codigo_comunan%in% c(5201,  5104, 10206, 11203),]

install.packages("spdep")
library(spdep)

# Convertir tu objeto sfc a SpatialPolygonsDataFrame
datamapa1_sp <- as(datamapa1$geometry, "Spatial")

# Crear el objeto de vecindad
nb <- poly2nb(datamapa1_sp, queen = TRUE)

listw <- nb2listw(nb, style = "W")



if (!require(spatialreg)) {
  install.packages("spatialreg")
}

# Carga el paquete en tu sesión de R
library(spatialreg)


model <- spatialreg::lagsarlm(
  formula = brecha ~ prop_rural_2020 + prop_poblacion_pueblos_originarios_ley_chile_2017 +
    promedio_anios_escolaridad25_2017 + propnna  + tasa_matricula_parvularia_neta + prop_ocup + 
    g_comercio_al_por_mayor_y_al_por_menor_reparacion_de_vehiculos_automotores_y_motocicletas_prop + 
    i_actividades_de_alojamiento_y_de_servicio_de_comidas_prop + 
    j_informacion_y_comunicaciones_prop + d_suministro_de_electricidad_gas_vapor_y_aire_acondicionado_prop + 
    c_industria_manufacturera_prop + b_explotacion_de_minas_y_canteras_prop + 
    e_suministro_de_agua_evacuacion_de_aguas_residuales_gestion_de_desechos_y_descontaminacion_prop + 
    o_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria_prop + 
    l_actividades_inmobiliarias_prop,
  data = datamapa1,
  listw = listw, zero.policy=TRUE
)
summary(model)
screenreg(model)


y_pred <- predict(model)
y_obs <- datamapa1$brecha
sse <- sum((y_pred - y_obs)^2)
sst <- sum((y_obs - mean(y_obs))^2)
pseudo_r2 <- 1 - (sse/sst)
empty_neighbours <- which(card(nb) == 0)
y_obs_included <- y_obs[-empty_neighbours]

# Luego puedes calcular el pseudo R^2 para las observaciones incluidas.
sse <- sum((y_pred - y_obs_included)^2)
sst <- sum((y_obs_included - mean(y_obs_included))^2)
pseudo_r2 <- 1 - (sse/sst)
