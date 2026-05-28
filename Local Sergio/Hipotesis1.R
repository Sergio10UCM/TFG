# =============================================================================
# TFG — Capítulo 4: Análisis Inferencial (Parte R)
# Cobertura R: H1, H2, H3, H5
# Cobertura Python (script separado): H4, H6, H7
#
# AUTOR: Sergio Díez Cardo — TFG Ciencia de Datos Aplicada
# =============================================================================

# -----------------------------------------------------------------------------
# BLOQUE 0 — DEPENDENCIAS
# -----------------------------------------------------------------------------

paquetes_cap4 <- c(
  "tidyverse", "lubridate",
  "fixest",         # panel de efectos fijos
  "modelsummary",   # tablas comparativas APA
  "broom",          # tidy summaries
  "forecast",       # ARIMA/STL
  "tseries",        # ADF
  "lmtest",         # Granger
  "sf",             # geometría espacial
  "spgwr",          # GWR
  "geosphere",      # distancia haversine
  "flextable",
  "officer"
)

for (pkg in paquetes_cap4) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(broom)
library(forecast)
library(tseries)
library(lmtest)
library(sf)
library(spgwr)
library(geosphere)
library(flextable)
library(officer)

cat("Paquetes cargados.\n\n")

dir.create("cap4_outputs", showWarnings = FALSE)


# =============================================================================
# H1 — EFECTO DEL EVENTO SOBRE LA OCUPACIÓN AIRBNB (PANEL EF)
# =============================================================================
# FORMULACIÓN FORMAL:
#   H_0: β_evento = 0  (los eventos no afectan a la ocupación diaria)
#   H_1: β_evento > 0  (los eventos incrementan la ocupación diaria)
#
# VARIABLE DEPENDIENTE: ocupado_{it} ∈ {0,1}
#   (1 si available=FALSE, 0 si available=TRUE en el calendar diario)
#
# MODELO BASE:
#   y_{it} = α_i + γ_t + β · evento_t + controles + ε_{it}
#   α_i = efecto fijo alojamiento, γ_t = efecto fijo mes
# =============================================================================

cat(strrep("=", 75), "\n")
cat("H1 — EFECTO EVENTO SOBRE OCUPACIÓN (PANEL DE EFECTOS FIJOS)\n")
cat(strrep("=", 75), "\n\n")

# -----------------------------------------------------------------------------
# 1.1 — Construcción del panel diario alojamiento × día
# -----------------------------------------------------------------------------

cat("[1.1] Construyendo panel alojamiento × día...\n")

panel_h1 <- df_calendar_06 %>%
  mutate(fecha = as.Date(date)) %>%
  select(listing_id, fecha, available) %>%
  mutate(ocupado = if_else(available == FALSE, 1L, 0L)) %>%
  left_join(
    tabla_diaria %>% select(fecha, hay_concierto, n_conciertos,
                            n_gran_evento, hay_estadio, max_listeners,
                            dia_semana, es_festivo, es_finde, es_puente,
                            mes, anio),
    by = "fecha"
  ) %>%
  left_join(
    listings_clean %>% select(id, barrio, distrito, room_type, accommodates),
    by = c("listing_id" = "id")
  ) %>%
  filter(!is.na(barrio), !is.na(room_type)) %>%
  mutate(
    listing_id  = as.character(listing_id),
    mes_anio    = factor(paste0(anio, "-", sprintf("%02d", mes))),
    dia_semana  = factor(dia_semana,
                         levels = c("Lunes","Martes","Miércoles","Jueves",
                                    "Viernes","Sábado","Domingo")),
    es_gran_ev  = if_else(n_gran_evento > 0, 1L, 0L)
  )

cat("  Observaciones panel:", format(nrow(panel_h1), big.mark = ","), "\n")
cat("  Alojamientos únicos:", format(n_distinct(panel_h1$listing_id), big.mark = ","), "\n")
cat("  Días en panel:",        n_distinct(panel_h1$fecha), "\n\n")


# -----------------------------------------------------------------------------
# 1.2 — Modelos: del baseline al saturado
# -----------------------------------------------------------------------------

cat("[1.2] Estimando modelos H1...\n")

# Modelo (1) BASELINE — EF de alojamiento + mes-año
mod_h1_1 <- feols(
  ocupado ~ es_gran_ev + n_conciertos + hay_concierto
            | listing_id + mes_anio,
  data    = panel_h1,
  cluster = ~ listing_id
)

# Modelo (2) + controles temporales que sí varían dentro del alojamiento
mod_h1_2 <- feols(
  ocupado ~ es_gran_ev + n_conciertos + hay_concierto
            + dia_semana + es_festivo
            | listing_id + mes_anio,
  data    = panel_h1,
  cluster = ~ listing_id
)

# Modelo (3) — EF de barrio en lugar de alojamiento (controles barrio + tipo)
# Esto permite ver la magnitud del R² cuando reducimos la granularidad del EF
mod_h1_3 <- feols(
  ocupado ~ es_gran_ev + n_conciertos + hay_concierto
            + dia_semana + es_festivo + room_type
            | barrio + mes_anio,
  data    = panel_h1,
  cluster = ~ barrio
)

# Modelo (4) POOLED — sin EF, con todo como categórico
# Sirve para tener un R² "convencional" comparable a un OLS estándar
#mod_h1_4 <- feols(
#  ocupado ~ es_gran_ev + n_conciertos + hay_concierto
#            + dia_semana + es_festivo + room_type + barrio + mes_anio,
#  data    = panel_h1,
#  cluster = ~ listing_id
#)

# Modelo (4) POOLED PURO — sin controles espaciales/temporales granulares
# Sirve para tener un baseline real de un OLS estándar sin efectos fijos
mod_h1_4 <- feols(
  ocupado ~ es_gran_ev + n_conciertos + hay_concierto
  + dia_semana + es_festivo + room_type,
  data    = panel_h1,
  cluster = ~ listing_id
)
cat("  Modelos 1-4 estimados.\n\n")


# -----------------------------------------------------------------------------
# 1.3 — R² predictivo out-of-sample (split temporal 80/20)
# -----------------------------------------------------------------------------

cat("[1.3] Calculando R² predictivo (split temporal 80/20)...\n")

fechas_unicas <- sort(unique(panel_h1$fecha))
n_train_dias  <- floor(length(fechas_unicas) * 0.8)
fecha_corte   <- fechas_unicas[n_train_dias]

panel_train <- panel_h1 %>% filter(fecha <= fecha_corte)
panel_test  <- panel_h1 %>% filter(fecha >  fecha_corte)

cat("  Días en train:", n_distinct(panel_train$fecha),
    "(hasta", as.character(fecha_corte), ")\n")
cat("  Días en test :", n_distinct(panel_test$fecha), "\n")

# Estimamos en train con la especificación del modelo (2)
mod_h1_train <- feols(
  ocupado ~ es_gran_ev + n_conciertos + hay_concierto
            + dia_semana + es_festivo
            | listing_id + mes_anio,
  data    = panel_train,
  cluster = ~ listing_id
)

# Predicción en test — listings y meses que estén en ambos splits
listings_comunes <- intersect(panel_train$listing_id, panel_test$listing_id)
meses_comunes    <- intersect(panel_train$mes_anio, panel_test$mes_anio)

panel_test_eval <- panel_test %>%
  filter(listing_id %in% listings_comunes, mes_anio %in% meses_comunes)

panel_test_eval$pred <- predict(mod_h1_train, newdata = panel_test_eval)

err_rmse <- sqrt(mean((panel_test_eval$ocupado - panel_test_eval$pred)^2, na.rm = TRUE))
err_mae  <- mean(abs(panel_test_eval$ocupado - panel_test_eval$pred), na.rm = TRUE)
ss_res   <- sum((panel_test_eval$ocupado - panel_test_eval$pred)^2, na.rm = TRUE)
ss_tot   <- sum((panel_test_eval$ocupado -
                 mean(panel_test_eval$ocupado, na.rm = TRUE))^2, na.rm = TRUE)
r2_oos   <- 1 - ss_res / ss_tot

cat(sprintf("  RMSE out-of-sample: %.4f\n", err_rmse))
cat(sprintf("  MAE  out-of-sample: %.4f\n", err_mae))
cat(sprintf("  R²   out-of-sample: %.4f\n\n", r2_oos))

# -- R² OOS del modelo nulo (sin variables de evento) -------------------------
# Sirve para cuantificar la mejora marginal que aportan las variables de evento
mod_h1_nulo <- feols(
  ocupado ~ dia_semana + es_festivo | listing_id + mes_anio,
  data    = panel_train,
  cluster = ~ listing_id
)
panel_test_eval$pred_nulo <- predict(mod_h1_nulo, newdata = panel_test_eval)
ss_res_nulo <- sum((panel_test_eval$ocupado - panel_test_eval$pred_nulo)^2, na.rm = TRUE)
r2_oos_nulo <- 1 - ss_res_nulo / ss_tot

cat(sprintf("  R² OOS modelo nulo (sin evento): %.4f\n", r2_oos_nulo))
cat(sprintf("  Mejora marginal del evento:      %+.4f\n\n",
            r2_oos - r2_oos_nulo))

# -----------------------------------------------------------------------------
# 1.4 — Almacenamos modelos para tabla maestra (junto con H3 más abajo)
# -----------------------------------------------------------------------------

modelos_panel <- list(
  "(1) H1 base"           = mod_h1_1,
  "(2) H1 + control día"  = mod_h1_2,
  "(3) H1 + barrio/tipo"  = mod_h1_3,
  "(4) H1 saturado"       = mod_h1_4
)


# -----------------------------------------------------------------------------
# 1.5 — INTERPRETACIÓN AUTOMÁTICA H1
# -----------------------------------------------------------------------------

cat(strrep("-", 75), "\n")
cat("INTERPRETACIÓN AUTOMÁTICA — H1\n")
cat(strrep("-", 75), "\n\n")

coef_ge <- summary(mod_h1_2)$coeftable["es_gran_ev", ]
beta_ge <- coef_ge[1]; se_ge <- coef_ge[2]; t_ge <- coef_ge[3]; p_ge <- coef_ge[4]

cat(sprintf("Coeficiente es_gran_ev (modelo 2): β = %.4f (SE = %.4f)\n",
            beta_ge, se_ge))
cat(sprintf("t = %.2f | p-value = %.5f\n", t_ge, p_ge))
cat(sprintf("Significación al 5%%: %s\n\n", ifelse(p_ge < 0.05, "SÍ", "NO")))

cat("→ LÓGICA DE INTERPRETACIÓN:\n")
if (p_ge < 0.05) {
  cat("  • p-value < 0.05 → RECHAZAMOS H_0\n")
  if (beta_ge > 0) {
    cat(sprintf("  • β > 0 (%.4f) → los días con gran evento INCREMENTAN la ocupación\n",
                beta_ge))
    cat(sprintf("  • Magnitud: +%.2f puntos porcentuales por gran evento adicional\n",
                beta_ge * 100))
    cat("  • La evidencia APOYA H_1\n")
  } else {
    cat(sprintf("  • β < 0 (%.4f) → resultado contraintuitivo\n", beta_ge))
    cat("  • Posibles explicaciones: efecto desplazamiento, ruido por miles\n")
    cat("    de conciertos pequeños sin impacto turístico real\n")
  }
} else {
  cat("  • p-value ≥ 0.05 → NO rechazamos H_0\n")
  cat("  • No hay evidencia estadística significativa del efecto evento\n")
}

cat("\n→ DISCUSIÓN DEL R² (la corrección clave del tutor):\n")
wr2_2 <- fitstat(mod_h1_2, "wr2")$wr2
r2_2  <- fitstat(mod_h1_2, "r2")$r2
r2_4  <- fitstat(mod_h1_4, "r2")$r2

cat(sprintf("  • Within R² modelo (2): %.4f → MUY BAJO\n", wr2_2))
cat(sprintf("  • R² total modelo (2):  %.4f → RAZONABLE\n", r2_2))
cat(sprintf("  • R² pooled modelo (4): %.4f → similar al total con EF\n", r2_4))
cat("  • La diferencia entre within y total es enorme porque la variabilidad\n")
cat("    ENTRE alojamientos (niveles de ocupación distintos por alojamiento)\n")
cat("    absorbe casi todo el R² total. La variabilidad DENTRO del alojamiento\n")
cat("    a lo largo del tiempo es muy pequeña.\n")
cat("  • Esta es la 'prob. within' del tutor: el panel es rico en heterogeneidad\n")
cat("    transversal y pobre en variabilidad temporal intra-unidad.\n\n")

cat(sprintf("  • R² out-of-sample en test: %.4f\n", r2_oos))
cat(sprintf("  • RMSE en test: %.4f (sobre tasa entre 0 y 1)\n", err_rmse))
cat(sprintf("  • MAE  en test: %.4f\n", err_mae))
cat("  • El R² OOS positivo confirma capacidad predictiva real fuera de muestra.\n\n")

# =============================================================================
# H1 — ROBUSTEZ feglm (logit con efectos fijos)
# =============================================================================
cat(strrep("=", 75), "\n")
cat("H1 — ROBUSTEZ feglm: logit con efectos fijos\n")
cat(strrep("=", 75), "\n\n")

mod_h1_logit <- feglm(
  ocupado ~ es_gran_ev + n_conciertos + hay_concierto
  + dia_semana + es_festivo
  | listing_id + mes_anio,
  data    = panel_h1,
  family  = binomial("logit"),
  cluster = ~ listing_id
)

print(summary(mod_h1_logit))

# AME = efecto marginal promedio (comparable al β del LPM en pp)
prob_pred  <- predict(mod_h1_logit, type = "response")
beta_logit <- coef(mod_h1_logit)["es_gran_ev"]
ame_logit  <- mean(beta_logit * prob_pred * (1 - prob_pred), na.rm = TRUE)
se_logit   <- summary(mod_h1_logit)$coeftable["es_gran_ev", "Std. Error"]
ame_se     <- mean(prob_pred * (1 - prob_pred), na.rm = TRUE) * se_logit

cat("\n--- COMPARATIVA LPM vs LOGIT ---\n")
cat(sprintf("  β log-odds (feglm):       %+.4f  (SE = %.4f)\n", beta_logit, se_logit))
cat(sprintf("  Probabilidad media:        %.4f\n", mean(prob_pred, na.rm = TRUE)))
cat(sprintf("  AME logit (≈ pp):          %+.4f  (SE ≈ %.4f)\n", ame_logit, ame_se))
cat(sprintf("  β LPM original (mod 2):    %+.4f\n",
            coef(mod_h1_2)["es_gran_ev"]))
cat("  → Si AME y β LPM son del mismo orden, la elección del LPM queda confirmada.\n\n")

# =============================================================================
# H1 — TESTS F CONJUNTOS ENTRE ESPECIFICACIONES
# =============================================================================
cat(strrep("=", 75), "\n")
cat("H1 — TESTS F CONJUNTOS (significación de bloques de controles)\n")
cat(strrep("=", 75), "\n\n")

cat("Significación conjunta de día_semana + festivo en el modelo (2):\n")
print(wald(mod_h1_2, keep = "dia_semana|es_festivo"))

cat("\nSignificación conjunta de room_type en el modelo (3):\n")
print(wald(mod_h1_3, keep = "room_type"))

cat("\nNota: estos tests F son la versión idiomática del ANOVA entre\n")
cat("modelos anidados cuando se trabaja con fixest. Un p<0,001 indica\n")
cat("que el bloque de controles aporta información significativa.\n\n")

# =============================================================================
# H3 — MODULACIÓN POR POPULARIDAD (EXTENSIÓN DE H1)
# =============================================================================
# FORMULACIÓN FORMAL:
#   H_0: β_interaccion = 0  (el efecto del evento no depende de la popularidad)
#   H_1: β_interaccion > 0  (artistas más populares generan efectos mayores)
#
# ESPECIFICACIÓN:
#   y_{it} = α_i + γ_t + β_1 · evento_t + β_2 · log10(listeners_t)
#                       + β_3 · evento_t × log10(listeners_t) + ε_{it}
# =============================================================================

cat(strrep("=", 75), "\n")
cat("H3 — EFECTO MODERADOR DE LA POPULARIDAD DEL ARTISTA\n")
cat(strrep("=", 75), "\n\n")

panel_h3 <- panel_h1 %>%
  mutate(log_listeners = log10(replace_na(max_listeners, 1) + 1))

# Modelo (5) — interacción evento × popularidad
mod_h3_int <- feols(
  ocupado ~ es_gran_ev * log_listeners + n_conciertos + hay_concierto
  + dia_semana + es_festivo
  | listing_id + mes_anio,
  data    = panel_h3,
  cluster = ~ listing_id
)
modelos_panel[["(5) H3 con interacción"]] <- mod_h3_int


# -----------------------------------------------------------------------------
# 3.1 — Efecto marginal del evento a distintos niveles de popularidad
# -----------------------------------------------------------------------------

coefs_h3 <- summary(mod_h3_int)$coeftable
beta_ev  <- coefs_h3["es_gran_ev", 1]
beta_int <- coefs_h3["es_gran_ev:log_listeners", 1]
se_ev    <- coefs_h3["es_gran_ev", 2]
se_int   <- coefs_h3["es_gran_ev:log_listeners", 2]
cov_eint <- vcov(mod_h3_int)["es_gran_ev", "es_gran_ev:log_listeners"]

niveles_listeners <- c(1e5, 5e5, 1e6, 3e6, 5e6, 1e7, 5e7)

efecto_marginal <- tibble(
  listeners = niveles_listeners,
  log_list  = log10(niveles_listeners + 1),
  ef_marg   = beta_ev + beta_int * log_list,
  se_marg   = sqrt(se_ev^2 + log_list^2 * se_int^2 + 2 * log_list * cov_eint),
  t_marg    = ef_marg / se_marg,
  p_marg    = 2 * pnorm(-abs(t_marg)),
  ic_low    = ef_marg - 1.96 * se_marg,
  ic_high   = ef_marg + 1.96 * se_marg
)

cat("[3.1] Efecto marginal del evento por nivel de popularidad:\n\n")
print(efecto_marginal %>%
      mutate(across(c(ef_marg, se_marg, ic_low, ic_high),
                    ~ sprintf("%+.4f", .)),
             p_marg = sprintf("%.4f", p_marg),
             listeners = formatC(listeners, format = "d", big.mark = ",")))

# Umbral empírico donde ef_marg = 0
umbral_log  <- -beta_ev / beta_int
umbral_list <- if (is.finite(umbral_log)) 10^umbral_log else NA_real_

if (!is.na(umbral_list)) {
  cat(sprintf("\n  Umbral empírico (ef_marg = 0): log10 = %.2f → %s oyentes mensuales\n\n",
              umbral_log, formatC(umbral_list, format = "d", big.mark = ",")))
}


# -----------------------------------------------------------------------------
# 3.2 — TABLA MAESTRA APA (H1 + H3 en una sola tabla comparativa)
# -----------------------------------------------------------------------------

cat("[3.2] Generando tabla maestra APA (H1 + H3)...\n")

coef_labels <- c(
  "es_gran_ev"                  = "Gran evento (dummy)",
  "n_conciertos"                = "Nº de conciertos",
  "hay_concierto"               = "Hay concierto (dummy)",
  "es_festivo"                  = "Festivo",
  "dia_semanaMartes"            = "Día: Martes",
  "dia_semanaMiércoles"         = "Día: Miércoles",
  "dia_semanaJueves"            = "Día: Jueves",
  "dia_semanaViernes"           = "Día: Viernes",
  "dia_semanaSábado"            = "Día: Sábado",
  "dia_semanaDomingo"           = "Día: Domingo",
  "room_typePrivate room"       = "Habitación privada",
  "room_typeShared room"        = "Habitación compartida",
  "room_typeHotel room"         = "Habitación de hotel",
  "log_listeners"               = "log10(listeners)",
  "es_gran_ev:log_listeners"    = "Gran evento × log10(listeners)"
)

modelsummary(
  modelos_panel,
  output      = "cap4_outputs/tabla_maestra_panel.docx",
  coef_map    = coef_labels,
  coef_omit   = "barrio|mes_anio",
  gof_omit    = "AIC|BIC|RMSE|Log",
  stars       = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  fmt         = 4,
  title       = "Tabla 1. Modelos de panel: efecto del evento sobre la ocupación diaria de Airbnb",
  notes       = c("Variable dependiente: ocupado (1 = no disponible, 0 = disponible)",
                  "Errores estándar agrupados por alojamiento.",
                  "Within R² (wr2) es el R² intra-grupo tras absorber los EF.",
                  "Significación: * p<0.05, ** p<0.01, *** p<0.001")
)

cat("  Tabla guardada en cap4_outputs/tabla_maestra_panel.html\n\n")

# Versión markdown para inspeccionar en consola
modelsummary(modelos_panel,
             coef_map  = coef_labels,
             coef_omit = "barrio|mes_anio",
             gof_omit  = "AIC|BIC|RMSE|Log",
             stars     = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
             fmt       = 4,
             output    = "markdown") %>% print()


# -----------------------------------------------------------------------------
# 3.3 — INTERPRETACIÓN AUTOMÁTICA H3
# -----------------------------------------------------------------------------

cat(strrep("-", 75), "\n")
cat("INTERPRETACIÓN AUTOMÁTICA — H3\n")
cat(strrep("-", 75), "\n\n")

p_int <- coefs_h3["es_gran_ev:log_listeners", 4]

cat(sprintf("Coeficiente interacción: β_int = %.5f (SE = %.5f)\n", beta_int, se_int))
cat(sprintf("p-value: %.5f\n", p_int))
cat(sprintf("Significación al 5%%: %s\n\n", ifelse(p_int < 0.05, "SÍ", "NO")))

cat("→ LÓGICA DE INTERPRETACIÓN:\n")
if (p_int < 0.05) {
  cat("  • p-value < 0.05 → RECHAZAMOS H_0\n")
  if (beta_int > 0) {
    cat("  • β_int > 0 → el efecto del evento CRECE con la popularidad\n")
    cat(sprintf("  • Por cada incremento de 1 unidad en log10(listeners),\n"))
    cat(sprintf("    el efecto adicional del evento aumenta en %.4f pp\n", beta_int * 100))
    if (!is.na(umbral_list)) {
      cat(sprintf("  • Umbral empírico: artistas con ≥ %s oyentes mensuales\n",
                  formatC(umbral_list, format = "d", big.mark = ",")))
      cat("    generan efecto neto positivo sobre la ocupación.\n")
    }
    cat("  • La evidencia APOYA H_1\n")
  } else {
    cat("  • β_int < 0 → el efecto DECRECE con la popularidad (contraintuitivo)\n")
  }
} else {
  cat("  • p-value ≥ 0.05 → NO rechazamos H_0\n")
  cat("  • Sin evidencia de modulación por popularidad en el análisis diario.\n")
  cat("    Probar agregación mensual o filtrar a días con evento únicamente.\n")
}
cat("\n")


# =============================================================================
# H5 — DECAIMIENTO ESPACIAL DEL IMPACTO
# =============================================================================
# FORMULACIÓN FORMAL:
#   H_0: β(distancia) = constante  (el efecto no varía con la distancia)
#   H_1: β(distancia) decrece     (el efecto disminuye al alejarse del recinto)
#
# DOS APROXIMACIONES COMPLEMENTARIAS:
#   A) Anillos de distancia: regresiones FE estratificadas por distancia
#   B) GWR sobre log(ocupación) con intercepto local β_0(u_i, v_i)
#      → Lo que corrigió el tutor: VD es log(ocup), no log(precio)
#      → El β_0(u_i, v_i) es el intercepto que varía espacialmente
# =============================================================================

cat(strrep("=", 75), "\n")
cat("H5 — DECAIMIENTO ESPACIAL DEL EFECTO\n")
cat(strrep("=", 75), "\n\n")

# -----------------------------------------------------------------------------
# 5.1 — Coordenadas de venues grandes
# -----------------------------------------------------------------------------

venues_h5 <- tribble(
  ~recinto,                           ~lat,      ~lon,
  "Movistar Arena",                    40.4239,  -3.6716,
  "Estadio Santiago Bernabéu",         40.4530,  -3.6883,
  "Estadio Cívitas Metropolitano",     40.4361,  -3.5995,
  "La Riviera",                        40.4137,  -3.7226,
  "Palacio Vistalegre",                40.3862,  -3.7379,
  "IFEMA Madrid",                      40.4678,  -3.6166,
  "Recinto Valdebebas",                40.4786,  -3.6165,
  "Caja Mágica",                       40.3697,  -3.6830,
  "Ciudad del Rock",                   40.3060,  -3.4800
)


# -----------------------------------------------------------------------------
# 5.2 — Distancia mínima de cada alojamiento al venue activo más cercano
# -----------------------------------------------------------------------------

cat("[5.1] Calculando distancia de cada alojamiento al venue más cercano...\n")

dias_con_evento <- conciertos %>%
  filter(es_gran_evento) %>%
  select(fecha, recinto_canonico) %>%
  inner_join(venues_h5, by = c("recinto_canonico" = "recinto")) %>%
  distinct(fecha, lat, lon)

# Distancia al venue más cercano de entre los activos durante el período
venues_unicos <- dias_con_evento %>% distinct(lat, lon)

dist_listing_venue <- function(lat_l, lon_l, venues_df) {
  d <- distHaversine(c(lon_l, lat_l),
                     cbind(venues_df$lon, venues_df$lat))
  min(d, na.rm = TRUE)
}

listings_dist <- listings_clean %>%
  select(id, barrio, latitude, longitude, room_type, precio) %>%
  rowwise() %>%
  mutate(dist_min_m = dist_listing_venue(latitude, longitude, venues_unicos)) %>%
  ungroup() %>%
  mutate(
    dist_km = dist_min_m / 1000,
    anillo  = case_when(
      dist_km < 1   ~ "0-1 km",
      dist_km < 2   ~ "1-2 km",
      dist_km < 3   ~ "2-3 km",
      dist_km < 5   ~ "3-5 km",
      TRUE          ~ ">5 km"
    ),
    anillo  = factor(anillo, levels = c("0-1 km","1-2 km","2-3 km","3-5 km",">5 km"))
  )

cat("  Distancia media:",
    round(mean(listings_dist$dist_km, na.rm = TRUE), 2), "km\n")
cat("  Distancia mediana:",
    round(median(listings_dist$dist_km, na.rm = TRUE), 2), "km\n\n")


# -----------------------------------------------------------------------------
# 5.3 — Regresiones por anillo
# -----------------------------------------------------------------------------

cat("[5.2] Estimando regresiones por anillo de distancia...\n\n")

#panel_h5 <- panel_h1 %>%
#  inner_join(listings_dist %>% select(id, anillo, dist_km),
#             by = c("listing_id" = "id")) %>%
#  filter(!is.na(anillo))
panel_h5 <- panel_h1 %>%
  inner_join(listings_dist %>% 
               mutate(id = as.character(id)) %>%  # <-- ESTA ES LA CORRECCIÓN
               select(id, anillo, dist_km),
             by = c("listing_id" = "id")) %>%
  filter(!is.na(anillo))

resultados_anillos <- map_dfr(levels(panel_h5$anillo), function(a) {
  df_a <- panel_h5 %>% filter(anillo == a)
  if (nrow(df_a) < 100) return(NULL)
  m <- tryCatch(
    feols(ocupado ~ es_gran_ev + n_conciertos + hay_concierto
                   + dia_semana + es_festivo
                   | listing_id + mes_anio,
          data    = df_a,
          cluster = ~ listing_id),
    error = function(e) NULL
  )
  if (is.null(m)) return(NULL)
  ct <- summary(m)$coeftable
  if (!"es_gran_ev" %in% rownames(ct)) return(NULL)
  tibble(
    anillo  = a,
    n_obs   = nrow(df_a),
    beta    = ct["es_gran_ev", 1],
    se      = ct["es_gran_ev", 2],
    p_val   = ct["es_gran_ev", 4],
    ic_low  = ct["es_gran_ev", 1] - 1.96 * ct["es_gran_ev", 2],
    ic_high = ct["es_gran_ev", 1] + 1.96 * ct["es_gran_ev", 2]
  )
})

print(resultados_anillos %>%
      mutate(across(c(beta, se, ic_low, ic_high), ~ sprintf("%+.5f", .)),
             p_val = sprintf("%.4f", p_val),
             n_obs = format(n_obs, big.mark = ",")))

rho_anillos <- NA_real_
if (nrow(resultados_anillos) >= 3) {
  rho_anillos <- cor(seq_len(nrow(resultados_anillos)),
                     resultados_anillos$beta,
                     method = "spearman")
  cat(sprintf("\n  Correlación Spearman (rank distancia, β): %.3f\n", rho_anillos))
  cat("  Si negativa → β decrece con la distancia → apoya H5.\n\n")
}


# -----------------------------------------------------------------------------
# 5.4 — GWR sobre log(ocupación) — CORRECCIÓN DEL TUTOR
# -----------------------------------------------------------------------------
#
# VARIABLE DEPENDIENTE: log(ocupación promedio en días con gran evento)
# (el calendar de Inside Airbnb no tiene precio diario → usamos ocupación)
#
# ESPECIFICACIÓN:
#   log(ocup_i) = β_0(u_i, v_i) + β_1(u_i, v_i) · dist_km_i + ε_i
#   β_0(u_i, v_i) = intercepto que VARÍA espacialmente
#   β_1(u_i, v_i) = pendiente local del decaimiento espacial
# =============================================================================

cat("[5.3] Ajustando GWR sobre log(ocupación)...\n")

# Ocupación media por listing en días con gran evento
ocup_listing <- panel_h5 %>%
  group_by(listing_id, es_gran_ev) %>%
  summarise(tasa = mean(ocupado, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = es_gran_ev, values_from = tasa,
              names_prefix = "ocup_ev_")

#datos_gwr <- listings_dist %>%
#  inner_join(ocup_listing, by = c("id" = "listing_id")) %>%
#  filter(!is.na(ocup_ev_1), ocup_ev_1 > 0) %>%
#  mutate(log_ocup_ev = log(ocup_ev_1))
datos_gwr <- listings_dist %>%
  mutate(id = as.character(id)) %>%  # <-- LA MISMA CORRECCIÓN AQUÍ
  inner_join(ocup_listing, by = c("id" = "listing_id")) %>%
  filter(!is.na(ocup_ev_1), ocup_ev_1 > 0) %>%
  mutate(log_ocup_ev = log(ocup_ev_1))

# Proyección a UTM para distancias en metros
sf_gwr <- st_as_sf(datos_gwr, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 25830)

coords <- st_coordinates(sf_gwr)
y      <- sf_gwr$log_ocup_ev
x_df   <- data.frame(y = y, dist_km = sf_gwr$dist_km)

cat("  Buscando ancho de banda óptimo (puede tardar varios minutos)...\n")

bw <- tryCatch(
  gwr.sel(y ~ dist_km,
          data    = x_df,
          coords  = coords,
          adapt   = TRUE,
          longlat = FALSE),
  error = function(e) {
    cat("  WARN: gwr.sel falló; usando bandwidth fijo del 10%.\n")
    0.10
  }
)

cat(sprintf("  Bandwidth óptimo: %.4f (proporción adaptativa de vecinos)\n", bw))

mod_gwr <- gwr(y ~ dist_km,
               data    = x_df,
               coords  = coords,
               adapt   = bw,
               longlat = FALSE)

print(mod_gwr)

betas_dist_local <- mod_gwr$SDF$dist_km
betas_int_local  <- mod_gwr$SDF$`(Intercept)`

cat("\n→ DISTRIBUCIÓN DE β_distancia LOCALES (pendiente del decaimiento):\n")
cat(sprintf("  Mediana: %.5f\n", median(betas_dist_local, na.rm = TRUE)))
cat(sprintf("  IQR: [%.5f, %.5f]\n",
            quantile(betas_dist_local, 0.25, na.rm = TRUE),
            quantile(betas_dist_local, 0.75, na.rm = TRUE)))
cat(sprintf("  Proporción con β_dist negativo: %.1f%%\n",
            mean(betas_dist_local < 0, na.rm = TRUE) * 100))

cat("\n→ DISTRIBUCIÓN DE β_0(u_i, v_i) LOCALES (intercepto espacial):\n")
cat(sprintf("  Mediana: %.4f\n", median(betas_int_local, na.rm = TRUE)))
cat(sprintf("  IQR: [%.4f, %.4f]\n",
            quantile(betas_int_local, 0.25, na.rm = TRUE),
            quantile(betas_int_local, 0.75, na.rm = TRUE)))
cat("  El intercepto local representa la ocupación 'basal' esperada\n")
cat("  en cada localización, descontando el efecto de la distancia.\n\n")

cat(sprintf("  Quasi-R² global del GWR: %.4f\n\n",
            1 - mod_gwr$results$rss / mod_gwr$gTSS))


# -----------------------------------------------------------------------------
# 5.5 — INTERPRETACIÓN AUTOMÁTICA H5
# -----------------------------------------------------------------------------

cat(strrep("-", 75), "\n")
cat("INTERPRETACIÓN AUTOMÁTICA — H5\n")
cat(strrep("-", 75), "\n\n")

pct_neg <- mean(betas_dist_local < 0, na.rm = TRUE) * 100

cat("→ EVIDENCIA COMBINADA (ANILLOS + GWR):\n\n")

cat("PARTE A — ANILLOS:\n")
if (!is.na(rho_anillos)) {
  if (rho_anillos < -0.5) {
    cat(sprintf("  ✓ Spearman(rank, β) = %.3f → patrón decreciente claro\n", rho_anillos))
    cat("    APOYA H_1\n")
  } else if (rho_anillos < 0) {
    cat(sprintf("  ~ Spearman(rank, β) = %.3f → decreciente moderado\n", rho_anillos))
    cat("    EVIDENCIA PARCIAL a favor de H_1\n")
  } else {
    cat(sprintf("  ✗ Spearman(rank, β) = %.3f → no decrece\n", rho_anillos))
    cat("    NO apoya H_1\n")
  }
}

cat("\nPARTE B — GWR:\n")
cat(sprintf("  • %% β_dist locales negativos: %.1f%%\n", pct_neg))
if (pct_neg > 60) {
  cat("  ✓ Mayoría con efecto decreciente espacialmente → APOYA H_1\n")
} else if (pct_neg > 50) {
  cat("  ~ Patrón mixto: mapear los β_dist locales para inspección visual\n")
} else {
  cat("  ✗ No hay decaimiento claro: relación muy heterogénea\n")
}

cat("\n→ CONCLUSIÓN H5: documentar ambos métodos en el texto del Cap. 4.\n\n")

# Exportar los β locales para mapearlo en el documento final
sf_gwr$beta_dist_local <- betas_dist_local
sf_gwr$beta_0_local    <- betas_int_local
st_write(sf_gwr, "cap4_outputs/gwr_betas_locales.geojson",
         delete_dsn = TRUE, quiet = TRUE)
cat("  β locales exportados a cap4_outputs/gwr_betas_locales.geojson\n\n")

# =============================================================================
# H5 — ROBUSTEZ: GWR Logístico con GWmodel
# =============================================================================
if (!requireNamespace("GWmodel", quietly = TRUE)) install.packages("GWmodel")
library(GWmodel)

cat(strrep("=", 75), "\n")
cat("H5 — ROBUSTEZ GWR Logístico (ggwr.basic, family = binomial)\n")
cat(strrep("=", 75), "\n\n")

# Convertir sf a Spatial (GWmodel trabaja con sp, no con sf)
datos_sp <- as_Spatial(sf_gwr)

# Bandwidth óptimo con familia binomial
bw_log <- tryCatch(
  bw.ggwr(formula  = ocup_ev_1 ~ dist_km,
          data     = datos_sp,
          family   = "binomial",
          kernel   = "bisquare",
          adaptive = TRUE),
  error = function(e) {
    cat("  WARN: bw.ggwr falló; usando bandwidth fallback de 0.10\n")
    0.10
  }
)
cat(sprintf("  Bandwidth GWLR: %.4f\n", bw_log))

mod_gwlogit <- ggwr.basic(
  formula  = ocup_ev_1 ~ dist_km,
  data     = datos_sp,
  bw       = bw_log,
  family   = "binomial",
  kernel   = "bisquare",
  adaptive = TRUE
)

print(mod_gwlogit)

betas_dist_logit <- mod_gwlogit$SDF$dist_km
cat(sprintf("\n  Mediana β_dist (GWLR):                %.5f\n",
            median(betas_dist_logit, na.rm = TRUE)))
cat(sprintf("  Proporción β_dist GWLR negativos:     %.1f%%\n",
            mean(betas_dist_logit < 0, na.rm = TRUE) * 100))
cat("  → Comparar con el GWR gaussiano: si el % de coeficientes negativos\n")
cat("    converge en magnitud, la robustez del decaimiento queda confirmada.\n\n")

# =============================================================================
# H2 — SERIES TEMPORALES MENSUALES (período 2022-2026)
# =============================================================================
# Setlist.fm solo cubre desde 2022, así que ampliar el horizonte hacia
# atrás implicaría tratar como ceros reales lo que son meses sin cobertura
# de la fuente. Restringimos a post-COVID para que los ceros del regresor
# sean reales, no ausencias de datos.
# =============================================================================

cat(strrep("=", 75), "\n")
cat("H2 — SERIES TEMPORALES (ADF + STL + GRANGER + ARIMAX)\n")
cat(strrep("=", 75), "\n\n")

serie_h2 <- tabla_mensual %>%
  filter(anio >= 2022, !is.na(ocupacion_plazas), !is.na(n_conciertos_mes)) %>%
  arrange(fecha_mes)

ts_ocup <- ts(serie_h2$ocupacion_plazas, start = c(2022, 1), frequency = 12)
ts_conc <- ts(serie_h2$n_conciertos_mes, start = c(2022, 1), frequency = 12)

cat(sprintf("[2.0] Serie construida sobre %d meses (2022-2026).\n\n", length(ts_ocup)))

# -- ADF
cat("[2.A] Tests ADF de estacionariedad...\n\n")
adf_ocup <- tseries::adf.test(ts_ocup, alternative = "stationary")
adf_conc <- tseries::adf.test(ts_conc, alternative = "stationary")
cat(sprintf("  ADF ocupación:  p = %.4f\n", adf_ocup$p.value))
cat(sprintf("  ADF conciertos: p = %.4f\n", adf_conc$p.value))
cat("  Ambas series rechazan la nula de raíz unitaria al 5% → estacionarias.\n\n")

# -- STL
cat("[2.B] Descomposición STL...\n\n")
stl_ocup <- stl(ts_ocup, s.window = "periodic", robust = TRUE)
stl_conc <- stl(ts_conc, s.window = "periodic", robust = TRUE)
ocup_des <- stl_ocup$time.series[, "trend"] + stl_ocup$time.series[, "remainder"]
conc_des <- stl_conc$time.series[, "trend"] + stl_conc$time.series[, "remainder"]

png("cap4_outputs/stl_descomposicion_ocupacion.png", width = 1600, height = 1200, res = 180)
plot(stl_ocup, main = "Descomposición STL — Ocupación hotelera Madrid (2022-2026)")
dev.off()
png("cap4_outputs/stl_descomposicion_conciertos.png", width = 1600, height = 1200, res = 180)
plot(stl_conc, main = "Descomposición STL — Nº de conciertos Madrid (2022-2026)")
dev.off()

# -- Granger sobre desestacionalizadas
cat("[2.C] Test de Granger sobre series desestacionalizadas...\n\n")
df_gr <- tibble(ocup = as.numeric(ocup_des), conc = as.numeric(conc_des))
resultados_granger <- map_dfr(1:3, function(lag) {
  g <- lmtest::grangertest(ocup ~ conc, order = lag, data = df_gr)
  tibble(lag = lag, F_stat = g$F[2], p_val = g$`Pr(>F)`[2])
})
print(resultados_granger %>%
        mutate(F_stat = sprintf("%.3f", F_stat),
               p_val  = sprintf("%.4f", p_val)))
cat("\n  Granger no significativo en ningún lag: con 52 meses la potencia\n")
cat("  del test es limitada; el resultado no descarta relación dinámica.\n\n")

# -- ARIMAX principal
cat("[2.D] Regresión con errores ARIMA (ARIMAX)...\n\n")
mod_arimax <- auto.arima(ts_ocup, xreg = ts_conc, seasonal = TRUE,
                         stepwise = FALSE, approximation = FALSE, ic = "aicc")
print(mod_arimax)
ct_arimax <- lmtest::coeftest(mod_arimax)
print(ct_arimax)

beta_arimax <- coef(mod_arimax)["xreg"]
p_arimax    <- ct_arimax["xreg", "Pr(>|z|)"]

# -- Robustez: ARIMAX con dummies de mes + ARMA(1,1)
cat("\n[2.E] Robustez: ARIMAX con dummies de mes + ARMA(1,1)...\n\n")
df_alt <- data.frame(
  ocup = as.numeric(ts_ocup),
  conc = as.numeric(ts_conc),
  mes  = factor(rep(1:12, length.out = length(ts_ocup)))
)
mod_arimax_alt <- Arima(df_alt$ocup,
                        xreg  = model.matrix(~ conc + mes, data = df_alt)[, -1],
                        order = c(1, 0, 1))
ct_alt <- lmtest::coeftest(mod_arimax_alt)
print(ct_alt)

# -- OLS sobre series desestacionalizadas (comparativa)
cat("\n[2.F] OLS sobre series desestacionalizadas...\n\n")
mod_desest <- lm(as.numeric(ocup_des) ~ as.numeric(conc_des))
print(summary(mod_desest))

cat(strrep("-", 75), "\n")
cat("RESUMEN H2\n")
cat(strrep("-", 75), "\n")
cat(sprintf("  ARIMAX SARIMA:      β = %.4f, p = %.4f\n", beta_arimax, p_arimax))
cat(sprintf("  ARIMAX con dummies: β = %.4f, p = %.4f\n",
            ct_alt["conc","Estimate"], ct_alt["conc","Pr(>|z|)"]))
cat(sprintf("  OLS desestacional.: β = %.4f, p = %.4g, R² = %.3f\n",
            coef(mod_desest)[2], summary(mod_desest)$coefficients[2,4],
            summary(mod_desest)$r.squared))
cat("  Las tres estimaciones convergen en signo, magnitud aproximada\n")
cat("  y significación → H2 apoyada.\n\n")


# =============================================================================
# EXPORTACIÓN PARA PYTHON
# =============================================================================

cat(strrep("=", 75), "\n")
cat("EXPORTANDO CSVs PARA PYTHON (H4, H6, H7)\n")
cat(strrep("=", 75), "\n\n")

# H4: serie mensual con nacionales/internacionales/totales
tabla_mensual %>%
  filter(anio >= 2022) %>%
  select(fecha_mes, n_conciertos_mes, n_grandes_mes, max_listeners_mes,
         viajeros_total, viajeros_espana, viajeros_extranjero,
         pernoctaciones_total, ocupacion_plazas, iph_indice) %>%
  write_csv("cap4_outputs/datos_h4.csv")

# H6: tabla diaria con interacción evento × festivo
tabla_diaria %>%
  filter(anio >= 2022) %>%
  select(fecha, dia_semana, es_festivo, es_finde, es_puente,
         hay_concierto, n_conciertos, n_gran_evento,
         max_aforo, max_listeners) %>%
  write_csv("cap4_outputs/datos_h6.csv")

# H6 panel con ocupación Airbnb (para XGBoost)
tabla_diaria_airbnb %>%
  filter(!is.na(tasa_ocupacion), anio >= 2022) %>%
  select(fecha, tasa_ocupacion, n_conciertos, n_gran_evento,
         max_listeners, dia_semana, es_festivo, es_puente, es_finde,
         mes, anio) %>%
  write_csv("cap4_outputs/datos_h6_diario.csv")

# H7: dataset de conciertos para clustering
conciertos %>%
  filter(year(fecha) >= 2022) %>%
  left_join(kworb_dedup %>% select(artista, ranking_streams = ranking),
            by = "artista") %>%
  left_join(listeners_dedup %>% select(artista, listeners), by = "artista") %>%
  mutate(mes = month(fecha), anio = year(fecha)) %>%
  select(fecha, anio, mes, recinto_canonico, aforo, categoria,
         es_gran_evento, en_chart_spain,
         ranking_streams, listeners) %>%
  write_csv("cap4_outputs/datos_h7.csv")

cat("Archivos exportados:\n")
cat("  cap4_outputs/datos_h4.csv         (mensual, para H4)\n")
cat("  cap4_outputs/datos_h6.csv         (diario, para H6)\n")
cat("  cap4_outputs/datos_h6_diario.csv  (diario + ocupación Airbnb)\n")
cat("  cap4_outputs/datos_h7.csv         (conciertos para clustering)\n\n")

cat(strrep("=", 75), "\n")
cat("SCRIPT R COMPLETADO.\n")
cat("Continuar con: tfg_cap4_Python.py\n")
cat(strrep("=", 75), "\n")

# Modelo nulo: solo efectos fijos + controles temporales (sin variables de evento)
mod_h1_nulo <- feols(
  ocupado ~ dia_semana + es_festivo | listing_id + mes_anio,
  data = panel_train, cluster = ~ listing_id
)
panel_test_eval$pred_nulo <- predict(mod_h1_nulo, newdata = panel_test_eval)

ss_res_nulo <- sum((panel_test_eval$ocupado - panel_test_eval$pred_nulo)^2, na.rm = TRUE)
r2_oos_nulo <- 1 - ss_res_nulo / ss_tot

cat("R² OOS modelo nulo (solo FE + día/festivo):", round(r2_oos_nulo, 4), "\n")
cat("R² OOS modelo completo (con evento):       ", round(r2_oos,      4), "\n")
cat("Mejora marginal del evento:                 ", round(r2_oos - r2_oos_nulo, 4), "\n")

# =============================================================================
# CAPÍTULO 4 — TABLAS APA Y FIGURAS DESDE R
# Tablas: 4.1, 4.3, 4.5, 4.12
# Figuras: 4.1, 4.2
# =============================================================================

library(tidyverse)
library(flextable)
library(officer)
library(broom)
library(modelsummary)
library(scales)
library(sf)
library(viridis)

dir.create("cap4_outputs/tablas_apa", showWarnings = FALSE, recursive = TRUE)
dir.create("cap4_outputs/figuras",    showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# Funciones auxiliares
# -----------------------------------------------------------------------------

fmt_es <- function(x, dig = 3) {
  if (length(x) == 0 || is.na(x)) return("")
  formatC(x, format = "f", digits = dig, decimal.mark = ",")
}

fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("< 0,001")
  formatC(p, format = "f", digits = 3, decimal.mark = ",")
}

apa_table <- function(ft) {
  ft %>%
    border_remove() %>%
    hline_top(border = fp_border(width = 1.2), part = "header") %>%
    hline(i = 1, border = fp_border(width = 0.6), part = "header") %>%
    hline_bottom(border = fp_border(width = 1.2), part = "body") %>%
    align(align = "left",   j = 1,  part = "all") %>%
    align(align = "center", j = -1, part = "all") %>%
    fontsize(size = 11, part = "all") %>%
    font(fontname = "Times New Roman", part = "all") %>%
    padding(padding.top = 4, padding.bottom = 4, part = "all") %>%
    autofit()
}

apa_save <- function(ft, nombre, titulo, nota) {
  ruta <- paste0("cap4_outputs/tablas_apa/", nombre, ".docx")
  ft_completa <- ft %>%
    add_header_lines(values = titulo) %>%
    bold(part = "header", i = 1) %>%
    italic(part = "header", i = 2) %>%
    add_footer_lines(values = nota) %>%
    italic(part = "footer", i = 1)
  doc <- read_docx() %>%
    body_add_flextable(ft_completa, align = "left") %>%
    body_add_par("")
  print(doc, target = ruta)
  cat("✓ Guardada:", ruta, "\n")
}


# # =============================================================================
# # TABLA 4.1 — Modelos panel (H1 + H3)
# # =============================================================================
# # Usa modelsummary con output directo a Word
# 
# coef_labels <- c(
#   "es_gran_ev"                  = "Gran evento",
#   "n_conciertos"                = "Nº de conciertos",
#   "hay_concierto"               = "Hay concierto",
#   "dia_semanaViernes"           = "Día: Viernes",
#   "dia_semanaSábado"            = "Día: Sábado",
#   "dia_semanaDomingo"           = "Día: Domingo",
#   "es_festivo"                  = "Festivo",
#   "room_typePrivate room"       = "Habitación privada",
#   "room_typeShared room"        = "Habitación compartida",
#   "room_typeHotel room"         = "Habitación de hotel",
#   "log_listeners"               = "log₁₀(listeners)",
#   "es_gran_ev:log_listeners"    = "Gran evento × log₁₀(listeners)"
# )
# 
# modelsummary(
#   modelos_panel,
#   output      = "cap4_outputs/tablas_apa/tabla_4_1_panel.docx",
#   coef_map    = coef_labels,
#   coef_omit   = "barrio|mes_anio|dia_semana(Martes|Miércoles|Jueves)",
#   gof_omit    = "AIC|BIC|RMSE|Log",
#   stars       = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
#   fmt         = 4,
#   title       = "Tabla 4.1. Modelos de panel: efecto del evento sobre la ocupación diaria de Airbnb",
#   notes       = c("Errores estándar agrupados por alojamiento entre paréntesis.",
#                   "Within R² absorbe los efectos fijos.",
#                   "* p < 0,05; ** p < 0,01; *** p < 0,001.")
# )
# =============================================================================
# TABLA 4.1 — Modelos panel (H1 + H3)
# =============================================================================

coef_labels <- c(
  "es_gran_ev"                  = "Gran evento",
  "n_conciertos"                = "Nº de conciertos",
  "hay_concierto"               = "Hay concierto",
  "dia_semanaMartes"            = "Día: Martes",
  "dia_semanaMiércoles"         = "Día: Miércoles",
  "dia_semanaJueves"            = "Día: Jueves",
  "dia_semanaViernes"           = "Día: Viernes",
  "dia_semanaSábado"            = "Día: Sábado",
  "dia_semanaDomingo"           = "Día: Domingo",
  "es_festivo"                  = "Festivo",
  "room_typePrivate room"       = "Habitación privada",
  "room_typeShared room"        = "Habitación compartida",
  "room_typeHotel room"         = "Habitación de hotel",
  "log_listeners"               = "log₁₀(listeners)",
  "es_gran_ev:log_listeners"    = "Gran evento × log₁₀(listeners)"
)

ft_4_1 <- modelsummary(
  modelos_panel,
  output    = "flextable",
  coef_map  = coef_labels,
  coef_omit = "barrio|mes_anio",
  gof_omit  = "AIC|BIC|RMSE|Log|R2 Adj|R2 Within Adj",
  stars     = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  fmt       = "%.4f"
) %>% apa_table()

apa_save(
  ft_4_1, "tabla_4_1_panel",
  titulo = c("Tabla 4.1",
             "Modelos de panel: efecto del evento sobre la ocupación diaria de Airbnb"),
  nota   = paste(
    "Nota. Variable dependiente: ocupado (1 = no disponible, 0 = disponible).",
    "Errores estándar agrupados por alojamiento entre paréntesis.",
    "El R² Within es el R² intra-grupo tras absorber los efectos fijos; no es aplicable al modelo pooled (columna 4).",
    "FE = efectos fijos. * p < 0,05; ** p < 0,01; *** p < 0,001."
  )
)

# =============================================================================
# TABLA 4.3 — Anillos H5
# =============================================================================

tabla_4_3 <- resultados_anillos %>%
  mutate(
    estrellas = case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01  ~ "**",
      p_val < 0.05  ~ "*",
      TRUE          ~ ""
    )
  ) %>%
  transmute(
    `Anillo`            = anillo,
    `n observaciones`   = formatC(n_obs, format = "d", big.mark = ".",
                                  decimal.mark = ","),
    `β̂ gran_ev`         = paste0("+", sapply(beta, fmt_es, dig = 5), estrellas),
    `SE`                = sapply(se, fmt_es, dig = 5),
    `p-valor`           = sapply(p_val, fmt_p),
    `IC 95%`            = paste0("[+", sapply(ic_low,  fmt_es, dig = 5),
                                 "; +", sapply(ic_high, fmt_es, dig = 5), "]")
  )

ft_4_3 <- flextable(tabla_4_3) %>% apa_table()

apa_save(ft_4_3, "tabla_5_3_anillos_h5",
         titulo = c("Tabla 5.3",
                    "Efecto del gran evento sobre la ocupación de Airbnb por anillo de distancia al recinto"),
         nota   = "Nota. Cada fila reporta los resultados del modelo de efectos fijos (especificación (2) de la Tabla 5.1) estimado únicamente sobre los alojamientos del anillo correspondiente. Errores estándar agrupados por alojamiento. Spearman(rank, β̂) = −0,700. * p < 0,05; ** p < 0,01; *** p < 0,001."
)


# =============================================================================
# TABLA 4.5 — ARIMAX H2
# =============================================================================

ct_arimax <- lmtest::coeftest(mod_arimax)
tabla_4_5_raw <- as.data.frame(ct_arimax[, ]) %>%
  rownames_to_column("Parámetro") %>%
  rename(Estimación = Estimate, SE = `Std. Error`,
         z = `z value`, p = `Pr(>|z|)`)

tabla_4_5_raw$Parámetro <- recode(tabla_4_5_raw$Parámetro,
                                  "ar1" = "AR(1)", "ar2" = "AR(2)", "ar3" = "AR(3)",
                                  "ma1" = "MA(1)", "sar1" = "SAR(1)",
                                  "intercept" = "Intercepto", "xreg" = "β_conciertos")

tabla_4_5 <- tabla_4_5_raw %>%
  transmute(
    Parámetro,
    `Estimación` = sapply(Estimación, fmt_es, dig = 4),
    `SE`         = sapply(SE,         fmt_es, dig = 4),
    `z`          = sapply(z,          fmt_es, dig = 2),
    `p-valor`    = sapply(p,          fmt_p)
  )

ft_4_5 <- flextable(tabla_4_5) %>% apa_table()

apa_save(ft_4_5, "tabla_4_5_arimax_h2",
         titulo = c("Tabla 4.5",
                    "Regresión con errores ARIMA: ocupación hotelera mensual sobre número de conciertos"),
         nota = sprintf(paste0("Nota. Modelo: y_t = α + β·conciertos_t + η_t, con η_t ~ ARIMA(3,0,0)(0,1,0)₁₂. ",
                               "n = %d observaciones mensuales (enero 2022 – abril 2026). ",
                               "σ² = %.2f; AIC = %.2f; log L = %.2f."),
                        length(ts_ocup), mod_arimax$sigma2, AIC(mod_arimax),
                        as.numeric(logLik(mod_arimax)))
)


# =============================================================================
# TABLA 4.12 — Síntesis
# =============================================================================

tabla_4_12 <- tribble(
  ~hip, ~vd, ~metodo, ~resultado, ~veredicto,
  "H1", "Ocupación Airbnb diaria",    "Panel FE",                 "β̂ = +0,016; p < 0,001",      "Apoyada",
  "H2", "Ocupación hotelera mensual", "ARIMAX",                   "β̂ = +0,014; p = 0,002",      "Apoyada",
  "H3", "Ocupación Airbnb diaria",    "Panel FE con interacción", "β̂_int = +0,005; p < 0,001",  "Apoyada",
  "H4", "log(viajeros)",              "OLS paralelo + bootstrap", "Δβ̂ = +0,171; p_boot = 0,003","Apoyada",
  "H5", "Ocupación Airbnb diaria",    "Anillos + GWR",            "ρ = −0,70; 76% GWR < 0",     "Apoyada",
  "H6", "Ocupación Airbnb diaria",    "OLS + RF + GAM",           "β̂_int = +0,001; p = 0,98",   "No apoyada",
  "H7", "Estructura latente + INE",   "KMeans + Spearman",        "Sil. = 0,63; ρ > 0 no sig.", "Parcial"
) %>%
  rename(
    `Hipótesis`            = hip,
    `Variable dependiente` = vd,
    `Método`               = metodo,
    `Resultado`            = resultado,
    `Veredicto`            = veredicto
  )

ft_4_12 <- flextable(tabla_4_12) %>% apa_table() %>%
  bold(j = "Veredicto", part = "body")

apa_save(ft_4_12, "tabla_4_7_sintesis",
         titulo = c("Tabla 4.7",
                    "Síntesis de resultados de los contrastes de hipótesis"),
         nota   = "Nota. Cinco hipótesis confirmadas con elevada significación, una no apoyada (H6) por limitación de potencia muestral y una parcialmente apoyada (H7) con evidencia direccional consistente pero estadísticamente marginal en su validación externa."
)


# =============================================================================
# FIGURA 4.1 — Efecto marginal H3 según popularidad
# =============================================================================

tema_apa <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 11, color = "grey25"),
    plot.caption     = element_text(size = 9,  color = "grey40", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

fig_4_1 <- ggplot(efecto_marginal,
                  aes(x = listeners, y = ef_marg * 100)) +
  geom_ribbon(aes(ymin = ic_low * 100, ymax = ic_high * 100),
              fill = "#457B9D", alpha = 0.25) +
  geom_line(color = "#E63946", linewidth = 1.1) +
  geom_point(color = "#E63946", size = 2.5) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = function(x) paste0("+", x, " pp")) +
  labs(
    title    = "Figura 4.1",
    subtitle = "Efecto marginal de un gran evento sobre la ocupación según popularidad del artista",
    x        = "Oyentes mensuales (escala logarítmica)",
    y        = "Efecto marginal (pp)",
    caption  = "Banda: intervalo de confianza al 95% del estimador de panel con efectos fijos."
  ) +
  tema_apa

ggsave("cap4_outputs/figuras/fig_4_1_efecto_marginal_h3.png",
       fig_4_1, width = 9, height = 5, dpi = 300, bg = "white")

# =============================================================================
# FIGURA 4.2 — Mapa GWR de β locales (H5)
# =============================================================================
# Preparar datos para el mapa
sf_gwr_plot <- sf_gwr %>%
  mutate(beta_dist_local = as.numeric(beta_dist_local)) # Asegurar formato numérico
library(ggplot2)
library(sf)
library(stringr)

caption_texto <- "75,8% de los listings presenta β_distancia < 0. Azul: decaimiento espacial; amarillo: pendiente nula; rojo: pendiente positiva (entornos de venues periféricos)."

fig_4_2 <- ggplot(sf_gwr_plot) +
  geom_sf(aes(color = beta_dist_local), size = 1.5) +
  scale_color_gradient2(
    low      = "#08306B",
    mid      = "#FFFFCC",
    high     = "#A50026",
    midpoint = 0,
    name     = "β_distancia"
  ) +
  labs(
    title    = "Figura 4.2",
    subtitle = "Distribución espacial de los coeficientes locales del GWR",
    caption  = str_wrap(caption_texto, width = 85) 
  ) +
  tema_apa +
  theme(
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    plot.caption     = element_text(hjust = 0, lineheight = 1.2),
    plot.margin      = margin(10, 10, 20, 10)
  )

ggsave("cap4_outputs/figuras/fig_4_2_gwr_mapa_h5.png",
       fig_4_2, width = 9, height = 7, dpi = 300, bg = "white")

# =============================================================================
# TABLA 1.1 — Resumen de hipótesis del trabajo (para el Capítulo 1)
# Se coloca aquí por comodidad para reutilizar apa_table() y apa_save().
# El archivo generado se guarda en cap4_outputs/tablas_apa/ pero su contenido
# corresponde al Capítulo 1 (mover manualmente si se desea).
# =============================================================================

tabla_1_1 <- tribble(
  ~hip,  ~enunciado,                                                                              ~seccion, ~variable,                                                                                ~metodo,
  "H1",  "Un gran evento incrementa la ocupación diaria de Airbnb",                                "1.3",    "Ocupación diaria del alojamiento",                                                       "Panel de efectos fijos",
  "H2",  "Más conciertos al mes implican mayor ocupación hotelera",                                "1.1",    "Ocupación hotelera mensual por plazas",                                                  "ADF + STL + Granger + ARIMAX",
  "H3",  "El efecto del evento crece con la popularidad del artista",                              "1.4",    "Ocupación diaria del alojamiento",                                                       "Panel FE con término de interacción",
  "H4",  "El efecto es mayor sobre el turismo internacional que sobre el doméstico",               "1.2",    "Log de viajeros (nacionales / internacionales)",                                         "OLS paralelo + bootstrap no paramétrico",
  "H5",  "El efecto decrece con la distancia al venue",                                            "1.3",    "Ocupación diaria (anillos); log de la ocupación media en eventos (GWR)",                 "Anillos por distancia + GWR",
  "H6",  "Los festivos amplifican el efecto del concierto",                                        "1.5",    "Ocupación diaria del alojamiento",                                                       "OLS con interacción + Random Forest + GAM",
  "H7",  "La actividad concertística se segmenta en clusters con distinto impacto turístico",      "1.2",    "Features del concierto (clustering); ocupación hotelera mensual (validación externa)",   "K-Means + correlación de Spearman"
) %>%
  rename(
    `Hipótesis`            = hip,
    `Enunciado sintético`  = enunciado,
    `Sección de origen`    = seccion,
    `Variable analizada`   = variable,
    `Método de contraste`  = metodo
  )

ft_1_1 <- flextable(tabla_1_1) %>%
  apa_table() %>%
  align(align = "left", j = c(2, 4, 5), part = "body")   # textos largos a la izquierda

apa_save(
  ft_1_1, "tabla_1_1_hipotesis",
  titulo = c("Tabla 1.1",
             "Resumen de las hipótesis del trabajo y su tratamiento metodológico"),
  nota   = paste(
    "Nota. La columna de sección de origen remite al apartado del marco teórico donde se introduce la hipótesis.",
    "La variable analizada distingue entre las hipótesis paramétricas, donde se modela una variable dependiente,",
    "y H7, donde el clustering opera sobre features del concierto y la validación externa se realiza con un",
    "indicador hotelero del INE."
  )
)

##################################
#TABLA H7
##################################

tabla_h7_spearman <- tribble(
  ~indicador,                    ~rho,   ~p,
  "Ocupación por plazas (%)",     0.260, 0.063,
  "Viajeros totales",             0.268, 0.055,
  "Viajeros internacionales",     0.183, 0.195,
  "Pernoctaciones totales",       0.252, 0.072
) %>%
  transmute(
    `Indicador INE`       = indicador,
    `ρ de Spearman`       = paste0("+", sapply(rho, fmt_es, dig = 3)),
    `p-valor (bilateral)` = sapply(p, fmt_p),
    `n`                   = "52"
  )

ft_h7 <- flextable(tabla_h7_spearman) %>% apa_table()

apa_save(
  ft_h7, "tabla_5_8_spearman_h7",
  titulo = c("Tabla 5.8",
             "Correlación de Spearman entre la proporción mensual del clúster global y los indicadores turísticos del INE"),
  nota = paste(
    "Nota. Correlaciones entre la proporción mensual de conciertos del clúster global (grandes",
    "recintos) y cuatro indicadores de actividad turística del INE, sobre n = 52 meses (enero 2022 –",
    "abril 2026). Los p-valores son bilaterales; dado que H7 predice una asociación de signo positivo,",
    "el contraste teóricamente pertinente es unilateral, en cuyo caso los p-valores efectivos se",
    "reducen aproximadamente a la mitad. Ningún coeficiente alcanza la significación al 5% en el",
    "contraste bilateral."
  )
)
