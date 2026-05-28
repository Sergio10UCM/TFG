# =============================================================================
# Análisis Descriptivo
# Prerequisito: Importacion.R ejecutado en la misma sesión
# =============================================================================

# Carga de paquetes necesarios para manipulación, fechas, gráficos y modelos
library(tidyverse) # Metapaquete para manipulación y visualización de datos (dplyr, ggplot2, etc.)
library(lubridate) # Tratamiento avanzado de fechas
library(scales)    # Formateo de escalas en gráficos (porcentajes, euros, etc.)
library(patchwork) # Composición de múltiples gráficos (aunque no se usa explícitamente, es útil tenerlo)
library(ggridges)  # Gráficos de crestas (ridgeline plots) para densidades
library(corrplot)  # Visualización de matrices de correlación
library(sf)        # Manejo de datos espaciales (mapas)
library(ranger)    # Implementación rápida de algoritmos Random Forest
library(vip)       # Visualización de la importancia de variables en modelos

# Tema y paleta ----------------------------------------------------------------
# Definición de la paleta de colores corporativa para los gráficos del TFG
C_EVENTO  <- "#E63946" # Rojo (destacar eventos/conciertos)
C_BASE    <- "#457B9D" # Azul (línea base, sin eventos)
C_ACENTO  <- "#F4A261" # Naranja (destacar elementos secundarios)
C_FONDO   <- "#F8F9FA" # Gris muy claro para el fondo de las figuras

# Creación de un tema personalizado para homogeneizar el estilo de todos los gráficos
theme_tfg <- theme_minimal(base_size = 12) +
  theme(plot.background  = element_rect(fill = C_FONDO, color = NA),
        panel.grid.minor = element_blank(), # Eliminación de la cuadrícula secundaria para mayor limpieza
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(color = "grey40", size = 10),
        legend.position  = "bottom") # Leyendas en la parte inferior por defecto

# Aplicación global del tema personalizado a todo el script
theme_set(theme_tfg)

# =============================================================================
# BLOQUE 1 — PREPARACIÓN DE DATOS
# =============================================================================

# -- 1.1  Normalización de recintos -------------------------------------------
# Creación de un diccionario manual para unificar las distintas formas en que
# aparece escrito un mismo recinto en la base de datos de Setlist.fm
recintos_eq <- tribble(
  ~raw,                                                ~canonico,
  "WiZink Center",                                     "Movistar Arena",
  "Palacio de los Deportes de la Comunidad de Madrid", "Movistar Arena",
  "Barclaycard Center",                                "Movistar Arena",
  "Palacio de los Deportes",                           "Movistar Arena",
  "Movistar Arena",                                    "Movistar Arena",
  "Wizink Center",                                     "Movistar Arena",
  "Palacio de los Deportes Comunidad de Madrid",       "Movistar Arena",
  "Estadio Santiago Bernabéu",                         "Estadio Santiago Bernabéu",
  "Santiago Bernabéu Stadium",                         "Estadio Santiago Bernabéu",
  "Bernabéu",                                          "Estadio Santiago Bernabéu",
  "Estadio Cívitas Metropolitano",                     "Estadio Riyadh Air Metropolitano",
  "Wanda Metropolitano",                               "Estadio Riyadh Air Metropolitano",
  "Metropolitano",                                     "Estadio Riyadh Air Metropolitano",
  "Estadio La Peineta",                                "Estadio Riyadh Air Metropolitano",
  "Estadio Riyadh Air Metropolitano",                  "Estadio Riyadh Air Metropolitano",
  "Estadio Vicente Calderón",                          "Estadio Vicente Calderón",
  "IFEMA - Recinto Ferial de Madrid",                  "IFEMA Madrid",
  "IFEMA Madrid",                                      "IFEMA Madrid",
  "IFEMA",                                             "IFEMA Madrid",
  "Feria de Madrid",                                   "IFEMA Madrid",
  "La Riviera",                                        "La Riviera",
  "Sala La Riviera",                                   "La Riviera",
  "Riviera",                                           "La Riviera",
  "Palacio Vistalegre",                                "Palacio Vistalegre",
  "Vistalegre",                                        "Palacio Vistalegre",
  "Frontón de los Reyes",                              "Frontón de los Reyes",
  "Sala Mon Live",                                     "Sala Mon Live",
  "Mon Live",                                          "Sala Mon Live",
  "Joy Eslava",                                        "Joy Eslava",
  "El Sol",                                            "Sala El Sol",
  "Sala El Sol",                                       "Sala El Sol",
  "Wurlitzer Ballroom",                                "Wurlitzer Ballroom",
  "Real Jardín Botánico Alfonso XIII",                 "Real Jardín Botánico",
  "Copérnico",                                         "Sala Copérnico",
  "Sala Copérnico",                                    "Sala Copérnico",
  "Sala Nazca",                                        "Sala Nazca",
  "Nazca",                                             "Sala Nazca",
  "Iberdrola Music",                                   "Iberdrola Music",
  "Revi Live",                                         "Revi Live",
  "Sala Villanos",                                     "Sala Villanos",
  "Changó Live",                                       "Changó Live",
  "Changó",                                            "Changó Live",
  "Sala Caracol",                                      "Sala Villanos",
  "Sala Caracol Madrid",                               "Sala Villanos",
  "Sala But",                                          "Sala But",
  "But",                                               "Sala But",
  "Sala Galileo Galilei",                              "Sala Galileo Galilei",
  "Galileo Galilei",                                   "Sala Galileo Galilei",
  "Sala Costello Club",                                "Sala Costello Club",
  "Costello Club",                                     "Sala Costello Club",
  "Sala Clamores",                                     "Sala Clamores",
  "Clamores",                                          "Sala Clamores",
  "Moby Dick Club",                                    "Moby Dick Club",
  "Moby Dick",                                         "Moby Dick Club",
  "Café Central",                                      "Café Central",
  "Sala Cool",                                         "Sala Cool",
  "Café Berlín",                                       "Café Berlín",
  "Sala Bash",                                         "Sala 0",
  "Bash",                                              "Sala 0",
  "Rock Palace",                                       "Rock Palace",
  "Teatro Real",                                       "Teatro Real",
  "Auditorio Nacional de Música",                      "Auditorio Nacional de Música",
  "Auditorio Nacional",                                "Auditorio Nacional de Música",
  "Teatro Coliseum",                                   "Teatro Coliseum",
  "Gran Teatro CaixaBank Príncipe Pío",                "Gran Teatro Príncipe Pío",
  "Teatro Príncipe Pío",                               "Gran Teatro Príncipe Pío",
  "Teatros del Canal",                                 "Teatros del Canal",
  "Circo Price",                                       "Circo Price",
  "Teatro Circo Price",                                "Circo Price",
  "Teatro de la Zarzuela",                             "Teatro de la Zarzuela",
  "Teatro Fernán Gómez",                               "Teatro Fernán Gómez",
  "Centro Cultural Conde Duque",                       "Centro Cultural Conde Duque",
  "Conde Duque",                                       "Centro Cultural Conde Duque",
  "Jardines del Buen Retiro",                          "Jardines del Buen Retiro",
  "Recinto Valdebebas",                                "Recinto Valdebebas",
  "Valdebebas",                                        "Recinto Valdebebas",
  "Mad Cool Festival",                                 "Recinto Valdebebas",
  "Hipódromo de la Zarzuela",                          "Hipódromo de la Zarzuela",
  "Estadio Vallehermoso",                              "Estadio Vallehermoso",
  "Ciudad del Rock (Arganda del Rey)",                 "Ciudad del Rock",
  "Ciudad del Rock",                                   "Ciudad del Rock",
  "Download Festival Madrid",                          "Ciudad del Rock",
  "Caja Mágica",                                       "Caja Mágica",
  "Caja Magica",                                       "Caja Mágica",
  "Espacio Ibercaja Delicias",                         "Espacio Ibercaja Delicias",
  "Tomavistas Festival",                               "Jardines del Buen Retiro",
  "La Paqui",                                          "Sala But",
  "Sala Paqui",                                        "Sala But",
  "Sala Siroco",                                       "Sala Siroco",
  "Siroco",                                            "Sala Siroco",
  "Shoko",                                             "Shoko Madrid",
  "Shoko Madrid",                                      "Shoko Madrid",
  "Shôko",                                             "Shoko Madrid",
  "Shôko Madrid",                                      "Shoko Madrid",
  "Sala Shoko",                                        "Shoko Madrid",
  "Sala Shôko",                                        "Shoko Madrid"
)

# Aplicación del diccionario a la base de datos principal de conciertos
conciertos <- df_conciertos_final %>%
  mutate(recinto = str_squish(recinto)) %>%  # Elimina espacios múltiples
  left_join(recintos_eq, by = c("recinto" = "raw")) %>%
  mutate(recinto_canonico = coalesce(canonico, recinto)) %>% # Si no está en el diccionario, mantiene el original
  select(-canonico)

# -- 1.2  Aforos y categorías -------------------------------------------------
library(tibble)
# Creación manual de un registro de aforos y categorías para los recintos canónicos
aforos <- tribble(
  ~recinto_canonico,                ~aforo,   ~categoria,
  "Movistar Arena",                  16000,   "Gran sala",
  "Estadio Santiago Bernabéu",       65000,   "Estadio",
  "Estadio Riyadh Air Metropolitano",   60000,   "Estadio",
  "Estadio Vicente Calderón",        54900,   "Estadio",
  "Estadio Vallehermoso",            15000,   "Estadio",
  "Palacio Vistalegre",              11200,   "Gran sala",
  "Rock Palace",                       150,   "Sala pequeña",
  "IFEMA Madrid",                    30000,   "Festival/Ferial",
  "Recinto Valdebebas",              80000,   "Festival/Ferial",
  "Ciudad del Rock",                 96000,   "Festival/Ferial",
  "Iberdrola Music",                 60000,   "Festival/Ferial",
  "Caja Mágica",                     35000,   "Festival/Ferial",
  "La Riviera",                       1500,   "Sala mediana",
  "Sala Mon Live",                     800,   "Sala mediana",
  "Frontón de los Reyes",              100,   "Sala pequeña",
  "Sala 0",                            390,   "Sala pequeña",
  "Espacio Ibercaja Delicias",        2400,   "Sala mediana",
  "Joy Eslava",                       1200,   "Sala mediana",
  "Sala But",                         1000,   "Sala mediana",
  "Revi Live",                         500,   "Sala mediana",
  "Sala Cool",                         520,   "Sala mediana",
  "Sala Copérnico",                    700,   "Sala mediana",
  "Wurlitzer Ballroom",                200,   "Sala pequeña",
  "Sala Nazca",                        220,   "Sala pequeña",
  "Moby Dick Club",                    300,   "Sala pequeña",
  "Changó Live",                       600,   "Sala mediana",
  "Sala Costello Club",                200,   "Sala pequeña",
  "Sala El Sol",                       640,   "Sala mediana",
  "Sala Galileo Galilei",              500,   "Sala mediana",
  "Sala Villanos",                     400,   "Sala mediana",
  "Café Berlín",                       250,   "Sala pequeña",
  "Sala Clamores",                     220,   "Sala pequeña",
  "Café Central",                      300,   "Sala pequeña",
  "Teatro Real",                      1900,   "Teatro/Auditorio",
  "Auditorio Nacional de Música",     2300,   "Teatro/Auditorio",
  "Teatro Coliseum",                  1400,   "Teatro/Auditorio",
  "Gran Teatro Príncipe Pío",         2000,   "Teatro/Auditorio",
  "Teatros del Canal",                 800,   "Teatro/Auditorio",
  "Circo Price",                      2100,   "Teatro/Auditorio",
  "Teatro de la Zarzuela",            1200,   "Teatro/Auditorio",
  "Teatro Fernán Gómez",               680,   "Teatro/Auditorio",
  "Centro Cultural Conde Duque",      1450,   "Teatro/Auditorio",
  "Jardines del Buen Retiro",         2000,   "Espacio al aire libre",
  "Real Jardín Botánico",             3000,   "Espacio al aire libre",
  "Hipódromo de la Zarzuela",         4700,   "Espacio al aire libre",
  "Sala Siroco",                        90,   "Sala pequeña",
  "Shoko Madrid",                      600,   "Sala mediana"
)

# Resumen estadístico exploratorio de los aforos
aforos %>% 
  summarise(n_recintos = n(),
            aforo_total = sum(aforo, na.rm = TRUE),
            aforo_medio = mean(aforo, na.rm = TRUE))

# Cruce de aforos con el dataframe de conciertos y creación de variables lógicas de impacto
conciertos <- conciertos %>%
  left_join(aforos, by = "recinto_canonico") %>%
  mutate(
    categoria = replace_na(categoria, "Sin clasificar"),
    # Se considera "gran evento" si supera 5.000 de aforo o pertenece a estadio/festival
    es_gran_evento = (!is.na(aforo) & aforo >= 5000) |
      categoria %in% c("Estadio", "Festival/Ferial")
  )

# -- 1.3  Popularidad de artistas (deduplicada) --------------------------------
# Selección de la mejor métrica (menor ranking o mayores oyentes) por artista único
kworb_dedup <- df_kworb_final %>%
  group_by(artista) %>% slice_min(ranking, n = 1, with_ties = FALSE) %>% ungroup()

listeners_dedup <- df_listeners_final %>%
  group_by(artista) %>% slice_max(listeners, n = 1, with_ties = FALSE) %>% ungroup()

# -- 1.3b  Relevancia en el mercado español (df_daily_final + df_weekly_final) ----
# Artistas con presencia en los charts de Spotify España: proxy de popularidad local

# Concatenación de artistas presentes en ránkings diarios y semanales de España
artistas_chart_spain <- bind_rows(
  df_daily_final  %>% distinct(artista) %>% mutate(fuente = "diario"),
  df_weekly_final %>% distinct(artista) %>% mutate(fuente = "semanal")
) %>%
  distinct(artista) %>% # Deduplicación final
  mutate(en_chart_spain = TRUE)

# Incorporación de la popularidad en España a la base de conciertos
conciertos <- conciertos %>%
  left_join(artistas_chart_spain, by = "artista") %>%
  mutate(en_chart_spain = replace_na(en_chart_spain, FALSE))

cat("Artistas en charts de España:", sum(conciertos$en_chart_spain), "conciertos (",
    round(mean(conciertos$en_chart_spain) * 100, 1), "% del total)\n")

# -- 1.4  Tabla diaria maestra ------------------------------------------------
# Agregación de variables a nivel diario
conciertos_dia <- conciertos %>%
  group_by(fecha) %>%
  summarise(
    n_conciertos  = n(),
    n_gran_evento = sum(es_gran_evento, na.rm = TRUE),
    max_aforo     = suppressWarnings(max(aforo, na.rm = TRUE)), # Aforo máximo del día
    hay_estadio   = any(categoria == "Estadio", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(max_aforo = if_else(is.infinite(max_aforo), NA_real_, max_aforo))

# Mapeo de métricas de popularidad global hacia la fecha del evento
conciertos_pop <- conciertos %>%
  left_join(kworb_dedup    %>% select(artista, ranking, streams)  %>% rename(rank_k = ranking, str_k = streams),  by = "artista") %>%
  left_join(listeners_dedup %>% select(artista, listeners) %>% rename(list_k = listeners), by = "artista") %>%
  group_by(fecha) %>%
  summarise(
    min_ranking   = suppressWarnings(min(rank_k,  na.rm = TRUE)),
    max_streams   = suppressWarnings(max(str_k,   na.rm = TRUE)),
    max_listeners = suppressWarnings(max(list_k,  na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # Limpieza de infinitos generados por grupos sin datos
  mutate(across(where(is.numeric), ~ if_else(is.infinite(.) | is.nan(.), NA_real_, .)))

# Generación del calendario maestro integrando fechas, festivos y eventos
tabla_diaria <- df_final %>%
  select(fecha, dia_semana, estado, es_laborable, tipo_festivo) %>%
  left_join(conciertos_dia, by = "fecha") %>%
  left_join(conciertos_pop, by = "fecha") %>%
  mutate(
    hay_concierto  = !is.na(n_conciertos),
    n_conciertos   = replace_na(n_conciertos, 0),
    n_gran_evento  = replace_na(n_gran_evento, 0),
    anio           = year(fecha),
    mes            = month(fecha),
    dia_semana_num = wday(fecha, week_start = 1), # Lunes = 1
    es_finde       = dia_semana %in% c("Sábado", "Domingo"),
    es_festivo     = estado == "Festivo",
    es_puente      = es_festivo & !es_finde,
    # Factorización para asegurar el orden cronológico en los gráficos
    dia_semana     = factor(dia_semana,
                            levels = c("Lunes","Martes","Miércoles",
                                       "Jueves","Viernes","Sábado","Domingo"))
  )

# -- 1.5  Ocupación Airbnb por fecha (del calendar; precio NO disponible) -----
# NOTA: price y adjusted_price son 100% NA en ambos snapshots de Inside Airbnb.
# Usamos available (sin NAs) como único indicador temporal de Airbnb.
# El precio se analiza con df_listings_06$price (precio base del anuncio).

# Cálculo de la tasa de ocupación diaria en Airbnb (junio)
ocup_diaria_06 <- df_calendar_06 %>%
  group_by(fecha = date) %>%
  summarise(
    tasa_ocupacion = mean(available == FALSE, na.rm = TRUE), # available=FALSE implica ocupado/bloqueado
    n_disponibles  = sum(available == TRUE),
    n_ocupados     = sum(available == FALSE),
    n_total        = n(),
    .groups = "drop"
  )

# Cruce con la tabla diaria maestra
tabla_diaria_airbnb <- tabla_diaria %>%
  left_join(ocup_diaria_06, by = "fecha")

# -- 1.5b  Comparativa snapshots junio vs. septiembre (df_calendar_09) -----------

# Cálculo de la ocupación para el snapshot de septiembre
ocup_diaria_09 <- df_calendar_09 %>%
  group_by(fecha = date) %>%
  summarise(
    tasa_ocupacion_sep = mean(available == FALSE, na.rm = TRUE),
    .groups = "drop"
  )

# Estadísticos resumen para mencionar en el texto
cat("Ocupación media snapshot junio 2025:", round(mean(ocup_diaria_06$tasa_ocupacion) * 100, 1), "%\n")
cat("Ocupación media snapshot sept. 2025:", round(mean(ocup_diaria_09$tasa_ocupacion_sep) * 100, 1), "%\n")

# -- 1.6  Listings: precio base y variables espaciales -----------------------
# Limpieza y selección de variables del archivo de listings (alojamientos únicos)
listings_clean <- df_listings_06 %>%
  select(id, neighbourhood_cleansed, neighbourhood_group_cleansed,
         latitude, longitude, room_type, accommodates, bedrooms, beds,
         price, minimum_nights, availability_30, availability_60,
         availability_90, availability_365,
         estimated_occupancy_l365d, estimated_revenue_l365d,
         number_of_reviews, review_scores_rating, reviews_per_month,
         instant_bookable, host_is_superhost,
         calculated_host_listings_count) %>%
  rename(barrio   = neighbourhood_cleansed,
         distrito = neighbourhood_group_cleansed,
         precio   = price) %>%
  mutate(
    tasa_ocup_proxy = estimated_occupancy_l365d / 365,  # Ratio de ocupación anual
    ingreso_diario  = if_else(estimated_occupancy_l365d > 0,
                              estimated_revenue_l365d / estimated_occupancy_l365d,
                              NA_real_)
  )

# -- 1.7  Tabla mensual -------------------------------------------------------
# Agregación a nivel mensual para cruzar con encuestas oficiales del INE
tabla_mensual <- tabla_diaria %>%
  group_by(anio, mes) %>%
  summarise(
    fecha_mes          = floor_date(min(fecha), "month"), # Fija la fecha al día 1 de cada mes
    n_conciertos_mes   = sum(n_conciertos),
    n_grandes_mes      = sum(n_gran_evento),
    dias_con_concierto = sum(hay_concierto),
    dias_festivo       = sum(es_festivo),
    max_aforo_mes      = suppressWarnings(max(max_aforo, na.rm = TRUE)),
    max_listeners_mes  = suppressWarnings(max(max_listeners, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    max_aforo_mes     = if_else(is.infinite(max_aforo_mes),     NA_real_, max_aforo_mes),
    max_listeners_mes = if_else(is.infinite(max_listeners_mes), NA_real_, max_listeners_mes)
  ) %>%
  # Cruces con las diferentes encuestas del INE, enlazando por la variable temporal fecha_mes
  left_join(df_ine_hotel_capacidad %>% select(fecha, ocupacion_plazas, ocupacion_plazas_finde,
                                              ocupacion_habitaciones, personal_empleado,
                                              plazas_estimadas, establecimientos),
            by = c("fecha_mes" = "fecha")) %>%
  left_join(df_ine_hotel_viajeros  %>% select(fecha, viajeros_total, viajeros_espana,
                                              viajeros_extranjero, pernoctaciones_total,
                                              pernoctaciones_espana, pernoctaciones_extranjero),
            by = c("fecha_mes" = "fecha")) %>%
  left_join(df_ine_iph             %>% select(fecha, iph_indice, iph_var_anual),
            by = c("fecha_mes" = "fecha")) %>%
  left_join(df_ine_aptos_tur       %>% select(fecha,
                                              ocupacion_plazas_aptos = ocupacion_plazas,
                                              ocupacion_aptos),
            by = c("fecha_mes" = "fecha"))

# -- 1.8  Cuota de mercado por tipo de alojamiento (df_ine_alojamiento_global) ---

# Transformación y cálculo de porcentajes de cuota de mercado
tabla_cuota_mercado <- df_ine_alojamiento_global %>%
  filter(fecha >= as.Date("2019-01-01"), !is.na(hotel_viajeros_total)) %>%
  mutate(
    # Suma de todos los viajeros para el denominador
    total = hotel_viajeros_total +
      replace_na(apto_viajeros_total,     0) +
      replace_na(albergue_viajeros_total,  0) +
      replace_na(rural_viajeros_total,     0) +
      replace_na(camping_viajeros_total,   0),
    cuota_hotel    = hotel_viajeros_total               / total,
    cuota_apto     = replace_na(apto_viajeros_total, 0) / total,
    cuota_resto    = 1 - cuota_hotel - cuota_apto,
    fecha_mes      = fecha
  ) %>%
  select(fecha_mes, cuota_hotel, cuota_apto, cuota_resto) %>%
  # Paso a formato largo (long) para facilitar el gráfico apilado
  pivot_longer(-fecha_mes, names_to = "tipo", values_to = "cuota") %>%
  mutate(tipo = recode(tipo,
                       "cuota_hotel" = "Hoteles",
                       "cuota_apto"  = "Apartamentos turísticos",
                       "cuota_resto" = "Resto (rural, campings, albergues)"
  ))

# -- 1.9  Gasto turístico anual (df_ine_familitur + df_ine_frontur) -----------
# Se usarán en el Capítulo 5; aquí solo preparamos los objetos

tabla_gasto_anual <- df_ine_familitur %>%
  select(fecha, anio, gasto_diario_pax_abs, gasto_pax_abs, viajes_abs, pernoctaciones_abs) %>%
  left_join(
    df_ine_frontur %>% select(anio, internacional_abs, internacional_var),
    by = "anio"
  )

cat("Tabla de gasto anual construida:", nrow(tabla_gasto_anual), "años\n")

# Comprobaciones finales de dimensiones
cat("Tablas construidas:\n")
cat("  tabla_diaria:", nrow(tabla_diaria), "días |", sum(tabla_diaria$hay_concierto), "con concierto\n")
cat("  tabla_diaria_airbnb:", nrow(tabla_diaria_airbnb), "días | tasa_ocup disponible:",
    sum(!is.na(tabla_diaria_airbnb$tasa_ocupacion)), "\n")
cat("  tabla_mensual:", nrow(tabla_mensual), "meses\n")
cat("  listings_clean:", nrow(listings_clean), "alojamientos\n\n")


# =============================================================================
# BLOQUE 2 — TABLAS DE ESTADÍSTICOS DESCRIPTIVOS PARA LA MEMORIA
# =============================================================================

# Construcción de la Tabla 1 para la memoria
cat("--- TABLA 1: Conciertos por año ---\n")
t1 <- conciertos %>%
  mutate(anio = year(fecha)) %>%
  group_by(anio) %>%
  summarise(
    total             = n(),
    fechas_distintas  = n_distinct(fecha),
    artistas_distintos = n_distinct(artista),
    recintos_distintos = n_distinct(recinto_canonico),
    pct_gran_evento   = round(mean(es_gran_evento, na.rm = TRUE) * 100, 1),
    pct_con_encore    = round(mean(hay_encore, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )
print(t1)

# Construcción de la Tabla 2 para la memoria
cat("\n--- TABLA 2: Precio base Airbnb por tipo de alojamiento (listings junio 2025) ---\n")
t2 <- listings_clean %>%
  group_by(room_type) %>%
  summarise(
    n            = n(),
    precio_medio = round(mean(precio, na.rm = TRUE), 1),
    precio_med   = round(median(precio, na.rm = TRUE), 1),
    precio_p25   = round(quantile(precio, 0.25, na.rm = TRUE), 1), # Percentil 25
    precio_p75   = round(quantile(precio, 0.75, na.rm = TRUE), 1), # Percentil 75
    .groups = "drop"
  ) %>%
  arrange(desc(n))
print(t2)

# Construcción de la Tabla 3 para la memoria
cat("\n--- TABLA 3: Ocupación Airbnb (calendar) por tipo de día ---\n")
t3 <- tabla_diaria_airbnb %>%
  filter(!is.na(tasa_ocupacion), anio >= 2022) %>%
  mutate(
    # Tipificación jerárquica del día (el orden de los case_when importa)
    tipo_dia = case_when(
      n_gran_evento > 0   ~ "Gran evento",
      hay_concierto       ~ "Concierto (sala pequeña/mediana)",
      es_festivo          ~ "Festivo sin evento",
      es_finde            ~ "Fin de semana sin evento",
      TRUE                ~ "Laborable sin evento"
    ),
    tipo_dia = factor(tipo_dia, levels = c("Gran evento",
                                           "Concierto (sala pequeña/mediana)",
                                           "Fin de semana sin evento",
                                           "Festivo sin evento",
                                           "Laborable sin evento"))
  ) %>%
  group_by(tipo_dia) %>%
  summarise(
    n_dias     = n(),
    ocup_media = round(mean(tasa_ocupacion) * 100, 1),
    ocup_med   = round(median(tasa_ocupacion) * 100, 1),
    .groups = "drop"
  )
print(t3)

# Construcción de la Tabla 4 para la memoria
cat("\n--- TABLA 4: Viajeros hoteleros y ocupación — medias por cuartil de conciertos ---\n")
t4 <- tabla_mensual %>%
  filter(!is.na(viajeros_total), anio >= 2022) %>%
  mutate(
    cuartil = ntile(n_conciertos_mes, 4), # Segmentación en cuartiles
    etiqueta = paste0("Q", cuartil)
  ) %>%
  group_by(etiqueta) %>%
  summarise(
    n_meses          = n(),
    rango_conc       = paste(min(n_conciertos_mes), "–", max(n_conciertos_mes)),
    viajeros_medio   = round(mean(viajeros_total, na.rm = TRUE)),
    ocup_hotel_media = round(mean(ocupacion_plazas, na.rm = TRUE), 1),
    iph_medio        = round(mean(iph_indice, na.rm = TRUE), 1),
    .groups = "drop"
  )
print(t4)


# =============================================================================
# BLOQUE 3 — FIGURAS DEL CAPÍTULO 3
# =============================================================================

# --- FIG 1: Evolución mensual de conciertos ----------------------------------
fig1 <- tabla_mensual %>%
  filter(anio >= 2022, fecha_mes <= as.Date("2026-04-01")) %>%
  ggplot(aes(x = fecha_mes, y = n_conciertos_mes)) +
  geom_col(aes(fill = factor(anio)), alpha = 0.9, show.legend = FALSE) +
  geom_smooth(method = "loess", se = FALSE, color = C_EVENTO, linewidth = 1) + # Tendencia LOESS
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 months") +
  scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.75, direction = -1) +
  labs(title    = "Figura 1. Evolución mensual del número de conciertos en Madrid (2022–2026)",
       subtitle = "Barras: total mensual | Línea: tendencia suavizada (LOESS) | Fuente: Setlist.fm",
       x = NULL, y = "Número de conciertos")

# Exportación del gráfico al directorio de trabajo actual
ggsave("fig01_conciertos_mensual.png", fig1, width = 12, height = 5, dpi = 300)
print(fig1)

# --- FIG 2: Heatmap día semana × mes -----------------------------------------
fig2 <- tabla_diaria %>%
  filter(anio >= 2022) %>%
  count(dia_semana, mes) %>% # Conteo cruzado para generar el mapa de calor
  ggplot(aes(x = factor(mes, labels = month.abb), y = fct_rev(dia_semana), fill = n)) +
  geom_tile(color = "white", linewidth = 0.4) + # Baldosas del heatmap
  geom_text(aes(label = n), size = 3, color = "white") + # Cifras superpuestas
  scale_fill_gradient(low = "#aec6cf", high = C_EVENTO, name = "Conciertos") +
  labs(title    = "Figura 2. Distribución de conciertos por día de la semana y mes (2022–2026)",
       subtitle = "Fuente: Setlist.fm",
       x = "Mes", y = NULL)

ggsave("fig02_heatmap_semana_mes.png", fig2, width = 10, height = 4, dpi = 300)
print(fig2)

# --- FIG 3: Top 20 recintos --------------------------------------------------
fig3 <- conciertos %>%
  count(recinto_canonico, categoria, sort = TRUE) %>%
  slice_max(n, n = 20) %>% # Filtro para los 20 superiores
  mutate(recinto_canonico = fct_reorder(recinto_canonico, n)) %>% # Orden en el eje Y
  ggplot(aes(x = n, y = recinto_canonico, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.2, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) + # Expansión para acomodar las etiquetas
  scale_fill_brewer(palette = "Set2", name = "Categoría") +
  labs(title    = "Figura 3. Top 20 recintos por volumen de conciertos (2022–2026)",
       subtitle = "Fuente: Setlist.fm",
       x = "Número de conciertos", y = NULL)

ggsave("fig03_top_recintos.png", fig3, width = 10, height = 6, dpi = 300)
print(fig3)

# --- FIG 4: Distribución del precio base Airbnb por tipo de alojamiento ------
fig4 <- listings_clean %>%
  filter(precio <= 400) %>% # Truncamiento de outliers para evitar deformación visual
  ggplot(aes(x = precio, fill = room_type, color = room_type)) +
  geom_density(alpha = 0.35, linewidth = 0.7) + # Curvas de densidad solapadas
  scale_x_continuous(labels = label_dollar(suffix = "€", prefix = ""),
                     limits = c(0, 400)) +
  scale_fill_brewer(palette  = "Set1", name = "Tipo") +
  scale_color_brewer(palette = "Set1", name = "Tipo") +
  labs(title    = "Figura 4. Distribución del precio base por noche en Airbnb Madrid",
       subtitle = "Precio del anuncio (listings) | Snapshot junio 2025 | Truncado en 400 €",
       x = "Precio base por noche (€)", y = "Densidad")

ggsave("fig04_precio_listings_densidad.png", fig4, width = 10, height = 5, dpi = 300)
print(fig4)

# --- FIG 5: Tasa de ocupación Airbnb (calendar) en días con/sin concierto ----
fig5 <- tabla_diaria_airbnb %>%
  filter(!is.na(tasa_ocupacion)) %>%
  mutate(grupo = if_else(hay_concierto, "Con concierto", "Sin concierto")) %>%
  ggplot(aes(x = grupo, y = tasa_ocupacion, fill = grupo)) +
  geom_violin(alpha = 0.55, trim = TRUE) + # Gráfico de violín
  geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") + # Boxplot interno superpuesto
  scale_fill_manual(values = c("Con concierto" = C_EVENTO, "Sin concierto" = C_BASE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(title    = "Figura 5. Tasa de ocupación de Airbnb: días con y sin concierto",
       subtitle = "Proxy: proporción de anuncios con available = FALSE | Calendar junio 2025",
       x = NULL, y = "Tasa de ocupación", fill = NULL) +
  theme(legend.position = "none")

ggsave("fig05_ocupacion_violin.png", fig5, width = 7, height = 5, dpi = 300)
print(fig5)

# Versión alternativa de la Fig 5 (Densidad)
fig5 <- tabla_diaria_airbnb %>%
  filter(!is.na(tasa_ocupacion)) %>%
  mutate(grupo = if_else(hay_concierto, "Con concierto", "Sin concierto")) %>%
  ggplot(aes(x = tasa_ocupacion, fill = grupo, color = grupo)) +
  geom_density(alpha = 0.45, linewidth = 0.9) +
  scale_fill_manual(values  = c("Con concierto" = C_EVENTO, "Sin concierto" = C_BASE)) +
  scale_color_manual(values = c("Con concierto" = C_EVENTO, "Sin concierto" = C_BASE)) +
  scale_x_continuous(labels = percent_format()) +
  labs(title    = "Figura 4.9. Tasa de ocupación de Airbnb: días con y sin concierto",
       subtitle = "Proxy: proporción de anuncios con available = FALSE | Calendar junio 2025",
       x = "Tasa de ocupación", y = "Densidad",
       fill = NULL, color = NULL) +
  theme(legend.position = "bottom")

# Configuración de fondo blanco explícito en el guardado
ggsave("fig05_ocupacion_densidad.png", fig5, width = 7, height = 5, dpi = 300, bg = "white")
print(fig5)

# Test preliminar H1 (ocupación, no precio): contraste de hipótesis no paramétrico
test_ocup <- wilcox.test(tasa_ocupacion ~ hay_concierto,
                         data = tabla_diaria_airbnb %>% filter(!is.na(tasa_ocupacion)))
cat("\nTest Wilcoxon ocupación (días con/sin concierto):\n")
cat("  p-value:", format.pval(test_ocup$p.value, digits = 3), "\n")
cat("  Mediana con concierto:",
    round(median(tabla_diaria_airbnb$tasa_ocupacion[tabla_diaria_airbnb$hay_concierto], na.rm = TRUE), 3), "\n")
cat("  Mediana sin concierto:",
    round(median(tabla_diaria_airbnb$tasa_ocupacion[!tabla_diaria_airbnb$hay_concierto], na.rm = TRUE), 3), "\n")

# --- FIG 6: Ocupación por día de la semana (con/sin concierto) ---------------
fig6 <- tabla_diaria_airbnb %>%
  filter(!is.na(tasa_ocupacion)) %>%
  mutate(grupo = if_else(hay_concierto, "Con concierto", "Sin concierto")) %>%
  group_by(dia_semana, grupo) %>%
  summarise(ocup_media = mean(tasa_ocupacion), .groups = "drop") %>%
  ggplot(aes(x = dia_semana, y = ocup_media, fill = grupo)) +
  geom_col(position = "dodge") + # Gráfico de barras agrupado
  scale_fill_manual(values = c("Con concierto" = C_EVENTO, "Sin concierto" = C_BASE)) +
  scale_y_continuous(labels = percent_format()) +
  labs(title    = "Figura 6. Ocupación media de Airbnb por día de la semana",
       subtitle = "Comparativa días con y sin concierto | Calendar junio 2025",
       x = NULL, y = "Tasa de ocupación media", fill = NULL)

ggsave("fig06_ocupacion_dia_semana.png", fig6, width = 10, height = 5, dpi = 300)
print(fig6)

# --- Figura: tasa de ocupación vs número de conciertos por día ----------------
# Muestra que la relación no es lineal: muchos conciertos pequeños = baja ocupación;
# pocos grandes eventos = alta ocupación

fig_ocup_conc <- tabla_diaria_airbnb %>%
  filter(!is.na(tasa_ocupacion), anio >= 2022) %>%
  mutate(
    tipo_dia = case_when(
      n_gran_evento >= 2    ~ "≥2 grandes eventos",
      n_gran_evento == 1    ~ "1 gran evento",
      n_conciertos >= 10    ~ "≥10 conciertos (pequeños)",
      n_conciertos > 0      ~ "1-9 conciertos",
      TRUE                  ~ "Sin conciertos"
    ),
    tipo_dia = factor(tipo_dia,
                      levels = c("Sin conciertos", "1-9 conciertos",
                                 "≥10 conciertos (pequeños)",
                                 "1 gran evento", "≥2 grandes eventos"))
  ) %>%
  ggplot(aes(x = n_conciertos, y = tasa_ocupacion)) +
  geom_point(aes(color = tipo_dia), alpha = 0.4, size = 1.2) + # Gráfico de dispersión
  geom_smooth(method = "loess", se = TRUE,
              color = "black", linewidth = 0.9) +
  scale_color_manual(
    values = c("Sin conciertos"            = "grey70",
               "1-9 conciertos"            = C_BASE,
               "≥10 conciertos (pequeños)" = "#A8DADC",
               "1 gran evento"             = C_ACENTO,
               "≥2 grandes eventos"        = C_EVENTO),
    name = NULL
  ) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "Figura X. Tasa de ocupación de Airbnb en función del número de conciertos por día",
    subtitle = "Cada punto es un día | Línea LOESS con IC 95% | Calendar junio 2025",
    x = "Número de conciertos en el día",
    y = "Tasa de ocupación Airbnb"
  )

ggsave("fig_ocup_vs_conciertos.png", fig_ocup_conc,
       width = 10, height = 5, dpi = 300)
print(fig_ocup_conc)

# --- FIG 7: Ocupación hotelera y conciertos ----------------------------------

# Factor de escala utilizado para alinear visualmente los dos ejes (barras y línea)
escala_grafico <- 4.5

fig7 <- tabla_mensual %>%
  filter(anio >= 2019) %>%
  ggplot(aes(x = fecha_mes)) +
  
  # 1. Las barras de conciertos (C_EVENTO)
  geom_col(aes(y = n_conciertos_mes, fill = anio >= 2022), show.legend = FALSE, alpha = 0.85) +
  scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = C_EVENTO)) +
  
  # 2. La línea y puntos de ocupación (C_BASE) — Puntos siempre azules
  geom_line(aes(y = ocupacion_plazas * escala_grafico), color = C_BASE, linewidth = 0.9) +
  geom_point(aes(y = ocupacion_plazas * escala_grafico), color = C_BASE, size = 1.5, show.legend = FALSE) +
  
  # 3. Control de ejes (Tope 100%) a través del parámetro sec.axis
  scale_y_continuous(
    name = "Número de conciertos",
    limits = c(0, 100 * escala_grafico), 
    sec.axis = sec_axis(~ . / escala_grafico, name = "Ocupación por plazas",
                        breaks = seq(0, 100, by = 20),
                        labels = function(x) paste0(x, "%"))
  ) +
  
  labs(title    = "Figura 7. Ocupación hotelera y actividad concertística (2019–2026)",
       subtitle = "Barras: Nº de conciertos (Setlist.fm) | Línea: Ocupación hotelera por plazas (EOH, INE) | Período gris: anterior a datos de conciertos",
       x = NULL)

ggsave("fig07_ocupacion_conciertos.png", fig7, width = 12, height = 6, dpi = 300)
print(fig7)

# Gráfico de dispersión para observar el salto temporal (recuperación pandemia vs nueva etapa)
fig_scatter_mensual <- tabla_mensual %>%
  filter(!is.na(ocupacion_plazas)) %>%
  mutate(
    etapa = case_when(
      anio < 2022   ~ "Pre-pandemia / recuperación",
      anio == 2022  ~ "2022",
      anio == 2023  ~ "2023",
      anio == 2024  ~ "2024",
      TRUE          ~ "2025–2026"
    ),
    etapa = factor(etapa, levels = c("Pre-pandemia / recuperación",
                                     "2022", "2023", "2024", "2025–2026"))
  ) %>%
  ggplot(aes(x = fecha_mes, y = ocupacion_plazas,
             color = etapa, label = format(fecha_mes, "%b"))) +
  geom_point(size = 2.8, alpha = 0.85) +
  geom_smooth(aes(group = 1), method = "loess", se = FALSE,
              color = "grey40", linewidth = 0.7, linetype = "dashed") +
  # Línea base de referencia correspondiente a 2019
  geom_hline(yintercept = mean(tabla_mensual$ocupacion_plazas[tabla_mensual$anio >= 2019 &
                                                                tabla_mensual$anio < 2020],
                               na.rm = TRUE),
             linetype = "dotted", color = "grey60") +
  annotate("text", 
           x = as.Date("2020-06-01"), 
           y = mean(tabla_mensual$ocupacion_plazas[tabla_mensual$anio >= 2019 &
                                                     tabla_mensual$anio < 2020],
                    na.rm = TRUE) + 1.5,
           label = "Media pre-pandemia", 
           size = 3, 
           color = "grey50",
           hjust = 0) + 
  scale_color_manual(
    values = c("Pre-pandemia / recuperación" = "grey60",
               "2022" = "#A8DADC", "2023" = C_BASE,
               "2024" = C_ACENTO, "2025–2026" = C_EVENTO),
    name = NULL
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Figura X. Dispersión mensual de la ocupación hotelera (2019–2026)",
    subtitle = "Cada punto = un mes | Línea: tendencia LOESS | Referencia: media 2019",
    x = NULL, y = "Ocupación por plazas (%)"
  )

ggsave("fig_scatter_ocupacion_mensual.png", fig_scatter_mensual,
       width = 11, height = 5, dpi = 300)
print(fig_scatter_mensual)

# --- FIG 8: Viajeros nacionales vs. internacionales (mensual) ----------------
fig8 <- tabla_mensual %>%
  filter(anio >= 2019, !is.na(viajeros_total)) %>%
  select(fecha_mes, viajeros_espana, viajeros_extranjero) %>%
  pivot_longer(-fecha_mes, names_to = "tipo", values_to = "viajeros") %>%
  mutate(tipo = if_else(tipo == "viajeros_espana", "Nacionales", "Internacionales")) %>%
  ggplot(aes(x = fecha_mes, y = viajeros / 1e3, fill = tipo)) +
  geom_area(alpha = 0.7) + # Gráfico de área apilada
  scale_fill_manual(values = c("Nacionales" = C_BASE, "Internacionales" = C_ACENTO)) +
  scale_y_continuous(labels = function(x) paste0(x, " k")) +
  labs(title    = "Figura 8. Viajeros en hoteles de Madrid: nacionales vs. internacionales",
       subtitle = "Miles de viajeros mensuales | Fuente: Encuesta de Ocupación Hotelera, INE",
       x = NULL, y = "Viajeros (miles)", fill = NULL)

ggsave("fig08_viajeros_composicion.png", fig8, width = 12, height = 5, dpi = 300)
print(fig8)

# --- FIG 9: IPH + ocupación hotelera (doble eje) -----------------------------
iph_base <- tabla_mensual %>%
  filter(anio >= 2019, !is.na(iph_indice), !is.na(ocupacion_plazas))

# Cálculo dinámico del factor de escala para equilibrar visualmente ambos indicadores
escala <- max(iph_base$iph_indice, na.rm = TRUE) /
  max(iph_base$ocupacion_plazas, na.rm = TRUE)

fig9 <- iph_base %>%
  ggplot(aes(x = fecha_mes)) +
  geom_line(aes(y = iph_indice, color = "IPH (base 2008)"), linewidth = 0.9) +
  geom_line(aes(y = ocupacion_plazas * escala, color = "Ocupación hotelera"),
            linewidth = 0.9, linetype = "dashed") +
  scale_y_continuous(
    name     = "IPH (base 100 = 2008)",
    sec.axis = sec_axis(~ . / escala, name = "Ocupación por plazas (%)",
                        labels = function(x) paste0(x, "%"))
  ) +
  scale_color_manual(values = c("IPH (base 2008)" = C_EVENTO,
                                "Ocupación hotelera" = C_BASE)) +
  labs(title    = "Figura 9. Índice de Precios Hoteleros y ocupación por plazas",
       subtitle = "Fuente: INE | Ejes escalados para comparación visual",
       x = NULL, color = NULL)

ggsave("fig09_iph_ocupacion.png", fig9, width = 12, height = 5, dpi = 300)
print(fig9)

# --- FIG 10: Dispersión conciertos mensuales vs. viajeros --------------------
fig10 <- tabla_mensual %>%
  filter(anio >= 2022, !is.na(viajeros_total)) %>%
  ggplot(aes(x = n_conciertos_mes, y = viajeros_total / 1e3)) +
  geom_point(aes(color = factor(anio)), size = 2.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) + # Ajuste lineal
  scale_color_brewer(palette = "Set1", name = "Año") +
  scale_y_continuous(labels = function(x) paste0(x, " k")) +
  labs(title    = "Figura 10. Relación entre densidad de conciertos y viajeros en hoteles",
       subtitle = "Cada punto = un mes (2022–2026) | Línea: ajuste lineal | Fuentes: Setlist.fm e INE",
       x = "Nº de conciertos en el mes", y = "Viajeros hoteleros (miles)")

ggsave("fig10_conciertos_viajeros.png", fig10, width = 9, height = 5, dpi = 300)
print(fig10)

# Correlación de Spearman (incluir en texto) para apoyar la Fig. 10
cor_test <- cor.test(
  tabla_mensual$n_conciertos_mes[tabla_mensual$anio >= 2022],
  tabla_mensual$viajeros_total[tabla_mensual$anio >= 2022],
  method = "spearman", use = "complete.obs"
)
cat("\nCorrelación Spearman conciertos–viajeros (2022–2026):\n")
cat("  rho =", round(cor_test$estimate, 3),
    "| p-value =", format.pval(cor_test$p.value, digits = 3), "\n\n")

# --- FIG 11: Popularidad de artistas que actúan en Madrid -------------------
artistas_pop <- conciertos %>%
  distinct(artista) %>%
  left_join(kworb_dedup %>% select(artista, ranking), by = "artista") %>%
  mutate(
    # Creación de rangos ("buckets") para categorizar a los artistas por ranking
    grupo = case_when(
      ranking <= 100   ~ "Top 100 global",
      ranking <= 500   ~ "Top 101–500",
      ranking <= 1000  ~ "Top 501–1.000",
      ranking <= 3000  ~ "Top 1.001–3.000",
      TRUE             ~ "Fuera del top 3.000"
    ),
    grupo = factor(grupo, levels = c("Top 100 global","Top 101–500",
                                     "Top 501–1.000","Top 1.001–3.000",
                                     "Fuera del top 3.000"))
  ) %>%
  count(grupo)

fig11 <- artistas_pop %>%
  ggplot(aes(x = grupo, y = n, fill = grupo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.8) +
  scale_fill_manual(values = c("#E63946","#F4A261","#A8DADC","#457B9D","#d3d3d3")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Figura 11. Artistas que actuaron en Madrid clasificados por popularidad global en Spotify",
       subtitle = "Solo artistas presentes en el top 3.000 de Kworb por streams totales | Fuentes: Setlist.fm y Kworb",
       x = NULL, y = "Número de artistas únicos") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("fig11_popularidad_artistas.png", fig11, width = 9, height = 5, dpi = 300)
print(fig11)

# --- FIG 12: Precio Airbnb (listings) por barrio — top 20 barrios -----------
fig12 <- listings_clean %>%
  group_by(barrio) %>%
  summarise(
    n            = n(),
    precio_medio = mean(precio, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  filter(n >= 50) %>% # Se evitan sesgos en barrios con muy poca representación
  slice_max(precio_medio, n = 20) %>%
  mutate(barrio = fct_reorder(barrio, precio_medio)) %>%
  ggplot(aes(x = precio_medio, y = barrio)) +
  geom_col(fill = C_ACENTO) +
  geom_text(aes(label = paste0(round(precio_medio), "€")), hjust = -0.1, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = label_dollar(suffix = "€", prefix = "")) +
  labs(title    = "Figura 12. Top 20 barrios de Madrid por precio medio de Airbnb",
       subtitle = "Solo barrios con ≥ 50 alojamientos activos | Precio base del anuncio | Snapshot junio 2025",
       x = "Precio medio por noche (€)", y = NULL)

ggsave("fig12_precio_barrio.png", fig12, width = 10, height = 6, dpi = 300)
print(fig12)

# --- FIG 13: Matriz de correlaciones mensuales (Spearman) --------------------
vars_cor <- tabla_mensual %>%
  filter(anio >= 2022) %>%
  select(n_conciertos_mes, n_grandes_mes, max_listeners_mes,
         ocupacion_plazas, viajeros_total, viajeros_espana,
         viajeros_extranjero, pernoctaciones_total,
         iph_indice, ocupacion_plazas_aptos) %>%
  drop_na()

# Generación del gráfico de matriz de correlación solo si hay suficientes datos
if (nrow(vars_cor) >= 10) {
  cor_mat <- cor(vars_cor, method = "spearman")
  colnames(cor_mat) <- rownames(cor_mat) <- c(
    "Conciertos/mes", "Grandes eventos", "Max. listeners",
    "Ocup. hotelera", "Viajeros total", "Viajeros nac.",
    "Viajeros int.", "Pernoctaciones", "IPH", "Ocup. aptos."
  )
  png("fig13_correlaciones.png", width = 2200, height = 1800, res = 180)
  corrplot(cor_mat, method = "color", type = "upper",
           tl.cex = 0.75, tl.col = "black",
           addCoef.col = "black", number.cex = 0.65,
           col = colorRampPalette(c(C_BASE, "white", C_EVENTO))(200),
           title = "Figura 13. Correlaciones de Spearman (variables mensuales, 2022–2026)",
           mar = c(0, 0, 2, 0))
  dev.off()
  cat("Figura 13 guardada.\n")
} else {
  cat("Pocas observaciones completas para la matriz de correlaciones.\n")
}

# NUEVA REPRESENTACIÓN DE CORRELACIONES (Con matriz de significatividad estadística)
# Paso 1: matriz con p-values utilizando el paquete Hmisc
if (!require(Hmisc)) install.packages("Hmisc")
library(Hmisc)

mat_cor <- vars_cor %>% as.matrix()
cor_hmisc <- rcorr(mat_cor, type = "spearman")

cor_r <- cor_hmisc$r
cor_p <- cor_hmisc$P

# Rellenamos los NAs de la diagonal generados por rcorr con 0
diag(cor_p) <- 0

# Aplicar los nombres limpios a las nuevas matrices
nombres_limpios <- c(
  "Conciertos/mes", "Grandes eventos", "Max. listeners",
  "Ocup. hotelera", "Viajeros total", "Viajeros nac.",
  "Viajeros int.", "Pernoctaciones", "IPH", "Ocup. aptos."
)
colnames(cor_r) <- rownames(cor_r) <- nombres_limpios
colnames(cor_p) <- rownames(cor_p) <- nombres_limpios

# 2. Creamos la matriz de colores para el texto (control de significancia)
color_texto <- matrix("black", nrow = nrow(cor_r), ncol = ncol(cor_r))
color_texto[cor_p > 0.05] <- "grey60"  # Gris para las correlaciones no significativas

# 3. EXTRAEMOS LOS COLORES EN EL ORDEN EXACTO QUE BUSCA CORRPLOT
# Al usar type = "upper", corrplot lee un vector con el triángulo superior ordenado por columnas
vector_colores <- color_texto[upper.tri(color_texto, diag = TRUE)]

png("fig13_correlaciones_v2.png", width = 2400, height = 2000, res = 180)

# Pintamos el gráfico mejorado
corrplot(cor_r,
         method      = "color",
         type        = "upper",
         tl.cex      = 0.72,
         tl.col      = "black",
         addCoef.col = vector_colores, # Vector ordenado correctamente
         number.cex  = 0.89,
         col         = colorRampPalette(c(C_BASE, "white", C_EVENTO))(200),
         title       = "Figura 13. Correlaciones de Spearman (variables mensuales, 2022–2026)\nCifras en gris: no significativas al 5% (p > 0.05)",
         mar         = c(0, 0, 3, 0))

dev.off()

cat("Figura 13 ejecutada correctamente de forma limpia.\n")

# --- FIG 14: Ridgeline precio Airbnb por mes (listings × availability_30) ---
# Proxy de precio efectivo: precio_base × (1 - availability_30/30)
fig14 <- listings_clean %>%
  mutate(
    mes_escrapado = month(as.Date("2025-06-27")),  # fecha del scrape
    precio_filtrado = if_else(precio <= 400, precio, NA_real_)
  ) %>%
  filter(!is.na(precio_filtrado)) %>%
  # Para el ridgeline usamos el precio base directamente (no hay variación temporal)
  # y la distribución por distrito como proxy de heterogeneidad espacial
  ggplot(aes(x = precio_filtrado, y = fct_reorder(distrito, precio_filtrado, median),
             fill = stat(x))) +
  geom_density_ridges_gradient(scale = 2.5, rel_min_height = 0.01, bandwidth = 15) +
  scale_fill_gradientn(colors = c(C_BASE, "white", C_EVENTO), name = "€") +
  scale_x_continuous(labels = label_dollar(suffix = "€", prefix = "")) +
  labs(title    = "Figura 14. Distribución del precio base de Airbnb por distrito de Madrid",
       subtitle = "Ridgeline plot | Precio del anuncio (listings) | Snapshot junio 2025",
       x = "Precio por noche (€)", y = NULL)

ggsave("fig14_ridgeline_precio_distrito.png", fig14, width = 10, height = 8, dpi = 300)
print(fig14)

# --- FIG 14b: Mapa coroplético precio Airbnb por barrio + venues ----------

# Coordenadas CORREGIDAS de los principales recintos (CRS WGS84)
# Definidas aquí directamente para no depender del Cap. 4
# Todos los recintos del top 20 + estadios principales
# Coordenadas verificables en Google Maps
venues_cap3 <- tribble(
  ~recinto,                           ~lat,      ~lon,
  "Movistar Arena",                    40.4239,  -3.6716,
  "Estadio Santiago Bernabéu",         40.4530,  -3.6883,
  "Estadio Cívitas Metropolitano",     40.4361,  -3.5995,
  "La Riviera",                        40.4137,  -3.7226,
  "Palacio Vistalegre",                40.3862,  -3.7379,
  "IFEMA Madrid",                      40.4678,  -3.6166,
  "Recinto Valdebebas",                40.4786,  -3.6165,
  "Caja Mágica",                       40.3697,  -3.6830,
  "Sala Mon Live",                     40.4346,  -3.7153,
  "Sala El Sol",                       40.4199,  -3.7011,
  "Joy Eslava",                        40.4168,  -3.7061,
  "Sala But",                          40.4223,  -3.6981,
  "Wurlitzer Ballroom",                40.4188,  -3.7102,
  "Real Jardín Botánico",              40.4112,  -3.6921,
  "Sala Copérnico",                    40.4337,  -3.6905,
  "Sala Nazca",                        40.4381,  -3.7135,
  "Moby Dick Club",                    40.4464,  -3.7021,
  "Revi Live",                         40.4096,  -3.6771,
  "Sala Villanos",                     40.4091,  -3.7012,
  "Changó Live",                       40.4257,  -3.7001,
  "Sala Siroco",                       40.4247,  -3.7157,
  "Sala Clamores",                     40.4350,  -3.7005,
  "Shoko Madrid",                      40.3975,  -3.6935
)

# Nº de conciertos por venue (se usará para la rampa de tamaño de los puntos)
conciertos_por_venue <- conciertos %>%
  count(recinto_canonico, name = "n_conciertos")

# Conversión a objeto espacial sf (Simple Features)
venues_sf_cap3 <- venues_cap3 %>%
  left_join(conciertos_por_venue,
            by = c("recinto" = "recinto_canonico")) %>%
  mutate(n_conciertos = replace_na(n_conciertos, 0)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Precio mediano por barrio (aplicando consistencia con Fig 12)
precio_barrio <- listings_clean %>%
  filter(!is.na(precio), precio > 0) %>%
  group_by(barrio) %>%
  summarise(precio_mediano = median(precio, na.rm = TRUE),
            n_listings     = n(),
            .groups = "drop") %>%
  # SILENCIAMOS LOS BARRIOS CON POCOS LISTINGS (< 50) asignando NA
  mutate(precio_mediano = if_else(n_listings >= 50, precio_mediano, NA_real_))

# Unir precio al geojson de barrios importado previamente
# geo_neigh_06 tiene columna "neighbourhood" con el nombre del barrio
mapa_precios <- geo_neigh_06 %>%
  left_join(precio_barrio,
            by = c("neighbourhood" = "barrio"))

# Construcción de la figura del mapa coroplético
fig_mapa <- ggplot() +
  geom_sf(data = mapa_precios,
          aes(fill = precio_mediano),
          color = "white", linewidth = 0.15) +
  scale_fill_gradient(
    low      = "#d0e4f5",
    high     = C_EVENTO,
    name     = "Precio mediano\n(€/noche)",
    na.value = "grey88",
    labels   = function(x) paste0(x, "€")
  ) +
  geom_sf(data  = venues_sf_cap3,
          aes(size = n_conciertos),
          shape = 21,
          fill  = C_ACENTO,
          color = "black",
          alpha = 0.85,
          stroke = 0.5) +
  scale_size_continuous(
    name   = "Nº conciertos\n(2022–2026)",
    range  = c(2, 12),
    breaks = c(100, 300, 600, 1000)
  ) +
  coord_sf(
    xlim = c(-3.90, -3.52), # Recorte de coordenadas para centrar la ciudad
    ylim = c(40.32, 40.57)
  ) +
  labs(
    title    = "Figura 14b. Precio mediano de Airbnb por barrio y actividad concertística en Madrid",
    subtitle = "Coroplético: precio mediano del anuncio (€/noche) | Puntos: venues, tamaño ∝ nº conciertos 2022–2026",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = C_FONDO, color = NA),
    panel.grid       = element_line(color = "grey90", linewidth = 0.3),
    axis.text        = element_text(size = 8, color = "grey50"),
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(color = "grey40", size = 9)
  )

ggsave("fig14b_mapa_precios_venues.png", fig_mapa,
       width = 11, height = 9, dpi = 300)
print(fig_mapa)

# =============================================================================
# BLOQUE 4 — IMPORTANCIA DE VARIABLES (Random Forest)
# =============================================================================

# Variable dependiente: tasa de ocupación diaria de Airbnb
# (precio no disponible en el calendar; si se consiguiese en el futuro,
#  reemplazar tasa_ocupacion por log(precio_medio))

# Preparación de datos para el modelo Random Forest
df_rf <- tabla_diaria_airbnb %>%
  filter(!is.na(tasa_ocupacion), anio >= 2022) %>%
  mutate(
    # Transformación logarítmica para atenuar asimetrías fuertes en listeners y aforo
    log_max_listeners = log10(replace_na(max_listeners, 1) + 1),
    log_max_aforo     = log10(replace_na(max_aforo, 1) + 1),
    hay_gran_evento   = as.integer(n_gran_evento > 0)
  ) %>%
  select(tasa_ocupacion, n_conciertos, hay_gran_evento,
         log_max_listeners, log_max_aforo,
         dia_semana_num, mes, es_festivo, es_finde, anio) %>%
  drop_na()

cat("\nObservaciones para el Random Forest:", nrow(df_rf), "\n")

# Entrenamiento del modelo Random Forest
set.seed(42) # Semilla de reproducibilidad
rf_mod <- ranger(tasa_ocupacion ~ ., data = df_rf,
                 num.trees = 500, importance = "impurity")

cat("R² del modelo (tasa_ocupacion):", round(rf_mod$r.squared, 3), "\n\n")

# Visualización de la importancia de cada variable en el modelo (Gini)
fig15 <- vip(rf_mod, num_features = 10, geom = "col",
             aesthetics = list(fill = C_ACENTO)) +
  labs(title    = "Figura 15. Importancia de variables — Random Forest sobre tasa de ocupación de Airbnb",
       subtitle = "Métrica: reducción de impureza (Gini) | 500 árboles | Período 2022–2026",
       y = "Importancia (reducción de impureza)", x = NULL)

ggsave("fig15_importancia_variables.png", fig15, width = 9, height = 5, dpi = 300)
print(fig15)

# --- FIG 16: Cuota de mercado por tipo de alojamiento reglado ----------------

fig16 <- tabla_cuota_mercado %>%
  filter(!is.na(cuota)) %>%   
  mutate(tipo = factor(tipo, levels = c("Hoteles",
                                        "Apartamentos turísticos",
                                        "Resto (rural, campings, albergues)"))) %>%
  ggplot(aes(x = fecha_mes, y = cuota, fill = tipo)) +
  geom_area(alpha = 0.85) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("Hoteles"                         = C_BASE,
                               "Apartamentos turísticos"         = C_ACENTO,
                               "Resto (rural, campings, albergues)" = "#A8DADC")) +
  labs(title    = "Figura 16. Cuota de mercado por tipo de alojamiento reglado en Madrid (2019–2026)",
       subtitle = "% de viajeros totales en alojamientos reglados | Fuente: INE (Tabla 2941)",
       x = NULL, y = "Proporción de viajeros", fill = NULL)

ggsave("fig16_cuota_mercado.png", fig16, width = 12, height = 5, dpi = 300)
print(fig16)

# Extracción tabular de la importancia del RF para documentar en la memoria
tabla_imp <- vi(rf_mod) %>%
  mutate(
    Variable = recode(Variable,
                      "dia_semana_num"    = "Día de la semana",
                      "mes"               = "Mes del año",
                      "anio"              = "Año",
                      "n_conciertos"      = "Nº de conciertos en el día",
                      "hay_gran_evento"   = "Gran evento (dummy)",
                      "log_max_listeners" = "log10(oyentes mensuales máximos)",
                      "log_max_aforo"     = "log10(aforo del recinto)",
                      "es_festivo"        = "Día festivo",
                      "es_finde"          = "Fin de semana"
    ),
    Importancia_relativa = round(Importance / max(Importance) * 100, 1)
  ) %>%
  arrange(desc(Importance)) %>%
  select(Variable, Importancia_relativa)

cat("Importancia relativa de variables (sobre ocupación Airbnb):\n")
print(tabla_imp)


# =============================================================================
# RESUMEN FINAL
# =============================================================================
cat("\n", strrep("=", 60), "\n")
cat("FIGURAS GUARDADAS (16 archivos .png en el directorio de trabajo):\n")
for (i in sprintf("fig%02d", 1:16)) cat(" ", i, "\n")
cat(strrep("=", 60), "\n")
cat("NOTA SOBRE PRECIO AIRBNB:\n")
cat("  Las columnas price y adjusted_price del calendar están vacías\n")
cat("  en ambos snapshots de Inside Airbnb Madrid.\n")
cat("  Alternativas para el Capítulo 4:\n")
cat("  (a) Usar precio base del listing como variable dependiente\n")
cat("      en una regresión cross-sectional con efectos de barrio.\n")
cat("  (b) Contactar con Inside Airbnb para versiones históricas\n")
cat("      con precio diario en el calendar.\n")
cat("  (c) Usar la tasa de ocupación (disponible) como variable\n")
cat("      principal y el precio base como control estático.\n")
cat(strrep("=", 60), "\n")
