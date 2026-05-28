# =============================================================================
# TFG: Impacto socioeconómico de los conciertos en Madrid
# Script 00 — Importación y limpieza de todos los datasets
# =============================================================================


# -----------------------------------------------------------------------------
# BLOQUE A — RUTAS LOCALES
# -----------------------------------------------------------------------------

RUTA_BASE <- "~/Library/Mobile Documents/com~apple~CloudDocs/Universidad/4º/TFG/Bases de datos"

ruta_conciertos  <- file.path(RUTA_BASE, "setlist.fm/dataset/completo/conciertos_madrid_2022_2026.csv")
ruta_kworb       <- file.path(RUTA_BASE, "kworb.net/kworb_artists_careful.csv")
ruta_listeners   <- file.path(RUTA_BASE, "kworb.net/kworb_listeners.csv")
ruta_daily       <- file.path(RUTA_BASE, "kworb.net/kworb_spain_daily_totals.csv")
ruta_weekly      <- file.path(RUTA_BASE, "kworb.net/kworb_spain_weekly_totals.csv")
ruta_airbnb_base <- file.path(RUTA_BASE, "InsideAirbnb")  # subcarpetas: 2025-06 y 2025-09


# -----------------------------------------------------------------------------
# BLOQUE B — PAQUETES
# -----------------------------------------------------------------------------

paquetes <- c("httr", "readxl", "readr", "dplyr", "tidyr",
              "lubridate", "stringr", "sf")

for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(httr)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(sf)

cat("Paquetes cargados.\n\n")


# -----------------------------------------------------------------------------
# BLOQUE C — CALENDARIO LABORAL DE MADRID
# Fuente: Portal de Datos Abiertos del Ayuntamiento de Madrid (URL directa)
# Objeto generado: df_final
# -----------------------------------------------------------------------------

cat("[1/8] Descargando Calendario Laboral de Madrid...\n")

url_calendario <- "https://datos.madrid.es/egob/catalogo/300082-6-calendario_laboral.xls"
tf_calendario  <- tempfile(fileext = ".xlsx")

GET(url_calendario, user_agent("Mozilla/5.0"), write_disk(tf_calendario, overwrite = TRUE))

df_final <- read_excel(tf_calendario, guess_max = 1000) %>%
  rename(
    fecha        = 1,
    dia_semana   = 2,
    estado       = 3,
    tipo_festivo = 4,
    festividad   = 5
  ) %>%
  mutate(
    fecha      = as.Date(fecha),
    dia_semana = str_to_title(dia_semana) %>%
                 str_replace("Miercoles", "Miércoles") %>%
                 str_replace("Sabado",    "Sábado"),
    estado     = str_to_title(estado) %>%
                 str_replace("Sabado", "Sábado"),
    es_laborable = (estado == "Laborable"),
    festividad   = if_else(is.na(festividad), "No Festivo", festividad),
    tipo_festivo = case_when(
      str_detect(tipo_festivo, "Traslado") ~ "Festivo de la Comunidad de Madrid",
      is.na(tipo_festivo)                  ~ "No Festivo",
      TRUE                                 ~ tipo_festivo
    )
  )

cat("   OK — df_final:", nrow(df_final), "días |",
    as.character(min(df_final$fecha)), "→", as.character(max(df_final$fecha)), "\n\n")


# -----------------------------------------------------------------------------
# BLOQUE D — SETLIST.FM (conciertos Madrid 2022–2026)
# Fuente: fichero CSV local generado por web scraping propio
# Objeto generado: df_conciertos_final
# -----------------------------------------------------------------------------

cat("[2/8] Cargando conciertos de Setlist.fm...\n")

df_conciertos_final <- read_csv(
  ruta_conciertos,
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  select(-ciudad, -pais) %>%
  mutate(
    fecha        = dmy(fecha),
    artista      = str_trim(artista),
    recinto      = str_trim(recinto),
    recinto      = if_else(is.na(recinto),     "No especificado", recinto),
    info_evento  = if_else(is.na(info_evento), "Sin info",        info_evento),
    tour         = if_else(is.na(tour),        "Sin tour",        tour),
    tiene_alias  = !is.na(artista_alias)
  )

cat("   OK — df_conciertos_final:", nrow(df_conciertos_final), "conciertos |",
    as.character(min(df_conciertos_final$fecha)), "→",
    as.character(max(df_conciertos_final$fecha)), "\n\n")


# -----------------------------------------------------------------------------
# BLOQUE E — KWORB (streams globales, listeners, charts Spain)
# Fuente: ficheros CSV locales generados por web scraping propio
# Objetos generados: df_kworb_final, df_listeners_final,
#                    df_daily_final, df_weekly_final
# -----------------------------------------------------------------------------

cat("[3/8] Cargando datos de Kworb / Spotify...\n")

# -- E1. Streams globales
df_kworb_final <- read_csv(
  ruta_kworb,
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  mutate(
    as_lead    = coalesce(as_lead,    0),
    solo       = coalesce(solo,       0),
    as_feature = coalesce(as_feature, 0),
    # Expansión de millones a cifras absolutas
    streams    = streams    * 1e6,
    daily      = daily      * 1e6,
    as_lead    = as_lead    * 1e6,
    solo       = solo       * 1e6,
    as_feature = as_feature * 1e6
  )

cat("   OK — df_kworb_final:", nrow(df_kworb_final), "artistas\n")

# -- E2. Monthly listeners
df_listeners_final <- read_csv(
  ruta_listeners,
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  mutate(
    ranking        = as.integer(ranking),
    listeners      = as.numeric(listeners),
    daily_change   = as.numeric(daily_change),
    peak_pos       = as.integer(peak_pos),
    peak_listeners = as.numeric(peak_listeners)
  )

cat("   OK — df_listeners_final:", nrow(df_listeners_final), "artistas\n")

# -- E3. Spain Daily chart totals
df_daily_final <- read_csv(
  ruta_daily,
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  select(-spotify_id, -peak_veces, -peak_streams) %>%
  mutate(
    dias_top10     = coalesce(dias_top10, 0L),
    artista        = str_trim(str_remove(artista, " -$")),
    titulo         = coalesce(titulo, "Sin Título Especificado"),
    dias           = as.integer(dias),
    dias_top10     = as.integer(dias_top10),
    peak_pos       = as.integer(peak_pos),
    total_streams  = as.numeric(total_streams)
  )

cat("   OK — df_daily_final:", nrow(df_daily_final), "canciones\n")

# -- E4. Spain Weekly chart totals
df_weekly_final <- read_csv(
  ruta_weekly,
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  select(-spotify_id, -peak_veces, -peak_streams) %>%
  mutate(
    semanas_top10  = coalesce(semanas_top10, 0L),
    artista        = str_trim(str_remove(artista, " -$")),
    titulo         = coalesce(titulo, "Sin Título Especificado"),
    semanas        = as.integer(semanas),
    semanas_top10  = as.integer(semanas_top10),
    peak_pos       = as.integer(peak_pos),
    total_streams  = as.numeric(total_streams)
  )

cat("   OK — df_weekly_final:", nrow(df_weekly_final), "canciones\n\n")


# -----------------------------------------------------------------------------
# BLOQUE F — INSIDE AIRBNB (snapshots junio y septiembre 2025)
# Fuente: ficheros locales descargados de insideairbnb.com
# Objetos generados: df_listings_06, df_calendar_06, geo_neigh_06,
#                    df_reviews_06, df_reviews_sum_06  (ídem _09)
# -----------------------------------------------------------------------------

cat("[4/8] Cargando datos de Inside Airbnb...\n")

procesar_airbnb <- function(snapshot_mes) {
  path_base <- file.path(ruta_airbnb_base, snapshot_mes, "")

  df_listings <- read_csv(
    paste0(path_base, "listings.csv.gz"),
    show_col_types = FALSE
  ) %>%
    mutate(
      price                  = parse_number(price),
      neighbourhood_cleansed = str_trim(neighbourhood_cleansed),
      property_type          = str_trim(property_type),
      room_type              = str_trim(room_type)
    ) %>%
    filter(!is.na(price), price > 0, price <= 1500, minimum_nights <= 30)

  ids_validos <- unique(df_listings$id)

  df_calendar <- read_csv(
    paste0(path_base, "calendar.csv.gz"),
    col_types     = cols(price = col_character(), adjusted_price = col_character(), .default = col_guess()),
    show_col_types = FALSE
  ) %>%
    mutate(
      price          = parse_number(price),
      adjusted_price = parse_number(adjusted_price)
    ) %>%
    filter(listing_id %in% ids_validos, is.na(price) | (price > 0 & price <= 1500))

  geo_neigh <- st_read(paste0(path_base, "neighbourhoods.geojson"), quiet = TRUE) %>%
    mutate(neighbourhood = str_trim(neighbourhood))

  df_reviews <- read_csv(
    paste0(path_base, "reviews.csv.gz"),
    show_col_types = FALSE
  ) %>%
    filter(listing_id %in% ids_validos)

  df_reviews_sum <- read_csv(
    paste0(path_base, "reviews-summary.csv"),
    show_col_types = FALSE
  ) %>%
    filter(listing_id %in% ids_validos)

  list(listings = df_listings, calendar = df_calendar, geo = geo_neigh,
       reviews = df_reviews, reviews_sum = df_reviews_sum)
}

airbnb_06 <- procesar_airbnb("2025-06")
df_listings_06    <- airbnb_06$listings
df_calendar_06    <- airbnb_06$calendar
geo_neigh_06      <- airbnb_06$geo
df_reviews_06     <- airbnb_06$reviews
df_reviews_sum_06 <- airbnb_06$reviews_sum
rm(airbnb_06)

cat("   OK — Junio 2025 | listings:", nrow(df_listings_06),
    "| filas calendar:", nrow(df_calendar_06), "\n")

airbnb_09 <- procesar_airbnb("2025-09")
df_listings_09    <- airbnb_09$listings
df_calendar_09    <- airbnb_09$calendar
geo_neigh_09      <- airbnb_09$geo
df_reviews_09     <- airbnb_09$reviews
df_reviews_sum_09 <- airbnb_09$reviews_sum
rm(airbnb_09)

cat("   OK — Septiembre 2025 | listings:", nrow(df_listings_09),
    "| filas calendar:", nrow(df_calendar_09), "\n\n")


# -----------------------------------------------------------------------------
# BLOQUE G — INE (fuentes remotas, todas desde URLs directas del INE)
# Objetos generados: df_ine_aptos_tur, df_ine_hotel_capacidad,
#                    df_ine_hotel_viajeros, df_ine_familitur,
#                    df_ine_frontur, df_ine_iph, df_ine_alojamiento_global
# -----------------------------------------------------------------------------

cat("[5/8] Descargando INE — Apartamentos turísticos...\n")

df_ine_aptos_tur <- read_csv2(
  "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/2021.csv",
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  filter(`Comunidades y Ciudades Autónomas` == "13 Madrid, Comunidad de") %>%
  mutate(
    Total = if_else(Total == "..", NA_character_, Total),
    Total = parse_number(Total, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  select(Periodo, `Apartamentos y personal empleado`, Total) %>%
  pivot_wider(names_from = `Apartamentos y personal empleado`, values_from = Total) %>%
  rename(
    plazas_estimadas        = `Número de plazas estimadas`,
    aptos_estimados         = `Número de apartamentos estimados`,
    ocupacion_plazas        = `Grado de ocupación por plazas`,
    ocupacion_plazas_finde  = `Grado de ocupación por plazas en fin de semana`,
    ocupacion_aptos         = `Grado de ocupación por apartamentos`,
    ocupacion_aptos_finde   = `Grado de ocupación por apartamentos en fin de semana`,
    personal_empleado       = `Personal empleado`
  ) %>%
  mutate(fecha = ym(Periodo)) %>%
  relocate(fecha, Periodo) %>%
  arrange(fecha)

cat("   OK — df_ine_aptos_tur:", nrow(df_ine_aptos_tur), "meses\n")

# ----

cat("[6/8] Descargando INE — Encuesta de Ocupación Hotelera (capacidad + viajeros)...\n")

df_ine_hotel_capacidad <- read_csv2(
  "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/2066.csv",
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  filter(
    `Comunidades y Ciudades Autónomas` == "13 Madrid, Comunidad de",
    is.na(Provincias)
  ) %>%
  mutate(
    Total = if_else(Total == "..", NA_character_, as.character(Total)),
    Total = parse_number(Total, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  select(Periodo, `Establecimientos y personal empleado (plazas)`, Total) %>%
  pivot_wider(names_from = `Establecimientos y personal empleado (plazas)`, values_from = Total) %>%
  rename(
    establecimientos        = `Número de establecimientos abiertos estimados`,
    plazas_estimadas        = `Número de plazas estimadas`,
    habitaciones_estimadas  = `Número de habitaciones estimadas`,
    ocupacion_plazas        = `Grado de ocupación por plazas`,
    ocupacion_plazas_finde  = `Grado de ocupación por plazas en fin de semana`,
    ocupacion_habitaciones  = `Grado de ocupación por habitaciones`,
    personal_empleado       = `Personal empleado`
  ) %>%
  mutate(fecha = ym(Periodo)) %>%
  relocate(fecha, Periodo) %>%
  arrange(fecha)

df_ine_hotel_viajeros <- read_csv2(
  "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/2074.csv",
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  filter(
    `Comunidades y Ciudades Autónomas` == "13 Madrid, Comunidad de",
    is.na(Provincias)
  ) %>%
  mutate(
    Total = if_else(Total == "..", NA_character_, as.character(Total)),
    Total = parse_number(Total, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  mutate(
    residencia_limpia  = if_else(is.na(`Residencia: Nivel 2`), "Total", `Residencia: Nivel 2`),
    metrica_compuesta  = paste(`Viajeros y pernoctaciones`, residencia_limpia, sep = "_")
  ) %>%
  select(Periodo, metrica_compuesta, Total) %>%
  pivot_wider(names_from = metrica_compuesta, values_from = Total) %>%
  rename(
    viajeros_total             = `Viajero_Total`,
    viajeros_espana            = `Viajero_Residentes en España`,
    viajeros_extranjero        = `Viajero_Residentes en el Extranjero`,
    pernoctaciones_total       = `Pernoctaciones_Total`,
    pernoctaciones_espana      = `Pernoctaciones_Residentes en España`,
    pernoctaciones_extranjero  = `Pernoctaciones_Residentes en el Extranjero`
  ) %>%
  mutate(fecha = ym(Periodo)) %>%
  relocate(fecha, Periodo) %>%
  arrange(fecha)

cat("   OK — df_ine_hotel_capacidad:", nrow(df_ine_hotel_capacidad), "meses\n")
cat("   OK — df_ine_hotel_viajeros:", nrow(df_ine_hotel_viajeros), "meses\n")

# ----

cat("[7/8] Descargando INE — Familitur y Frontur...\n")

df_ine_familitur <- read_csv2(
  "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/24927.csv",
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  filter(Destino == "13 Madrid, Comunidad de") %>%
  mutate(
    Total = if_else(Total == "..", NA_character_, as.character(Total)),
    Total = parse_number(Total, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  mutate(
    concepto_clean = case_when(
      `Concepto turístico` == "Viajes"                       ~ "viajes",
      `Concepto turístico` == "Duración media de los viajes" ~ "duracion_media",
      `Concepto turístico` == "Pernoctaciones"               ~ "pernoctaciones",
      `Concepto turístico` == "Gasto total"                  ~ "gasto_total",
      `Concepto turístico` == "Gasto medio por persona"      ~ "gasto_pax",
      `Concepto turístico` == "Gasto medio diario por persona" ~ "gasto_diario_pax",
      TRUE ~ "otro"
    ),
    tipo_clean = case_when(
      `Tipo de dato` == "Valor absoluto"         ~ "abs",
      `Tipo de dato` == "Distribución porcentual" ~ "pct",
      `Tipo de dato` == "Variación anual"         ~ "var",
      TRUE ~ "otro"
    ),
    metrica_compuesta = paste(concepto_clean, tipo_clean, sep = "_")
  ) %>%
  select(Periodo, metrica_compuesta, Total) %>%
  pivot_wider(names_from = metrica_compuesta, values_from = Total) %>%
  mutate(
    anio  = as.integer(Periodo),
    fecha = make_date(anio, 1, 1)
  ) %>%
  select(-Periodo) %>%
  relocate(fecha, anio) %>%
  arrange(fecha)

df_ine_frontur <- read_csv2(
  "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/23988.csv",
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  filter(`Comunidades autónomas` == "13 Madrid, Comunidad de") %>%
  mutate(
    Total = if_else(Total == "..", NA_character_, as.character(Total)),
    Total = parse_number(Total, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  mutate(
    tipo_clean = case_when(
      `Tipo de dato` == "Dato base"               ~ "internacional_abs",
      `Tipo de dato` == "Tasa de variación anual" ~ "internacional_var",
      TRUE ~ "otro"
    )
  ) %>%
  select(Periodo, tipo_clean, Total) %>%
  pivot_wider(names_from = tipo_clean, values_from = Total) %>%
  mutate(
    anio  = as.integer(Periodo),
    fecha = make_date(anio, 1, 1)
  ) %>%
  select(-Periodo) %>%
  relocate(fecha, anio) %>%
  arrange(fecha)

cat("   OK — df_ine_familitur:", nrow(df_ine_familitur), "años\n")
cat("   OK — df_ine_frontur:",   nrow(df_ine_frontur),   "años\n")

# ----

cat("[8/8] Descargando INE — IPH e Índice Global de Alojamientos...\n")

df_ine_iph <- read_csv2(
  "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/12156.csv",
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  filter(`Comunidades y Ciudades Autónomas` == "13 Madrid, Comunidad de") %>%
  mutate(
    Total = if_else(Total == "..", NA_character_, as.character(Total)),
    Total = parse_number(Total, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  mutate(
    tipo_clean = case_when(
      str_detect(`Tipo de dato`, "Índice") ~ "iph_indice",
      str_detect(`Tipo de dato`, "Tasa")   ~ "iph_var_anual",
      TRUE ~ "otro"
    )
  ) %>%
  select(Periodo, tipo_clean, Total) %>%
  pivot_wider(names_from = tipo_clean, values_from = Total) %>%
  mutate(fecha = ym(Periodo)) %>%
  relocate(fecha, Periodo) %>%
  arrange(fecha)

df_ine_alojamiento_global <- read_csv2(
  "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/2941.csv",
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  filter(`Comunidades y Ciudades Autónomas` == "13 Madrid, Comunidad de") %>%
  mutate(
    Total = if_else(Total %in% c(".", ".."), NA_character_, as.character(Total)),
    Total = parse_number(Total, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  mutate(
    tipo_aloj_clean = case_when(
      grepl("Hotelera",    `Tipo de alojamiento`) ~ "hotel",
      grepl("Apartamentos",`Tipo de alojamiento`) ~ "apto",
      grepl("Campings",    `Tipo de alojamiento`) ~ "camping",
      grepl("Rural",       `Tipo de alojamiento`) ~ "rural",
      grepl("Albergues",   `Tipo de alojamiento`) ~ "albergue",
      TRUE ~ "otro"
    ),
    residencia_clean = case_when(
      !is.na(`Residencia: Nivel 2`) & grepl("España",     `Residencia: Nivel 2`) ~ "espana",
      !is.na(`Residencia: Nivel 2`) & grepl("Extranjero", `Residencia: Nivel 2`) ~ "extranjero",
      TRUE ~ "total"
    ),
    metrica_clean = if_else(grepl("Viajero", `Viajeros y pernoctaciones`), "viajeros", "pernoctaciones"),
    col_final = paste(tipo_aloj_clean, metrica_clean, residencia_clean, sep = "_")
  ) %>%
  select(Periodo, col_final, Total) %>%
  pivot_wider(names_from = col_final, values_from = Total) %>%
  mutate(fecha = ym(Periodo)) %>%
  relocate(fecha, Periodo) %>%
  arrange(fecha)

cat("   OK — df_ine_iph:", nrow(df_ine_iph), "meses\n")
cat("   OK — df_ine_alojamiento_global:", nrow(df_ine_alojamiento_global), "meses\n\n")


# -----------------------------------------------------------------------------
# RESUMEN FINAL — confirmación de todos los objetos en el entorno
# -----------------------------------------------------------------------------

cat(strrep("=", 65), "\n")
cat("IMPORTACIÓN COMPLETADA — objetos disponibles en el entorno:\n")
cat(strrep("=", 65), "\n")

objetos_tfg <- c(
  "df_final", "df_conciertos_final",
  "df_kworb_final", "df_listeners_final", "df_daily_final", "df_weekly_final",
  "df_listings_06", "df_calendar_06", "geo_neigh_06", "df_reviews_06", "df_reviews_sum_06",
  "df_listings_09", "df_calendar_09", "geo_neigh_09", "df_reviews_09", "df_reviews_sum_09",
  "df_ine_aptos_tur", "df_ine_hotel_capacidad", "df_ine_hotel_viajeros",
  "df_ine_familitur", "df_ine_frontur", "df_ine_iph", "df_ine_alojamiento_global"
)

for (obj in objetos_tfg) {
  if (exists(obj)) {
    x <- get(obj)
    dims <- if (inherits(x, "data.frame")) paste0(nrow(x), " × ", ncol(x)) else class(x)[1]
    cat(sprintf("  %-35s %s\n", obj, dims))
  } else {
    cat(sprintf("  %-35s *** NO CARGADO ***\n", obj))
  }
}

cat(strrep("=", 65), "\n")
cat("Listo. Puedes ejecutar tfg_analisis_completo.R a continuación.\n")
