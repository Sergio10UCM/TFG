# =============================================================================
# Script Importación y limpieza de todos los datasets
# =============================================================================
# INSTRUCCIONES:
#   1. Ajustar las rutas locales del Bloque A si la estructura de carpetas
#      es distinta. Las fuentes remotas (INE, Calendario) no requieren cambios.
#   2. Ejecutar el script completo de una vez (Ctrl+Shift+Enter en RStudio).
#   3. Al final se encuentra un resumen con los objetos cargados y su tamaño.
# =============================================================================


# -----------------------------------------------------------------------------
# BLOQUE A — RUTAS LOCALES (ajusta aquí si cambia tu estructura de carpetas)
# -----------------------------------------------------------------------------

# Ruta base relativa (MODIFICAR por la ruta real donde se alojen los datos)
RUTA_BASE <- "./datos"

# Definición de las rutas específicas concatenando la ruta base con el nombre del archivo
ruta_conciertos  <- file.path(RUTA_BASE, "setlist.fm/dataset/completo/conciertos_madrid_2022_2026.csv")
ruta_kworb       <- file.path(RUTA_BASE, "kworb.net/kworb_artists_careful.csv")
ruta_listeners   <- file.path(RUTA_BASE, "kworb.net/kworb_listeners.csv")
ruta_daily       <- file.path(RUTA_BASE, "kworb.net/kworb_spain_daily_totals.csv")
ruta_weekly      <- file.path(RUTA_BASE, "kworb.net/kworb_spain_weekly_totals.csv")
ruta_airbnb_base <- file.path(RUTA_BASE, "InsideAirbnb")  # subcarpetas: 2025-06 y 2025-09


# -----------------------------------------------------------------------------
# BLOQUE B — PAQUETES
# -----------------------------------------------------------------------------

# Vector con los nombres de los paquetes necesarios para el script
paquetes <- c("httr", "readxl", "readr", "dplyr", "tidyr",
              "lubridate", "stringr", "sf")

# Bucle para instalar los paquetes automáticamente si no están presentes en el sistema
for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# Carga de las librerías en el entorno de trabajo
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

# URL directa al archivo Excel del calendario laboral
url_calendario <- "https://datos.madrid.es/egob/catalogo/300082-6-calendario_laboral.xls"
# Creación de un archivo temporal para almacenar la descarga
tf_calendario  <- tempfile(fileext = ".xlsx")

# Petición GET para descargar el archivo, simulando un navegador (user_agent) para evitar bloqueos
GET(url_calendario, user_agent("Mozilla/5.0"), write_disk(tf_calendario, overwrite = TRUE))

# Lectura y limpieza del archivo Excel temporal
df_final <- read_excel(tf_calendario, guess_max = 1000) %>%
  # Renombrado de columnas por posición para homogeneizar nombres
  rename(
    fecha        = 1,
    dia_semana   = 2,
    estado       = 3,
    tipo_festivo = 4,
    festividad   = 5
  ) %>%
  # Transformaciones de tipo de dato y limpieza de texto
  mutate(
    fecha      = as.Date(fecha), # Conversión a formato fecha
    dia_semana = str_to_title(dia_semana) %>% # Capitalización del día
      str_replace("Miercoles", "Miércoles") %>% # Corrección de tildes
      str_replace("Sabado",    "Sábado"),
    estado     = str_to_title(estado) %>%
      str_replace("Sabado", "Sábado"),
    es_laborable = (estado == "Laborable"), # Creación de variable booleana para días laborables
    festividad   = if_else(is.na(festividad), "No Festivo", festividad), # Relleno de valores nulos
    tipo_festivo = case_when( # Estandarización de la tipología del festivo
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

# Lectura del dataset de conciertos con codificación UTF-8
df_conciertos_final <- read_csv(
  ruta_conciertos,
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  # Eliminación de columnas redundantes (todas ocurren en Madrid, España)
  select(-ciudad, -pais) %>%
  # Limpieza y formateo de variables
  mutate(
    fecha        = dmy(fecha), # Conversión de formato Día-Mes-Año a objeto Date
    artista      = str_trim(artista), # Eliminación de espacios en blanco sobrantes
    recinto      = str_trim(recinto),
    recinto      = if_else(is.na(recinto),     "No especificado", recinto), # Manejo de NA
    info_evento  = if_else(is.na(info_evento), "Sin info",        info_evento),
    tour         = if_else(is.na(tour),        "Sin tour",        tour),
    tiene_alias  = !is.na(artista_alias) # Variable lógica: indica si el artista tiene alias registrado
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
    # Sustitución de valores nulos por 0 en métricas de rol del artista
    as_lead    = coalesce(as_lead,    0),
    solo       = coalesce(solo,       0),
    as_feature = coalesce(as_feature, 0),
    # Expansión de las métricas (estaban en millones, se pasan a cifras absolutas)
    streams    = streams    * 1e6,
    daily      = daily      * 1e6,
    as_lead    = as_lead    * 1e6,
    solo       = solo       * 1e6,
    as_feature = as_feature * 1e6
  )

cat("   OK — df_kworb_final:", nrow(df_kworb_final), "artistas\n")

# -- E2. Monthly listeners (Oyentes mensuales)
df_listeners_final <- read_csv(
  ruta_listeners,
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  # Aseguramiento de los tipos de datos correctos para clasificaciones y conteos
  mutate(
    ranking        = as.integer(ranking),
    listeners      = as.numeric(listeners),
    daily_change   = as.numeric(daily_change),
    peak_pos       = as.integer(peak_pos),
    peak_listeners = as.numeric(peak_listeners)
  )

cat("   OK — df_listeners_final:", nrow(df_listeners_final), "artistas\n")

# -- E3. Spain Daily chart totals (Listas diarias España)
df_daily_final <- read_csv(
  ruta_daily,
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  # Se descartan variables no necesarias para este análisis
  select(-spotify_id, -peak_veces, -peak_streams) %>%
  mutate(
    dias_top10     = coalesce(dias_top10, 0L), # Reemplazo de NAs por entero 0
    artista        = str_trim(str_remove(artista, " -$")), # Limpieza de caracteres residuales en el nombre
    titulo         = coalesce(titulo, "Sin Título Especificado"),
    dias           = as.integer(dias),
    dias_top10     = as.integer(dias_top10),
    peak_pos       = as.integer(peak_pos),
    total_streams  = as.numeric(total_streams)
  )

cat("   OK — df_daily_final:", nrow(df_daily_final), "canciones\n")

# -- E4. Spain Weekly chart totals (Listas semanales España)
df_weekly_final <- read_csv(
  ruta_weekly,
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  # Aplicación de limpieza equivalente a las listas diarias
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

# Función encapsulada para evitar código repetitivo al procesar diferentes meses
procesar_airbnb <- function(snapshot_mes) {
  # Construcción de la ruta dinámica según el mes proporcionado
  path_base <- file.path(ruta_airbnb_base, snapshot_mes, "")
  
  # 1. Carga y limpieza de anuncios (listings)
  df_listings <- read_csv(
    paste0(path_base, "listings.csv.gz"),
    show_col_types = FALSE
  ) %>%
    mutate(
      price                  = parse_number(price), # Extracción del valor numérico del precio
      neighbourhood_cleansed = str_trim(neighbourhood_cleansed),
      property_type          = str_trim(property_type),
      room_type              = str_trim(room_type)
    ) %>%
    # Filtro de calidad: descartar precios anómalos o estancias mínimas muy largas
    filter(!is.na(price), price > 0, price <= 1500, minimum_nights <= 30)
  
  # Vector de IDs válidos para filtrar el resto de tablas referenciadas
  ids_validos <- unique(df_listings$id)
  
  # 2. Carga y limpieza del calendario de disponibilidad/precios
  df_calendar <- read_csv(
    paste0(path_base, "calendar.csv.gz"),
    col_types     = cols(price = col_character(), adjusted_price = col_character(), .default = col_guess()),
    show_col_types = FALSE
  ) %>%
    mutate(
      price          = parse_number(price),
      adjusted_price = parse_number(adjusted_price)
    ) %>%
    # Se cruza con los anuncios válidos y se filtran precios fuera de rango normal
    filter(listing_id %in% ids_validos, is.na(price) | (price > 0 & price <= 1500))
  
  # 3. Carga del componente geoespacial (barrios)
  geo_neigh <- st_read(paste0(path_base, "neighbourhoods.geojson"), quiet = TRUE) %>%
    mutate(neighbourhood = str_trim(neighbourhood))
  
  # 4. Carga de las reseñas completas filtradas por IDs válidos
  df_reviews <- read_csv(
    paste0(path_base, "reviews.csv.gz"),
    show_col_types = FALSE
  ) %>%
    filter(listing_id %in% ids_validos)
  
  # 5. Carga del resumen de reseñas filtradas por IDs válidos
  df_reviews_sum <- read_csv(
    paste0(path_base, "reviews-summary.csv"),
    show_col_types = FALSE
  ) %>%
    filter(listing_id %in% ids_validos)
  
  # Retorno de todos los dataframes procesados en formato lista
  list(listings = df_listings, calendar = df_calendar, geo = geo_neigh,
       reviews = df_reviews, reviews_sum = df_reviews_sum)
}

# Ejecución de la función para la foto (snapshot) de Junio 2025
airbnb_06 <- procesar_airbnb("2025-06")
# Desempaquetado de la lista en objetos individuales
df_listings_06    <- airbnb_06$listings
df_calendar_06    <- airbnb_06$calendar
geo_neigh_06      <- airbnb_06$geo
df_reviews_06     <- airbnb_06$reviews
df_reviews_sum_06 <- airbnb_06$reviews_sum
rm(airbnb_06) # Limpieza de memoria borrando la lista temporal

cat("   OK — Junio 2025 | listings:", nrow(df_listings_06),
    "| filas calendar:", nrow(df_calendar_06), "\n")

# Ejecución de la función para la foto (snapshot) de Septiembre 2025
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
  # Filtrado específico para la Comunidad de Madrid
  filter(`Comunidades y Ciudades Autónomas` == "13 Madrid, Comunidad de") %>%
  # Limpieza de valores nulos de la fuente oficial ("..") y formateo numérico español
  mutate(
    Total = if_else(Total == "..", NA_character_, Total),
    Total = parse_number(Total, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  # Reestructuración: las categorías de la columna pasan a ser variables propias (anchura)
  select(Periodo, `Apartamentos y personal empleado`, Total) %>%
  pivot_wider(names_from = `Apartamentos y personal empleado`, values_from = Total) %>%
  # Renombrado intuitivo de las variables pivotadas
  rename(
    plazas_estimadas        = `Número de plazas estimadas`,
    aptos_estimados         = `Número de apartamentos estimados`,
    ocupacion_plazas        = `Grado de ocupación por plazas`,
    ocupacion_plazas_finde  = `Grado de ocupación por plazas en fin de semana`,
    ocupacion_aptos         = `Grado de ocupación por apartamentos`,
    ocupacion_aptos_finde   = `Grado de ocupación por apartamentos en fin de semana`,
    personal_empleado       = `Personal empleado`
  ) %>%
  # Generación de la variable fecha ordenable
  mutate(fecha = ym(Periodo)) %>%
  relocate(fecha, Periodo) %>%
  arrange(fecha)

cat("   OK — df_ine_aptos_tur:", nrow(df_ine_aptos_tur), "meses\n")

# ----

cat("[6/8] Descargando INE — Encuesta de Ocupación Hotelera (capacidad + viajeros)...\n")

# Capacidad hotelera
df_ine_hotel_capacidad <- read_csv2(
  "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/2066.csv",
  locale        = locale(encoding = "UTF-8"),
  show_col_types = FALSE
) %>%
  # Filtrado por Madrid a nivel autonómico (is.na(Provincias) elimina el desglose provincial)
  filter(
    `Comunidades y Ciudades Autónomas` == "13 Madrid, Comunidad de",
    is.na(Provincias)
  ) %>%
  mutate(
    Total = if_else(Total == "..", NA_character_, as.character(Total)),
    Total = parse_number(Total, locale = locale(grouping_mark = ".", decimal_mark = ","))
  ) %>%
  # Pivotado y renombrado como en apartamentos turísticos
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

# Viajeros y pernoctaciones
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
  # Creación de una métrica cruzando el tipo de estadía y la procedencia
  mutate(
    residencia_limpia  = if_else(is.na(`Residencia: Nivel 2`), "Total", `Residencia: Nivel 2`),
    metrica_compuesta  = paste(`Viajeros y pernoctaciones`, residencia_limpia, sep = "_")
  ) %>%
  select(Periodo, metrica_compuesta, Total) %>%
  pivot_wider(names_from = metrica_compuesta, values_from = Total) %>%
  # Renombrado de las métricas compuestas
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

# Encuesta Familitur (turismo de residentes)
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
  # Normalización de los conceptos turísticos para crear columnas legibles
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
    metrica_compuesta = paste(concepto_clean, tipo_clean, sep = "_") # Ej: "viajes_abs"
  ) %>%
  select(Periodo, metrica_compuesta, Total) %>%
  pivot_wider(names_from = metrica_compuesta, values_from = Total) %>%
  # Al ser datos anuales, se establece el 1 de enero como fecha de referencia
  mutate(
    anio  = as.integer(Periodo),
    fecha = make_date(anio, 1, 1)
  ) %>%
  select(-Periodo) %>%
  relocate(fecha, anio) %>%
  arrange(fecha)

# Encuesta Frontur (turismo internacional)
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
  # Limpieza del tipo de métrica para el pivoteo
  mutate(
    tipo_clean = case_when(
      `Tipo de dato` == "Dato base"               ~ "internacional_abs",
      `Tipo de dato` == "Tasa de variación anual" ~ "internacional_var",
      TRUE ~ "otro"
    )
  ) %>%
  select(Periodo, tipo_clean, Total) %>%
  pivot_wider(names_from = tipo_clean, values_from = Total) %>%
  # Al igual que Familitur, son datos anuales, se mapean al 1 de enero
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

# Índice de Precios Hoteleros (IPH)
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
  # Diferenciación entre el valor del índice y su tasa de variación
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

# Global de Alojamientos Turísticos (agregado)
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
  # Limpieza y agrupación jerárquica de variables complejas (alojamiento, residencia y métrica)
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
    # Concatenación final para crear una columna descriptiva única
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

# Lista de todos los objetos que deberían haberse generado en la ejecución
objetos_tfg <- c(
  "df_final", "df_conciertos_final",
  "df_kworb_final", "df_listeners_final", "df_daily_final", "df_weekly_final",
  "df_listings_06", "df_calendar_06", "geo_neigh_06", "df_reviews_06", "df_reviews_sum_06",
  "df_listings_09", "df_calendar_09", "geo_neigh_09", "df_reviews_09", "df_reviews_sum_09",
  "df_ine_aptos_tur", "df_ine_hotel_capacidad", "df_ine_hotel_viajeros",
  "df_ine_familitur", "df_ine_frontur", "df_ine_iph", "df_ine_alojamiento_global"
)

# Bucle de comprobación: verifica si el objeto existe e imprime sus dimensiones
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
cat("Listo. Puedes ejecutar Exploracion.R a continuación.\n")
