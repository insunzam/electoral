# ==============================================================================
# CRUCE PADRÓN ELECTORAL SERVEL 2025 × POBLACIÓN RURAL CENSO 2017 (INE)
# Investigación: Votantes en comunas rurales de Chile - Elección Presidencial 2025
#
# Fuentes:
#   - SERVEL: servel.cl/estadisticas-de-padron-electoral-2025/
#             (Excel: "Total de electores por comuna, sexo y rango de edad")
#   - ODEPA + BCN:    Desde odepa la clasificación de comunas urbano/rural y la población
#             al 2024. Del BCN una estimación del personas U/R por comuna para 
#   el censo 2017, aplicada a ODEPA.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. INSTALACIÓN Y CARGA DE PAQUETES
# ------------------------------------------------------------------------------
paquetes <- c("tidyverse", "readxl", "writexl", "janitor", "stringr",
              "scales", "ggplot2", "knitr")

instalar_si_falta <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(paquetes, instalar_si_falta))

library(tidyverse)
library(readxl)
library(writexl)
library(janitor)     # limpieza de nombres de columnas
library(stringr)
library(scales)
library(ggplot2)


# ==============================================================================
# 2. CARGA DE DATOS
# ==============================================================================
# Ajusta las rutas a donde guardaste los archivos descargados.
# Si usas RStudio, puedes usar setwd() o rutas relativas.

RUTA_SERVEL <- "datos/participacion_2025_servel.xlsx"
RUTA_INE    <- "datos/ComunasPNDR_Censo24.csv"

# ------------------------------------------------------------------------------
# 2a. PADRÓN SERVEL 2025
# Estructura esperada del Excel de SERVEL:
#   Región | Provincia | Comuna | Total | Hombres | Mujeres | [rangos etarios]
# 
# ------------------------------------------------------------------------------
leer_servel <- function(ruta) {
  message("Cargando padrón SERVEL 2025...")

#  df <- read_excel(ruta, skip = 1) |>      # skip=1 si tiene fila de título
  df <- read_excel(ruta) |>      # skip=1 si tiene fila de título
    clean_names() |>
    filter(!is.na(comuna)) |>
    rename_with(~ str_replace_all(.x, "n_mero_de_electores|electores", "electores")) |>
    select(region, provincia, comuna,
           total_electores = matches("^total$|^total_electores$|^n_mero"),
           hombres = matches("^hombres$|^masculino"),
           mujeres = matches("^mujeres$|^femenino")) |>
    mutate(
      across(c(total_electores, hombres, mujeres), as.numeric),
      comuna = str_to_upper(str_trim(comuna)),
      region = str_trim(region)
    )

  message(glue::glue("  → {nrow(df)} comunas cargadas del padrón SERVEL"))
  df
#  View(df)
}

# ------------------------------------------------------------------------------
# 2b. DATOS CENSO 2017 — POBLACIÓN URBANO/RURAL POR COMUNA (INE)
# Estructura esperada del CSV del INE:
#   REGION | PROVINCIA | COMUNA | NOMBRE_COMUNA | PERSONAS | URBANO | RURAL
# El archivo se llama: "Censo2017_Resultados_Comunas_urbano_rural.csv"
# También puedes usar el del SIIT de la BCN con la misma estructura.
# ------------------------------------------------------------------------------
leer_ine <- function(ruta) {
  message("Cargando datos Censo 2017 (INE)...")

  # Detectar separador automáticamente (puede ser , o ;)
  separador <- ifelse(
    grepl(";", readLines(ruta, n = 2)[2]),
    ";", ","
  )

  df <- read_delim(ruta, delim = separador, locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE) |>
    clean_names() |>
    rename_with(~ case_when(
      str_detect(.x, "region")   ~ "region_ine",
      str_detect(.x, "provincia") ~ "provincia_ine",
      str_detect(.x, "^cod|^cut|^codigo") ~ "cut_comuna",
      str_detect(.x, "nombre") ~ "nombre_comuna",
      str_detect(.x, "total|^personas$") ~ "poblacion_total",
      str_detect(.x, "urbano") ~ "poblacion_urbana",
      str_detect(.x, "rural") ~ "poblacion_rural",
      TRUE ~ .x
    ), .cols = everything()) |>
    select(any_of(c("cut_comuna", "region_ine", "nombre_comuna",
                    "poblacion_total", "poblacion_urbana", "poblacion_rural"))) |>
    filter(!is.na(nombre_comuna), !is.na(poblacion_total)) |>
    mutate(
      across(c(poblacion_total, poblacion_urbana, poblacion_rural), as.numeric),
      nombre_comuna = str_to_upper(str_trim(nombre_comuna)),
      pct_rural  = round(poblacion_rural  / poblacion_total * 100, 1),
      pct_urbana = round(poblacion_urbana / poblacion_total * 100, 1)
    )

  message(glue::glue("  → {nrow(df)} comunas cargadas del Censo INE"))
  df
#  View(df)
}


# ==============================================================================
# 3. NORMALIZACIÓN DE NOMBRES DE COMUNA (para el join)
# Los nombres pueden tener tildes, mayúsculas o variantes distintas entre SERVEL e INE.
# Esta función estandariza para maximizar el matching.
# ==============================================================================
normalizar_nombre <- function(x) {
  x |>
    str_to_upper() |>
    str_trim() |>
    stringi::stri_trans_general("Latin-ASCII") |>  # elimina tildes
    str_replace_all("[^A-Z0-9 ]", "") |>            # elimina caracteres especiales
    str_squish()
}

# Tabla de correcciones manuales para casos conocidos donde los nombres difieren
# entre SERVEL e INE (agrega más si detectas discrepancias en tu cruce)
correcciones_nombres <- tribble(
  ~nombre_servel,            ~nombre_ine,
  "AISEN",                   "AYSEN",
  "ISLA DE PASCUA",          "ISLA DE PASCUA",
  "O HIGGINS",               "O'HIGGINS",
  "PAIHUANO",                "PAIGUANO",
  "LA CALERA",               "CALERA",
  "TREHUACO",                "TREGUACO",
  "LLAY-LLAY",               "LLAILLAY"
)

#"CABO DE HORNOS",          "CABO DE HORNOS (EX NAVARINO)",


# ==============================================================================
# 4. CRUCE (JOIN) ENTRE SERVEL E INE
# ==============================================================================
cruzar_datos <- function(df_servel, df_ine) {
  message("\nRealizando cruce SERVEL × INE...")

  # Normalizar nombres en ambas fuentes
  df_servel <- df_servel |>
    mutate(comuna_norm = normalizar_nombre(comuna))

  df_ine <- df_ine |>
    mutate(comuna_norm = normalizar_nombre(nombre_comuna))

  # Aplicar correcciones manuales al lado SERVEL
  for (i in seq_len(nrow(correcciones_nombres))) {
    df_servel <- df_servel |>
      mutate(comuna_norm = ifelse(
        comuna_norm == normalizar_nombre(correcciones_nombres$nombre_servel[i]),
        normalizar_nombre(correcciones_nombres$nombre_ine[i]),
        comuna_norm
      ))
  }

  # Join principal
  df_cruce <- df_servel |>
    left_join(df_ine, by = "comuna_norm")

  # Diagnóstico de matching
  n_total  <- nrow(df_cruce)
  n_match  <- sum(!is.na(df_cruce$poblacion_total))
  n_falla  <- n_total - n_match

  message(glue::glue(
    "  → {n_match}/{n_total} comunas cruzadas correctamente ({round(n_match/n_total*100,1)}%)"
  ))

  if (n_falla > 0) {
    fallas <- df_cruce |> filter(is.na(poblacion_total)) |> pull(comuna)
    message(glue::glue("  → {n_falla} sin match: {paste(fallas, collapse=', ')}"))
    message("    Agrega estas comunas a la tabla 'correcciones_nombres' en la sección 3.")
  }

  df_cruce
}


# ==============================================================================
# 5. CÁLCULO DE MÉTRICAS DE INTERÉS
# ==============================================================================
calcular_metricas <- function(df) {
  df |>
    mutate(
      # Estimación de electores rurales (aplica % rural del Censo al padrón 2025)
      electores_rurales_est  = round(total_electores * pct_rural  / 100),
      electores_urbanos_est  = round(total_electores * pct_urbana / 100),

      # Clasificación comunal según ODEPA / Política Nacional de Desarrollo Rural
      clasificacion = case_when(
        pct_rural >= 60  ~ "Predominantemente rural",
        pct_rural >= 40  ~ "Mixta (mayoría rural)",
        pct_rural >= 20  ~ "Mixta (mayoría urbana)",
        pct_rural >= 5   ~ "Predominantemente urbana",
        TRUE             ~ "Urbana (casi 0% rural)"
      ),
      clasificacion = factor(clasificacion, levels = c(
        "Predominantemente rural", "Mixta (mayoría rural)",
        "Mixta (mayoría urbana)", "Predominantemente urbana",
        "Urbana (casi 0% rural)"
      ))
    ) |>
    arrange(desc(electores_rurales_est))
}


# ==============================================================================
# 6. TABLAS RESUMEN
# ==============================================================================
resumen_general <- function(df) {
  list(
    # Top 30 comunas por electores rurales absolutos
    top30_absoluto = df |>
      filter(!is.na(electores_rurales_est)) |>
      select(region, comuna, total_electores, electores_rurales_est,
             electores_urbanos_est, pct_rural, clasificacion) |>
      slice_head(n = 30),

    # Top 30 comunas por porcentaje de ruralidad (con > 1.000 electores)
    top30_pct_rural = df |>
      filter(!is.na(pct_rural), total_electores >= 1000) |>
      arrange(desc(pct_rural)) |>
      select(region, comuna, total_electores, electores_rurales_est,
             pct_rural, clasificacion) |>
      slice_head(n = 30),

    # Resumen por región
    por_region = df |>
      filter(!is.na(electores_rurales_est)) |>
      group_by(region) |>
      summarise(
        n_comunas            = n(),
        total_electores_reg  = sum(total_electores, na.rm = TRUE),
        electores_rurales    = sum(electores_rurales_est, na.rm = TRUE),
        pct_rural_region     = round(electores_rurales / total_electores_reg * 100, 1),
        .groups = "drop"
      ) |>
      arrange(desc(electores_rurales)),

    # Resumen por clasificación
    por_clasificacion = df |>
      filter(!is.na(clasificacion)) |>
      group_by(clasificacion) |>
      summarise(
        n_comunas         = n(),
        total_electores   = sum(total_electores, na.rm = TRUE),
        electores_rurales = sum(electores_rurales_est, na.rm = TRUE),
        .groups = "drop"
      )
  )
}


# ==============================================================================
# 7. VISUALIZACIONES
# ==============================================================================
graficar <- function(df, resumenes) {

  # Tema común
  tema <- theme_classic(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey50", size = 11),
      axis.text     = element_text(size = 10),
      legend.position = "bottom"
    )

  # Gráfico 1: Top 20 comunas por electores rurales absolutos
  g1 <- df |>
    filter(!is.na(electores_rurales_est)) |>
    slice_head(n = 20) |>
    mutate(comuna = fct_reorder(str_to_title(str_to_lower(comuna)),
                                 electores_rurales_est)) |>
    ggplot(aes(x = electores_rurales_est, y = comuna, fill = clasificacion)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = comma(electores_rurales_est, big.mark = ".")),
              hjust = -0.1, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)),
                       labels = comma_format(big.mark = ".")) +
    scale_fill_manual(values = c(
      "Predominantemente rural" = "#1D9E75",
      "Mixta (mayoría rural)"   = "#5DCAA5",
      "Mixta (mayoría urbana)"  = "#378ADD",
      "Predominantemente urbana" = "#85B7EB",
      "Urbana (casi 0% rural)"  = "#D3D1C7"
    )) +
    labs(
      title    = "Top 20 comunas: electores rurales estimados",
      subtitle = "Padrón SERVEL 2025 × % ruralidad Censo 2017 (INE)",
      x        = "Electores rurales estimados",
      y        = NULL,
      fill     = "Clasificación comunal",
      caption  = "Fuentes: SERVEL (padrón definitivo 2025) e INE (Censo 2017)"
    ) +
    tema

  ggsave("output/graf1_top20_electores_rurales.png", g1,
         width = 10, height = 8, dpi = 150)

  # Gráfico 2: Electores rurales por región
  g2 <- resumenes$por_region |>
    mutate(region = fct_reorder(region, electores_rurales)) |>
    ggplot(aes(x = electores_rurales, y = region)) +
    geom_col(fill = "#1D9E75", width = 0.7) +
    geom_text(aes(label = paste0(electores_rurales, "")),
              hjust = -0.2, size = 3.2, color = "grey30") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                       labels = comma_format(big.mark = ".")) +
    labs(
      title    = "Electores rurales estimados por región",
      subtitle = "% indica proporción sobre el total de electores de la región",
      x        = "Electores rurales estimados",
      y        = NULL,
      caption  = "Fuentes: SERVEL 2025 e INE Censo 2017"
    ) +
    tema

#  geom_text(aes(label = paste0(pct_rural_region, "%")),
  
  ggsave("output/graf2_electores_rurales_por_region.png", g2,
         width = 10, height = 7, dpi = 150)

  # Gráfico 3: Dispersión electores totales vs % rural
  g3 <- df |>
    filter(!is.na(pct_rural), !is.na(total_electores)) |>
    ggplot(aes(x = pct_rural, y = total_electores,
               color = clasificacion, size = electores_rurales_est)) +
    geom_point(alpha = 0.65) +
    scale_y_log10(labels = comma_format(big.mark = ".")) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_size_continuous(range = c(1, 8), labels = comma_format(big.mark = ".")) +
    scale_color_manual(values = c(
      "Predominantemente rural" = "#085041",
      "Mixta (mayoría rural)"   = "#1D9E75",
      "Mixta (mayoría urbana)"  = "#378ADD",
      "Predominantemente urbana" = "#0C447C",
      "Urbana (casi 0% rural)"  = "#888780"
    )) +
    labs(
      title    = "Distribución de comunas: % ruralidad vs. total de electores",
      subtitle = "Tamaño del punto proporcional a electores rurales estimados",
      x        = "% población rural (Censo 2017)",
      y        = "Total electores padrón 2025 (escala log)",
      color    = "Clasificación",
      size     = "Electores rurales est.",
      caption  = "Fuentes: SERVEL 2025 e INE Censo 2017"
    ) +
    tema +
    theme(legend.position = "right")

  ggsave("output/graf3_dispersion_ruralidad_electores.png", g3,
         width = 11, height = 7, dpi = 150)

  message("  → 3 gráficos guardados en output/")
  list(g1, g2, g3)
}


# ==============================================================================
# 8. EXPORTAR RESULTADOS A EXCEL
# ==============================================================================
exportar_excel <- function(df, resumenes) {
  message("\nExportando resultados a Excel...")

  hojas <- list(
    "Cruce_completo"       = df |> select(-comuna_norm) |>
                               arrange(desc(electores_rurales_est)),
    "Top30_absoluto"       = resumenes$top30_absoluto,
    "Top30_pct_rural"      = resumenes$top30_pct_rural,
    "Resumen_por_region"   = resumenes$por_region,
    "Por_clasificacion"    = resumenes$por_clasificacion
  )

  write_xlsx(hojas, path = "output/resultados_cruce_rural_2025.xlsx")
  message("  → output/resultados_cruce_rural_2025.xlsx creado con 5 hojas")
}


# ==============================================================================
# 9. PIPELINE PRINCIPAL — ejecuta todo en orden
# ==============================================================================
main <- function() {

  # Crear carpetas de salida si no existen
  dir.create("output", showWarnings = FALSE)

  message("=== INICIO DEL ANÁLISIS ===\n")

  # Carga
  servel <- leer_servel(RUTA_SERVEL)
  ine    <- leer_ine(RUTA_INE)

  # Cruce
  cruce <- cruzar_datos(servel, ine)

  # Métricas
  cruce_final <- calcular_metricas(cruce)

  # Resúmenes
  resumenes <- resumen_general(cruce_final)

  # Mostrar vista previa en consola
  message("\n--- TOP 15 COMUNAS POR ELECTORES RURALES ESTIMADOS ---")
  resumenes$top30_absoluto |>
    slice_head(n = 15) |>
    mutate(across(where(is.numeric), ~ format(.x, big.mark = "."))) |>
    print(n = 15)

  message("\n--- ELECTORES RURALES POR REGIÓN ---")
  print(resumenes$por_region)

  # Gráficos
  message("\nGenerando gráficos...")
  graficar(cruce_final, resumenes)

  # Exportar
  exportar_excel(cruce_final, resumenes)

  message("\n=== ANÁLISIS COMPLETADO ===")
  message("Resultados en la carpeta output/")

  invisible(list(datos = cruce_final, resumenes = resumenes))
}

# ------------------------------------------------------------------------------
# EJECUTAR
# Descomenta la siguiente línea para correr el análisis completo:
resultados <- main()
# ------------------------------------------------------------------------------

# Para explorar los resultados interactivamente después de correr main():
# View(resultados$datos)
# View(resultados$resumenes$top30_absoluto)
# View(resultados$resumenes$por_region)
