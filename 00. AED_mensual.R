# =========================================================
# LIBRERÍAS
# =========================================================
suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(ggplot2)
})

# =========================================================
# PARÁMETROS
# =========================================================
nombre_archivo  <- "data/RegistrosDiarios_pr_2026-01-05.xlsx"
nombre_estacion <- "PASTOS GRANDES"

# Probabilidades de excedencia solicitadas (de menor a mayor para orden lógico)
probs_exc <- c(0.05, 0.10, 0.20, 0.5, 0.85)

# =========================================================
# 1. DATOS DIARIOS
# =========================================================
get_monthly_matrix <- function(data_daily) {
  
  meses <- c("Abr","May","Jun","Jul","Ago","Sep",
             "Oct","Nov","Dic","Ene","Feb","Mar")
  
  data_daily %>%
    mutate(
      date_h  = date %m-% months(3),
      year_h  = year(date_h),
      month_h = month(date_h)
    ) %>%
    group_by(year_h, month_h) %>%
    summarise(
      RR_month = sum(RR, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Mes = factor(month_h, levels = 1:12, labels = meses)
    ) %>%
    select(year_h, Mes, RR_month) %>%
    pivot_wider(names_from = Mes, values_from = RR_month) %>%
    arrange(year_h)
}

# =========================================================
# 2. MATRIZ MENSUAL
# =========================================================
get_monthly_matrix <- function(data_daily) {
  
  meses <- c("Abr","May","Jun","Jul","Ago","Sep",
             "Oct","Nov","Dic","Ene","Feb","Mar")
  
  
  data_daily %>%
    mutate(
      date_h  = date %m-% months(3),
      year_h  = year(date_h),
      month_h = month(date_h)
    ) %>%
    group_by(year_h, month_h) %>%
    summarise(RR_month = sum(RR, na.rm = TRUE), .groups = "drop") %>%
    mutate(Mes = factor(month_h, levels = 1:12, labels = meses)) %>%
    select(year_h, Mes, RR_month) %>%
    pivot_wider(names_from = Mes, values_from = RR_month) %>%
    arrange(year_h)
}
# =========================================================
# 3. MATRIZ TÍPICA DE EXCEDENCIA
# =========================================================
build_matriz_pexcc <- function(matriz_mensual) {
  
  meses <- setdiff(names(matriz_mensual), "year_h")
  
  # Convertir a formato largo
  matriz_long <- matriz_mensual %>%
    pivot_longer(cols = all_of(meses), names_to = "Mes", values_to = "RR_month") %>%
    group_by(Mes) %>%
    arrange(desc(RR_month), .by_group = TRUE) %>%  # ORDEN de mayor a menor
    mutate(
      m = row_number(),
      N = n(),
      Pexc = m / (N + 1)
    ) %>%
    ungroup()
  
  # Convertir a formato ancho: cada fila = Pexc
  matriz_wide <- matriz_long %>%
    select(Pexc, Mes, RR_month) %>%
    pivot_wider(names_from = Mes, values_from = RR_month) %>%
    arrange(Pexc)
  
  matriz_wide
}

# =========================================================
# 4. CURVAS DE EXCEDENCIA POR INTERPOLACIÓN
# =========================================================
calc_curvas_pexcc <- function(matriz_pexcc, probs_exc) {
  
  meses <- setdiff(names(matriz_pexcc), "Pexc")
  
  # Interpolación para cada mes usando su propia columna y Pexc
  curvas <- lapply(meses, function(mes) {
    approx(
      x = matriz_pexcc$Pexc,
      y = matriz_pexcc[[mes]],
      xout = probs_exc,
      rule = 2
    )$y
  })
  
  # Convertir a data.frame
  curvas_df <- as.data.frame(do.call(cbind, curvas))
  colnames(curvas_df) <- meses
  
  curvas_df %>%
    mutate(Pexc = probs_exc) %>%
    relocate(Pexc)
}

# =========================================================
# 5. EJECUCIÓN COMPLETA
# =========================================================
data_daily     <- get_data_daily(nombre_archivo, nombre_estacion)
matriz_mensual <- get_monthly_matrix(data_daily)
matriz_pexcc   <- build_matriz_pexcc(matriz_mensual)
curvas_pexcc   <- calc_curvas_pexcc(matriz_pexcc, probs_exc)

# =========================================================
# 6. GRÁFICO CORREGIDO CON PROBABILIDAD DE EXCEDENCIA
# =========================================================

# Definir el orden correcto de los meses hidrológicos
meses_hidro <- c("Abr","May","Jun","Jul","Ago","Sep",
                 "Oct","Nov","Dic","Ene","Feb","Mar")

# VERIFICAR SI TODOS LOS MESES ESTÁN PRESENTES EN LOS DATOS
cat("Verificación de meses en curvas_pexcc:\n")
cat("Columnas disponibles:", paste(colnames(curvas_pexcc), collapse = ", "), "\n")

# Asegurar que curvas_pexcc tenga todas las columnas de meses
meses_faltantes <- setdiff(meses_hidro, colnames(curvas_pexcc))
if (length(meses_faltantes) > 0) {
  cat("Meses faltantes en los datos:", paste(meses_faltantes, collapse = ", "), "\n")
  cat("Agregando columnas faltantes con valores NA...\n")
  
  # Agregar columnas faltantes con NA
  for (mes in meses_faltantes) {
    curvas_pexcc[[mes]] <- NA
  }
}

# Convertir a formato largo con PROBABILIDAD DE EXCEDENCIA
curvas_long <- curvas_pexcc %>%
  # Reordenar columnas para que Pexc esté primero
  select(Pexc, all_of(meses_hidro)) %>%
  pivot_longer(cols = -Pexc, names_to = "Mes", values_to = "RR") %>%
  mutate(
    # ¡IMPORTANTE!: Convertir Mes a factor con el orden correcto
    Mes = factor(Mes, levels = meses_hidro),
    # Crear etiquetas de probabilidad de EXCEDENCIA (Pexc)
    # Ordenar de MAYOR probabilidad de excedencia a MENOR
    Prob_label = factor(
      paste0("P", Pexc * 100, "%"),  # P5%, P10%, P20%, P50%, P85%
      levels = paste0("P", sort(probs_exc, decreasing = TRUE) * 100, "%")
    ),
    # También mantener el valor numérico para orden
    Pexc_numeric = Pexc
  ) %>%
  arrange(desc(Pexc_numeric))  # Ordenar de mayor a menor probabilidad de excedencia

# Verificar el orden de las probabilidades
cat("\nOrden de las probabilidades en el gráfico:\n")
print(unique(curvas_long$Prob_label))
cat("Valores numéricos correspondientes:\n")
print(unique(curvas_long$Pexc_numeric))

# Crear el gráfico CON PROBABILIDAD DE EXCEDENCIA
ggplot(curvas_long, aes(x = Mes, y = RR, color = Prob_label, group = Prob_label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  # Forzar a mostrar todos los meses en el eje X
  scale_x_discrete(limits = meses_hidro, drop = FALSE) +
  labs(
    title = paste("Curvas mensuales hidrológicas -", nombre_estacion),
    subtitle = "Precipitación mensual según probabilidad de excedencia",
    x = "Mes hidrológico",
    y = "Precipitación mensual (mm)",
    color = "Probabilidad\nde excedencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.1)),
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
  ) +
  scale_color_brewer(
    palette = "Set1",
    guide = guide_legend(reverse = TRUE)  # Opcional: invertir orden en leyenda
  )

# Opcional: Guardar el gráfico
# ggsave("curvas_excedencia_mensual.png", width = 12, height = 7, dpi = 300)

# =========================================================
# 7. VERIFICACIÓN ADICIONAL
# =========================================================
cat("\n=== VERIFICACIÓN DE VALORES ===\n")
cat("Para un mes específico (ej. 'Ene'), los valores deberían disminuir al aumentar Pexc:\n")

# Mostrar valores para Enero ordenados por Pexc
if ("Ene" %in% colnames(curvas_pexcc)) {
  verificacion <- curvas_pexcc %>%
    select(Pexc, Ene) %>%
    arrange(Pexc)
  
  print(verificacion)
  cat("\nInterpretación:\n")
  cat("- P5%: Solo el 5% de los años tienen precipitación MAYOR que este valor (valor ALTO)\n")
  cat("- P85%: El 85% de los años tienen precipitación MAYOR que este valor (valor BAJO)\n")
}

# =========================================================
# GUARDAR DATA DIARIA EN CSV
# =========================================================

# Crear carpeta si no existe
dir.create("resultados", showWarnings = FALSE)

data_daily_out <- data_daily %>%
  transmute(
    Fecha    = as.Date(date),
    Estacion = nombre_estacion,
    RR       = RR
  )

write.csv(
  data_daily_out,
  file = "resultados/data_daily.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)