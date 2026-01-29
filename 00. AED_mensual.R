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
nombre_estacion <- "COPIAPO"

probs_exc <- c(0.05, 0.10, 0.20, 0.50, 0.85)

# =========================================================
# 1. DATOS DIARIOS
# =========================================================
get_data_daily <- function(nombre_archivo, nombre_estacion) {
  
  dt  <- read.xlsx(nombre_archivo, sheet = 2, detectDates = TRUE)
  est <- read.xlsx(nombre_archivo, sheet = 1)
  
  id_est <- est %>%
    filter(name == nombre_estacion) %>%
    pull(id)
  
  if (length(id_est) == 0) stop("Estación no encontrada.")
  
  dt %>%
    filter(id == id_est) %>%
    mutate(
      date = as.Date(date, origin = "1899-12-30"),
      RR   = value
    ) %>%
    select(date, RR)
}

# =========================================================
# 2. MATRIZ MENSUAL (AÑO HIDROLÓGICO)
# =========================================================
get_monthly_matrix <- function(data_daily, umbral_cobertura = 0.8) {
  
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
      RR_sum = sum(RR, na.rm = TRUE),
      dias   = n(),
      validos = sum(!is.na(RR)),
      cobertura = validos / dias,
      RR_month = ifelse(cobertura >= umbral_cobertura, RR_sum, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(Mes = factor(month_h, levels = 1:12, labels = meses)) %>%
    select(year_h, Mes, RR_month) %>%
    pivot_wider(names_from = Mes, values_from = RR_month) %>%
    arrange(year_h)
}

# =========================================================
# 3. MATRIZ TÍPICA DE EXCEDENCIA (CORRECTA)
# =========================================================
build_matriz_pexcc <- function(matriz_mensual) {
  
  meses <- setdiff(names(matriz_mensual), "year_h")
  
  ranking_mes <- lapply(meses, function(mes) {
    
    x <- matriz_mensual[[mes]]
    x <- x[!is.na(x)]
    
    if (length(x) < 2) return(NULL)
    
    x_ord <- sort(x, decreasing = TRUE)
    N     <- length(x_ord)
    
    data.frame(
      Pexc = (1:N) / (N + 1),
      RR   = x_ord
    )
  })
  
  names(ranking_mes) <- meses
  
  Pexc_comun <- sort(unique(unlist(
    lapply(ranking_mes, function(df) df$Pexc)
  )))
  
  matriz_pexcc <- data.frame(Pexc = Pexc_comun)
  
  for (mes in meses) {
    
    df <- ranking_mes[[mes]]
    
    if (is.null(df)) {
      matriz_pexcc[[mes]] <- NA_real_
    } else {
      matriz_pexcc[[mes]] <- approx(
        x = df$Pexc,
        y = df$RR,
        xout = Pexc_comun,
        rule = 2
      )$y
    }
  }
  
  matriz_pexcc %>% arrange(Pexc)
}

# =========================================================
# 4. CURVAS DE EXCEDENCIA
# =========================================================
calc_curvas_pexcc <- function(matriz_pexcc, probs_exc) {
  
  meses <- setdiff(names(matriz_pexcc), "Pexc")
  
  curvas <- lapply(meses, function(mes) {
    approx(
      x = matriz_pexcc$Pexc,
      y = matriz_pexcc[[mes]],
      xout = probs_exc,
      rule = 2
    )$y
  })
  
  curvas_df <- as.data.frame(curvas)
  colnames(curvas_df) <- meses
  
  curvas_df %>%
    mutate(Pexc = probs_exc) %>%
    relocate(Pexc)
}

# =========================================================
# 5. EJECUCIÓN
# =========================================================
data_daily     <- get_data_daily(nombre_archivo, nombre_estacion)
matriz_mensual <- get_monthly_matrix(data_daily)
matriz_pexcc   <- build_matriz_pexcc(matriz_mensual)
curvas_pexcc   <- calc_curvas_pexcc(matriz_pexcc, probs_exc)

# =========================================================
# 6. GRÁFICO
# =========================================================
meses_hidro <- c("Abr","May","Jun","Jul","Ago","Sep",
                 "Oct","Nov","Dic","Ene","Feb","Mar")

curvas_long <- curvas_pexcc %>%
  pivot_longer(cols = -Pexc, names_to = "Mes", values_to = "RR") %>%
  mutate(
    Mes = factor(Mes, levels = meses_hidro),
    Pexc_lab = paste0("P", Pexc * 100, "%")
  )

ggplot(curvas_long,
       aes(Mes, RR, color = Pexc_lab, group = Pexc_lab)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = paste("Curvas mensuales de excedencia –", nombre_estacion),
    x = "Mes hidrológico",
    y = "Precipitación mensual (mm)",
    color = "Prob. excedencia"
  ) +
  theme_minimal()

# =========================================================
# 7. GUARDAR RESULTADOS
# =========================================================
dir.create("resultados", showWarnings = FALSE)

write.csv(matriz_mensual,
          "resultados/matriz_mensual.csv",
          row.names = FALSE)

write.csv(matriz_pexcc,
          "resultados/matriz_pexcc.csv",
          row.names = FALSE)

write.csv(curvas_pexcc,
          "resultados/curvas_excedencia.csv",
          row.names = FALSE)
