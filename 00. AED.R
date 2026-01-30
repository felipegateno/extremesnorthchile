suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(zoo)
  library(ggplot2)
  library(patchwork)
})


# Directorio
nombre_archivo = "data/RegistrosDiarios_pr_2026-01-05.xlsx"
nombre_estacion = "PASTOS GRANDES"
get_data_estacion <- function(nombre_archivo, nombre_estacion) {
  
  
  
  # Leer datos
  dt <- read.xlsx(nombre_archivo, sheet = 2,
                  detectDates = TRUE)
  dt_estaciones <- read.xlsx(nombre_archivo, sheet = 1)
  
  # Obtener ID estación
  id_estacion <- dt_estaciones %>%
    filter(name == nombre_estacion) %>%
    pull(id)
  
  if (length(id_estacion) == 0) {
    stop("La estación no existe en el archivo.")
  }
  
  # Construir serie diaria continua
  data_estacion <- dt %>%
    filter(id %in% id_estacion) %>%
    mutate(
      date = as.Date(date, origin = "1899-12-30")
    ) %>%
    rename(RR = value) %>%
    arrange(date) %>%
    complete(
      date = seq(min(date), max(date), by = "day"),
      fill = list(RR = NA_real_)
    ) %>%
    mutate(
      date_hidrologico = date %m-% months(3),
      year = year(date_hidrologico)
    ) %>% 
    select(-variable)
  
  return(data_estacion)
}

calcular_metricas_anuales <- function(data_estacion,
                                      year_min = 1966,
                                      year_max = 2025) {
  
  # ---------------------------
  # Tabla base de años
  # ---------------------------
  years_tbl <- tibble(year = year_min:year_max)
  
  # ---------------------------
  # Conteo y calidad de datos
  # ---------------------------
  n_wet_dry <- data_estacion %>%
    group_by(year) %>%
    summarise(
      n_days   = n(),
      n_valid  = sum(!is.na(RR)),
      pct_valid = 100 * n_valid / n_days,
      n_wet = if (all(is.na(RR))) NA_integer_ else sum(RR > 0, na.rm = TRUE),
      n_dry = if (all(is.na(RR))) NA_integer_ else sum(RR == 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---------------------------
  # RxDdia
  # ---------------------------
  RxDdia <- function(D) {
    data_estacion %>%
      arrange(date) %>%
      mutate(RR_D = zoo::rollsum(RR, k = D, fill = NA, align = "center")) %>%
      group_by(year) %>%
      summarise(
        !!paste0("Rx", D, "day") :=
          if (all(is.na(RR))) NA_real_
        else max(RR_D, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  Rx1day <- RxDdia(1)
  Rx5day <- RxDdia(5)
  
  # ---------------------------
  # SDII
  # ---------------------------
  SDII <- data_estacion %>%
    group_by(year) %>%
    summarise(
      SDII = if (all(is.na(RR))) NA_real_
      else if (sum(RR > 0, na.rm = TRUE) == 0) NA_real_
      else sum(RR[RR > 0], na.rm = TRUE) / sum(RR > 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---------------------------
  # Rnnmm
  # ---------------------------
  Rnnmm <- function(nn, name) {
    data_estacion %>%
      group_by(year) %>%
      summarise(
        !!name := if (all(is.na(RR))) NA_integer_
        else sum(RR >= nn, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  R10mm <- Rnnmm(10, "R10mm")
  R20mm <- Rnnmm(20, "R20mm")
  
  # ---------------------------
  # CDD / CWD
  # ---------------------------
  CD <- function(tipo) {
    data_estacion %>%
      arrange(date_hidrologico) %>%
      mutate(
        condicion = case_when(
          tipo == "seco"   & RR == 0 ~ TRUE,
          tipo == "humedo" & RR >  0 ~ TRUE,
          TRUE ~ FALSE
        )
      ) %>%
      group_by(year) %>%
      summarise(
        !!tipo := {
          if (all(is.na(RR))) NA_integer_
          else {
            r <- rle(condicion)
            if (any(r$values)) max(r$lengths[r$values]) else 0
          }
        },
        .groups = "drop"
      )
  }
  
  CDD <- CD("CDD")
  CWD <- CD("CWD")
  
  # ---------------------------
  # PRCPTOT
  # ---------------------------
  PRCPTOT <- data_estacion %>%
    group_by(year) %>%
    summarise(
      PRCPTOT = if (all(is.na(RR))) NA_real_
      else sum(RR[RR > 0], na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---------------------------
  # Percentiles
  # ---------------------------
  ref <- data_estacion %>%
    filter(year >= year_min, year <= year_max, RR > 0)
  
  p95 <- quantile(ref$RR, 0.95, na.rm = TRUE, type = 6)
  p99 <- quantile(ref$RR, 0.99, na.rm = TRUE, type = 6)
  
  Rper <- function(p, name) {
    data_estacion %>%
      group_by(year) %>%
      summarise(
        !!name := if (all(is.na(RR))) NA_real_
        else sum(RR[RR > p], na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  R95pTOT <- Rper(p95, "R95pTOT")
  R99pTOT <- Rper(p99, "R99pTOT")
  
  # ---------------------------
  # MATRIZ FINAL
  # ---------------------------
  metricas <- years_tbl %>%
    left_join(n_wet_dry, by = "year") %>%
    left_join(Rx1day, by = "year") %>%
    left_join(Rx5day, by = "year") %>%
    left_join(SDII, by = "year") %>%
    left_join(R10mm, by = "year") %>%
    left_join(R20mm, by = "year") %>%
    left_join(CDD, by = "year") %>%
    left_join(CWD, by = "year") %>%
    left_join(PRCPTOT, by = "year") %>%
    left_join(R95pTOT, by = "year") %>%
    left_join(R99pTOT, by = "year")
  
  return(metricas)
}

data_estacion <- get_data_estacion(
  nombre_archivo ,
  nombre_estacion
)

metricas_data <- calcular_metricas_anuales(data_estacion)


#graficos

theme_metrica <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, colour = "grey30"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

plot_metrica_ts <- function(data, var, subtitulo, ylab) {
  ggplot(
    data,
    aes(x = year, y = .data[[var]])
  ) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    labs(
      title = nombre_estacion,
      subtitle = subtitulo,
      x = "Año hidrológico",
      y = ylab
    ) +
    theme_metrica
}

p_PRCPTOT <- plot_metrica_ts(
  metricas_data,
  "PRCPTOT",
  "Precipitación total anual en días húmedos",
  "PRCPTOT (mm/año)"
)

p_SDII <- plot_metrica_ts(
  metricas_data,
  "SDII",
  "Índice de intensidad diaria",
  "SDII (mm/día húmedo)"
)

p_R95 <- plot_metrica_ts(
  metricas_data,
  "R95pTOT",
  "Precipitación anual sobre el percentil 95",
  "R95pTOT (mm)"
)

p_R99 <- plot_metrica_ts(
  metricas_data,
  "R99pTOT",
  "Precipitación anual sobre el percentil 99",
  "R99pTOT (mm)"
)


(p_PRCPTOT / p_SDII / p_R95) +
  plot_annotation(
    title = nombre_estacion,
    subtitle = "Evolución temporal de cantidad, intensidad y extremos de precipitación"
  )

ggplot(
  metricas_data,
  aes(R95pTOT, R99pTOT)
) +
  geom_point(alpha = 0.7, na.rm = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  coord_equal() +
  theme_metrica +
  labs(
    title = nombre_estacion,
    subtitle = "Comparación entre extremos moderados y severos",
    x = "R95pTOT (mm)",
    y = "R99pTOT (mm)"
  )


metricas_data_long <- metricas_data %>%
  select(PRCPTOT, SDII, Rx1day, Rx5day, R95pTOT, R99pTOT) %>%
  pivot_longer(everything(), names_to = "metrica", values_to = "valor")

ggplot(
  metricas_data_long,
  aes(metrica, valor)
) +
  geom_boxplot(outlier.shape = NA, fill = "grey85") +
  geom_jitter(width = 0.15, alpha = 0.5, size = 1) +
  coord_flip() +
  theme_metrica +
  labs(
    title = nombre_estacion,
    subtitle = "Distribución interanual de métricas de precipitación",
    x = NULL,
    y = "Valor"
  )


plot_comp <- function(data, x, y, xlab, ylab, subtitulo = NULL) {
  ggplot(data, aes({{x}}, {{y}})) +
    geom_point(alpha = 0.7, na.rm = TRUE) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
    theme_metrica +
    labs(
      title = nombre_estacion,
      subtitle = subtitulo,
      x = xlab,
      y = ylab
    )
}


p_line <- plot_metrica_ts(
  metricas_data,
  "PRCPTOT",
  "Cantidad anual de precipitación",
  "PRCPTOT (mm/año)"
)

p_scatter <- plot_comp(
  metricas_data,
  PRCPTOT, SDII,
  "PRCPTOT (mm/año)",
  "SDII (mm/día)",
  "Cantidad vs intensidad"
)

p_box <- ggplot(metricas_data, aes(x = "", y = Rx5day)) +
  geom_boxplot(fill = "grey85") +
  geom_jitter(width = 0.08, alpha = 0.6) +
  theme_metrica +
  labs(
    title = nombre_estacion,
    subtitle = "Variabilidad interanual de extremos",
    x = NULL,
    y = "Rx5día (mm)"
  )

(p_line | p_scatter | p_box) +
  plot_annotation(
    title = nombre_estacion,
    subtitle = "Cantidad, intensidad y extremos de la precipitación"
  )



##
p_Rnn <- ggplot(metricas_data, aes(x = year)) +
  
  geom_line(
    aes(y = R10mm, color = "R10mm"),
    linewidth = 1
  ) +
  geom_point(
    aes(y = R10mm, color = "R10mm"),
    size = 2
  ) +
  
  geom_line(
    aes(y = R20mm, color = "R20mm"),
    linewidth = 1
  ) +
  geom_point(
    aes(y = R20mm, color = "R20mm"),
    size = 2
  ) +
  
  scale_color_manual(
    name = "Métrica",
    values = c(
      "R10mm" = "#1f78b4",
      "R20mm" = "#b2182b"
    ),
    labels = c(
      "R10mm" = "Días con precipitación ≥ 10 mm",
      "R20mm" = "Días con precipitación ≥ 20 mm"
    )
  ) +
  
  labs(
    title = nombre_estacion,
    subtitle = "Frecuencia anual de eventos de precipitación moderada y severa",
    x = "Año hidrológico",
    y = "Número de días (días/año)"
  ) +
  
  theme_metrica +
  theme(
    legend.position = "bottom"
  )

# 👇 ESTA LÍNEA ES LA CLAVE
p_Rnn
