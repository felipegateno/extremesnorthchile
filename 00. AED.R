
# Directorio
setwd("C:/Marcos- Memoria/01. Tecnico/extremesnorthchile/data")

# Librerías
library(openxlsx)
library(dplyr)
library(lubridate)
library(zoo)  
library(tidyr)# para rollsum()
library(ggplot2)
# -------------------------------
# 1. Leer datos de VISHMA21
# -------------------------------

nombre_archivo <- "RegistrosDiarios_pr_2026-01-05.xlsx"
nombre_estacion <-  "PASTOS GRANDES"

dt <- read.xlsx(nombre_archivo, sheet = 2)
dt_estaciones <- read.xlsx(nombre_archivo, sheet = 1)

# -------------------------------
# 2. Filtrar estación
# -------------------------------
id_estacion <- dt_estaciones %>%
  filter(name == nombre_estacion) %>%
  pull(id)

#data estación continua
data_estacion <- dt %>%
  filter(id %in% id_estacion) %>%
  mutate(
    # Fecha desde Excel
    date = as.Date(date, origin = "1899-12-30")
  ) %>%
  # Renombrar precipitación
  rename(RR = value) %>%
  
  # Ordenar por fecha (buena práctica)
  arrange(date) %>%
  
  # Crear serie diaria continua y rellenar faltantes con NA
  complete(
    date = seq(min(date), max(date), by = "day"),
    fill = list(RR = NA_real_)
  ) %>%
  
  # Volver a calcular fecha hidrológica (importante después de complete)
  mutate(
    date_hidrologico = date %m-% months(3)
  )
#
library(openxlsx)

write.xlsx(
  data_estacion,
  file = "data_estacion_continua_Pastos_Grandes.xlsx",
  sheetName = "Datos_diarios",
  rowNames = FALSE
)


# -------------------------------
# 5. RxDdía mensual
# RxDdía_j = max(RR_ij)
# -------------------------------

RxDdia_anual <- function(data_estacion, D) {
  
  data_estacion %>%
    arrange(date) %>%   # MUY importante para rollsum
    mutate(
      year = year(date_hidrologico),
      RR_Ddias = zoo::rollsum(RR, k = D, align = "center", fill = NA)
    ) %>%
    group_by(year) %>%
    summarise(
      RxDdia = if (all(is.na(RR_Ddias))) NA_real_
      else max(RR_Ddias, na.rm = TRUE),
      .groups = "drop"
    )
}

Rx1dia_anual <- RxDdia_anual(data_estacion, 1)
Rx5dia_anual <- RxDdia_anual(data_estacion, 5)
  
# -------------------------------
# 7. SDII Índice de intensidad mensual
# SDII_i = sum(RR_wj)/W
# -------------------------------

SDII_anual <- data_estacion %>%
  mutate(
    year  = year(date_hidrologico), lluvioso=RR>0 # DefinimosRR ≥ 0 mm
  ) %>%

  group_by(year) %>%
  filter(any(lluvioso)) %>% 
  summarise(
    SDII =sum(RR[lluvioso], na.rm = TRUE) / sum(lluvioso),
    .groups = "drop"
  )

SDII_anual

# -------------------------------
# 8. Rnnmm Conteo anual de dias cunado PRCP > nnmm, nn es un umbral definido
# RRij >20mm
# -------------------------------
Rnnmm <- function(data_estacion, nn) {
  
  data_estacion %>%
    mutate(year = year(date_hidrologico), lluvioso=RR>0) %>%
    group_by(year) %>% #debe ser por año hidrologico
    filter(any(lluvioso)) %>% 
    summarise(
      Rnnmm = sum(RR >= nn, na.rm = TRUE),
      .groups = "drop"
    )
} 
#Recuento anual de dias con PRCP >= 10 mm
Rn10mm <- Rnnmm(data_estacion,10)
Rn10mm

#Recuento anual de dias con PRCP >= 20 mm
R20mm <- Rnnmm(data_estacion,20)
R20mm

# -------------------------------
# 9. CDD Dureción máxima del período seco, número máximo de días consecutivos con RR >=0
#   CWD Dureción máxima del período seco, número máximo de días consecutivos con RR >=0
# RRij <1mm o >1mm
# -------------------------------
CD <- function(data_estacion, tipo = c("seco", "humedo")) {
  
  stopifnot(is.data.frame(data_estacion))
  tipo <- match.arg(tipo)
  
  # Vector completo de años hidrológicos presentes en la data
  years_full <- data_estacion %>%
    mutate(year = lubridate::year(date_hidrologico)) %>%
    distinct(year)
  
  data_estacion %>%
    dplyr::arrange(date_hidrologico) %>%
    dplyr::mutate(
      year = lubridate::year(date_hidrologico),
      condicion = dplyr::case_when(
        tipo == "seco"   & !is.na(RR) & RR == 0 ~ TRUE,
        tipo == "humedo" & !is.na(RR) & RR >  0 ~ TRUE,
        TRUE ~ FALSE   # NA rompe rachas
      )
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      CD = if (all(is.na(RR))) {
        NA_integer_                 # año sin datos
      } else {
        r <- rle(condicion)
        if (any(r$values)) max(r$lengths[r$values]) else 0
      },
      .groups = "drop"
    ) %>%
    #Forzar aparición de todos los años
    dplyr::right_join(years_full, by = "year") %>%
    dplyr::arrange(year)
}

CDD <- CD(data_estacion, "seco")
CWD <- CD(data_estacion, "humedo")

# -------------------------------
# 10.per_PTOT, PRCP tortal anual cuando RR>PER_p
# -------------------------------
ref_periodo <- data_estacion %>% filter( year(date_hidrologico) >= 1966, year(date_hidrologico) <= 2025, RR > 0 ) 
p95 <- quantile(ref_periodo$RR, 0.95, na.rm = TRUE, type = 6) 
p99 <- quantile(ref_periodo$RR, 0.99, na.rm = TRUE, type = 6)

R_per_PTOT <- function(data_estacion, umbral,
                       year_min = 1966, year_max = 2025) {
  
  # Vector completo de años hidrológicos
  years_full <- tibble(year = year_min:year_max)
  
  data_estacion %>%
    mutate(
      year = lubridate::year(date_hidrologico)
    ) %>%
    group_by(year) %>%
    summarise(
      R_per_PTOT = if (all(is.na(RR))) NA_real_
      else sum(RR[RR > umbral], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Forzar años faltantes
    right_join(years_full, by = "year") %>%
    arrange(year)
}


R95pTOT <- R_per_PTOT(data_estacion, p95)
R95pTOT

R99pTOT <- R_per_PTOT(data_estacion, p99)
R99pTOT

# -------------------------------
# 11.PRCPTOT precipitación total anual en días húmedos
#PRCPTOT_j= sum(RR_ij)
# -------------------------------


PRCPTOT <- function(data_estacion) {
  
  data_estacion %>%
    filter(RR > 0) %>%              # solo días húmedos en zona norte
    mutate(year = year(date_hidrologico)) %>%
    group_by(year) %>%
    summarise(
      PRCPTOT = sum(RR, na.rm = TRUE),
      .groups = "drop"
    )
}
PRCPTOT_anual <- PRCPTOT(data_estacion)
PRCPTOT_anual

# -------------------------------
#RESUMEN: 
# -------------------------------
Rx1dia_anual <- Rx1dia_anual %>% rename(Rx1day = RxDdia)
Rx5dia_anual <- Rx5dia_anual %>% rename(Rx5day = RxDdia)

Rn10mm <- Rn10mm %>% rename(R10mm = Rnnmm)
R20mm  <- R20mm  %>% rename(R20mm = Rnnmm)

CDD <- CDD %>% rename(CDD = CD)
CWD <- CWD %>% rename(CWD = CD)

R95pTOT <- R95pTOT %>% rename(
  R95pTOT = R_per_PTOT
)

R99pTOT <- R99pTOT %>% rename(
  R99pTOT = R_per_PTOT
)

#Matriz final

n_wet_dry_anual <- data_estacion %>%
  mutate(
    year = year(date_hidrologico),
    wet  = RR > 0,
    dry  = RR == 0
  ) %>%
  group_by(year) %>%
  summarise(
    # total de días registrados ese año
    n_days = n(),
    
    # días con dato válido
    n_valid = sum(!is.na(RR)),
    
    # porcentaje de datos válidos
    pct_valid = 100 * n_valid / n_days,
    
    # días húmedos
    n_wet = if (all(is.na(RR))) NA_integer_
    else sum(wet, na.rm = TRUE),
    
    # días secos
    n_dry = if (all(is.na(RR))) NA_integer_
    else sum(dry, na.rm = TRUE),
    
    .groups = "drop"
  )


years_tbl <- data_estacion %>%
  mutate(year = year(date_hidrologico)) %>%
  distinct(year)


metricas_data <- years_tbl %>%
  full_join(n_wet_dry_anual, by = "year") %>%
  full_join(Rx1dia_anual,    by = "year") %>%
  full_join(Rx5dia_anual,    by = "year") %>%
  full_join(SDII_anual,      by = "year") %>%
  full_join(Rn10mm,          by = "year") %>%
  full_join(R20mm,           by = "year") %>%
  full_join(CDD,             by = "year") %>%
  full_join(CWD,             by = "year") %>%
  full_join(PRCPTOT_anual,   by = "year") %>%
  full_join(R95pTOT,         by = "year") %>%
  full_join(R99pTOT,         by = "year") %>%
  arrange(year)

metricas_data

#Graficos Utiles

#Graficos de evolucion temporal
# titulo con nombre de la estacion
# subtitulo con nombre ed la metrica
# unidades a los ejes

#Nombre estacion

nombre_estacion <- dt_estaciones %>%
    filter(id ==  id_estacion) %>%
  pull(name)


########
#Evolución temporal (tendencias y cambios)
theme_metrica <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12),
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_metrica <- function(data, var, nombre_y) {
  ggplot(data, aes(x = year, y = .data[[var]])) +
    geom_line(linewidth = 1, na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    labs(
      title = nombre_estacion,
      x = "Año",
      y = nombre_y
    ) + theme_metrica
}

plot_PRCPTOT <- plot_metrica(metricas_data, "PRCPTOT", "Precipitación anual (mm)")

plot_SDII <- plot_metrica(metricas_data, "SDII", "SDII (mm/día húmedo)")

plot_R95pTOT <- plot_metrica(metricas_data, "R95pTOT", "Precipitación > P95 (mm)")

plot_R99TOT <- plot_metrica(metricas_data, "R99pTOT", "Precipitación > P99 (mm)")

metricas_data %>%
  select(year, R95pTOT, R99pTOT) %>%
  pivot_longer(-year, names_to = "metrica", values_to = "valor") %>%
  ggplot(aes(year, valor)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ metrica, scales = "free_y",
             labeller = labeller(
               metrica = c(
                 R95pTOT = "P95",
                 R99pTOT = "P99"
               )
             )) +
  labs(
    title = nombre_estacion,
    x = "Año",
    y = "Precipitación (mm)"
  ) +
  theme_metrica

plot_R95Y99pTOT <- ggplot(metricas_data,aes(x = R95pTOT, y =R99pTOT )) +
  # geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  coord_equal(ylim = c(0,80), xlim = c(0,80))+
  theme_minimal()

##########

#Distribución y variabilidad internual


metricas_data_long <- metricas_data %>%
  select(PRCPTOT, SDII, Rx1day, Rx5day, R95pTOT, R99pTOT) %>%
  pivot_longer(
    cols = everything(),
    names_to = "metrica",
    values_to = "valor"
  )

ggplot(metricas_data_long, aes(x = metrica, y = valor)) +
  geom_boxplot(outlier.shape = NA, fill = "grey80") +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  theme_minimal() +
  labs(
    title = "Distribución interanual de métricas de precipitación",
    x = NULL,
    y = "Valor"
  ) +
  coord_flip()

ggplot(metricas_data, aes(x = SDII)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribución interanual del SDII",
    x = "SDII (mm/día)",
    y = "Frecuencia"
  )

##COMPARACOIÓN ENTRE METRICAS
plot_comp <- function(data, x, y, xlab, ylab) {
  ggplot(data, aes(x = {{x}}, y = {{y}})) +
    geom_point(na.rm = TRUE, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black", na.rm = TRUE) +
    theme_minimal() +
    labs(x = xlab, y = ylab)
}

plot_comp(
  metricas_data,
  PRCPTOT, Rx5day,
  "PRCPTOT (mm/año)",
  "Rx5día (mm)"
)

plot_comp(
  metricas_data,
  SDII, Rx1day,
  "SDII (mm/día lluvioso)",
  "Rx1día (mm)"
)

plot_comp(
  metricas_data,
  R95pTOT, PRCPTOT,
  "R95pTOT (mm)",
  "PRCPTOT (mm/año)"
)

##### Prubas con Patchwork


library(patchwork)

p1 <- ggplot(metricas_data, aes(year, PRCPTOT)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(y = "PRCPTOT (mm)", x = NULL)

p2 <- ggplot(metricas_data, aes(year, SDII)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(y = "SDII (mm/día)", x = NULL)

p3 <- ggplot(metricas_data, aes(year, Rx5day)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(y = "Rx5día (mm)", x = "Año")

(p1 / p2 / p3) +
  plot_annotation(
    title = nombre_estacion,
    subtitle = "Evolución temporal de precipitación, intensidad y extremos"
  )


#PRUEBA 2
p1 <- plot_comp(metricas_data, PRCPTOT, SDII,
                "PRCPTOT (mm)", "SDII (mm/día)")

p2 <- plot_comp(metricas_data, PRCPTOT, Rx5day,
                "PRCPTOT (mm)", "Rx5día (mm)")

p3 <- plot_comp(metricas_data, R95pTOT, PRCPTOT,
                "R95pTOT (mm)", "PRCPTOT (mm)")

(p1 | p2) / p3 +
  plot_annotation(
    title = "Relación entre métricas de precipitación"
  )


##Prueba 3
p1 <- ggplot(metricas_data, aes(year, n_wet)) +
  geom_line(na.rm = TRUE) +
  theme_minimal() +
  labs(y = "Días húmedos", x = NULL)

p2 <- ggplot(metricas_data, aes(year, CDD)) +
  geom_line(na.rm = TRUE) +
  theme_minimal() +
  labs(y = "CDD (días)", x = NULL)

p3 <- ggplot(metricas_data, aes(year, PRCPTOT)) +
  geom_line(na.rm = TRUE) +
  theme_minimal() +
  labs(y = "PRCPTOT (mm)", x = "Año")

(p1 / p2 / p3)


#PANEL ENTRE PRCPTOT + SDII + extremos

p_line <- ggplot(metricas_data, aes(year, PRCPTOT)) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  theme_minimal() +
  labs(
    title = "Evolución temporal",
    y = "PRCPTOT (mm)",
    x = "Año"
  )
p_scatter <- ggplot(metricas_data, aes(PRCPTOT, SDII)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Relación cantidad–intensidad",
    x = "PRCPTOT (mm)",
    y = "SDII (mm/día)"
  )
p_box <- ggplot(metricas_data, aes(x = "", y = Rx5day)) +
  geom_boxplot(width = 0.3) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Variabilidad interanual de extremos",
    y = "Rx5día (mm)",
    x = NULL
  )
p_hist <- ggplot(metricas_data, aes(PRCPTOT)) +
  geom_histogram(
    binwidth = 20,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  theme_minimal() +
  labs(
    title = "Distribución de PRCPTOT",
    x = "PRCPTOT (mm)",
    y = "Frecuencia"
  )

library(patchwork)

(p_line | p_scatter|p_box) /
  (   p_hist) +
  plot_annotation(
    title = nombre_estacion,
    subtitle = "Cantidad, intensidad y extremos de la precipitación"
  )

