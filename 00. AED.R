
# Directorio
setwd("C:/Marcos- Memoria/01. Tecnico/extremesnorthchile/data")

# Librerías
library(openxlsx)
library(dplyr)
library(lubridate)
library(zoo)  
library(tidyr)# para rollsum()
# -------------------------------
# 1. Leer datos de VISHMA21
# -------------------------------

nombre_archivo <- "RegistrosDiarios_pr_2026-01-05.xlsx"
dt <- read.xlsx(nombre_archivo, sheet = 2)
dt_estaciones <- read.xlsx(nombre_archivo, sheet = 1)

# -------------------------------
# 2. Filtrar estación
# -------------------------------
id_estacion <- dt_estaciones %>%
  filter(name == "PASTOS GRANDES") %>%
  pull(id)

#data estación continua
data_estacion <- dt %>%
  filter(id %in% id_estacion) %>%
  mutate(
    # Fecha desde Excel
    date = as.Date(date, origin = "1899-12-30"),
    
    # Fecha desplazada 3 meses (año hidrológico)
    date_hidrologico = date %m-% months(3)
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
    SDII = sum(RR[lluvioso], na.rm = T) / sum(lluvioso),
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
  
  # Chequeo de seguridad
  stopifnot(is.data.frame(data_estacion))
  
  tipo <- match.arg(tipo)
  
  data_estacion %>%
    dplyr::arrange(date_hidrologico) %>%
    dplyr::mutate(
      year = lubridate::year(date_hidrologico),
      condicion = dplyr::case_when(
        tipo == "seco"   & !is.na(RR) & RR == 0 ~ TRUE,
        tipo == "humedo" & !is.na(RR) & RR >  0 ~ TRUE,
        TRUE ~ FALSE   # incluye NA y rompe rachas
      )
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      CD = {
        r <- rle(condicion)
        if (any(r$values)) max(r$lengths[r$values]) else 0
      },
      .groups = "drop"
    )
}
CDD <- CD(data_estacion, "seco")
CWD <- CD(data_estacion, "humedo")

# -------------------------------
# 10.per_PTOT, PRCP tortal anual cuando RR>PER_p
# -------------------------------
ref_periodo <- data_estacion %>%
  filter(
    year(date_hidrologico) >= 1980,
    year(date_hidrologico) <= 2025,
    RR > 0
  )

p95 <- quantile(ref_periodo$RR, 0.95, na.rm = TRUE)

p99 <- quantile(ref_periodo$RR, 0.99, na.rm = TRUE)

R_per_PTOT <- function(data_estacion, umbral) {
  
  data_estacion %>%
    mutate(
      year = year(date_hidrologico),
      wet = RR > 0,
      dry = RR == 0
    ) %>%
    group_by(year) %>%
    summarise(
      n_wet = sum(wet, na.rm = TRUE),                 # días húmedos
      n_dry = sum(dry, na.rm = TRUE),                 # días secos
      R_per_PTOT = sum(RR[RR > umbral], na.rm = TRUE),# precip sobre percentil
      .groups = "drop"
    )
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
  R95pTOT = R_per_PTOT,
  n_wet_95 = n_wet,
  n_dry_95 = n_dry
)

R99pTOT <- R99pTOT %>% rename(
  R99pTOT = R_per_PTOT,
  n_wet_99 = n_wet,
  n_dry_99 = n_dry
)

metricas_data <- Rx1dia_anual %>%
  full_join(Rx5dia_anual,  by = "year") %>%
  full_join(SDII_anual,    by = "year") %>%
  full_join(Rn10mm,        by = "year") %>%
  full_join(R20mm,         by = "year") %>%
  full_join(CDD,           by = "year") %>%
  full_join(CWD,           by = "year") %>%
  full_join(PRCPTOT_anual, by = "year") %>%
  full_join(R95pTOT,       by = "year") %>%
  full_join(R99pTOT,       by = "year") %>%
  arrange(year)

metricas_data
