
# Directorio
setwd("C:/Marcos- Memoria/01. Tecnico/extremesnorthchile/data")

# Librerías
library(openxlsx)
library(dplyr)
library(lubridate)
library(zoo)   # para rollsum()
# -------------------------------
# 1. Leer datos
# -------------------------------
data <- read.xlsx("Sol_AM006T_0028840_PrecipitacionH.xlsx", sheet = 1)

# -------------------------------
# 2. Filtrar estación
# -------------------------------
data_pastos <- data %>%
  filter(ESTACIÓN == "PASTOS GRANDES")

# -------------------------------
# 3. Crear fecha-hora CORRECTA
# -------------------------------
data_pastos <- data_pastos %>%
  mutate(
    datetime = dmy_hm(
      paste(Fecha, Hora),
      tz = "America/Santiago"
    )
  )

# -------------------------------
# 4. Precipitación diaria (RR_ij)
# -------------------------------
precip_diaria <- data_pastos %>%
  mutate(fecha = as.Date(datetime)) %>%
  group_by(fecha) %>%
  summarise(
    RR = sum(Valor, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------
# 5. Rx1día mensual
# Rx1día_j = max(RR_ij)
# -------------------------------
Rx1dia_mensual <- precip_diaria %>%
  mutate(
    year  = year(fecha),
    month = month(fecha)
  ) %>%
  group_by(year, month) %>%
  summarise(
    Rx1dia = max(RR, na.rm = TRUE),
    .groups = "drop"
  )

Rx1dia_mensual


# -------------------------------
# 6. Rx5día mensual
# Rx5día_j = max(RR_ij)
# -------------------------------
precip_5dias <- precip_diaria %>%
  arrange(fecha) %>%
  mutate(
    RR_5dias = rollsum(RR, k = 5, align = "right", fill = NA)
  )


Rx5dia_mensual <- precip_5dias %>%
  mutate(
    year  = year(fecha),
    month = month(fecha)
  ) %>%
  group_by(year, month) %>%
  summarise(
    Rx5dia = max(RR_5dias, na.rm = TRUE),
    .groups = "drop"
  )

Rx5dia_mensual
# -------------------------------
# 7. SDII Índice de intensidad mensual
# SDII_i = sum(RR_wj)/W
# -------------------------------
precip_diaria <- precip_diaria %>%
  mutate(lluvioso = RR > 0)      # DefinimosRR ≥ 0 mm

SDII_mensual <- precip_diaria %>%
  mutate(
    year  = year(fecha),
    month = month(fecha)
  ) %>%
  group_by(year, month) %>%
  summarise(
    SDII = sum(RR[lluvioso], na.rm = TRUE) / sum(lluvioso),
    .groups = "drop"
  )

SDII_mensual

# -------------------------------
# 8. Rnnmm Conteo anual de dias cunado PRCP > nnmm, nn es un umbral definido
# RRij >20mm
# -------------------------------
Rnnmm <- function(precip_diaria, nn) {
  
  precip_diaria %>%
    mutate(year = year(fecha)) %>%
    group_by(year) %>%
    summarise(
      Rnnmm = sum(RR >= nn, na.rm = TRUE),
      .groups = "drop"
    )
} 
#Recuento anual de dias con PRCP >= 10 mm
Rn10mm <- Rnnmm(precip_diaria,10)
Rn10mm

#Recuento anual de dias con PRCP >= 20 mm
R20mm <- Rnnmm(precip_diaria,20)
R20mm

# -------------------------------
# 9. CDD Dureción máxima del período seco, número máximo de días consecutivos con RR >=0
#   CWD Dureción máxima del período seco, número máximo de días consecutivos con RR >=0
# RRij <1mm o >1mm
# -------------------------------
CD <- function(precip_diaria, tipo = c("seco", "humedo")) {
  
  tipo <- match.arg(tipo)
  
  precip_diaria %>%
    mutate(
      year = year(fecha),
      condicion = ifelse(
        tipo == "seco", RR < 1, RR >= 1
      )
    ) %>%
    arrange(fecha) %>%
    group_by(year) %>%
    summarise(
      CD = {
        r <- rle(condicion)
        if (any(r$values)) max(r$lengths[r$values]) else 0
      },
      .groups = "drop"
    )
}

CDD <- CD(precip_diaria, "seco")
CDD
CWD <- CD(precip_diaria, "humedo")
CWD

# -------------------------------
# 10.per_PTOT, PRCP tortal anual cuando RR>PER_p
# -------------------------------
ref_periodo <- precip_diaria %>%
  filter(
    year(fecha) >= 1980,
    year(fecha) <= 2025,
    RR >= 0                     #Definmimos que la precipitacion sea mayor a 0 por ser zona de bajas precipitaciones
  )

p95 <- quantile(ref_periodo$RR, 0.95, na.rm = TRUE)
p99 <- quantile(ref_periodo$RR, 0.99, na.rm = TRUE)

R_per_PTOT <- function(precip_diaria, umbral) {
  
  precip_diaria %>%
    filter(RR > 0) %>%              # días lluviosos
    mutate(year = year(fecha)) %>%
    group_by(year) %>%
    summarise(
      R_per_PTOT = sum(RR[RR > umbral], na.rm = TRUE),
      .groups = "drop"
    )
}

R95pTOT <- R_per_PTOT(precip_diaria, p95)
R95pTOT

R99pTOT <- R_per_PTOT(precip_diaria, p99)
R99pTOT


# -------------------------------
# 11.PRCPTOT precipitación total anual en días húmedos
#PRCPTOT_j= sum(RR_ij)
# -------------------------------


PRCPTOT <- function(precip_diaria) {
  
  precip_diaria %>%
    filter(RR > 0) %>%              # solo días húmedos en zona norte
    mutate(year = year(fecha)) %>%
    group_by(year) %>%
    summarise(
      PRCPTOT = sum(RR, na.rm = TRUE),
      .groups = "drop"
    )
}
PRCPTOT_anual <- PRCPTOT(precip_diaria)
PRCPTOT_anual

# -------------------------------
#RESUMEN: pendienteee
# -------------------------------

