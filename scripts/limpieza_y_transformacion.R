matrimonios <- read.csv('./fuentes/conjunto_de_datos/conjunto_de_datos_matrimonios_2021.csv')
diccionario <- read.csv('./fuentes/diccionario_de_datos/diccionario_de_datos_matrimonios_2021.csv')

# Inspeccionar dataset y diccionario

View(matrimonios)
View(diccionario)

# Reemplazar claves especiales por NA según corresponda

## Localidad

matrimonios$loc_regis[matrimonios$loc_regis == 7777 | matrimonios$loc_regis == 9999 | matrimonios$mun_regis == 999] <- NA
matrimonios$locrh_con1[matrimonios$locrh_con1 == 7777 | matrimonios$locrh_con1 == 9999 | matrimonios$munrh_con1 == 999] <- NA
matrimonios$locrh_con2[matrimonios$locrh_con2 == 7777 | matrimonios$locrh_con2 == 9999 | matrimonios$munrh_con2 == 999] <- NA
matrimonios$dis_re_oax[matrimonios$dis_re_oax == 999] <- NA

## Tamaño de localidad

matrimonios$tam_loc_re[matrimonios$tam_loc_re == 99] <- NA
matrimonios$tlorh_con1[matrimonios$tlorh_con1 == 99] <- NA
matrimonios$tlorh_con2[matrimonios$tlorh_con2 == 99] <- NA

## Día y mes

matrimonios$dia_regis[matrimonios$dia_regis == 99] <- NA
matrimonios$mes_regis[matrimonios$mes_regis == 99] <- NA

## Régimen matrimonial

matrimonios$regimen_ma[matrimonios$regimen_ma == 9] <- NA

## Edad

matrimonios$edad_con1[matrimonios$edad_con1 == 99] <- NA
matrimonios$edad_con2[matrimonios$edad_con2 == 99] <- NA

## Ocupación

matrimonios$ocup_con1[matrimonios$ocup_con1 == 97 | matrimonios$ocup_con1 == 98 | matrimonios$ocup_con1 == 99] <- NA
matrimonios$ocup_con2[matrimonios$ocup_con2 == 97 | matrimonios$ocup_con2 == 98 | matrimonios$ocup_con2 == 99] <- NA

## Escolaridad

matrimonios$escol_con1[matrimonios$escol_con1 == 9] <- NA
matrimonios$escol_con2[matrimonios$escol_con2 == 9] <- NA

## Contrayente trabaja

matrimonios$conactcon1[matrimonios$conactcon1 == 9] <- NA
matrimonios$conactcon2[matrimonios$conactcon2 == 9] <- NA

## Situación laboral del contrayente

matrimonios$sitlabcon1[matrimonios$sitlabcon1 == 9] <- NA
matrimonios$sitlabcon2[matrimonios$sitlabcon2 == 9] <- NA

## Posición de trabajo del contrayente

matrimonios$postracon1[matrimonios$postracon1 == 9] <- NA
matrimonios$postracon2[matrimonios$postracon2 == 9] <- NA

# Establecer una estrategia de limpieza para cada campo al porcentaje de NAs y tipo de dato

sum(is.na(matrimonios$loc_regis)) / nrow(matrimonios) * 100
# 0.09% -> omitir filas con NA

sum(is.na(matrimonios$locrh_con1)) / nrow(matrimonios) * 100
# 13.75% -> omitir filas con NA

sum(is.na(matrimonios$locrh_con2)) / nrow(matrimonios) * 100
# 13.00% -> omitir filas con NA

sum(is.na(matrimonios$dis_re_oax)) / nrow(matrimonios) * 100
# 97.14% -> omitir columna

sum(is.na(matrimonios$tam_loc_re)) / nrow(matrimonios) * 100
# 0% -> ignorar

sum(is.na(matrimonios$tlorh_con1)) / nrow(matrimonios) * 100
# 8.09% -> rellenar con moda

sum(is.na(matrimonios$tlorh_con2)) / nrow(matrimonios) * 100
# 7.44% -> rellenar con moda

sum(is.na(matrimonios$dia_regis)) / nrow(matrimonios) * 100
# 0% -> ignorar

sum(is.na(matrimonios$mes_regis)) / nrow(matrimonios) * 100
# 0% -> ignorar

sum(is.na(matrimonios$regimen_ma)) / nrow(matrimonios) * 100
# 1.21% -> rellenar con moda

sum(is.na(matrimonios$edad_con1)) / nrow(matrimonios) * 100
# 0.25% -> rellenar con mediana

sum(is.na(matrimonios$edad_con2)) / nrow(matrimonios) * 100
# 0.26% -> rellenar con mediana

sum(is.na(matrimonios$ocup_con1)) / nrow(matrimonios) * 100
# 62.77% -> ignorar, se utilizará para aplicar un algoritmo de clasificación

sum(is.na(matrimonios$ocup_con2)) / nrow(matrimonios) * 100
# 62.77% -> ignorar, se utilizará para aplicar un algoritmo de clasificación

sum(is.na(matrimonios$escol_con1)) / nrow(matrimonios) * 100
# 7.0% -> rellenar con moda

sum(is.na(matrimonios$escol_con2)) / nrow(matrimonios) * 100
# 7.13% -> rellenar con moda

sum(is.na(matrimonios$conactcon1)) / nrow(matrimonios) * 100
# 3.07% -> omitir filas con NA

sum(is.na(matrimonios$conactcon2)) / nrow(matrimonios) * 100
# 6.95% -> omitir filas con NA

sum(is.na(matrimonios$sitlabcon1)) / nrow(matrimonios) * 100
# 3.07% -> omitir filas con NA

sum(is.na(matrimonios$sitlabcon2)) / nrow(matrimonios) * 100
# 6.95% -> omitir filas con NA

sum(is.na(matrimonios$postracon1)) / nrow(matrimonios) * 100
# 8.73% -> omitir columna

sum(is.na(matrimonios$postracon2)) / nrow(matrimonios) * 100
# 47.72% -> omitir columa

# Aplicar limpìeza de datos

## Omisión de columna

matrimonios <- matrimonios[, -c(21, 33, 35)]

## Omisión de filas con NA

matrimonios <- matrimonios[
  !(
    is.na(matrimonios$loc_regis) |
    is.na(matrimonios$locrh_con1) |
    is.na(matrimonios$locrh_con2) |
    is.na(matrimonios$conactcon1) |
    is.na(matrimonios$conactcon2) |
    is.na(matrimonios$sitlabcon1) |
    is.na(matrimonios$sitlabcon2)
  )
  ,
]

## Remplazo de NAs con medidas de tendencia central

library(modeest)

matrimonios$tlorh_con1[is.na(matrimonios$tlorh_con1)] <- mfv(matrimonios$tlorh_con1, na_rm = TRUE)
matrimonios$tlorh_con2[is.na(matrimonios$tlorh_con2)] <- mfv(matrimonios$tlorh_con2, na_rm = TRUE)
matrimonios$regimen_ma[is.na(matrimonios$regimen_ma)] <- mfv(matrimonios$regimen_ma, na_rm = TRUE)
matrimonios$escol_con1[is.na(matrimonios$escol_con1)] <- mfv(matrimonios$escol_con1, na_rm = TRUE)
matrimonios$escol_con2[is.na(matrimonios$escol_con2)] <- mfv(matrimonios$escol_con2, na_rm = TRUE)

matrimonios$edad_con1[is.na(matrimonios$edad_con1)] <- median(matrimonios$edad_con1, na.rm = TRUE)
matrimonios$edad_con2[is.na(matrimonios$edad_con2)] <- median(matrimonios$edad_con2, na.rm = TRUE)

# guardar resultado

write.csv(matrimonios, './resultados/conjunto_de_datos/matrimonios.csv')

sum(is.na(matrimonios$ocup_con1) & is.na(matrimonios$ocup_con2))
sum(is.na(matrimonios$ocup_con1) & !is.na(matrimonios$ocup_con2))
sum(!is.na(matrimonios$ocup_con1) & is.na(matrimonios$ocup_con2))