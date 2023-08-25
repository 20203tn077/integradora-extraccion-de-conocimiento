
# ----------------------------------------------------------------------------------------------------------------------

# Aplicar valores de catálogos

## Localidad

catemima21 <- read.csv('./fuente/catalogos/catemlma21.csv', encoding = 'UTF-8')

matrimonios$loc_regis <- catemima21$nom_loc[match(
  paste(matrimonios$ent_regis, matrimonios$mun_regis, matrimonios$loc_regis),
  paste(catemima21$cve_ent, catemima21$cve_mun, catemima21$cve_loc)
)]

matrimonios$locrh_con1 <- catemima21$nom_loc[match(
  paste(matrimonios$entrh_con1, matrimonios$munrh_con1, matrimonios$locrh_con1),
  paste(catemima21$cve_ent, catemima21$cve_mun, catemima21$cve_loc)
)]

matrimonios$locrh_con2 <- catemima21$nom_loc[match(
  paste(matrimonios$entrh_con2, matrimonios$munrh_con2, matrimonios$locrh_con2),
  paste(catemima21$cve_ent, catemima21$cve_mun, catemima21$cve_loc)
)]

## Tamaño de localidad

tamano_localidad <- read.csv('./fuente/catalogos/tamano_localidad.csv')

matrimonios$tam_loc_re <- tamano_localidad$descripcion[match(matrimonios$tam_loc_re, tamano_localidad$clave)]
matrimonios$tlorh_con1 <- tamano_localidad$descripcion[match(matrimonios$tlorh_con1, tamano_localidad$clave)]
matrimonios$tlorh_con2 <- tamano_localidad$descripcion[match(matrimonios$tlorh_con2, tamano_localidad$clave)]

## Régimen matrimonial

regimen_matrimonial <- read.csv('./fuente/catalogos/regimen_matrimonial.csv')

matrimonios$regimen_ma <- regimen_matrimonial$descripcion[match(matrimonios$regimen_ma, regimen_matrimonial$clave)]

## Género

genero <- read.csv('./fuente/catalogos/genero.csv')

matrimonios$genero <- genero$descripcion[match(matrimonios$genero, genero$clave)]

## Condición biológica

condicion_biologica <- read.csv('./fuente/catalogos/condicion_biologica.csv')

matrimonios$sexo_con1 <- condicion_biologica$descripcion[match(matrimonios$sexo_con1, condicion_biologica$clave)]
matrimonios$sexo_con2 <- condicion_biologica$descripcion[match(matrimonios$sexo_con2, condicion_biologica$clave)]

## Edad

matrimonios$edad_con1 <- ifelse(matrimonios$edad_con1 == 99, NA, matrimonios$edad_con1)
matrimonios$edad_con2 <- ifelse(matrimonios$edad_con2 == 99, NA, matrimonios$edad_con2)

## Nacionalidad

nacionalidad <- read.csv('./fuente/catalogos/nacionalidad.csv')

matrimonios$naci_con1 <- nacionalidad$descripcion[match(matrimonios$naci_con1, nacionalidad$clave)]
matrimonios$naci_con2 <- nacionalidad$descripcion[match(matrimonios$naci_con2, nacionalidad$clave)]

## Ocupación

ocupacion <- read.csv('./fuente/catalogos/ocupacion.csv')

matrimonios$ocup_con1 <- ocupacion$descripcion[match(matrimonios$ocup_con1, ocupacion$clave)]
matrimonios$ocup_con2 <- ocupacion$descripcion[match(matrimonios$ocup_con2, ocupacion$clave)]

## Escolaridad

escolaridad <- read.csv('./fuente/catalogos/escolaridad.csv')

matrimonios$escol_con1 <- escolaridad$descripcion[match(matrimonios$escol_con1, escolaridad$clave)]
matrimonios$escol_con2 <- escolaridad$descripcion[match(matrimonios$escol_con2, escolaridad$clave)]

## Contrayente trabaja

contrayente_trabaja <- read.csv('./fuente/catalogos/contrayente_trabaja.csv')

matrimonios$conactcon1 <- contrayente_trabaja$descripcion[match(matrimonios$conactcon1, contrayente_trabaja$clave)]

matrimonios$conactcon2 <- contrayente_trabaja$descripcion[match(matrimonios$conactcon2, contrayente_trabaja$clave)]

## Situación laboral del contrayente

situacion_laboral_contrayente <- read.csv('./fuente/catalogos/situacion_laboral_contrayente.csv')

matrimonios$sitlabcon1 <- situacion_laboral_contrayente$descripcion[match(matrimonios$sitlabcon1, situacion_laboral_contrayente$clave)]
matrimonios$sitlabcon2 <- situacion_laboral_contrayente$descripcion[match(matrimonios$sitlabcon2, situacion_laboral_contrayente$clave)]

## Posición de trabajo del contrayente

posicion_trabajo_contrayente <- read.csv('./fuente/catalogos/posicion_trabajo_contrayente.csv')

matrimonios$postracon1 <- posicion_trabajo_contrayente$descripcion[match(matrimonios$postracon1, posicion_trabajo_contrayente$clave)]
matrimonios$postracon2 <- posicion_trabajo_contrayente$descripcion[match(matrimonios$postracon2, posicion_trabajo_contrayente$clave)]

## Tipo de contrayentes

tipo_contrayentes <- read.csv('./fuente/catalogos/tipo_contrayentes.csv')

matrimonios$tipo_con <- tipo_contrayentes$descripcion[match(matrimonios$tipo_con, tipo_contrayentes$clave)]

# Reemplazar nemónicos de columnas por nombres completos presentes en diccionario de datos

names(matrimonios) <- substr(diccionario$nombre_campo, 1, nchar(diccionario$nombre_campo) - 1)

library(mice)
md.pattern(matrimonios)

write.csv(matrimonios, './final/conjunto_de_datos/res.csv')
asd <- na.omit(matrimonios)
View(asd)

View(head(asd))

summary(matrimonios)

md.pattern(asd)
