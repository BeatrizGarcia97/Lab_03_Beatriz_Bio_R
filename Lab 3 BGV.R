#=========================================================
# Laboratorio 03
# Analisis parametrico con diseño factorial en madera de melina
#=========================================================

#---------------------------------------------------------
# 1. Cargar paquetes
#---------------------------------------------------------

#install.packages("readxl")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("car")
#install.packages("emmeans")
#install.packages("multcomp")
#install.packages("multcompView")

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(multcomp)
library(multcompView)

#---------------------------------------------------------
# 2. Importar datos
#---------------------------------------------------------

datos <- read_excel(
  "Lab_03/secado_melina.xlsx",
  sheet = "data"
)

#---------------------------------------------------------
# 3. Exploracion inicial
#---------------------------------------------------------

names(datos)
str(datos)
head(datos)
summary(datos)

# Visualizar datos
View(datos)

#---------------------------------------------------------
# 4. Preparacion de variables
#---------------------------------------------------------

datos$Proceso_produccion <-
  as.factor(datos$Proceso_produccion)

datos$Metodo_secado <-
  as.factor(datos$Metodo_secado)

datos$Presencia_curvatura <-
  as.factor(datos$Presencia_curvatura)

datos$Presencia_rajadura <-
  as.factor(datos$Presencia_rajadura)

# Crear variable de tratamiento
datos$Tratamiento <-
  interaction(
    datos$Proceso_produccion,
    datos$Metodo_secado
  )

# Verificar niveles
levels(datos$Proceso_produccion)
levels(datos$Metodo_secado)

#---------------------------------------------------------
# 5. Estadistica descriptiva
#---------------------------------------------------------

# Calidad
resumen_calidad <-
  datos %>%
  group_by(
    Proceso_produccion,
    Metodo_secado
  ) %>%
  summarise(
    n = n(),
    media = mean(Calidad_pct),
    sd = sd(Calidad_pct),
    min = min(Calidad_pct),
    max = max(Calidad_pct),
    .groups = "drop"
  )

resumen_calidad
View(resumen_calidad)

# Curvatura
resumen_curvatura <-
  datos %>%
  group_by(
    Proceso_produccion,
    Metodo_secado
  ) %>%
  summarise(
    media = mean(Curvatura_mm),
    sd = sd(Curvatura_mm),
    .groups = "drop"
  )

resumen_curvatura
View(resumen_curvatura)

# Rajadura
resumen_rajadura <-
  datos %>%
  group_by(
    Proceso_produccion,
    Metodo_secado
  ) %>%
  summarise(
    media = mean(Rajadura_cm),
    sd = sd(Rajadura_cm),
    .groups = "drop"
  )

resumen_rajadura
View(resumen_rajadura)

#---------------------------------------------------------
# 6. Boxplot de Calidad
#---------------------------------------------------------

grafico_boxplot <-
  ggplot(
    datos,
    aes(
      x = Tratamiento,
      y = Calidad_pct,
      fill = Metodo_secado
    )
  ) +
  geom_boxplot() +
  theme_bw() +
  labs(
    title = "Distribucion de Calidad por Tratamiento",
    x = "Tratamiento",
    y = "Calidad (%)"
  )

grafico_boxplot

# Guardar grafico
ggsave(
  "boxplot_calidad.png",
  grafico_boxplot,
  width = 7,
  height = 5
)

#---------------------------------------------------------
# 7. Grafico de interaccion
#---------------------------------------------------------

medias <-
  datos %>%
  group_by(
    Proceso_produccion,
    Metodo_secado
  ) %>%
  summarise(
    media = mean(Calidad_pct),
    se = sd(Calidad_pct) / sqrt(n()),
    .groups = "drop"
  )

medias
View(medias)

grafico_interaccion <-
  ggplot(
    medias,
    aes(
      x = Metodo_secado,
      y = media,
      color = Proceso_produccion,
      group = Proceso_produccion
    )
  ) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(
    aes(
      ymin = media - se,
      ymax = media + se
    ),
    width = 0.1
  ) +
  theme_bw() +
  labs(
    title = "Grafico de interaccion para Calidad",
    x = "Metodo de secado",
    y = "Calidad promedio (%)"
  )

grafico_interaccion

# Guardar grafico
ggsave(
  "grafico_interaccion.png",
  grafico_interaccion,
  width = 7,
  height = 5
)

#---------------------------------------------------------
# 8. ANOVA factorial - Calidad
#---------------------------------------------------------

modelo_calidad <-
  aov(
    Calidad_pct ~
      Proceso_produccion *
      Metodo_secado,
    data = datos
  )

summary(modelo_calidad)

#---------------------------------------------------------
# 9. Verificacion de supuestos - Calidad
#---------------------------------------------------------

# Extraer residuos
residuos_calidad <-
  residuals(modelo_calidad)

# Normalidad
shapiro.test(residuos_calidad)

# Homogeneidad de varianzas
leveneTest(
  Calidad_pct ~
    Proceso_produccion *
    Metodo_secado,
  data = datos
)

# Graficos diagnosticos
par(mfrow = c(2,2))
plot(modelo_calidad)

# QQ Plot
qqnorm(residuos_calidad)
qqline(residuos_calidad)

#---------------------------------------------------------
# 10. Comparacion de medias Tukey - Calidad
#---------------------------------------------------------

emm_calidad <-
  emmeans(
    modelo_calidad,
    ~ Proceso_produccion *
      Metodo_secado
  )

# Comparaciones multiples
pairs(
  emm_calidad,
  adjust = "tukey"
)

# Medias ajustadas
summary(emm_calidad)

# Letras de agrupacion
letras_calidad <-
  cld(
    emm_calidad,
    adjust = "tukey",
    Letters = letters
  )

letras_calidad

View(
  as.data.frame(letras_calidad)
)

#---------------------------------------------------------
# 11. ANOVA factorial - Curvatura
#---------------------------------------------------------

modelo_curvatura <-
  aov(
    Curvatura_mm ~
      Proceso_produccion *
      Metodo_secado,
    data = datos
  )

summary(modelo_curvatura)

#---------------------------------------------------------
# 12. Supuestos - Curvatura
#---------------------------------------------------------

residuos_curvatura <-
  residuals(modelo_curvatura)

# Normalidad
shapiro.test(residuos_curvatura)

# Homogeneidad
leveneTest(
  Curvatura_mm ~
    Proceso_produccion *
    Metodo_secado,
  data = datos
)

# Graficos diagnosticos
par(mfrow = c(2,2))
plot(modelo_curvatura)

#---------------------------------------------------------
# 13. ANOVA factorial - Rajadura
#---------------------------------------------------------

modelo_rajadura <-
  aov(
    Rajadura_cm ~
      Proceso_produccion *
      Metodo_secado,
    data = datos
  )

summary(modelo_rajadura)

#---------------------------------------------------------
# 14. Supuestos - Rajadura
#---------------------------------------------------------

residuos_rajadura <-
  residuals(modelo_rajadura)

# Normalidad
shapiro.test(residuos_rajadura)

# Homogeneidad
leveneTest(
  Rajadura_cm ~
    Proceso_produccion *
    Metodo_secado,
  data = datos
)

# Graficos diagnosticos
par(mfrow = c(2,2))
plot(modelo_rajadura)

#---------------------------------------------------------
# 15. Boxplots adicionales
#---------------------------------------------------------

# Curvatura
grafico_curvatura_box <-
  ggplot(
    datos,
    aes(
      x = Tratamiento,
      y = Curvatura_mm,
      fill = Metodo_secado
    )
  ) +
  geom_boxplot() +
  theme_bw() +
  labs(
    title = "Distribucion de Curvatura",
    x = "Tratamiento",
    y = "Curvatura (mm)"
  )

grafico_curvatura_box

# Rajadura
grafico_rajadura_box <-
  ggplot(
    datos,
    aes(
      x = Tratamiento,
      y = Rajadura_cm,
      fill = Metodo_secado
    )
  ) +
  geom_boxplot() +
  theme_bw() +
  labs(
    title = "Distribucion de Rajadura",
    x = "Tratamiento",
    y = "Rajadura (cm)"
  )

grafico_rajadura_box

#---------------------------------------------------------
# 16. Frecuencias de defectos
#---------------------------------------------------------

# Tabla curvatura
tabla_curvatura <-
  table(
    datos$Tratamiento,
    datos$Presencia_curvatura
  )

tabla_curvatura

View(as.data.frame(tabla_curvatura))

# Tabla rajadura
tabla_rajadura <-
  table(
    datos$Tratamiento,
    datos$Presencia_rajadura
  )

tabla_rajadura

View(as.data.frame(tabla_rajadura))

#---------------------------------------------------------
# 17. Graficos de frecuencia
#---------------------------------------------------------

# Curvatura
grafico_frec_curvatura <-
  ggplot(
    datos,
    aes(
      x = Tratamiento,
      fill = Presencia_curvatura
    )
  ) +
  geom_bar(position = "dodge") +
  theme_bw() +
  labs(
    title = "Frecuencia de presencia de curvatura",
    x = "Tratamiento",
    y = "Frecuencia"
  )

grafico_frec_curvatura

# Rajadura
grafico_frec_rajadura <-
  ggplot(
    datos,
    aes(
      x = Tratamiento,
      fill = Presencia_rajadura
    )
  ) +
  geom_bar(position = "dodge") +
  theme_bw() +
  labs(
    title = "Frecuencia de presencia de rajadura",
    x = "Tratamiento",
    y = "Frecuencia"
  )

grafico_frec_rajadura

#---------------------------------------------------------
# 18. Resumen final
#---------------------------------------------------------

resumen_final <-
  datos %>%
  group_by(
    Proceso_produccion,
    Metodo_secado
  ) %>%
  summarise(
    Calidad_promedio = mean(Calidad_pct),
    Curvatura_promedio = mean(Curvatura_mm),
    Rajadura_promedio = mean(Rajadura_cm),
    .groups = "drop"
  )

resumen_final

View(resumen_final)

# Exportar resumen
write.csv(
  resumen_final,
  "resumen_final.csv",
  row.names = FALSE
)

#---------------------------------------------------------
# FIN DEL SCRIPT
#---------------------------------------------------------