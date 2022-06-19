# =============================================================================#
# Fecha: 2022-01-14 
# Práctica 4
# Original: Ana Escoto

# =============================================================================#

# Paquetes ----
if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere
pacman::p_load(tidyverse,
               readxl,
               writexl, 
               haven,
               sjlabelled, 
               janitor,
               infer, 
               ggpubr,
               magrittr,
               gt,
               GGally,
               broom,
               DescTools,
               wesanderson,
               gtsummary,
               srvyr,
               car, # verificar supuestos y postestimaciones
               sjPlot, # graficar modelos y sus efectos
               jtools, # presentación de modelos, errores corrección, gráficos
                      # tablas de modelos
               sandwich, # dependencia de jtools errores
               huxtable) # dependencia de jtools tablas

ags_t321 <- read_dta("./datos/datos/AGS_SDEMT321.dta", encoding="latin1") %>%
  clean_names()

ags_t321 %<>%
  filter(r_def==0) %>% 
  filter(!c_res==2) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(clase2==1) %>% 
  filter(anios_esc<99)

# Correlación ----

## Estimación del coeficiente de Pearson  ----
ags_t321 %>% 
  with(
    cor(log(ing_x_hrs), anios_esc)
  )

cor(ags_t321$ing_x_hrs, ags_t321$anios_esc)

## Pruebas de hipótesis

ags_t321 %>% 
  with(
    cor.test(log(ing_x_hrs), anios_esc)
  )
cor_test<-
  ags_t321 %>% 
  with(
    cor.test(log(ing_x_hrs), anios_esc)
  )

tidy(cor_test)

# Modelo simple ----

ags_t321 %<>% 
  mutate(log_ing_x_hrs=log(ing_x_hrs))

modelo <- ags_t321 %>% 
  with(lm(log_ing_x_hrs~anios_esc))

summary(modelo) # resultado forma1
anova(modelo)

# Diagnósticos ----

plot(modelo)

# Para identificar outliers

car::outlierTest(modelo)

outliers<-c(1368,1384)

ags_t321$row<-rownames(ags_t321)

ags_t321 %>% 
  filter(!row==outliers)


ggpubr::ggqqplot(ags_t321$log_ing_x_hrs)

## Homocedasticidad ----

car::ncvTest(modelo)

#Ho: Los errores son homocedásticos
#Ha: Los errores no son homocedásticos

1.4105e-10<0.05

# Rechazo la Ho.
# Puedo afirmar que hay heterocedasticidad

# plot studentized residuals vs. fitted values 
car::spreadLevelPlot(modelo)


# Agregando una variable----

ags_t321 %>% 
  ggplot() +
  aes(x=anios_esc, y=log(ing_x_hrs), alpha=I(0.5), color=as_label(sex)) + 
  geom_jitter()+
  geom_smooth(method = lm)



modelo1<-ags_t321 %>% 
  mutate(sex=as_label(sex)) %>% 
  with(
    lm(log_ing_x_hrs ~ anios_esc + sex)
  )

rm(cor_test)

summary(modelo1)

pruebaf0<-anova(modelo, modelo1)
pruebaf0

# Ho: el modelo más grande explica igual o menos que el modelo más pequeño
# Ha: el modelo más grande explica más que el modelo más pequeño


# Otra variable más ----
modelo1<-ags_t321 %>% 
  mutate(sex=as_label(sex)) %>% 
  with(
    lm(log_ing_x_hrs ~ anios_esc + sex)
  )

summary(modelo2)

pruebaf1<-anova(modelo1, modelo2)
pruebaf1

modelo3<-ags_t321 %>% 
  mutate(sex=as_label(sex)) %>%   
  mutate(t_loc_tri=as_factor(t_loc_tri)) %>% 
  with(
    lm(log_ing_x_hrs ~ anios_esc + sex + t_loc_tri)
  )

summary(modelo3)

modelo3<-ags_t321 %>% 
  mutate(sex=as_label(sex)) %>%   
  mutate(sex=relevel(sex, ref="Mujer")) %>% 
  mutate(t_loc_tri=as_factor(t_loc_tri)) %>% 
  with(
    lm(log_ing_x_hrs ~ anios_esc + sex + t_loc_tri)
  )

summary(modelo3)

# Verificando la multicolinealidad

car::vif(modelo2)

# Jtools

jtools::summ(modelo)

jtools::summ(modelo, robust="HC3")

jtools::summ(modelo2)
jtools::summ(modelo2,  scale=T) # valores estandarizados

jtools::export_summs(modelo, modelo1, modelo2, modelo3)

# Gráficos

sjPlot::plot_model(modelo2)
sjPlot::plot_models(modelo, modelo1, modelo2, modelo3)


# Post-estimacion  ----

sjPlot::plot_model(modelo2,
                   type="pred",
                   terms = "anios_esc")

pred<-sjPlot::plot_model(modelo2,
                     type="pred",
                     terms = "anios_esc")

pred$data

plot_model(modelo2, 
           type="pred", 
           terms = c("anios_esc","sex")) + theme_blank()


plot_model(modelo2, 
           type="pred", 
           terms = c("sex","anios_esc")) 


# Extensiones a la regresión lineal 


modelo_int1<-ags_t321 %>% 
  mutate(t_loc_tri=as_label(t_loc_tri)) %>% 
  with(
    lm(log_ing_x_hrs ~ anios_esc * t_loc_tri )
  )

summary(modelo_int1)

anova(modelo1, modelo_int1)

plot_model(modelo_int1, type="int", terms = c("t_loc_tri", "anios_esc"))



# Logaritmo

modelo_log<-ags_t321 %>% 
  mutate(sex=as_label(sex)) %>% 
  with(
    lm(log(ing_x_hrs) ~ log(eda) + sex))

summary(modelo_log)


plot_model(modelo_log, type="pred", terms ="eda")


# Efectos cuadráticos

modelo_quadr<-ags_t321 %>% 
  mutate(sex=as_label(sex)) %>%
  with(
    lm(log(ing_x_hrs)~ sex + anios_esc + I(anios_esc^2))
  )

summary(modelo_quadr)

plot_model(modelo_quadr, type="pred", terms = c("anios_esc"))