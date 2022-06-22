# =============================================================================#
# Fecha: 2022-22-06 
# Práctica 3
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
               broom, # hacer formato tidy
               DescTools, # herramientas estadísticas
               wesanderson, # Wes Anderson 
               gtsummary, # tablas de resultados con inferencia
               srvyr, # diseño de encuestas
               car) # Companion of Applied Regression

# Datos ----
ags_t321 <- read_dta("./datos/datos/AGS_SDEMT321.dta", encoding="latin1") %>% 
  clean_names()

ICI_2021 <- read_excel("./datos/datos/ICI_2021.xlsx",
                       sheet = "ICI 2021") %>% 
  clean_names()

ags_t321 %<>%
  filter(r_def==0) %>% 
  filter(!c_res==2)


# Análisis de Varianza 

ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  ggplot() +
  aes(x=ing_x_hrs) +
  geom_density() + facet_wrap(~t_loc_tri)


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  ggplot() +
  aes(x=log(ing_x_hrs), 
      fill=as_factor(t_loc_tri),
      color=as_factor(t_loc_tri),
      alpha=I(0.5)) +
  geom_density() + theme_minimal()


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>%
  group_by(t_loc_tri) %>% 
  summarise(media_ing=mean(ing_x_hrs),
            media_log=mean(log(ing_x_hrs)),
            sd_ing=sd(ing_x_hrs),
            sd_log=sd(log(ing_x_hrs)))

# El análisis de varianza


anova<-ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>%
  with(
    aov(log(ing_x_hrs) ~ as_factor(t_loc_tri))
  )

summary(anova)

#Ho: La varianza explicada por los grupos no es mayor que entre los grupos
#Ha: La varianza explicada por los grupos es mayor que entre los grupos

#Ho: µ1=µ2=µ3=µ4
#Ha: Que al menos una de las medias sea diferente

broom::tidy(anova)

TukeyHSD(anova)


# Evaluar los supuestos

# Para evaluar la igualdad de varianzas
# Prueba de Barlett

ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>%
  with(
    bartlett.test(log(ing_x_hrs) ~ as_factor(t_loc_tri) )
  )

# Ho: Las varianzas son iguales
# Ha: Al menos una varianza no es igual

# Se rechaza la igualdad de varianzas
# Por lo tanto no cumplo mi supuesto :(
# Ni a nivel, ni con la transformación logarítmica de los ingresos. 

# Prueba de normalidad

ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(t_loc_tri==4) %>% 
  with(
    ks.test(log(ing_x_hrs),
            "pnorm", mean=mean(log(ing_x_hrs)), sd=sd(log(ing_x_hrs)))
  )
# Ho: Las distribuciones son iguales
# En este caso, los ingresos de mi muestra tienen un distribución igual a una normal
# Ha: La distribución no es normal

# * La distribución de los ingresos por hora no es normal *
# * La distribución del logaritmo de los ingresos no es normal



## Kruskal-Wallis ----

kruskal<-ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    kruskal.test(ing_x_hrs ~ as_factor(t_loc_tri))
  )
kruskal
tidy(kruskal)

# Ho: las distribuciones entre los grupos son iguales
# Ha: al menos una distribución es diferente

ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    DescTools::DunnTest(ing_x_hrs ~ as_factor(t_loc_tri))
  )

DunnTest(ing_x_hrs ~ as_factor(t_loc_tri), data=ags_t321[ags_t321$ing_x_hrs>0 & ags_t321$clase2==1,])

DunnTest(ags_t321[ags_t321$ing_x_hrs>0 & ags_t321$clase2==1,]$ing_x_hrs ~ as_factor(ags_t321[ags_t321$ing_x_hrs>0 & ags_t321$clase2==1,]$t_loc_tri))

# Factores de expansión

ags_t321 %>% 
  select(fac_tri)

ags_t321 %>% 
  tabyl(sex)

ags_t321 %>% 
  group_by(sex) %>% 
  tally()

ags_t321 %>% 
  group_by(as_label(sex)) %>% 
  tally(fac_tri) %>% 
  adorn_totals() %>% 
  adorn_percentages("all") %>% 
  adorn_pct_formatting()



ags_t321 %>% 
  dplyr::count(as_label(sex), wt=fac_tri) %>% 
  adorn_totals() %>% 
  adorn_percentages("all") %>% 
  adorn_pct_formatting()

# De doble entrada

ags_t321 %>% 
  count(as_label(sex), as_label(niv_ins), wt=fac_tri) %>% 
  tidyr::pivot_wider(names_from = `as_label(sex)`, 
                     values_from = n) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting()
  

# Diseño complejo ----


ags_srvy <- ags_t321 %>%
  srvyr::as_survey_design(weights = fac_tri)

# Todo el diseño de la ENOE


ags_srvy <- ags_t321 %>%
  srvyr::as_survey_design(
    upm,
    weights = fac_tri,
    strata = est_d_tri,
    nest = TRUE)

ags_srvy %>% 
  filter(eda>14 & eda<99) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(clase2==1) %>% 
  summarise(media=mean(ing_x_hrs), # sin diseño
            media_ponderada=survey_mean(ing_x_hrs) # con diseño
            )

t_test<-ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  filter(ing_x_hrs>0) %>% 
  with(t.test(ing_x_hrs))

t_test$stderr


ags_srvy %>% 
  filter(eda>14 & eda<99) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(clase2==1) %>% 
  summarise(
            media_ponderada=survey_mean(ing_x_hrs, vartype = "ci") # con diseño
  )


ags_srvy %>% 
  filter(eda>14 & eda<99) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(clase2==1) %>% 
  summarise(
    mediana_ponderada=survey_median(ing_x_hrs, vartype = "ci") # con diseño
  )

# Tabulados de sexo

ags_srvy %>% 
  mutate(sex=as_label(sex)) %>% 
  group_by(sex) %>% 
  summarise(proporcion=survey_mean(vartype = "ci"),
            total=survey_total(vartype = "ci")) %>% 
  adorn_totals()


# Introducción a la regresión lineal 


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  ggplot() +
  aes(y=log(ing_x_hrs), x=anios_esc) +
  geom_point()

ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  ggplot() +
  aes(y=log(ing_x_hrs), x=anios_esc) +
  geom_jitter()


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  ggplot() +
  aes(y=log(ing_x_hrs), x=anios_esc, alpha=I(0.4)) +
  geom_jitter()


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  ggplot() +
  aes(y=log(ing_x_hrs), x=anios_esc, alpha=I(0.4)) +
  geom_jitter() +
  geom_smooth(method=lm)


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  with(
    lm(log(ing_x_hrs) ~ anios_esc)
  )


model<-ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  filter(anios_esc<99) %>% 
  with(
    lm(log(ing_x_hrs) ~ anios_esc)
  )

summary(model)
confint(model)
anova(model)

tidy(model)
broom::glance(model)

model %>% 
  gtsummary::tbl_regression()

model %>% 
  gtsummary::tbl_regression() %>% 
  add_significance_stars() %>% 
  add_glance_table()