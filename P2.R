# =============================================================================#
# Fecha: 2022-01-11 
# Práctica 2
# Original: Ana Escoto
# Autor: modifiqué la versión X
# =============================================================================#

# Paquetes ----

if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere
pacman::p_load(tidyverse,
               readxl,
               writexl, 
               haven,
               sjlabelled, 
               janitor,
               infer, # Inferencia
               ggpubr,
               magrittr,
               gt,
               GGally, # Extensión de ggplot
               broom, # limpiar los resultados
               DescTools, # Inferencia
               wesanderson) # Los colores de las películas de Wes Anderson 
# Datos ----

ags_t321 <- read_dta("./datos/datos/AGS_SDEMT321.dta", encoding="latin1") %>% 
  clean_names()

ICI_2021 <- read_excel("./datos/datos/ICI_2021.xlsx", sheet = "ICI 2021") %>% 
  clean_names()

ags_t321 %<>%
  filter(r_def==0) %>% # entrevistas definitivas
  filter(!c_res==2) # habitantes no ausentes

# Visualización de datos ----

## Gráficos de base ----

plot(as_label(ags_t321$niv_ins))
plot(ags_t321$niv_ins)

barplot(table(as_label(ags_t321$niv_ins)))

hist(ICI_2021$esperanza_de_vida)
boxplot(ICI_2021$esperanza_de_vida)

## GGplot2 ----

#Gráfico simple de barra
ags_t321 %>% 
  ggplot2::ggplot() + 
  aes(as_label(niv_ins)) + # hasta acá es el lienzo
  geom_bar() # geometría


ags_t321 %>% 
  ggplot2::ggplot() + 
  aes(as_label(niv_ins), 
      fill=as_label(niv_ins),
        ) +
  geom_bar()


ags_t321 %>% 
  ggplot2::ggplot() + 
  aes(as_label(niv_ins), 
      fill=as_label(niv_ins),
  ) +
  geom_bar() +
  coord_flip() + guides(fill="none")

# Para variables cuantitativas 

ags_t321 %>% 
  ggplot()+
  aes(eda) + 
  geom_histogram()


ags_t321 %>% 
  ggplot()+
  aes(eda) + 
  geom_density()


ags_t321 %>% 
  filter(clase2==1) %>%  # nos quedamos sólo con los ocupados
  select(eda, ing_x_hrs, anios_esc) %>% 
  GGally::ggpairs()

# Inferencia ----

t.test(ags_t321$ing_x_hrs)
prueba<-t.test(ags_t321$ing_x_hrs)

10.9528+prueba$stderr*1.96 
10.9528-prueba$stderr*1.96 

10.9528+sd(ags_t321$ing_x_hrs)/sqrt(12553)*1.96 
10.9528-sd(ags_t321$ing_x_hrs)/sqrt(12553)*1.96 

mean(ags_t321$ing_x_hrs)+sd(ags_t321$ing_x_hrs)/sqrt(12553)*qnorm(0.975)

mean(ags_t321$ing_x_hrs)+sd(ags_t321$ing_x_hrs)/sqrt(12553)*qnorm(0.025)
mean(ags_t321$ing_x_hrs)+sd(ags_t321$ing_x_hrs)/sqrt(12553)*qnorm(0.975, lower.tail = FALSE)


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
  t.test(ing_x_hrs)
  )


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, conf.level=0.99)
  )


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, conf.level=0.95)
  )


# Explicitar la Hipótesis nula

# Supongamos que alguien afirma que los ingresos no han cambiado con respecto al
# año pasado, que fueron de 40 pesos por hora. 

# Ho: mu=40

ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, conf.level=0.95, mu=40)
  )

#Ha: mu≠40

.00006823<0.05 # mi p-value debe ser menor a mi nivel de significancia para
#rechazar la Ho y afirmar la alternativa.
# Resultados significativos 

.00006823<0.01 # mi p-value debe ser menor a mi nivel de significancia para
# resultados muy significativos


ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, # variable
           conf.level=0.95,# nivel de confianzaa
           mu=40, # Hipótesis nula
           alternative="two.sided"
           )
  )


# Yo quiero afirmar que las personas ganan más

#Ho: µ <= 40
#Ha: µ >40

ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, # variable
           conf.level=0.95,# nivel de confianzaa
           mu=40, # Hipótesis nula
           alternative="greater"
    )
  )

# Alguien quiere afirmar que los trabajadores ganan Menos

#Ho: µ >= 40
#Ha: µ <  40
ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs, # variable
           conf.level=0.95,# nivel de confianzaa
           mu=40, # Hipótesis nula
           alternative="less"
    )
  )


# Con el paquete infer

ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  infer::t_test(response=ing_x_hrs, mu=40, alternative="less")


#Proporción ----

ejemplo<-c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0)
mean(ejemplo)
table(ejemplo)

prop.test(x=8,
          n=15)

table(as_label(ags_t321$clase1))

prop.test(x=5551,
          n=5551+4478)

## Re-etiquetado

etiqueta_pea<-c("PEA", "PNEA")

ags_t321 %>% 
  filter(eda>14 & eda<99) %>%  # Me quedo con la PET
  sjlabelled::set_labels(clase1, labels=etiqueta_pea) %>% 
  mutate(clase1=as_label(clase1)) %>% 
  tabyl(clase1)


ags_t321 %>% 
  filter(eda>14 & eda<99) %>%  # Me quedo con la PET
  sjlabelled::set_labels(clase1, labels=etiqueta_pea) %>% 
  with(
    table(as_label(clase1))
  ) %>% 
  prop.test()

#Una prueba de hipótesis 

# HO: p=0.5
# HA: p≠0.5


ags_t321 %>% 
  filter(eda>14 & eda<99) %>%  # Me quedo con la PET
  sjlabelled::set_labels(clase1, labels=etiqueta_pea) %>% 
  with(
    table(as_label(clase1))
  ) %>% 
  prop.test(p=0.5, # valor objetivo asociado a la Ho
            alternative = "two.sided")


ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  set_labels(clase1, labels=etiqueta_pea) %>% 
  mutate(clase1=as_label(clase1)) %>% # tengo que convertir la variable  
  infer::prop_test(clase1 ~ NULL  ,
                   p=0.5, 
                   alternative="less")

# H0: p>=0.5
# HA: p<0.5
# # No hay evidencia para afirmar que menos de la 50% de la PET está dentro de la PEA

ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  set_labels(clase1, labels=etiqueta_pea) %>% 
  mutate(clase1=as_label(clase1)) %>% # tengo que convertir la variable  
  infer::prop_test(clase1 ~ NULL  ,
                   p=0.5, 
                   alternative="less",
                   success="PEA",
                   z=TRUE)

prop<-ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  set_labels(clase1, labels=etiqueta_pea) %>% 
  mutate(clase1=as_label(clase1)) %>% # tengo que convertir la variable  
  infer::prop_test(clase1 ~ NULL  ,
                   p=0.5, 
                   alternative="less",
                   success="PEA",
                   z=TRUE)

# Estimaciones bivariada ----

## diferencia de medias ----

ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  filter(ing_x_hrs>0) %>% 
  group_by(as_label(sex)) %>% 
  summarise(media=mean(ing_x_hrs))

# Necesito un proceso de inferencia
# Sobre las diferencia. Tengo que fijarme si mi intervalo tiene el 0

ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs ~ as_label(sex))
  )

# Ho: µ1=µ2 
# Ha: µ1≠µ2

# Conclusión: No se rechaza la Ho.
# No tengo evidencia estadística suficiente
# para decir que los hombres y mujeres
# tienen remuneraciones por hora distintas
# en Aguascalientes


ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    t.test(ing_x_hrs ~ as_label(sex),
           alternative= "less")
  )

#Ho: µ1-µ2>=0

#Ha: µ1-µ2<0
#Ha: µ1<µ2

# No tengo evidencia estadística suficiente
# para decir que las mujeres ganan más 
# por hora que los varones
# en Aguascalientes

# Con infer:

ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  filter(ing_x_hrs>0) %>% 
  mutate(sex=as_label(sex)) %>% 
  infer::t_test(ing_x_hrs ~ sex,
                order = c("Mujer", "Hombre"))


## diferencia de proporciones

# Vamos a ver la diferencia en términos de inserción 

ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  set_labels(clase1, labels=etiqueta_pea) %>% 
  mutate(clase1=as_label(clase1)) %>% 
  mutate(sex=as_label(sex)) %>% 
  infer::prop_test(clase1 ~ sex, 
                   order=c("Hombre", "Mujer"),
                   success="PEA",
                   z=TRUE)



ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  set_labels(clase1, labels=etiqueta_pea) %>% 
  mutate(clase1=as_label(clase1)) %>% 
  mutate(sex=as_label(sex)) %>% 
  infer::prop_test(clase1 ~ sex, 
                   order=c("Mujer", "Hombre"),
                   success="PEA",
                   z=TRUE,
                   alternative="less")

#Ho: p_mujeres-p_varones>=0
#Ha: p_mujeres<p_varones
#Ha: p_mujeres-p_varones<0

# Estimación de varianzas ----

## una sola varianza ----
ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    DescTools::VarTest(ing_x_hrs)
  )

ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    DescTools::VarTest(ing_x_hrs, sigma.squared=100)
  )

test2<-ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    DescTools::VarTest(ing_x_hrs, sigma.squared=100)
  )

sqrt(test2$conf.int)

broom::tidy(test2) # me da resultados en formato tidy

## Diferencia de dos varianzas



ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    var.test(ing_x_hrs ~ as_label(sex))
  )



ags_t321 %>% 
  filter(clase2==1) %>% 
  filter(ing_x_hrs>0) %>% 
  with(
    var.test(ing_x_hrs ~ as_label(sex),
             alternative="greater")
  )

# Prueba chi-cuadrado ----

ags_t321 %>% 
  filter(eda>14 & eda<99) %>% 
  mutate(clase1=as_label(clase1)) %>% 
  mutate(sex=as_label(sex)) %>% 
  tabyl(clase1, sex, show_missing_levels = FALSE) %>% 
  janitor::chisq.test()



