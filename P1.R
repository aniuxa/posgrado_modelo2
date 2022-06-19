5+5 # Esto es una suma
5+5
install.packages("wesanderson")
library(wesanderson)

# Paquetes a utilizar ----
if (!require("pacman")) install.packages("pacman")#instala pacman si se requiere
pacman::p_load(tidyverse, # grupo de paquetes
               readxl, # lee archivos de excel
               writexl, # escribe archivos de excel
               haven, # lee archivos desde stata y spss
               sjlabelled, # uso de etiquetas
               janitor, # limpieza de nombres y tabulados
               infer, # inferencia con formato tidy
               ggpubr, # extensión de ggplot2
               magrittr, # Pipes o tuberías
               gt,# grammar of tables
               GGally) # extensión de ggplot2

# Cargar nuestros datos ####

AGS_SDEMT321 <- haven::read_dta("datos/datos/AGS_SDEMT321.dta",
                                encoding="latin1")
rm(AGS_SDEMT321)

ags_t321 <- haven::read_dta("datos/datos/AGS_SDEMT321.dta",
                                encoding="latin1")

ICI_2021 <- readxl::read_excel("datos/datos/ICI_2021.xlsx",
                               sheet = "ICI 2021")

# Manipulación y revisión de datos ----

ags_t321 %>% 
  head()

head(ags_t321)

ags_t321 |>
  select(SEX,EDA) |>
  head()

## limpieza de nombres ----

names(ags_t321)
names(ICI_2021)

ICI_2021<-ICI_2021 %>%
  janitor::clean_names()

names(ICI_2021)

ags_t321 %<>% 
  clean_names()

names(ags_t321)


## select y filter ----

ags_t321 %>% 
  dplyr::filter(eda>11) %>%  
  dplyr::select(sex, eda)


ags_t321<-ags_t321 %>% 
  filter(r_def==0) %>% # entrevistas completas
  filter(!c_res==2) # residentes habituales

# Tabulados ----

ags_t321 %>%
  dplyr::mutate(sex=sjlabelled::as_label(sex)) %>%
  janitor::tabyl(sex)

ags_t321 %>%
  select(sex)%>%
  sjlabelled::get_labels()

ags_t321 %>%
  mutate(sex=as_label(sex))%>%
  tabyl(sex)%>%
  adorn_totals() # agrega los totales

ags_t321 %>%
  mutate(sex=as_label(sex))%>%
  tabyl(sex)%>%
  adorn_totals() %>%
  adorn_pct_formatting(digits=2) # agrega el %%

ags_t321%>%
  mutate(niv_ins=as_label(niv_ins))%>%
  tabyl(niv_ins)%>%
  adorn_totals()%>%
  adorn_pct_formatting()


# Bivariada

ags_t321%>%
  mutate(sex=as_label(sex))%>%
  mutate(niv_ins=as_label(niv_ins))%>%
  tabyl(niv_ins, sex) %>%
  adorn_totals("row")


ags_t321%>%
  mutate(sex=as_label(sex))%>%
  mutate(niv_ins=as_label(niv_ins))%>%
  tabyl(niv_ins, sex) %>%
  adorn_totals("col")

ags_t321%>%
  mutate(sex=as_label(sex))%>%
  mutate(niv_ins=as_label(niv_ins))%>%
  tabyl(niv_ins, sex) %>%
  adorn_totals(c("col","row"))


# Agregando porcentajes

ags_t321%>%
  mutate(sex=as_label(sex))%>%
  mutate(niv_ins=as_label(niv_ins))%>%
  tabyl(niv_ins, sex) %>%
  adorn_totals(c("col","row")) %>%
  adorn_percentages("row") %>% # porcentaje de fila<s
  adorn_pct_formatting()

ags_t321%>%
  mutate(sex=as_label(sex))%>%
  mutate(niv_ins=as_label(niv_ins))%>%
  tabyl(niv_ins, sex) %>%
  adorn_totals(c("col","row")) %>%
  adorn_percentages("col") %>% # porcentaje de columnas
  adorn_pct_formatting()

ags_t321%>%
  mutate(sex=as_label(sex))%>%
  mutate(niv_ins=as_label(niv_ins))%>%
  tabyl(niv_ins, sex) %>%
  adorn_totals(c("col","row")) %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting()

tabla<-ags_t321%>%
  mutate(sex=as_label(sex))%>%
  mutate(niv_ins=as_label(niv_ins))%>%
  tabyl(niv_ins, sex) %>%
  adorn_totals(c("col","row")) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting()

#Grammar of tables ----

gt_tabla <-gt::gt(tabla)

gt_tabla <- gt_tabla %>%
  tab_header(
    title = "Distribución de la población según nivel de escolaridad y sexo",
    subtitle = "Aguascalientes, trimestre III de 2021"
  )

gt_tabla

gt_tabla<-gt_tabla %>%
  tab_source_note(
    source_note = "Fuente: Cálculos propios con datos de INEGI"
  )

gt_tabla


# Descriptivos variables cuantitativas ----

summary(ags_t321$ing_x_hrs)

ags_t321%>%
  dplyr::summarise(nombre_del_indicador=mean(ing_x_hrs))

#Incorporando filtros para identificar la población objetivo
ags_t321%>%
  filter(clase2==1) %>%
  filter(ing_x_hrs>0)%>%
  dplyr::summarise(nombre_del_indicador=mean(ing_x_hrs))

# Se agregan columnas como indicadores

ags_t321%>%
  filter(clase2==1) %>%
  filter(ing_x_hrs>0)%>%
  dplyr::summarise(media=mean(ing_x_hrs),
                   mediana=median(ing_x_hrs),
                   desviacion=sd(ing_x_hrs),
                   edad_media=mean(eda),
                   escol_media=mean(anios_esc))

#Introducimos una variable de grupo como filas
ags_t321%>%
  filter(clase2==1) %>% # población ocupada
  filter(ing_x_hrs>0)%>% # población con algún ingreso
  dplyr::group_by(as_label(sex))%>% # agrupa por sexo la base de datos
  summarise(media=mean(ing_x_hrs),
                   mediana=median(ing_x_hrs),
                   desviacion=sd(ing_x_hrs),
                   edad_media=mean(eda),
                   escol_media=mean(anios_esc)) # hace cinco indicadores

