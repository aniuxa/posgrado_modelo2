#============================================================================#
#Fecha: 2022-06-17 
# Sesión práctica
#Autora: Ana Escoto
# ============================================================================#

#Paquetes ----
#install.packages("sjPlot", dependencies=T) # solito porque da problmas
library(sjPlot)

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse, # sobretodo para dplyr
              haven, #importación
              janitor, #tablas
              sjlabelled, # etiquetas
              DescTools, # Paquete para estimaciones y pruebas
              infer, # tidy way 
              broom,  # Una escobita para limpiar (pero es para arreglar)
              estimatr, car, stargazer, ggpubr, 
              jtools, lm.beta, robustbase, sandwich,
              officer,flextable,huxtable, ggstance, kableExtra,
               ResourceSelection, lmtest, mlogit, nnet) # Nuevos



# Datos ----
ags_t321 <- read_dta("./datos/datos/AGS_SDEMT321.dta", encoding="latin1") %>% 
  clean_names()

mydata<- ags_t321%>% 
  filter(clase2==1) %>%  # me quedo con la población ocupada
#  filter(ing_x_hrs>0) %>% # ingresos válidos
  filter(anios_esc<99) %>% # quito missings anios de escolaridad
  filter(eda>14 & eda<99) %>% #PET
  mutate(sex=as_label(sex)) %>% # Ojo, hicimos este cambio en la práctica pasada
  select(eda, sex, anios_esc, ing_x_hrs, pos_ocu, imssissste, medica5c, ent)  
  
tail(mydata)

remove(SDEMT122)

# Creando una variable dicotómica ----
mydata$y_binomial<-mydata$imssissste<4
mydata$y_binomial<-as_numeric(mydata$y_binomial)

mydata %>% 
  tabyl(y_binomial)




## Modelo simple ----

modelo0<-glm(y_binomial~ anios_esc, # especificación del modelo
             family = binomial("logit"), # qué tipo de función enlaca
             data=mydata, # objeto con los datos
             na.action=na.exclude) # missings

summary(modelo0)
confint(modelo0)



## Pidiendo los resultados ----
summ(modelo0)


## -------------------------------------------------------------------
summary(mydata$anios_esc)


## -------------------------------------------------------------------
xanios_esc <- 0:21
predict(modelo0, list(anios_esc=1), type="response")

## -------------------------------------------------------------------
y_logito <- predict(modelo0, list(anios_esc = xanios_esc))
y_prob<- predict(modelo0, list(anios_esc = xanios_esc), type= "response")

results_m0<-cbind(y_logito, y_prob, xanios_esc)
results_m0<-as.data.frame(results_m0)


## -------------------------------------------------------------------
ggplot(data=results_m0, aes(x=xanios_esc, y=y_prob)) +
  geom_point()



## -------------------------------------------------------------------

exp(coef(modelo0))



## -------------------------------------------------------------------
jtools::summ(modelo0, exp=TRUE )



## -------------------------------------------------------------------
modelo1<-glm(y_binomial ~ anios_esc + sex, # formula
             family = binomial("logit"), # función de enlace
             data=mydata, 
             na.action=na.exclude)
summary(modelo1)
confint(modelo1)
summ(modelo1, exp=T)

## -------------------------------------------------------------------


#H0: es el modelo restringido es igual o mejor que el modelo no restringido
#HA: el modelo no restringido es mejor que el modelo restringido
## -------------------------------------------------------------------
lrtest0<-lrtest(modelo0, modelo1)
lrtest0


## -------------------------------------------------------------------
modelo2<-glm(y_binomial ~ anios_esc + sex + eda,
             family = binomial("logit"), 
             data=mydata, 
             na.action=na.exclude)
summary(modelo2)

broom::glance(modelo0)
broom::glance(modelo1)
broom::glance(modelo2)

## -------------------------------------------------------------------
lrtest1<-lrtest(modelo0, modelo2)
lrtest1


## -------------------------------------------------------------------

hoslem.test(mydata$y_binomial, fitted(modelo0))
# Porcentaje de predichos
fitted<-as.data.frame(fitted.values(modelo2))
fitted$predicho<-as_numeric(fitted$`fitted.values(modelo2)`>0.5)
table(mydata$y_binomial, fitted$predicho) # de aquí se saca el R2 de conteo

## ----mylatextable---------------------------------------------------
#stargazer(modelo0, modelo1,modelo2, type = 'latex', header=FALSE)



## ----mytextable2----------------------------------------------------
stargazer(modelo0, modelo1,modelo2, 
          type = 'text', header=FALSE)
        



## -------------------------------------------------------------------
stargazer(modelo0, modelo1,modelo2, 
          type = 'text', header=FALSE,
          apply.coef = exp,
          apply.se   = exp)



## -------------------------------------------------------------------
export_summs(modelo0, modelo1,modelo2, exp=T)



## -------------------------------------------------------------------
plot_model(modelo2)


## -------------------------------------------------------------------
get<-plot_model(modelo2)
get$data



## -------------------------------------------------------------------
plot_model(modelo2, terms= c("sex", "anios_esc"), type="pred")



## -------------------------------------------------------------------
plot_models(modelo1, modelo2) + ggtitle("P(acceso a servicios médicos)")


## -------------------------------------------------------------------
mprobit<-glm(y_binomial ~ anios_esc, 
             family = binomial("probit"), 
             data=mydata, 
             na.action=na.exclude)
summary(mprobit)




## -------------------------------------------------------------------
stargazer(modelo0, mprobit,   type = 'text', header=FALSE)



predict(modelo0, list(anios_esc=9, sex="Mujer", eda=40 ), type="response")
predict(modelo0, list(anios_esc=12, sex="Mujer", eda=40 ), type="response")

