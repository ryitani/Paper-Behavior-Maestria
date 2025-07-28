
base <- read.csv('C:/Users/Lenovo/Desktop/Analisis_Behavior _Edo_Abril 2025/Base_insects.csv', header = TRUE)

attach(base)
names(base)
str(base)



# Cargar paquetes necesarios
library(lme4)

# Asegurarme de que las variables sean factores
base$EspecieFicus <- as.factor(base$EspecieFicus)
base$Fases <- as.factor(base$Fases)
base$Arbol_ID <- as.factor(base$Arbol_ID)




# MODELO SALLY ####
base$maniobra_binaria <- ifelse(base$Maniobra == "SAL", 1, 0)

table(base$maniobra_binaria)


# Modelo mixto completo con distribucion binomial 
modelo_SAL <- glmer(maniobra_binaria ~ Densidad_follaje + Disp_figs + EspecieFicus + Fases + Num_aves +
                      (1 | Arbol_ID),
                    data = base,
                    family = binomial(link = "logit"),
                    control = glmerControl(optimizer = "bobyqa"))


# Variantes del modelo ####


modelo_SAL_AL <- glmer(maniobra_binaria ~ Densidad_follaje + Disp_figs + Alimento + EspecieFicus + Fases + Num_aves +
                      (1 | Arbol_ID),
                    data = base,
                    family = binomial(link = "logit"),
                    control = glmerControl(optimizer = "bobyqa"))

r.squaredGLMM(modelo_SAL_AL)

AIC(modelo_SAL_AL)

summary(modelo_SAL_AL)


# Modelo mixto con distribucion binomial + Densidad_follaje

modelo_SAL_1 <- glmer(maniobra_binaria ~ EspecieFicus + Fases + Densidad_follaje +
                      (1 | Arbol_ID),
                    data = base,
                    family = binomial(link = "logit"),
                    control = glmerControl(optimizer = "bobyqa"))

#Paquete DHARMA
res_bin <- simulateResiduals(modelo_SAL_1)
plot(res_bin, rank =T)


# Para revisar la sobredispersión de mi GLMM
od.point <- function(modelobject){
  x <- sum(resid(modelobject, type ="pearson")^2)
  rdf <-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(modelo_SAL_1)


# summary

summary(modelo_SAL_1)

# Modelo mixto con distribucion binomial + Num_aves

modelo_SAL_2 <- glmer(maniobra_binaria ~ EspecieFicus + Fases + Num_aves +
                        (1 | Arbol_ID),
                      data = base,
                      family = binomial(link = "logit"),
                      control = glmerControl(optimizer = "bobyqa"))


#Paquete DHARMA
res_bin <- simulateResiduals(modelo_SAL_2)
plot(res_bin, rank =T)


# Para revisar la sobredispersión de mi GLMM
od.point <- function(modelobject){
  x <- sum(resid(modelobject, type ="pearson")^2)
  rdf <-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(modelo_SAL_2)

# summary

summary(modelo_SAL_2)

# Modelo mixto con distribucion binomial + Disp_figs

modelo_SAL_3 <- glmer(maniobra_binaria ~ EspecieFicus + Fases + Disp_figs +
                        (1 | Arbol_ID),
                      data = base,
                      family = binomial(link = "logit"),
                      control = glmerControl(optimizer = "bobyqa"))


#Paquete DHARMA
res_bin <- simulateResiduals(modelo_SAL_3)
plot(res_bin, rank =T)


# Para revisar la sobredispersión de mi GLMM
od.point <- function(modelobject){
  x <- sum(resid(modelobject, type ="pearson")^2)
  rdf <-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(modelo_SAL_3)


# summary

summary(modelo_SAL_3)



# Comparar modelos por AIC
AIC(modelo_SAL, modelo_SAL_1, modelo_SAL_2, modelo_SAL_3)

# df      AIC
# modelo_SAL    8 614.8154
# modelo_SAL_1  6 619.2965
# modelo_SAL_2  6 615.3708
# modelo_SAL_3  6 618.4876



# Crear mi modelo nulo

 nulo_modelo_SAL <- glmer(maniobra_binaria ~ 
                            (1 | Arbol_ID),
                          data = base,
                          family = binomial(link = "logit"),
                          control = glmerControl(optimizer = "bobyqa"))



# Para comparar mi modelo nulo con mi modelo 

anova(nulo_modelo_SAL, modelo_SAL, test ="Chi")

# npar    AIC    BIC  logLik -2*log(L) Chisq Df Pr(>Chisq)    
# nulo_modelo_SAL    2 626.98 635.22 -311.49    622.98                        
# modelo_SAL         8 614.82 647.80 -299.41    598.82 24.16  6  0.0004881 ***



# Para calcular el coeficiente de determinación (R2) de mi modelo utilice la función r.squared.GLMM del paquete “MuMIn” del software R. 
#install.packages("MuMIn")
library("MuMIn")

r.squaredGLMM(modelo_SAL)
# R2m       R2c
# theoretical 0.09207885 0.1628621
# delta       0.07701425 0.1362170


r.squaredGLMM(modelo_SAL_1)

# R2m        R2c
# theoretical 0.05194373 0.08096349
# delta       0.04276102 0.06665062


r.squaredGLMM(modelo_SAL_2)
# R2m        R2c
# theoretical 0.06909683 0.11333812
# delta       0.05723822 0.09388669


r.squaredGLMM(modelo_SAL_3)
# R2m        R2c
# theoretical 0.05394267 0.08483382
# delta       0.04443968 0.06988878

# Ver resultados
summary(modelo_SAL)

# Graficar la relacion entre la maniobra de Sally y el numero de aves presentes durante el monitoreo


library(ggplot2)
library(ggeffects)

# Generar predicciones del modelo solo para la variable Num_aves
pred_sally <- ggpredict(modelo_SAL_AL, terms = "Num_aves")

# Graficar
ggplot(pred_sally, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "black") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "gray70") +
  labs(
    x = "Número de aves en el árbol",
    y = "Probabilidad de la maniobra Sally",
    title = "Relación entre número de aves y uso de la maniobra Sally"
  ) +
  theme_minimal(base_size = 14)

###

library(ggplot2)

ggplot(base, aes(x = Num_aves, y = maniobra_binaria)) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "blue") +
  labs(x = "Número de aves", y = "Probabilidad de maniobra SALLY") +
  theme_minimal()


# MODELO ESPIGAR ####

# Crear variable binaria: 1 si ESP, 0 si no
base$maniobra_esp <- ifelse(base$Maniobra == "ESP", 1, 0)

table(base$maniobra_esp)


# Ajustar modelo binomial (puedes cambiar predictores según tu interés)
# Modelo mixto para "ESP"
library(lme4)
modelo_ESP <- glmer(maniobra_esp ~ Densidad_follaje + Disp_figs + EspecieFicus + Fases + Num_aves + (1 | Arbol_ID),
                    family = binomial(link = "logit"),
                    data = base,
                    control = glmerControl(optimizer = "bobyqa"))


#Paquete DHARMA
res_bin_esp <- simulateResiduals(modelo_ESP)
plot(res_bin_esp, rank =T)


# Para revisar la sobredispersión de mi GLMM
od.point <- function(modelobject){
  x <- sum(resid(modelobject, type ="pearson")^2)
  rdf <-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(modelo_ESP)

# Variantes del modelo Espigar ####

# Con Alimento
modelo_ESP_AL <- glmer(maniobra_esp ~ Densidad_follaje + Disp_figs + Alimento + EspecieFicus + Fases + Num_aves + (1 | Arbol_ID),
                    family = binomial(link = "logit"),
                    data = base,
                    control = glmerControl(optimizer = "bobyqa"))


# Modelo con Densidad del follaje 

modelo_ESP_1 <- glmer(maniobra_esp ~  EspecieFicus + Fases + Densidad_follaje + (1 | Arbol_ID),
                    family = binomial(link = "logit"),
                    data = base,
                    control = glmerControl(optimizer = "bobyqa"))
# boundary (singular) fit: see help('isSingular')


modelo_ESP_glm <- glm(maniobra_esp ~ EspecieFicus + Fases + Densidad_follaje,
                      family = binomial(link = "logit"),
                      data = base)

AIC(modelo_ESP_1, modelo_ESP_glm, modelo_ESP_AL)

# Modelo con variable Num_aves

modelo_ESP_2a <- glmer(maniobra_esp ~  EspecieFicus + Fases + Num_aves + (1 | Arbol_ID),
                      family = binomial(link = "logit"),
                      data = base,
                      control = glmerControl(optimizer = "bobyqa"))
# boundary (singular) fit: see help('isSingular')

modelo_ESP_2b <- glm(maniobra_esp ~ EspecieFicus + Fases + Num_aves,
                      family = binomial(link = "logit"),
                      data = base)

AIC(modelo_ESP_2a, modelo_ESP_2b)

# Modelo con variable Disp_figs 

modelo_ESP_3a <- glmer(maniobra_esp ~  EspecieFicus + Fases + Disp_figs + (1 | Arbol_ID),
                       family = binomial(link = "logit"),
                       data = base,
                       control = glmerControl(optimizer = "bobyqa"))

# boundary (singular) fit: see help('isSingular')

modelo_ESP_3b <- glm(maniobra_esp ~ EspecieFicus + Fases + Disp_figs,
                     family = binomial(link = "logit"),
                     data = base)


AIC(modelo_ESP_3a, modelo_ESP_3b)

# Crear mi modelo nulo

nulo_modelo_ESP <- glmer(maniobra_esp ~ (1 | Arbol_ID),
                         family = binomial(link = "logit"),
                         data = base,
                         control = glmerControl(optimizer = "bobyqa"))



# Para comparar mi modelo nulo con mi modelo 

anova(nulo_modelo_ESP, modelo_ESP, test ="Chi")

# npar    AIC    BIC  logLik -2*log(L)  Chisq Df
# nulo_modelo_ESP    5 488.24 508.86 -239.12    478.24          
# modelo_ESP         8 480.40 513.38 -232.20    464.40 13.846  3
# Pr(>Chisq)   
# nulo_modelo_ESP              
# modelo_ESP        0.003123 **





summary(modelo_ESP)
summary(modelo_ESP_AL)

# Para calcular el coeficiente de determinación (R2) de mi modelo utilice la función r.squared.GLMM del paquete “MuMIn” del software R. 
install.packages("MuMIn")
library("MuMIn")

r.squaredGLMM(modelo_ESP)

# R2m        R2c
# theoretical 0.11327463 0.13869375
# delta       0.07028437 0.08605636


# R2 de las variantes del modelo Espigar


r.squaredGLMM(modelo_ESP_1)

r.squaredGLMM(modelo_ESP_glm)
r.squaredGLMM(modelo_ESP_2a)
r.squaredGLMM(modelo_ESP_2b)
r.squaredGLMM(modelo_ESP_3a)
r.squaredGLMM(modelo_ESP_3b)
r.squaredGLMM(modelo_ESP_AL)


# Poshoc de mis variables independientes ####

#install.packages("ggeffects")
library("ggeffects")
library("ggplot2")
#install.packages("effects") #extrapola
library("effects")


# Poshoc Fases ####


# Para comparar y graficar mis 3 categorias de Fase #
pred_fase= ggeffect(modelo_ESP_AL, terms = c("Fases"))
plot(pred_fase) + labs( x = "Fases de fructificación", y = "effect") + theme(plot.title = element_blank())


#####diferencia entre las variables categoricas #####

install.packages('emmeans')
library("emmeans")

fasesESP = emmeans(modelo_ESP,~ Fases, rg.limit = 23050)
pairs(fasesESP,
      adjust="tukey")


####Graficar GEE and mixed-effect models.#####

emm_out_FasesESP <- emmeans(modelo_ESP, specs = c("Fases"), rg.limit = 23050,
                       at = list(diagnose = "4"),
                       regrid = "response") %>%
  as.data.frame()

plot(ggemmeans(modelo_ESP, terms = c("Fases"))) +
  ggplot2::ggtitle("")


# Poshoc EspecieFicus ####

# Para comparar y graficar mis categorias de EspecieFicus #
pred_sp= ggeffect(modelo_SAL_AL, terms = c("EspecieFicus"))
plot(pred_sp) + labs( x = "Especie Ficus", y = "effect") + theme(plot.title = element_blank())


#####diferencia entre las variables categoricas #####

install.packages('emmeans')
library("emmeans")

EspESP = emmeans(modelo_SAL_AL,~ EspecieFicus, rg.limit = 23050)
pairs(EspESP,
      adjust="tukey")


####Graficar GEE and mixed-effect models.#####

emm_out_EspESP <- emmeans(modelo_SAL_AL, specs = c("EspecieFicus"), rg.limit = 23050,
                            at = list(diagnose = "4"),
                            regrid = "response") %>%
  as.data.frame()

plot(ggemmeans(modelo_SAL_AL, terms = c("EspecieFicus"))) +
  ggplot2::ggtitle("")



library(emmeans)

emm <- emmeans(modelo_ESP_AL, ~ EspecieFicus, type = "response")
plot(emm) +
  labs(x = "Especie de Ficus", y = "Probabilidad de espigar")



# MODELO DE TRAGAR ####

# Crear variable binaria: 1 si TRA, 0 si no
base$maniobra_tra <- ifelse(base$Maniobra == "TRA", 1, 0)
table(base$maniobra_tra)

# Ajustar modelo binomial
modelo_TRA <- glmer(maniobra_tra ~ Densidad_follaje + Disp_figs + EspecieFicus + Fases + Num_aves + (1 | Arbol_ID),
                    family = binomial(link = "logit"),
                    data = base,
                    control = glmerControl(optimizer = "bobyqa"))

#Paquete DHARMA
res_bin_tra <- simulateResiduals(modelo_TRA_AL)
plot(res_bin_tra, rank =T)

# Para revisar la sobredispersión de mi GLMM
od.point <- function(modelobject){
  x <- sum(resid(modelobject, type ="pearson")^2)
  rdf <-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(modelo_TRA)


# Variantes del modelo Tragar ####

# con variable Alimento
modelo_TRA_AL <- glmer(maniobra_tra ~   Alimento + EspecieFicus + Fases +  (1 | Arbol_ID),
                    family = binomial(link = "logit"),
                    data = base,
                    control = glmerControl(optimizer = "bobyqa"))


modelo_TRA_1 <- glmer(maniobra_tra ~ EspecieFicus + Fases + Densidad_follaje  + (1 | Arbol_ID),
                    family = binomial(link = "logit"),
                    data = base,
                    control = glmerControl(optimizer = "bobyqa"))


modelo_TRA_2 <- glmer(maniobra_tra ~ EspecieFicus + Fases + Num_aves  + (1 | Arbol_ID),
                      family = binomial(link = "logit"),
                      data = base,
                      control = glmerControl(optimizer = "bobyqa"))

modelo_TRA_3 <- glmer(maniobra_tra ~ EspecieFicus + Fases + Disp_figs  + (1 | Arbol_ID),
                      family = binomial(link = "logit"),
                      data = base,
                      control = glmerControl(optimizer = "bobyqa"))



# Comparar los modelos con AIC

AIC(modelo_TRA, modelo_TRA_1, modelo_TRA_2, modelo_TRA_3,modelo_TRA_AL)


# df      AIC
# modelo_TRA    8 472.8450
# modelo_TRA_1  6 470.1619
# modelo_TRA_2  6 469.4692
# modelo_TRA_3  6 469.5268


# R2 de cada modelo

r.squaredGLMM(modelo_TRA)
r.squaredGLMM(modelo_TRA_1)
r.squaredGLMM(modelo_TRA_2)
r.squaredGLMM(modelo_TRA_3)

r.squaredGLMM(modelo_TRA_AL)



# Crear mi modelo nulo

nulo_modelo_TRA <- glmer(maniobra_tra ~ Densidad_follaje + Disp_figs + Num_aves + (1 | Arbol_ID),
                         family = binomial(link = "logit"),
                         data = base,
                         control = glmerControl(optimizer = "bobyqa"))



# Para comparar mi modelo nulo con mi modelo 

anova(nulo_modelo_TRA, modelo_TRA, test ="Chi")

#Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#1       453     464.95                     
#2       450     463.31  3   1.6434   0.6496



# Para calcular el coeficiente de determinación (R2) de mi modelo utilice la función r.squared.GLMM del paquete “MuMIn” del software R. 
install.packages("MuMIn")
library("MuMIn")

r.squaredGLMM(modelo_TRA)


# Ver resumen del modelo
summary(modelo_TRA)



