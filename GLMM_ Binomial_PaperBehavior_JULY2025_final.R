
base <- read.csv('C:/Users/Lenovo/Desktop/Analisis_Behavior _Edo_Abril 2025/Base_insects.csv', header = TRUE)

attach(base)
names(base)



# Cargar paquetes necesarios
library(lme4)

# Asegurarte de que las variables sean factores
base$EspecieFicus <- as.factor(base$EspecieFicus)
base$Fases <- as.factor(base$Fases)
base$Arbol_ID <- as.factor(base$Arbol_ID)




# MODELO SALLY ####
base$maniobra_binaria <- ifelse(base$Maniobra == "SAL", 1, 0)

table(base$maniobra_binaria)


# Modelo mixto binomial con efecto aleatorio por árbol
modelo_SAL <- glmer(maniobra_binaria ~ Densidad_follaje + Disp_figs + EspecieFicus + Fases + Num_aves +
                      (1 | Arbol_ID),
                    data = base,
                    family = binomial(link = "logit"),
                    control = glmerControl(optimizer = "bobyqa"))




#Paquete DHARMA
res_bin <- simulateResiduals(modelo_SAL)
plot(res_bin, rank =T)


# Para revisar la sobredispersión de mi GLMM
od.point <- function(modelobject){
  x <- sum(resid(modelobject, type ="pearson")^2)
  rdf <-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(modelo_SAL)


# Crear mi modelo nulo

nulo_modelo_SAL <- glmer(maniobra_binaria ~ Densidad_follaje + Disp_figs + Num_aves +
                           (1 | Arbol_ID),
                         data = base,
                         family = binomial(link = "logit"),
                         control = glmerControl(optimizer = "bobyqa"))

# Para comparar mi modelo nulo con mi modelo 

anova(nulo_modelo_SAL, modelo_SAL, test ="Chi")

#npar    AIC    BIC  logLik -2*log(L)  Chisq Df Pr(>Chisq)  
#nulo_modelo_mixto    4 621.37 637.86 -306.68    613.37                       
#modelo_mixto         7 619.60 648.46 -302.80    605.60 7.7641  3    0.05115 .

# Para calcular el coeficiente de determinación (R2) de mi modelo utilice la función r.squared.GLMM del paquete “MuMIn” del software R. 
#install.packages("MuMIn")
library("MuMIn")

r.squaredGLMM(modelo_SAL)
#R2m        R2c
#theoretical 0.05731055 0.09300335
#delta       0.04728861 0.07673978



# Ver resultados
summary(modelo_SAL)

# Resultados de Sally ####


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



# Crear mi modelo nulo

nulo_modelo_ESP <- glmer(maniobra_esp ~ Densidad_follaje + Disp_figs + Num_aves + (1 | Arbol_ID),
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


# Para calcular el coeficiente de determinación (R2) de mi modelo utilice la función r.squared.GLMM del paquete “MuMIn” del software R. 
install.packages("MuMIn")
library("MuMIn")

r.squaredGLMM(modelo_ESP)

# R2m        R2c
# theoretical 0.11327463 0.13869375
# delta       0.07028437 0.08605636

# Para comparar y graficar mis 3 categorias de Fase #
#install.packages("ggeffects")
library("ggeffects")
library("ggplot2")

#install.packages("effects") #extrapola
library("effects")

pred_fase= ggeffect(modelo_ESP, terms = c("Fases"))
plot(pred_fase) + labs( x = "Fases de fructificación", y = "effect") + theme(plot.title = element_blank())


#####diferencia entre las variables categoricas #####

install.packages('emmeans')
library("emmeans")

fasesESP = emmeans(modelo_ESP,~ Fases, rg.limit = 23050)
pairs(fasesESP,
      adjust="tukey")

# Opcion 2: 
#Salio el mismo resultado que en la anterior

#fases_emms = emmeans(mod_ABU_z, ~fase, nuisance = c("clima", "muerdago", "horario"))
#pairs(fases,
#      adjust="tukey")

####Graficar GEE and mixed-effect models.#####

emm_out_FasesESP <- emmeans(modelo_ESP, specs = c("Fases"), rg.limit = 23050,
                       at = list(diagnose = "4"),
                       regrid = "response") %>%
  as.data.frame()

plot(ggemmeans(modelo_ESP, terms = c("Fases"))) +
  ggplot2::ggtitle("")




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
res_bin_tra <- simulateResiduals(modelo_TRA)
plot(res_bin_tra, rank =T)

# Para revisar la sobredispersión de mi GLMM
od.point <- function(modelobject){
  x <- sum(resid(modelobject, type ="pearson")^2)
  rdf <-summary(modelobject)$AICtab[5]
  return(x/rdf)
}
od.point(modelo_TRA)


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



