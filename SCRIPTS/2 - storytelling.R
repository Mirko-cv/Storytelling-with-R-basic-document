#===========================================================#
# DATA STORYTELLING CON R                                   #
# Sesión: 02                                                #
# Fecha: 26/07/2026                                         #
# Docente-Github: Mirko-cv                                  #
#===========================================================#



#1. Que es storytelling

#A. evidencia -> datos
#B. mensaje -> comunicar
#C. audiencia -> convencer


#1.1 Que no veremos

#No aprenderemos a programar
#   aprenderemos estadistica -> Estadstica inferencial
#No aprederemos a modelar
#No aprederemos ETL

#1.1 Que (si) veremos

#R como medio para mostrar una idea/problema
#R para estimar parametros de muestras complejas (ENAHO)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#=================================================#
# CASO 1. Eficiencia de EPS (Agua y Saneamiento)  #
#=================================================#
library(dplyr)
library(ggplot2)
library(ggpubr)

df<- readxl::read_excel("data_eficiencia.xlsx")

#describir datos
# %>%  ctrl + shift +
df %>% str()


data <-df %>%  mutate(Tamaño = "Total") %>%
  bind_rows(df) %>%  
  mutate(Tamaño=case_when(Tamaño=="Total"~"Total",
                          Tamaño %in% c("Grande 1","Muy Grande")~"Group 1",
                          Tamaño%in% c("Grande 2")~"Group 2",T~NA)) %>% 
  select(Puntaje_eficiencia,Propiedad2, Tamaño) 


data %>% 
  ggplot( aes(y= Puntaje_eficiencia,x=Propiedad2))+
  geom_boxplot(aes(fill=factor(Propiedad2)),alpha=0.7)+
  scale_fill_manual(values = c("#BCE4D8", "#49A4B9"))+
  geom_jitter(alpha=0.1,size=3)+
  facet_grid(~Tamaño,scales="free")+
  stat_compare_means(method = "wilcox.test", label.y=1.3)+
  stat_compare_means(method = "kruskal.test", label.y=1.25)+
  theme(legend.position = "none",axis.text.y = element_blank())+
  labs(x="",y="Efficiency score")+
  theme_relato_academico()+scale_fill_viridis_d()




data %>% 
  ggplot(aes( x=Puntaje_eficiencia, fill = factor(Tamaño)) )+
  geom_density(alpha = 1.2)+
  facet_grid(~Tamaño)+
  theme_relato()+
  labs(title= "Distribución de puntajes de eficiencia de EPS",
       subtitle = "2022 - según tamaño",
       fill= "Tamaño",
       y = "densidad",
       x="Puntaje Eficiencia")+
  scale_fill_brewer(palette = "Pastel2")
  
compare <- list(c("Group 1","Group 2"),
                c("Group 1", "Total"),
                c("Group 2","Total") )
data %>% 
  ggplot(aes( y=Puntaje_eficiencia, x=Tamaño, fill = factor(Tamaño)) )+
  geom_boxplot(alpha = 1.2)+
  geom_jitter(shape=21,alpha=0.5, size=3)+
  stat_compare_means(method = "wilcox.test",
                     comparisons=compare)+
  theme_relato()+
  labs(title= "Boxplot de puntajes de eficiencia de EPS",
       subtitle = "2022 - según tamaño",
       fill= "Tamaño",
       y = "",
       x="Puntaje Eficiencia")+
  scale_fill_brewer(palette = "Pastel2")




data %>% 
  ggplot(aes( y=Puntaje_eficiencia, x=Propiedad2, fill = factor(Propiedad2)) )+
  geom_boxplot(alpha = 1.2)+
  geom_jitter(shape=21,alpha=0.5, size=3)+
  stat_compare_means(method = "wilcox.test", label.y = 1.5)+
  theme_relato()+
  labs(title= "Boxplot de puntajes de eficiencia de EPS",
       subtitle = "2022 - según propiedad",
       fill= "Tamaño",
       y = "",
       x="Puntaje Eficiencia")+
  scale_fill_brewer(palette = "Pastel2")

# grafico de burbujas

df %>% 
  select(Activo.Total, Agua.comercializada,
        Conexiones_ag, Personal.propio.total,
        País) %>%
  ggplot(aes(y= log(Agua.comercializada),x = Personal.propio.total))+
  geom_point(aes(size=log(Activo.Total), fill = factor(País) ), pch=21)+
  geom_smooth(col="#f00478", se=F)+
  labs(title="Fontera de posibilidades de producción - EPS en America Latina",
       size="Tamaño de activos",
       fill="País",
       Caption = "Nota. Información tomada de ADERASA",
       y="Log (agua comercializada)",
       x = "personal total")+
  theme_relato_academico()


#========================================================#
# CASO 2. Estiamción de promedios para muestra compleja  #
#========================================================#

library(haven)
library(geodata)


#----------------
df_p <- data.frame(
  Provincia = sprintf("%02d", 1:9),
  NAME_2 = c("Huancayo", "Concepción","Jauja", "Junín", "Tarma",
             "Yauli","Satipo","Chanchamayo","Chupaca")
)

#df<-read_dta("enaho01a-2023-500.dta")

data<-df %>% filter(grepl("^12",df$ubigeo),ocu500==1)%>% 
  select(ing_net=p530a,ubigeo,conglome,ocupinf,fac500a,estrato) %>% 
  mutate(Provincia=substr(ubigeo,3,4)) %>% 
  left_join(df_p) #%>% write_dta("Enaho_Empleo_Resumen_05.dta")
#---------------

data <- read_dta("Enaho_Empleo_Resumen_05.dta")

#ing_net=ing_liq peor incluye valorizacion de los que reciben especies por sueldo



# ESTIMAMOS PROPORCIONES

#ingreso promedio
data$ing_net %>% mean(.,na.rm=T) #811

#informalidad
sum(data$ocupinf==1,na.rm=T)/length(data$ocupinf) #82%



# seleccionamos variables relevantes
prueba_informalidad <- data %>% select(ocupinf,fac500a,conglome,estrato,NAME_2) %>% 
  mutate(ocupinf=if_else(ocupinf==1,1,0))%>% na.omit()

prueba_ingreso <- data %>% select(ing_net,fac500a,conglome,estrato,NAME_2) %>% na.omit()

#estimacion de proporcion de informalidad
prop.test(x=sum(prueba_informalidad$ocupinf==1),
          n=length(prueba_informalidad$ocupinf),
          alternative = "greater")

#estimación del promedio de ingreso neto
t.test(prueba_ingreso$ing_net,alternative = "greater")

#================#
# USANDO SURVEY  #
#================#

library(survey)



dsg_1 <- svydesign(ids = ~conglome,strata = ~estrato,
                 weights = ~fac500a, data = prueba_informalidad)

dsg_2 <- svydesign(ids = ~conglome,strata = ~estrato,
                 weights = ~fac500a, data = prueba_ingreso)


# prueba para muestras complejas

svymean(~ocupinf, dsg_1) #82%

svymean(~ing_net, dsg_2) #844


#Hannsen Hurwitz Madow



#=============

prop_prov_infor <- svyby(
  ~ocupinf,
  ~NAME_2,
  dsg_1,
  svymean,
  vartype = "ci"
)

prop_prov_ingr <- svyby(
  ~ing_net,
  ~NAME_2,
  dsg_2,
  svymean,
  vartype = "ci"
)

#---------------

mp<-geodata::gadm(country = "PER", level=2,path=tempdir()) %>% sf::st_as_sf() %>% 
  filter(NAME_1=="Junín")



mp %>% left_join(prop_prov_infor,by="NAME_2") %>% 
  ggplot()+
  geom_sf(aes(fill=ocupinf),col="black",size=0.7)+
  geom_sf_label(aes(label = NAME_2),
                size = 3.8)+
  geom_sf_text(aes(label =round(ocupinf,2)),
               size = 3.9,nudge_y=+0.1,col="white")+
  scale_fill_gradient(low = "#aba7fe",
                      high = "#0b00fc",
                      name="Tasa\ninformalidad")+
  theme_minimal()+
  labs(x = NULL,y = NULL,
       title = "",
       subtitle = "",
       caption = "Fuente: Instituto Nacional de Estadística e Informática\nElaboración: Propia")


library(ggrepel)


mp %>% left_join(prop_prov_ingr,by="NAME_2") %>% 
  ggplot()+
  geom_sf(aes(fill=ing_net),col="black",size=0.7)+
  geom_label_repel(aes(label = NAME_2, geometry = geometry),
                   stat= "sf_coordinates",colour = "black",fill="transparent",
                size = 3.8)+
  geom_sf_text(aes(label =round(ing_net,2)),
               size = 3.9,col="white")+
  scale_fill_gradient(low="blue",high="red", space ="Lab" )+
  theme_minimal()+
  labs(x = NULL,y = NULL,
       title = "",
       subtitle = "",
       caption = "Fuente: Instituto Nacional de Estadística e Informática\nElaboración: Propia")


