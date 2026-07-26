#===========================================================#
# DATA STORYTELLING CON R                                   #
# Sesión: 01                                                #
# Fecha: 03/02/2026                                         #
# Docente-Github: Mirko-cv                                  #
#===========================================================#



#1. Que es storytelling

  #A. evidencia -> datos
  #B. mensaje -> comunicar
  #C. audiencia -> convencer


#1.1 Que no veremos

  #No aprenderemos a programar
  #No aprenderemos estadistica
  #No aprederemos a modelar
  #No aprederemos ETL

#1.1 Que (si) veremos

  #R como medio para mostrar una idea/problema



#===================================#
# CASO 1. PRECIOS GASOHOL REGULAR   #
#===================================#
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(readxl)
library(dplyr)
library(ggplot2)

df = readxl::read_excel("Grifos_bandera_precio.xlsx",sheet = "Sheet1")

# describimos los datos

df %>%  glimpse()


df_plot <- df %>% filter(DISTRITO %in% c("ATE","VILLA EL SALVADOR","LA VICTORIA"))

#---- USO DE GGPLOT

df_plot %>% ggplot(aes(x=fecha,y=Bandera,colour=DISTRITO))+
  geom_line(size=1.5)+
  theme_minimal()+
  labs(title="Precios Promedios del Gasohol Regular en 3 distritos",
       subtitle = "2024",
       caption = "Fuente: SCOP-OSINERGMIN",
       y="Precio Bandera")
  

p1<-df_plot %>% 
  ggplot(aes(x=fecha,y=Bandera, col=DISTRITO))+
  geom_line()


p2<-df_plot %>% 
  ggplot(aes(x=fecha,y=Bandera, col=DISTRITO))+
  geom_line(size=1.5)+
  labs(title="Precios diarios del Gasohol Regular - EVPC Bandera",
       subtitle="(Promedios diarios 2024)",
       y="Precio", x="", col="Distrito",
       caption="Nota. los EVPC bandera utilizan marcas conocidas para vender combustibles (franquicia)")+
  theme_minimal()+
  scale_color_viridis_d()
  

library(patchwork)

p1/p2

#----------

df_plot %>% group_by(DISTRITO) %>% summarise(Promedio = mean(Bandera, na.rm=T))



p3<-df_plot %>% 
  ggplot(aes(x=fecha,y=Bandera, col=DISTRITO))+
  geom_line()+
  facet_grid(~DISTRITO)


p4<-df_plot %>% 
  ggplot(aes(x=fecha,y=Bandera, col=DISTRITO))+
  geom_line(size=1.5)+
  facet_grid(~DISTRITO)+
  labs(title="Precios diarios del Gasohol Regular - EVPC Bandera",
       subtitle="(Promedios diarios 2024)",
       y="Precio", x="", col="Distrito",
       caption="Nota. los EVPC bandera utilizan marcas conocidas para vender combustibles (franquicia)")+
  theme_minimal()+
  scale_color_viridis_d()

p2/p4

# ¿cual es mejor?

p2/p4 + plot_annotation(
  tag_levels = "1",
)




#
df_plot %>% tidyr::pivot_longer()



df_plot %>% tidyr::pivot_longer(cols=c("Bandera","MARCA","Sin Bandera")) %>% 
  ggplot(aes(x=fecha,y=value, col=name))+
  geom_line(size=1.5)+
  facet_grid(~DISTRITO)+
  labs(title="Precios diarios del Gasohol Regular - EVPC Bandera",
       subtitle="(Promedios diarios 2024)",
       y="Precio", x="", col="Distrito",
       caption="Nota. los EVPC bandera utilizan marcas conocidas para vender combustibles (franquicia)")+
  theme_minimal()+
  scale_color_viridis_d()+
  theme_relato_dark()


#======================================#
# CASO 2. BRECHA ENERGÉTICA Y PROGRESO #
#======================================#

library(ggrepel)


df<-read_xlsx("Correlaciones.xlsx", sheet="Correlaciones",skip=1)



X<-"Consumo de energía eléctrica (kWh per cápita)"

df %>% filter(!!sym(X)<2500) %>%  
  ggplot(aes(x = `Consumo de energía eléctrica (kWh per cápita)`, 
                   y = `Indice de desarrollo humano (IDH)`)) +
  geom_point()+
  geom_smooth()



# ¿ Qué más agregar?

df<-df %>%  
  select(`Country Name`,`Consumo de energía eléctrica (kWh per cápita)`,
         `Indice de desarrollo humano (IDH)`, 
         `tipo ingreso`)

r2<-summary(lm(df$`Indice de desarrollo humano (IDH)`~df$`Consumo de energía eléctrica (kWh per cápita)`))$r.squared

df<-df %>% filter(!!sym(X)<2500) %>% na.omit()



df%>%   ggplot(aes(x = `Consumo de energía eléctrica (kWh per cápita)`, 
                 y = `Indice de desarrollo humano (IDH)`)) +
  geom_point( aes(color = `Country Name`=="Colombia", 
                  fill = `Country Name`=="Colombia", 
                  shape = factor(`tipo ingreso`)),size = 3)+
  scale_color_manual(values = c("TRUE" = "red2", "FALSE" = "gold"),guide = "none") +
  scale_fill_manual(values  = c("TRUE" = "gold", "FALSE" = "yellow"),guide = "none")+
  geom_smooth(method = "lm", se = FALSE, color = "orange", linetype = "dotted")+
  annotate("text", y = 0.5, x = 2000,
           label = paste0("R² = ", round(r2, 2)), 
           hjust = 1, size = 5, fontface = "bold",col="white")+
  labs(title="Relación acceso energético y desarrollo",
       subtitle = "2023",
       x="Consumo eléctrico (kWh per cápita)",
       y="IDH",
       shape='Tipo')+theme_relato_dark()
  ggthemes::theme_solarized(light = F)+
  theme(legend.position = "bottom")




set.seed(14)
df %>% sample_frac(0.2) %>%   ggplot(aes(x = `Consumo de energía eléctrica (kWh per cápita)`, 
                   y = `Indice de desarrollo humano (IDH)`)) +
  geom_point(aes(shape = factor(`tipo ingreso`)),col="yellow", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "orange", linetype = "dotted") +
  annotate("text", y = 0.5, x = 2000,
           label = paste0("R² = ", round(r2, 2)), 
           hjust = 1, size = 5, fontface = "bold",col="white")+
  geom_text_repel(aes(label = `Country Name`, 
                      color = "black", 
                      fontface = "bold"),
                  size =4, 
                  show.legend = FALSE, 
                  max.overlaps = 100, 
                  box.padding = 0.4,
                  point.padding = 0.3,
                  force=20, #aumenta el rango de lineas
  )+
  labs(title="Relación acceso energético y desarrollo",
       subtitle = "2023",
       x="Consumo eléctrico (kWh per cápita)",
       y="IDH",
       shape='Tipo')+
  ggthemes::theme_solarized(light = F)+
  theme(legend.position = "bottom")




















