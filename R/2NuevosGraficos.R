library(tidyverse)
datos <- rio::import("Data/2014-2019.xlsx")
view(datos)
names(datos)
datos$Año
datos$`TOTAL CAUSAS SISTEMA RESPIRATORIO`
class(datos$Año)

datos2 <- datos %>%
  mutate(Año=as.factor(Año)) %>% 
  mutate(Semana=as.integer(Semana))
names(datos2)

class(datos2$Año)
class(datos2$Semana)


datos2 %>% 
  ggplot(aes(Semana,`TOTAL CAUSAS SISTEMA RESPIRATORIO`,col=Año))+
  geom_point()+
  ylim(0,200000)+
  geom_smooth(method='lm', formula=y~x+I(x^2), col='tomato')
max(datos$`SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA`)

ggplot(datos2,aes(x=Semana,y=`SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA`,color=Año))+
  geom_line(lwd=0.8)+
  labs(title="Evolución por semana del total de atenciones de Urgencia",
       x="Semana estadística",y="Total atenciones urgencia",
       caption="Elaboración propia con datos obtenidos desde Ministerio Salud")+
  scale_y_continuous(limit = c(0,500000), breaks=c(0,100000,200000,300000,400000,500000),
                     labels=c("0","100.000","200.000","300.000","400.000","500.000"))+
  scale_x_continuous(limit = c(0,53), breaks=c(0,10,20,30,40,50))+
  annotate("point", x = 21, y = 401636, colour = "#56B4E9")+
  annotate("text",x=20,y=420000,
           label="Influenza-2019",
           parse=TRUE,
           col="#56B4E9")+
  annotate("point", x = 33, y = 401636, colour = "#56B4E9")+
  annotate("text",x=35,y=420000,
           label="Influenza-2015",
           parse=TRUE,
           col="#56B4E9")+
  geom_vline(xintercept=11,
             col="deeppink",linetype = "dashed")+
  annotate("text",x=17,y=50000,
           label="Covid-19",
           parse=TRUE,
           col="deeppink")

ggsave("Plots/TotalAtencionesHD.png",height = 7,width = 10,dpi=150)





mod2 <- lm( `TOTAL CAUSAS SISTEMA RESPIRATORIO`~ Semana + I(Semana^2), data=datos)
summary(mod2)
anova(mod2)

confint(mod2,parm="Semana",level=0.95)

nuevo <- data.frame(Semana=25)
predict(object=mod2, newdata=nuevo, interval="confidence", level=0.95)


library(areaplot)
datos3 <- rio::import("Data/datos2014totales.xlsx")
x <- datos3$Semana
y <- datos3[,c(2,11,17,27,28)]
cols <- hcl.colors(6, palette = "PinkYl")
view(datos3)
View(y)
areaplot(x, y,
         main = "Gráfico de áreas apiladas de las principales causas de las atenciones de urgencia, año 2014.",
         col = cols,
         xlab = "Semana Estadística",
         ylab="",
         legend = TRUE,
         args.legend = list(x = "topright", cex = 0.65),
         ylim = c(0, 600000),
         sub = "Elaboración propia con datos obtenidos desde Ministerio Salud")

?plot



# Packages
library(ggplot2)
library(dplyr)

# create data
time <- as.numeric(rep(seq(1,7),each=7))  # x Axis
value <- runif(49, 10, 100)               # y Axis
group <- rep(LETTERS[1:7],times=7)        # group, one shape per group
data <- data.frame(time, value, group)
view(data)
# stacked area chart
ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()


datostest <- rio::import("Data/2014PrincipalesCausas.xlsx")
view(datostest)

datostest$Causa
ggplot(datostest,aes(x=Semana,y=`Atenciones de Urgencia`,fill=Causa))+
  geom_area()+
  labs(title="Gráfico de áreas apiladas de las principales causas de las atenciones de urgencia, año 2014.",
       x="Semana estadística",y="Total atenciones urgencia",
       caption="Elaboración propia con datos obtenidos desde Ministerio Salud")+
  scale_y_continuous(limit = c(0,500000), breaks=c(0,100000,200000,300000,400000,500000),
                     labels=c("0","100.000","200.000","300.000","400.000","500.000"))+
  scale_x_continuous(limit = c(0,53), breaks=c(0,10,20,30,40,50))

ggsave("Plots/AreasApiladasHD.png",height = 7,width = 10,dpi=150)

datos2$`TOTAL CAUSAS SISTEMA CIRCULATORIO`
ggplot(datos2,aes(x=Semana,y=`TOTAL CAUSAS SISTEMA CIRCULATORIO`,color=Año))+
  geom_line(lwd=0.8)+
  labs(title="Evolución por semana atenciones de urgencia por causas sistema circulatorio",
       x="Semana estadística",y="Total atenciones urgencia",
       caption="Elaboración propia con datos obtenidos desde Ministerio Salud")+
  scale_x_continuous(limit = c(0,53), breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(limit = c(0,10000), breaks=c(0,2000,4000,6000,8000,10000))+
  geom_vline(xintercept=11,
             col="deeppink",linetype = "dashed")+
  annotate("text",x=15,y=2000,
           label="Covid-19",
           parse=TRUE,
           col="deeppink")
ggsave("Plots/SistemaCirculatorioHD.png",height = 7,width = 10,dpi=150)

datos2$`Infarto agudo miocardio`
ggplot(datos2,aes(x=Semana,y=`Infarto agudo miocardio`,color=Año))+
  geom_line(lwd=0.8)+
  labs(title="Evolución por semana atenciones de urgencia por Infarto agudo miocardio",
       x="Semana estadística",y="Total atenciones urgencia",
       caption="Elaboración propia con datos obtenidos desde Ministerio Salud")+
  scale_x_continuous(limit = c(0,53), breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(limit = c(0,1000), breaks=c(0,200,400,600,800,1000))+
  geom_vline(xintercept=11,
             col="deeppink",linetype = "dashed")+
  annotate("text",x=15,y=100,
           label="Covid-19",
           parse=TRUE,
           col="deeppink")

ggsave("Plots/InfartosHD.png",height = 7,width = 10,dpi=150)

datos2$`Accidente vascular encefálico`
ggplot(datos2,aes(x=Semana,y=`Accidente vascular encefálico`,color=Año))+
  geom_line(lwd=0.8)+
  labs(title="Evolución por semana atenciones de urgencia por Accidente vascular encefálico",
       x="Semana estadística",y="Total atenciones urgencia",
       caption="Elaboración propia con datos obtenidos desde Ministerio Salud")+
  scale_x_continuous(limit = c(0,53), breaks=c(0,10,20,30,40,50))+
  scale_y_continuous(limit = c(0,1000), breaks=c(0,200,400,600,800,1000))+
  geom_vline(xintercept=11,
             col="deeppink",linetype = "dashed")+
  annotate("text",x=15,y=200,
           label="Covid-19",
           parse=TRUE,
           col="deeppink")

ggsave("Plots/AccidenteVascularHD.png",height = 7,width = 10,dpi=150)











