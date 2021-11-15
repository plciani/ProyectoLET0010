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

ggplot(datos2,aes(x=Semana,y=`SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA`,color=Año))+
  geom_line(lwd=0.8)+
  labs(title="Evolución por semana del total de atenciones de Urgencia",
       x="Semana estadística",y="Total atenciones urgencia",
       caption="Elaboración propia con datos obtenidos desde Ministerio Salud")+
  scale_y_continuous(limit = c(0,500000), breaks=c(0,100000,200000,300000,400000,500000),
                     labels=c("0","100.000","200.000","300.000","400.000","500.000"))+
  scale_x_continuous(limit = c(0,53), breaks=c(0,10,20,30,40,50))+
  geom_hline(yintercept=max(datos$`SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA`),
             col="#56B4E9",linetype = "dashed")+
  annotate("text",x=50,y=420000,
           label="Máximo",
           parse=TRUE,
           col="#56B4E9")


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

areaplot(x, y,
         main = "Gráfico de áreas apiladas de las principales causas de las atenciones de urgencia, año 2014.",
         col = cols,
         xlab = "Semana Estadística",
         ylab="",
         legend = TRUE,
         args.legend = list(x = "topright", cex = 0.65),
         ylim = c(0, 600000),
         sub = "                                                                             Elaboración propia con datos obtenidos desde Ministerio Salud")

?plot