library(tidyverse)
library(rio)
#AtencionesUrgencia2014 <- read_excel("Data/AtencionesUrgencia2014.xlsx")
#View(AtencionesUrgencia2014)
#datosfinales <- AtencionesUrgencia2014

test1=rio::import("Data/AtencionesUrgencia2014.xlsx") %>% 
  rowid_to_column()

#names(datosfinales)
#view(datosfinales)

#datosfinales2 <- datosfinales %>% 
  #rowid_to_column()

#datosfinales2 %>% 
  #ggplot(aes(rowid,`SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA`))+
  #geom_point()


#datosfinales$`TOTAL CAUSAS SISTEMA RESPIRATORIO`[2]
#max(datosfinales2$`TOTAL CAUSAS SISTEMA RESPIRATORIO`)

#datosfinales2 %>% 
  #ggplot(aes(rowid,`TOTAL CAUSAS SISTEMA RESPIRATORIO`))+
  #geom_point()+
  #labs(title="Evolución por semana de entradas a Urgencia por causas atribuídas al sistema respiratorio, año 2014",
  #     x='Semanas',y="Total causas",caption="Elaboración propia con datos obtenidos desde Ministerio Salud")+
  #xlim(0,55)+
  #ylim(0,180000)

datosfinales2=test1
ggplot()+
  xlim(0,55)+
  ylim(0,180000)+
  geom_line(aes(datosfinales2$rowid[0:25],datosfinales2$`TOTAL CAUSAS SISTEMA RESPIRATORIO`[0:25]))+
  geom_line(aes(datosfinales2$rowid[25:38],datosfinales2$`TOTAL CAUSAS SISTEMA RESPIRATORIO`[25:38]),color="#56B4E9")+
  geom_line(aes(datosfinales2$rowid[38:53],datosfinales2$`TOTAL CAUSAS SISTEMA RESPIRATORIO`[38:53]))+
  labs(title="Evolución por semana de entradas a Urgencia por causas atribuídas al sistema respiratorio, año 2014",
       x='Semanas',y="Total causas",caption="Elaboración propia con datos obtenidos desde Ministerio Salud")+
  annotate("text",x=35,y=165000,
           label="Invierno",
           parse=TRUE,
           col="#56B4E9")+
  theme_minimal()
  
  
  






  