library(tidyverse)
library(readxl)
AtencionesUrgencia2014 <- read_excel("Data/AtencionesUrgencia2014.xlsx")
View(AtencionesUrgencia2014)
datosfinales <- AtencionesUrgencia2014

names(datosfinales)
view(datosfinales)

datosfinales2 <- datosfinales %>% 
  rowid_to_column()

datosfinales2 %>% 
  ggplot(aes(rowid,`SECCIÓN 1. TOTAL ATENCIONES DE URGENCIA`))+
  geom_point()

datosfinales2 %>% 
  ggplot(aes(rowid,`TOTAL CAUSAS SISTEMA RESPIRATORIO`))+
  geom_point()+
  labs(title="Evolución por semana de entradas a Urgencia por causas atribuídas al sistema respiratorio, año 2014",
       x='Semana',y="Total causas",caption="Elaboración propia con datos obtenidos desde Ministerio Salud")