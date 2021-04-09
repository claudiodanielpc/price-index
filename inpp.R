###Reporte Índice de precios al producto. Construcción

##Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,kableExtra,magick,webshot,
                dplyr,lubridate, scales)
webshot::install_phantomjs()

##Traer los datos
inpp<-read.csv("https://raw.githubusercontent.com/claudiodanielpc/price-index/master/inpp.CSV",
             encoding="latin",header=TRUE,check.names=FALSE)

##Trabajar con el archivo

inpp<-inpp%>%
  #long
  gather(month,value, -tipo)%>%
  ##extraer año
  mutate(year=paste0("20",substr(month,5,6)))%>%
  ##Reconstruir variable de mes
  
  ##En el código se usan muchos ifelse... no son tidy pero son funcionales
  
  mutate(month=substr(month,1,3))%>%
  mutate(month=ifelse(month=="ene",1,
                      ifelse(month=="feb",2,
                             ifelse(month=="mar",3,
                                    ifelse(month=="abr",4,
                                           ifelse(month=="may",5,
                                                  ifelse(month=="jun",6,
                                                         ifelse(month=="jul",7,
                                                                ifelse(month=="ago",8,
                                                                       ifelse(month=="sep",9,
                                                                              ifelse(month=="oct",10,
                                                                                     ifelse(month=="nov",11,
                                                                                            ifelse(month=="dic",12,"")))))))))))))%>%
  ###Crear fecha
  mutate(fecha=ymd(paste(year,month,"15",sep=" ")))%>%
###Limpiar tipo de indicador
  mutate(tipo=str_remove(tipo,"Índice nacional de precios productor. Base Julio 2019=100 [(]SCIAN 2013[)], Construcción, Materiales, alquiler de maquinaria y remuneraciones, "))%>%
  mutate(grupo=ifelse(str_length(tipo)==14,"Índice",
                      ifelse(str_length(tipo)==43 | str_length(tipo)==52 |
                               str_length(tipo)==57,"Subíndice","Subsubíndice")))%>%
mutate(catego=ifelse(str_detect(tipo,"Subíndice materiales de construcción"),
                                "Materiales de construcción",
                     ifelse(str_detect(tipo,"Subíndice alquiler de maquinaria y equipo"),
                                       "Alquiler de maquinaria y equipo",
                                       ifelse(str_detect(tipo,"Subíndice de remuneraciones"),
                                                         "Remuneraciones",""))))%>%
  mutate(catego=ifelse(grupo=="Índice","General",catego))%>%
  #Tidy :(
  #Limpiar componentes de materiales de construcción
  mutate(detalle=ifelse(catego=="Materiales de construcción" & grupo=="Subsubíndice",
                        str_remove(tipo,"Índice general, Subíndice materiales de construcción, "),catego))%>%
  ##Quitar tipos y dejar solo los insumos
  mutate(detalle=str_remove(detalle,"01. Minerales no metálicos, "))%>%
  mutate(detalle=str_remove(detalle,"02. Cemento y concreto, "))%>%
  mutate(detalle=str_remove(detalle,"03. Aglutinantes, "))%>%
  mutate(detalle=str_remove(detalle,"04. Artículos a base de arcilla, "))%>%
  mutate(detalle=str_remove(detalle,"05. Productos a base de concreto, "))%>%
  mutate(detalle=str_remove(detalle,"06. Partes estructurales de concreto, "))%>%
  mutate(detalle=str_remove(detalle,"07. Otros productos de concreto, "))%>%
  mutate(detalle=str_remove(detalle,"08. Otros artículos a base de minerales no metálicos, "))%>%
  mutate(detalle=str_remove(detalle,"09. Productos de madera, "))%>%
  mutate(detalle=str_remove(detalle,"10. Pinturas y similares, "))%>%
  mutate(detalle=str_remove(detalle,"11. Productos de plástico,"))%>%
  mutate(detalle=str_remove(detalle,"12. Otros productos químicos, "))%>%
  mutate(detalle=str_remove(detalle,"13. Productos metálicos, "))%>%
  mutate(detalle=str_remove(detalle,"14. Productos de alambre, "))%>%
  mutate(detalle=str_remove(detalle,"15. Equipos eléctricos, "))%>%
  mutate(detalle=str_remove(detalle,"16. Accesorios eléctricos, "))%>%
  mutate(detalle=str_remove(detalle,"17. Muebles y accesorios, "))%>%
  mutate(detalle=str_remove(detalle,"18. Otros materiales y accesorios, "))%>%
  ##Borrar la variable con la que se hizo el tidy
  select(-tipo)

##Una vez limpia y ordenada la data, se realizan los cálculos y gráficos

##tabla 1
inpp%>%
  filter(grupo=="Índice" | grupo=="Subíndice")%>%
  group_by(catego)%>%
  ##Cacular variaciones
  ##Mensual
  mutate(var=format(round(((value/lag(value,1)-1)*100),2),
                    nsmall=2))%>%
  ##Usar último dato
  filter(fecha==(max(fecha)))%>%
  select(catego,var)%>%
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Índice Nacional de Precios al Productor. Construcción<br>
marzo 2021</b></h>',
        format="html",
        align = "c",
        col.names = c("Tipo",
                      "%"))%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#FFD700")%>%
  row_spec(1:4, bold = F, color = "black", background = "white")%>%
  add_header_above(c(" ", "Variación mensual" = 1),
                   color="black",background="#FFD700")%>%
  footnote(general = "Elaborado por CANADEVI Nacional. Gerencia de Fondos de Vivienda. Coordinación de Indicadores de Vivienda
con información de INEGI.",
           general_title = "
Fuente: ")%>%
  as_image(file="precios1.png")




##Gráfica
inpp%>%
  filter(grupo=="Índice" | grupo=="Subíndice")%>%
  group_by(catego)%>%
  ##Cacular variaciones
  ##Anual
  mutate(var=(value/lag(value,12)-1)*100)%>%
  ##Eliminar Nas
  filter(!is.na(var))%>%
  filter(year>=2014)%>%
  ggplot(.,aes(x=fecha,y=var,group=catego))+
geom_line(aes(linetype=catego,color=catego),size=1.5)+
  #tipo de línea
  scale_linetype_manual(values=c("solid", "twodash",
                                 "solid","solid"))+
  #Colores de las líneas
  scale_color_manual("Tipo",values=c("#feb24c",
                                     "#636363",
                                     "#7fcdbb",
                                     "#33a02c"))+
  #Eliminar leyenda de linetype
  guides(linetype=FALSE)+
  geom_text(data =.%>% filter(fecha == last(fecha)), 
            aes(label = format(round(var,1))),
            color="black",
            hjust=0.5,vjust=2,size=5,fontface="bold")+
  theme_bw() +
  scale_x_date(date_breaks="3 months",date_labels = "%b %Y")+
  labs(
    title = "Índice Nacional de Precios al Productor. Construcción, marzo 2021",
    subtitle = "Variación % anual",
    y = "Var. %",
    x="",
    caption = "Fuente: Elaborado por CANDEVI Nacional. Gerencia de Fondos de Vivienda. Coordinación de Indicadores de Vivienda
con datos de INEGI."
  )+
  theme(plot.title = element_text(hjust = 0, size=20,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=12, face="italic"),
        plot.caption = element_text(hjust = 0,size=8),
        legend.position = "bottom",
        text=element_text(size=20),
        axis.text.x = element_text(angle = 90))
ggsave("precios2.png", height=10, width=20, units='in', dpi=300)


##Gráfica 2


inpp%>%
  filter(grupo=="Índice" | grupo=="Subíndice")%>%
  filter(fecha==max(fecha) | fecha=="2020-12-15")%>%
  group_by(catego)%>%
  ##Cacular variaciones
  ##Acumulada en el año
  mutate(var=(value/lag(value,1)-1)*100)%>%
  ungroup()%>%
  ##Eliminar Nas
  filter(!is.na(var))%>%
  arrange(desc(var))%>%
  mutate(catego=factor(catego, levels = catego))%>%
  ggplot(., aes(catego,var))+
  geom_col(fill="#feb24c")+
  geom_text(aes(label=paste(format(round(var,1)), "%")), 
            vjust=1.6, color="black", size=10, 
            fontface="bold")+
  theme_minimal() +
  labs(
    title = "Índice Nacional de Precios al Productor. Construcción",
    subtitle = "Variación % acumulada en el año, marzo 2021",
    y = "Var. % acumulada",
    x="",
    caption = "Fuente: Elaborado por CANDEVI Nacional. Gerencia de Fondos de Vivienda. Coordinación de Indicadores de Vivienda
con datos de INEGI."
  )+
  theme(plot.title = element_text(hjust = 0, size=20,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=12, face="italic"),
        plot.caption = element_text(hjust = 0,size=8),
        legend.position = "bottom",
        text=element_text(size=20))
ggsave("precios3.png", height=10, width=20, units='in', dpi=300)







##tabla 2
inpp%>%
  filter(grupo=="Subsubíndice" | grupo=="Materiales de construcción")%>%
  group_by(detalle)%>%
  ##Cacular variaciones
  ##Mensual
  #Número
  mutate(varcalc=(value/lag(value,1)-1)*100)%>%
  #caracter
  mutate(varet=format(round(((value/lag(value,1)-1)*100),2),
                      nsmall=2))%>%
  ungroup()%>%
  ##Usar último dato
  filter(fecha==(max(fecha)))%>% 
  ##Ordenar y diez productos con mayor variación
 slice_max(varcalc,n=10)%>%
    #top_n(10)
  select(detalle,varet)%>%
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Índice Nacional de Precios al Productor<br> 
Materiales de construcción<br>
Principales genéricos con mayor aumento de precios en el mes<br>
marzo 2021</b></h>',
        format="html",
        align = "c",
        col.names = c("Genérico",
                      "%"))%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#FFD700")%>%
  row_spec(1:10, bold = F, color = "black", background = "white")%>%
  add_header_above(c(" ", "Variación mensual" = 1),
                   color="black",background="#FFD700")%>%
  footnote(general = "Elaborado por CANADEVI Nacional. Gerencia de Fondos de Vivienda. Coordinación de Indicadores de Vivienda
con información de INEGI.",
           general_title = "
Fuente: ")%>%
  as_image(file="precios4.png")



##tabla 3
inpp%>%
  filter(grupo=="Subsubíndice" | grupo=="Materiales de construcción")%>%
  group_by(detalle)%>%
  ##Cacular variaciones
  ##Mensual
  #Número
  mutate(varcalc=(value/lag(value,1)-1)*100)%>%
  #caracter
  mutate(varet=format(round(((value/lag(value,1)-1)*100),2),
                    nsmall=2))%>%
  ungroup()%>%
  ##Usar último dato
  filter(fecha==(max(fecha)))%>% 
  ##Ordenar y diez productos con mayor variación
  slice_min(varcalc,n=10)%>%
  #top_n(10)
  select(detalle,varet)%>%
  kable(caption='<h1 style="color:black;font-size:20px;"><b>Índice Nacional de Precios al Productor<br> 
Materiales de construcción<br>
Principales genéricos con mayor disminución de precios en el mes<br>
        marzo 2021</b></h>',
        format="html",
        align = "c",
        col.names = c("Genérico",
                      "%"))%>%
  kable_styling(full_width = F)%>%
  row_spec(0, bold = F, color = "black", background = "#FFD700")%>%
  row_spec(1:10, bold = F, color = "black", background = "white")%>%
  add_header_above(c(" ", "Variación mensual" = 1),
                   color="black",background="#FFD700")%>%
  footnote(general = "Elaborado por CANADEVI Nacional. Gerencia de Fondos de Vivienda. Coordinación de Indicadores de Vivienda
con información de INEGI.",
           general_title = "
Fuente: ")%>%
  as_image(file="precios5.png")

  