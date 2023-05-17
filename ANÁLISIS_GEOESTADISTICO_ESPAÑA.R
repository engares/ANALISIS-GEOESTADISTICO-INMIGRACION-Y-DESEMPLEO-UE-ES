
lib <- c("tidyRSS", 'tidyverse','sf',"osmdata", 'ggmap','mapSpain','units',
         'spatstat.geom', 'fs','lubridate','tmap','classInt','showtext',
         'sysfonts','rvest',"rnaturalearth")
for (pqg in lib){
  if (!(pqg %in% installed.packages()))
    install.packages(pqg, repos = "http://cran.r-project.org")
  library(pqg, character.only = TRUE)
}


library(ggthemes)
library(ggalt)
library(scales)
library(pacman)
p_load(tidyverse, sf, sp, spData, spdep, mapSpain)

# TASA DE PARO POR COMUNIDADES --------------------------------------------

x4247 <- read_delim("./proyecto_final/data/espanola/tasa_paro/4247.csv", 
                    delim = "\t", escape_double = FALSE, 
                    trim_ws = TRUE)

x4247$Total <- as.numeric(sub(",", ".", sub(".", "", x4247$Total, fixed=TRUE), fixed=TRUE))

comunidades <-  esp_get_ccaa(); comunidades <- comunidades[,c(1,22)]

#Bases de datos para el análisis estadístico
x4247c1 <- x4247 %>% 
  filter(x4247$`Comunidades y Ciudades Autónomas` != "Total Nacional") %>%
  filter(Edad == "Total") %>%
  filter(Sexo == "Ambos sexos") %>%
  filter(str_detect(Periodo, 'T4'))%>%
  separate(`Comunidades y Ciudades Autónomas`, c("codauto", "nombre"), sep = " ")

local_plotc1 <- comunidades%>%
  full_join(x4247c1, by = "codauto")

# ESTADÍSTICOS BÁSICOS
summary(x4247c1[x4247c1$Periodo== '2022T4',])
sd(x4247c1[x4247c1$Periodo== '2022T4',]$Total, na.rm=TRUE); sd(x4247c1[x4247c1$Periodo== '2022T4',]$Total, na.rm=TRUE)^2
#cv
sd(x4247c1[x4247c1$Periodo== '2022T4',]$Total, na.rm=TRUE)/13.53

# Test de Kolmogorov-Smirnov
ks.test(x4247c1$Total, "pnorm")

hist(x4247c1$Total,main = "Fig 9: Distribución del ratio de desempleo en España", xlab= "Tasa de desempleo", ylab= "Frequencia")


#Representación

x4247c <- x4247 %>% 
  filter(x4247$`Comunidades y Ciudades Autónomas` != "Total Nacional") %>%
  filter(Periodo == "2022T4") %>%
  filter(Edad == "Total") %>%
  filter(Sexo == "Ambos sexos") %>%
  separate(`Comunidades y Ciudades Autónomas`, c("codauto", "nombre"), sep = " ")


#joining data
local_plot <- comunidades%>%
  full_join(x4247c, by = "codauto")

#tema del gráfico

theme_maps <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "ivory1", color = NA),
      panel.background = element_rect(fill = "ivory1", color = NA),
      legend.background = element_rect(fill = "ivory1", color = NA),
      panel.border = element_blank(),
      plot.title = element_text(size = 11, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
      legend.title = element_text(color = "grey40", size = 8),
      legend.text = element_text(color = "grey40", size = 7, hjust = 0),
      legend.position = c(0.7, 0.07),
      legend.text.align = 0,
      plot.margin = unit(c(.5,.5,.2,.5), "cm"),
      panel.spacing = unit(c(2,0.2,.2,0.2), "cm"))
}


#mapa final!

local_plot%>%
  ggplot(aes(fill = Total))+
  geom_sf(color = "white", size = 7)+
  geom_segment(aes(x = -8, y = 35, xend = -8, yend = 38), color = "grey60")+
  geom_segment(aes(x = -15, y = 38, xend = -8, yend = 38), color = "grey60")+
  scale_fill_distiller(palette = 'YlOrBr', trans = "reverse", 
                       labels = function(x) paste0(x, "%"))+
  guides(fill = guide_legend(direction = "horizontal",
                             keyheight = unit(1, units = "mm"),
                             label.position = "bottom"))+
  labs(title = "Fig. 11: Distribución territorial del paro en 2022",
       subtitle = "% de personas en situación de desempleo.",
       fill = NULL,
       caption = "Fuente: Elaboración propia basado en datos del INE")+
  theme_maps()


# POBLACIÓN INMIGRANTE POR COMUNIDADES --------------------------------------------

X0tamu004 <- read_delim("proyecto_final/data/espanola/num_inmigrantes/02005bsc.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

comunidades <-  esp_get_ccaa(); comunidades <- comunidades[,c(1,22)]

X0tamu004$Total <- as.numeric(X0tamu004$Total)

#Bases de datos para el análisis estadístico

X0tamu004c2 <- X0tamu004 %>% 
  filter(X0tamu004$`Comunidades y Ciudades Autonomas` != "Total Nacional") %>%
  filter(Sexo == "Ambos sexos") %>%
  separate(`Comunidades y Ciudades Autonomas`, c("codauto", "nombre"), sep = " ")

local_plotc2 <- comunidades%>%
  full_join(X0tamu004c2, by = "codauto")

# ESTADÍSTICOS BÁSICOS
summary(X0tamu004c2[X0tamu004c2$Periodo== 2022,] );

mean(X0tamu004c2[X0tamu004c2$Periodo== 2022,]$Total)
sd(X0tamu004c2[X0tamu004c2$Periodo== 2022,]$Total, na.rm=TRUE); sd(X0tamu004c2$Total, na.rm=TRUE)^2
#cv
sd(X0tamu004c2[X0tamu004c2$Periodo== 2022,]$Total, na.rm=TRUE)/mean(X0tamu004c2[X0tamu004c2$Periodo== 2022,]$Total)


# Test de Kolmogorov-Smirnov
ks.test(X0tamu004c2$Total, "pnorm")

hist(X0tamu004c2$Total,main = "Fig 8: Distribución de la inmigración en España", xlab= "Núm. de Inmigrantes", ylab= "Frequencia")
#Representación

X0tamu004c <- X0tamu004 %>% 
  filter(X0tamu004$`Comunidades y Ciudades Autonomas` != "Total Nacional") %>%
  filter(Periodo == "2022") %>%
  filter(Sexo == "Ambos sexos") %>%
  separate(`Comunidades y Ciudades Autonomas`, c("codauto", "nombre"), sep = " ")

#joining data
local_plot1 <- comunidades%>%
  full_join(X0tamu004c, by = "codauto")

#mapa final!

local_plot1%>%
  ggplot(aes(fill = Total))+
  geom_sf(color = "white", size = 7)+
  geom_segment(aes(x = -8, y = 35, xend = -8, yend = 38), color = "grey60")+
  geom_segment(aes(x = -15, y = 38, xend = -8, yend = 38), color = "grey60")+
  scale_fill_distiller(palette = 'YlOrBr', trans = "reverse")+
  guides(fill = guide_legend(direction = "horizontal",
                             keyheight = unit(1, units = "mm"),
                             label.position = "bottom"))+
  labs(title = "Fig. 10: Distribución territorial de la inmigración en 2022",
       subtitle = "Núm. de ciudadanos con nacionalidad extranjera o apátrida",
       fill = NULL,
       caption = "Fuente: Elaboración propia basado en datos del INE")+
  theme_maps()

# ANÁLISIS ESTADÍSTICO ----------------------------------------------------

'JUNTAMOS AMBOS DATAFRAMES'

data1 <- local_plot1[,c(1,4,6,7,8)]

colnames(data1)[4] <- "inmigr"; 

data1$tasa_paro <- local_plot$Total


plot(y=data1$inmigr, x = data1$tasa_paro)

ggplot(data1, aes( log(inmigr),tasa_paro)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fig. 12: Diagrama de dispersión de las variables  españolas en 2022",
       x = "Log. del núm. de inmigrantes",
       y = "Tasa de desempleo en %",
       caption =  "Fuente: Elaboración propia basado en datos del INE")+
  scale_x_continuous(label=comma)

cor(y=data1$tasa_paro, x = data1$inmigr, method = "spearman")


## ÍNDICE DE MORAN 

#centroid <- st_centroid(local_plot$geometry)

'Eliminamos Las islas y las ciudades autonómicas porque no es posible calcular el índice de moran-'

data1 <- subset(data1, !(data1$codauto %in% c('04','05','18','19')))

(wr <- spdep::poly2nb(data1, queen = TRUE)) 
centroid <- st_centroid(data1$geometry)


summary(wr);

plot(data1$geometry, border="blue")
plot(wr, centroid, col="grey", add=TRUE)

#Creamos una matriz con la lista de conexiones:

ww <- nb2listw(wr, style="B")

moran(data1$inmigr, ww, n=length(ww$neighbours), S0=Szero(ww)) #INDICE MORAN INMIGRACIÓN
moran(data1$tasa_paro, ww, n=length(ww$neighbours), S0=Szero(ww)) #INDICE MORAN PARO



## TEST GLOBAL MORAN 

moran.test(data1$inmigr, listw=ww)

moran.test(data1$tasa_paro, listw=ww)


## HOTSPOTS 

moran.plot(data1$inmigr, listw=nb2listw(wr, style="B"), col="blue", ylab="valores retardados", main = "Fig 13: Gráfico de Moran sobre la Inmigración Española", xlab= "Núm. de Inmigrantes")
moran.plot(data1$tasa_paro, listw=nb2listw(wr, style="B"), col="blue", ylab="valores retardados", main = "Fig 14: Gráfico de Moran sobre el desempleo Español", xlab= "Tasa de desempleo",)


#TEST LOCALES

localmoran(data1$inmigr, listw = ww)
localmoran(data1$tasa_paro, listw = ww)


LISA <- function(x,y) {
  P = localmoran(y, listw = ww)
  #head(P) # Se muestran los primeros 6 datos.
  #dim(P) # La dimension de P
  dif = y - mean(y)
  lag = lag.listw(ww,y) # Calcula el retardo (promedios)
  clag = dif - mean(lag) # Retardo - Media(Retardo)
  p = P[,5] # Toma la columna: Pr(z > 0) de P
  
  # Se inicializa vector numerico de longitud filas de P
  quadrant = vector(mode="numeric",length=nrow(P))+5
  quadrant[dif>0 & clag>0 & p<= 0.05] = 1 # Alto-Alto
  quadrant[dif<0 & clag<0 & p<= 0.05] = 2 # Bajo-Bajo
  quadrant[dif<0 & clag>0 & p<= 0.05] = 3 # Bajo-Alto
  quadrant[dif>0 & clag<0 & p<= 0.05] = 4 # Alto-Bajo
  
  # Grafico  
  brks = c(1,2,3,4,5)
  colors = c("red", "blue", "light blue", "pink", "white")
  plot(data1$geometry, border ="lightgray", col=colors[findInterval(quadrant,brks,all.inside=FALSE)])
  legend("bottomright", legend = c("High-High", "Low-Low", "Low-High", "High-Low", "Insignificant"),fill = colors, bty="n", cex=0.7, y.intersp=1, x.intersp=1)
  box()
  title("LISA Cluster Map")
}

LISA(ww, data1$inmigr)
LISA(ww, data1$tasa_paro)


# ANÁLISIS ESTADÍSTICO  CON TASA DE INMIGRACIÓN ----------------------------------------------------

'CARGAMOS DATOS DE LA TASA DE PARO'

data2 <- local_plot[,c(1,3,6,7)]

colnames(data2)[3] <- "tasa_paro"

X0tamu004bsc <- read_delim("proyecto_final/data/espanola/porcentaje_inmigrantes/0tamu004bsc.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


data2$tasa_inmgr<- X0tamu004bsc$Total


## ÍNDICE DE MORAN


#centroid <- st_centroid(local_plot$geometry)

'Eliminamos Las islas y las ciudades autonómicas porque no es posible calcular el índice de moran-'
data3 <- data2
data2 <- subset(data2, !(data2$codauto %in% c('04','05','18','19')))

(wr1 <- spdep::poly2nb(data2, queen = TRUE)) 
centroid1 <- st_centroid(data2$geometry)


summary(wr1);

plot(data2$geometry, border="blue")
plot(wr1, centroid1, col="grey", add=TRUE)

#Creamos una matriz con la lista de conexiones:

ww1 <- nb2listw(wr1, style="B")

moran(data2$tasa_inmgr, ww1, n=length(ww1$neighbours), S0=Szero(ww1)) #INDICE MORAN INMIGRACIÓN
moran(data2$tasa_paro, ww1, n=length(ww1$neighbours), S0=Szero(ww1)) #INDICE MORAN PARO



## TEST GLOBAL MORAN

moran.test(data2$tasa_inmgr, listw=ww1)

moran.test(data2$tasa_paro, listw=ww1)


## HOTSPOTS

moran.plot(data2$tasa_inmgr, listw=nb2listw(wr1, style="B"), col="blue", ylab="valores retardados", main = "Fig 16: Gráfico de Moran sobre la Tasa de Inmigración Española", xlab= "Tasa de Inmigrantes en %")
moran.plot(data2$tasa_paro, listw=nb2listw(wr1, style="B"), col="blue", ylab="valores retardados")


#TEST LOCALES

localmoran(data2$tasa_inmgr, listw = ww1)
localmoran(data2$tasa_paro, listw = ww1)


LISA(ww1, data2$tasa_paro)
LISA(ww1, data2$tasa_inmgr)




# TASA DE INMIGRACIÓN POR COMUNIDADES -------------------------------------

tabla_estadisticos <- function(variable) {
  library(dplyr)
  library(kableExtra)
  
  # Calcula los estadísticos básicos
  media <- mean(variable)
  minimo <- min(variable)
  maximo <- max(variable)
  desv_tipica <- sd(variable)
  coef_variacion <- desv_tipica / media
  
  # Crea la tabla con los estadísticos
  tabla <- tibble(Estadístico = c("Media", "Mínimo", "Máximo", "Desv. Típica", "Coef. Variación"),
                  Valor = c(media, minimo, maximo, desv_tipica, coef_variacion))
  
  # Muestra la tabla con kableExtra
  kable(tabla, format = "html", align = "c",) %>%
    kable_styling(full_width = F, bootstrap_options = "striped", font_size = 14)
}

tabla_estadisticos(data2$tasa_inmgr)

hist(data2$tasa_inmgr ,main = "Fig 20: Distribución de la tasa de inmigración en España", xlab= "Tasa de Inmigrantes en %", ylab= "Frequencia")
ks.test(data2$tasa_inmgr , "pnorm")


data3%>%
  ggplot(aes(fill = tasa_inmgr))+
  geom_sf(color = "white", size = 7)+
  geom_segment(aes(x = -8, y = 35, xend = -8, yend = 38), color = "grey60")+
  geom_segment(aes(x = -15, y = 38, xend = -8, yend = 38), color = "grey60")+
  scale_fill_distiller(palette = 'YlOrBr', trans = "reverse", 
                       labels = function(x) paste0(x, "%"))+
  guides(fill = guide_legend(direction = "horizontal",
                             keyheight = unit(1, units = "mm"),
                             label.position = "bottom"))+
  labs(title = "Fig. 21: Distribución territorial de la tasa de inmigración en 2022",
       subtitle = "% de población inmigrante.",
       fill = NULL,
       caption = "Fuente: Elaboración propia basado en datos del INE")+
  theme_maps()


ggplot(data2, aes(tasa_inmgr, tasa_paro) )+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fig 22: Diagrama de dispersión sobre las variable españolas en 2022",
       x = "Tasa de inmigración en %",
       y = "Tasa de desempleo e %")

cor(x=data2$tasa_paro, y = data2$tasa_inmgr, method = "spearman")


# MODELO GAM POBLACIÓN INMIGRANTE -----------------------------------------

adj_list <- st_touches(data1, data1)
names(adj_list) <- as.factor(data1$codauto)

library(gamm4)
CAR_modelll <- gamm4(formula = inmigr ~ 1 + s(tasa_paro) + s(as.factor(data1$codauto), xt=list(nb=adj_list), bs='mrf'), data=data1, REML = T)

CAR_modelll<- CAR_modelll$gam
summary(CAR_modelll)
data1$fitted111 <- predict(CAR_modelll)
ggplot(data1, aes(fill=fitted111)) + geom_sf(lwd=0.2) +
  scale_fill_viridis_c(name='Fitted\nValue',direction=-1)


ggplot(data1,aes(x=inmigr,y=fitted111)) +
  geom_point() +
  geom_smooth(method=MASS::rlm,se = FALSE) + coord_equal()

#APÉNDICE B

summary(CAR_modelll)
install.packages("APCtools")
library(APCtools)
k <- create_modelSummary(list(CAR_modelll), dat = data1)

# Crear la tabla de coeficientes
tabla_coef <- data.frame(
  model = "GAM",
  param = "(Intercept)",
  coef = 334389.4,
  se = 63.76,
  CI_lower = 334264.4,
  CI_upper = 334514.4,
  pvalue = "1"
)


# Mostrar la tabla de coeficientes con kableExtra
tabla_coef %>% kbl() %>%
  kable_styling(bootstrap_options = "striped", font_size = 14) %>%
  add_header_above(c(" " = 1, "Fig. 22: Tabla sobre los coeficientes paramétricos" = 6)) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, bold = TRUE, italic = TRUE)

# Crear la tabla de términos suavizados
tabla_suavizado <- data.frame(
  model = "GAM",
  param = c("s(tasa_paro)", "s(as.factor(data1$codauto))"),
  edf = c(1, 13),
  pvalue = c(0.9999, 5e-04)
)

tabla_suavizado$empty <- ""


# Mostrar la tabla de términos suavizados con kableExtra
tabla_suavizado %>% kbl() %>%
  kable_styling(bootstrap_options = "striped", font_size = 14) %>%
  add_header_above(c(" ", "Fig. 23: Tabla sobre los coeficientes suavizados" = 4)) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2, bold = TRUE, italic = TRUE)

#residuos

summary(data1$fitted111 - data1$inmigr)

plot(CAR_modelll$residuals ,main = "Fig. 24: Residuos del modelo", xlab= "Código de la comunidad autónoma", ylab= "Residuo")
?plot

data1$residuals <-  CAR_modelll$residuals
hist(as.numeric(data1$residuals) ,main = "Fig 25: Distribución de los residuos del modelo", xlab= "Residuos", ylab= "Frecuencia") #normalidad de los residuos

moran.test(data2$residuals,list=ww)
ks.test(data1$residuals , "pnorm")




# MODELO GAM TASA INMIGRACIÓN ---------------------------------------------


library(gamm4)

adj_list1 <- st_touches(data2, data2)
names(adj_list1) <- as.factor(data2$codauto)

CAR_model111<- gamm4(formula = tasa_inmgr ~ 1 + s(tasa_paro) + s(as.factor(data2$codauto), xt=list(nb=adj_list1), bs='mrf'), data=data2, REML = T)

class(CAR_model111)

CAR_model111 <- CAR_model111$gam
summary(CAR_model111)

#APÉNDICE MODELO C

data2$fitted111 <- predict(CAR_model111)



ggplot(data2,aes(x=tasa_inmgr,y=fitted111)) +
  geom_point() +
  geom_smooth(method=MASS::rlm,se = FALSE) + coord_equal()


summary(data2$fitted11 - data2$tasa_inmgr)

plot(CAR_model111$residuals ,main = "Fig 26: Residuos del modelo seleccionado", xlab= "Código de la comunidad autónoma", ylab= "Residuo")


data2$residuals <-  CAR_model111$residuals
hist(as.numeric(data2$residuals) ,main = "Fig 27: Distribución de los residuos del modelo seleccionado", xlab= "Residuos", ylab= "Frecuencia") #normalidad de los residuos
ks.test(data2$residuals, "pnorm")
moran.test(data2$residuals,list=ww1)
