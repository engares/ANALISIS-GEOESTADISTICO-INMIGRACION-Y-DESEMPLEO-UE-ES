library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)

# OBTENEMOS EL MAPA DESEADO -----------------------------------------------

SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)
class(SHP_0)
EU27 <- eu_countries %>% 
  filter(code != 'UK') %>% 
  select(geo = code, name)

SHP_27 <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU27, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()

SHP_27 %>% 
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()


# UNEMPLOYMENT ------------------------------------------------------------

#Unemployment rate by sex


tesem120 <- get_eurostat("tesem120", time_format = "num")

unique(tesem120$unit)

tesem120 <- tesem120 %>% 
  filter(unit == 'PC_ACT') %>% 
  filter((time > "2008"))%>%
  filter((geo %in% SHP_27$geo))%>%
  filter(sex == 'T')



# ESTADÍSTICOS BÁSICOS
summary(tesem120[tesem120$time== 2022,])
sd(tesem120[tesem120$time== 2022,]$values); sd(tesem120[tesem120$time== 2022,]$values)^2
#cv
sd(tesem120[tesem120$time== 2022,]$values) / mean(tesem120[tesem120$time== 2022,]$values) * 100


# Test de Kolmogorov-Smirnov
ks.test(tesem120$values, "pnorm")
hist(tesem120$values,main = "Figura2: Distribución del ratio de desempleo ", xlab= "Tasa de desempleo", ylab= "Frequencia")

#Gráfico de líneas de la media

tesem120mean <- tesem120 %>% 
  group_by(time) %>%
  filter(time> 2008)%>%
  summarize(xmean = mean(values))

tesem120mean$spain <- (tesem120[tesem120$geo== 'ES',]$values)

d_ends1 <- tesem120mean$xmean[18]
d_ends2 <- tesem120mean$spain[18]


tesem120mean %>% 
  ggplot(aes(time)) +
  geom_line(aes( y = xmean,,color = "Media UE"),size = 2, alpha = .7)+
  geom_line(aes(y= spain, ,color = "España"),size = 2, alpha = .7)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_color_manual(name='',
                     breaks=c('España', 'Media UE'),
                     values=c('España'='red','Media UE'='darkblue' ))+
  ggtitle("Fig 4: Tasa de desempleo desde el 2009") +
  labs(x = "Año", y = "Tasa de desempleo en %", caption = "Fuente: Elaboración propia basada en datos del eurostat")



#mapa

tesem120_shp <- tesem120 %>% 
  filter(unit == 'PC_ACT') %>% 
  filter(time == 2022) %>%
  filter(age == "Y15-74") %>%
  filter(sex == "T") %>%
  select(geo, values) %>% 
  inner_join(SHP_27, by = "geo") %>% 
  st_as_sf()


tesem120_shp %>% 
  ggplot(aes(fill = values)) +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  ggtitle(" Fig 6: Ratio de desempleo en la UE en 2022") +
  labs( caption = "Elaboración propia basada en datos del eurostat")+
  guides(fill=guide_legend(title="Tasa en %"))+
  theme_void()


?hist


# POBLACIÓN CIDADANÍA EXTRANJERA ------------------------------------------

#Immigration by age and sex

migr_pop2<- get_eurostat("migr_pop2ctz", time_format = "num")


unique(migr_pop2$citizen)

migr_pop2 <-
  migr_pop2 %>% 
  filter(unit == 'NR') %>% 
  filter(age == "TOTAL") %>%
  filter((geo %in% SHP_27$geo))%>%
  filter((citizen == "FOR_STLS"))%>%
  filter((time > "2008"))%>%
  filter(sex == "T")

#creamos el ratio


#ESTADÍSITICOS BÁSICOS
summary(migr_pop2[migr_pop2$time== 2022,])
sd(migr_pop2[migr_pop2$time== 2022,]$values); sd(migr_pop2[migr_pop2$time== 2022,]$values)^2
#cv
sd(migr_pop2[migr_pop2$time== 2022,]$values) / mean(migr_pop2[migr_pop2$time== 2022,]$values) * 100


# Test de Shapiro-Wilk
shapiro.test(migr_pop2$values)

# Test de Kolmogorov-Smirnov
ks.test(migr_pop2$values, "pnorm")

hist(migr_pop2$values)


#Gráfico de líneas de la media

migr_pop2mean <- migr_pop2 %>% 
  group_by(time) %>%
  filter(sex == "T")%>%
  summarize(xmean = mean(values))

migr_pop2mean$spain <- (migr_pop2[migr_pop2$geo== 'ES',]$values)

migr_pop2mean %>% 
  ggplot(aes(time))+
  geom_line(aes( y = xmean,,color = "Media UE"),size = 2, alpha = .7)+
  geom_line(aes(y= spain, ,color = "España"),size = 2, alpha = .7)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(label=comma)+
  scale_color_manual(name='',
                     breaks=c('España', 'Media UE'),
                     values=c('España'='red','Media UE'='darkblue' ))+
  ggtitle("Fig 3:Población con nacionalidad extranjera residiendo desde el 2009") +
  labs(x = "Año", y = "Num de inmigrantes ", caption =  "Fuente: Elaboración propia basado en datos de eurostat")

#mapa

migr_pop2_shp <- migr_pop2 %>% 
  filter(unit == 'NR') %>% 
  filter(time == 2022) %>%
  filter(age == "TOTAL") %>%
  filter(sex == "T") %>%
  select(geo, values) %>% 
  inner_join(SHP_27, by = "geo") %>% 
  st_as_sf()

library(scales) 


migr_pop2_shp %>% 
  ggplot(aes(fill = values)) +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  scale_fill_continuous(label=comma)+
  ggtitle(" Fig 5: Población con ciudadanía extranjera residiendo en la UE en 2022") +
  labs( caption = "Fuente: Elaboración propia basada en datos del eurostat")+
  guides(fill=guide_legend(title="Núm. de Inmigrantes"))+
  theme_void()


# ESTADÍSTICOS CORRELACIÓN ------------------------------------------------


#FALTAN  GRÁFICOS DE DISPERSIÓN.
cor(y=tesem120_shp$values,x = migr_pop2_shp$values, method = "spearman")

cor(x=tesem120_shp$values, y = migr_pop2_shp$values, method = "spearman")

cor(x=tesem120mean$xmean, y = migr_pop2mean$xmean, method = "spearman")


tesem120_shp1 <- tesem120_shp %>% st_drop_geometry()
migr_imm8_shp1 <- migr_imm8_shp %>% st_drop_geometry()


data1<- inner_join(tesem120_shp1,migr_imm8_shp1, by = "geo")

ggplot(data1, aes(log(values.y), values.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fig 7: Diagrama de dispersión de las variables en la UE 2022",
       x = "Log del núm. de inmigrantes ",
       y = "Tasa de paro", caption="Fuente: Elaboración propia basada en datos del eurostat")





plot(x=tesem120mean$xmean, y = migr_pop2mean$xmean, pch=20, col='blue',
     xlab='Tasa de desempleo', 
     ylab='Población inmigrante',
     main= "Fig. 7: Diagrama de dispersión")

plot(x=tesem120_shp$values, y=migr_pop2_shp$values, pch=20, col='blue',
     xlab='Tasa de desempleo', 
     ylab='Población inmigrante',
     main= "Fig. 7: Diagrama de dispersión")



