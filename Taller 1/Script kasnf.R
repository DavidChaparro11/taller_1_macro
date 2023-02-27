require(pacman)
p_load(haven, ggplot2, dplyr, tidyverse)
base <- read_dta("PWT90.dta")
base <- base %>% mutate(GDP_per_capita = rgdpe/pop, gdp_per_capita_us_100=GDP_per_capita/100)
base_colombia <- base %>% filter(country== "Colombia")
base_colombia <- base_colombia %>% mutate(ln_gdp = log(rgdpe))
base_colombia <- base_colombia %>% mutate(ln_GDPpc = log(GDP_per_capita))
##grafica_1
grafica <- ggplot(base_colombia, aes(year,ln_gdp)) +
  geom_point() +
  geom_smooth()+
  ggtitle("Logaritmo natural del PIB per capita en Colombia de 1950-2014")+
  ylab("Logaritmo natural del PIB per capita")+
  scale_x_continuous(breaks=seq(1950,2014, by=10))

grafica_col <- ggplot(base_colombia, aes(year,ln_GDPpc))+
  geom_point()+
  geom_smooth()+
  ggtitle("Logaritmo natural del PIB per capita en Colombia entre 1950 y 2014")+
  ylab("Logaritmo natural del PIB per capita")+
  scale_x_continuous(breaks = seq(1950,2014, by=10))
grafica_col
grafica
##grafica 2
paises <- c("Brazil", "United States", "Colombia", "India", "China","Russian Federation","Western Europe", "Japan", "Sub-Saharan Africa")
base_general <- base %>% filter(year >= 1980, country %in% paises)
graf_2 <- ggplot(base_general, aes(x=year, y=gdp_per_capita_us_100, color=country))+
  geom_line(size=1)+
  geom_path(size=0.4, alpha=0.8)+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  xlab("Year")+
  ylab("GDP per capita (US=100)")+
  ggtitle("The Spread of Economic Growth Since 1980")+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))
graf_2


base_2 <- read_dta("pwt80.dta")
base_2 <- base_2 %>% mutate(GDP_per_capita = rgdpe/pop, gdp_per_capita_us_100=GDP_per_capita/100)

ssafrica <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Camerún", "República Centroafricana", "Chad", "Comoras", "Congo", "Costa de Marfil", "Yibuti", "República Democrática del Congo", "Guinea Ecuatorial", "Eritrea", "Etiopía", "Gabón", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenia", "Lesoto", "Liberia", "Madagascar", "Malawi", "Malí", "Mauritania", "Mauricio", "Mozambique", "Namibia", "Níger", "Nigeria", "Ruanda", "Santo Tomé y Príncipe", "Senegal", "Seychelles", "Sierra Leona", "Somalia", "Sudáfrica", "Sudán del Sur", "Sudán", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabue")
base_2$region <- NA

west_europe <- c("Austria", "Belgium", "France", "Germany", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "Switzerland", "United Kingdom")
for (i in 2:length(base_2$country)){
  if (base_2$country[i] %in% ssafrica){
    base_2$region[i] <- "Sub Sahara Africa"
  } else if (base_2$country[i] %in% west_europe){
    base_2$region[i] <- "Western Europe"
  } else {
    base_2$region[i] <- ""
  }
}

pib_percapita_regiones <- aggregate(gdp_per_capita_us_100 ~ year+region, base_2, FUN=mean)


pib_percapita_filtrado <- filter(pib_percapita_regiones, region == "Sub Sahara Africa" | region == "Western Europe")


paises <- base_general %>% 
  filter(country == "Brazil" | 
           country == "United States" | 
           country == "Colombia" | 
           country == "India" | 
           country == "China" | 
           country == "Russian Federation" | 
           country == "Japan")


regiones_grafico <- pib_percapita_filtrado %>% filter(year >= 1980)


base_general$region <- NA
Colombia <- c("Colombia")
Brazil <- c("Brazil")
United_States <- c("United States")
India <- c("India")
China <- c("China")
Russian_Federation <- c("Russian Federation")
Japan <- c("Japan")


for (i in 2:length(base_general$country)){
  if (base_general$country[i] %in% Colombia){
    base_general$region[i] <- "Colombia"
  } else if (base_general$country[i] %in% Brazil){
    base_general$region[i] <- "Brazil"
  } else if (base_general$country[i] %in% United_States){
    base_general$region[i] <- "United States"
  } else if (base_general$country[i] %in% India){
    base_general$region[i] <- "India"
  } else if (base_general$country[i] %in% China){
    base_general$region[i] <- "China"
  } else if (base_general$country[i] %in% Russian_Federation){
    base_general$region[i] <- "Russian Federation"
  } else if (base_general$country[i] %in% Japan){
    base_general$region[i] <- "Japan"
  } else {
    base_2$region[i] <- ""
  }
}

paises_grafico_2 <- base_general %>% filter(year >= 1980)
paises_grafico_3 <- paises_grafico_2 %>% mutate(GDP_per_capita = rgdpe/pop, gdp_per_capita_us_100=GDP_per_capita/100)

plot(regiones_grafico$year, regiones_grafico$gdp_per_capita_us_100, type="l", xlab="Year", ylab="GDP per person")
lines(paises_grafico_3$year, paises_grafico_3$gdp_per_capita_us_100)






  
graph_region <- ggplot(regiones_grafico, aes(x=year, y=gdp_per_capita_us_100, color=region))+
  geom_line(size=1)+
  geom_path(size=0.4, alpha=0.8)+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  xlab("Year")+
  ylab("GDP per capita (US=100)")+
  ggtitle("The Spread of Economic Growth Since 1980")+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))
graph_region

graph_paises <- ggplot(paises_grafico_3, aes(x=year, y=gdp_per_capita_us_100, color=country))+
  geom_line(size=1)+
  geom_path(size=0.4, alpha=0.8)+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  xlab("Year")+
  ylab("GDP per capita (US=100)")+
  ggtitle("The Spread of Economic Growth Since 1980")+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))
graph_paises

install.packages("tidyr")
install.packages("tidyr", update = TRUE)
install.packages("ggpubr")
install.packages("rlang")
install.packages("rlang", update = TRUE)
install.packages("vctrs")
install.packages("cli")
library(tidyr)
library(ggplot2)
library(ggpubr)
ggarrange(graph_region, graph_paises, ncol = 2)

ggarrange(graph_region, graph_paises, ncol=2, nrow=1)





base_4 <- merge(base_2,pib_percapita_regiones, by="year")

paises <- c("Brazil", "United States", "Colombia", "India", "China","Russian Federation","Western Europe", "Japan", "Sub Sahara Africa")
base_4 <- base %>% filter(year >= 1980, country %in% paises)
graf_2 <- ggplot(base_4, aes(x=year, y=gdp_per_capita_us_100, color=country))+
  geom_line(size=1)+
  geom_path(size=0.4, alpha=0.8)+
  scale_color_brewer(palette="Set1")+
  theme_minimal()+
  xlab("Year")+
  ylab("GDP per capita (US=100)")+
  ggtitle("The Spread of Economic Growth Since 1980")+
  theme(plot.title=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))
graf_2