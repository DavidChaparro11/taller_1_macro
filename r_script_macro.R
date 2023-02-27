rm(list=ls())
require(pacman)
p_load(haven, ggplot2, dplyr, tidyverse)
base <- read_dta("PWT90.dta")
base <- base %>% mutate(GDP_per_capita = rgdpe/pop, gdp_per_capita_us_100=GDP_per_capita/100)
base_colombia <- base %>% filter(country== "Colombia")
base_colombia <- base_colombia %>% mutate(ln_gdp = log(rgdpe))

##grafica_1
grafica <- ggplot(base_colombia, aes(year,ln_gdp)) +
  geom_point() +
  geom_smooth()+
  ggtitle("Logaritmo natural del PIB Colombia de 1950-2014")+
  ylab("Logaritmo natural del PIB")+
  scale_x_continuous(breaks=seq(1950,2014, by=10))
grafica
##grafica 2
##africa
africa_subsahariana <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Camerún", "República Centroafricana", "Chad", "Comoras", "Congo", "Costa de Marfil", "Yibuti", "República Democrática del Congo", "Guinea Ecuatorial", "Eritrea", "Etiopía", "Gabón", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenia", "Lesoto", "Liberia", "Madagascar", "Malawi", "Malí", "Mauritania", "Mauricio", "Mozambique", "Namibia", "Níger", "Nigeria", "Ruanda", "Santo Tomé y Príncipe", "Senegal", "Seychelles", "Sierra Leona", "Somalia", "Sudáfrica", "Sudán del Sur", "Sudán", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabue")
africa_subsahariana_pib <- base %>% filter(country %in% africa_subsahariana)
africa_subsahariana_promedio <- africa_subsahariana_pib %>% group_by(year) %>% summarize(gdp_per_capita_us_100 = mean(gdp_per_capita_us_100))
africa_subsahariana_promedio <- africa_subsahariana_promedio %>% mutate(country="Sub-Saharan Africa")
africa_subsahariana_promedio <-  africa_subsahariana_promedio %>% select(country, year, gdp_per_capita_us_100)
base_general <- base %>% select(country, year, gdp_per_capita_us_100)
base_Africa <- rbind(base_general,africa_subsahariana_promedio)
##Western Europe
west_europe <- c("Austria", "Belgium", "France", "Germany", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "Switzerland", "United Kingdom")
west_europe <- base %>% filter(country %in% west_europe)
west_europe_p <- west_europe %>% group_by(year) %>% summarize(gdp_per_capita_us_100 = mean(gdp_per_capita_us_100))
west_europe_p <- west_europe_p %>% mutate(country = "Western Europe") %>% select(country, year, gdp_per_capita_us_100)
base_general <- rbind(base_Africa, west_europe_p)
paises <- c("Brazil", "United States", "Colombia", "India", "China","Russian Federation","Western Europe", "Japan", "Sub-Saharan Africa")
base_general <- base_general %>% filter(year >= 1980, country %in% paises)
paises <- c("Brazil", "United States", "Colombia", "India", "China","Russian Federation","Western Europe", "Japan", "Sub-Saharan Africa")
base_general <- base_general %>% filter(year >= 1980, country %in% paises)
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
