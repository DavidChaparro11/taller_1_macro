
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
