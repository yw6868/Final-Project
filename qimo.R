rm(list = ls())
climate_change <- read.csv(file = "climate_change.csv",TRUE, sep = ",", stringsAsFactors = FALSE)
str(climate_change)
dim(climate_change)
library(dplyr)
library(ggplot2)
library(plotly)
library("RColorBrewer")
#Handling Missing Data
new_climate_change <- na.omit(climate_change)
dim(new_climate_change)
#Exploratory Data Anlysis
#Aims to find patterns and relationships in data
climate_change_chart <- ggplot(climate_change, aes(x = Year, y = Temp, fill = CO2)) +
  xlab("Year") +
  ylab("Temperature") +
  theme_minimal(base_size = 14)
barplot <- climate_change_chart +
  geom_bar( position = "dodge", stat = "identity",color= "white")
ggplotly(barplot)
library(lubridate)
# adding Year-Month variable as date
climate_change_ymd <- climate_change %>%
  mutate(year_month = ymd(paste(climate_change$Year, climate_change$Month, truncated = 1)))
L1 <- ggplot(climate_change_ymd, aes(year_month, Temp)) +
  geom_line() +
  geom_smooth(se=FALSE, linetype = "dotted") +
  labs(title = "Temperature (1983-2008)",
       x = "Year",
       y = "Temperature") +
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(L1)
# Temperature Month-wise plot for each year in data
Tg <- ggplot(climate_change, aes(as.factor(Month), Temp)) +
  geom_point(aes(color = as.factor(Year))) +
  geom_line(aes(group = as.factor(Year),
                color = as.factor(Year)),
            alpha = 0.7) +
  labs(title = 'Temperature by month') +
  xlab("Months") +
  ylab("Temperature") +
  theme(axis.text.x = element_text(size = 6,angle = 90,hjust = 0.5, vjust = 0.5))
# theme(legend.position = "none")
ggplotly(Tg)
#Temperature-density distribution
library(ggridges)
ggplot(climate_change, aes(x = Temp, y = as.factor(Year))) +
  geom_density_ridges_gradient(aes(fill = ..x..),
                               scale = 3, size = 0.3, alpha = 0.5) +
  scale_fill_gradientn(colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
                       name = "Temp") +
  labs(title = 'Temperature density') +
  theme(legend.position = c(0.9,0.2)) +
  xlab("Temperature") +
  ylab("Year")+theme_minimal(base_size = 10)
#Variations of CO2, N2O, CH4 and MEI by year
library(ggpubr)
#par(mfrow=c(2,2))
scat_plot1 <-  ggplot(climate_change_ymd, aes(year_month, CO2))+geom_line(colour="blueviolet")+geom_smooth(method = "lm")+ggtitle("Carbon Dioxide")
scat_plot2<-  ggplot(climate_change_ymd, aes(year_month, N2O))+geom_line()+geom_smooth(method = "lm")+ggtitle("Nitrous Oxide")
scat_plot3<-  ggplot(climate_change_ymd, aes(year_month, CH4))+geom_line(colour="springgreen4")+geom_smooth(method = "lm")+ggtitle("Methane")
scat_plot4 <-  ggplot(climate_change_ymd, aes(year_month, MEI))+geom_line(colour="mediumorchid4")+ggtitle("MEI")
grapgh_arrange<-ggarrange(scat_plot1, scat_plot2, scat_plot3, scat_plot4 + rremove("x.text"),
                          labels = c("A", "B", "C", "D"),
                          ncol = 2, nrow = 2)
annotate_figure(grapgh_arrange,
                top = text_grob("Shows of CO_2, N2O, CH4 and MEI by year", color = "red", face = "bold", size = 14)
)
#Variations of CFC11, CFC12, Total Solar Irradiate (TSI) and Aerosols by year
scat_plot5 <-  ggplot(climate_change_ymd, aes(year_month, CFC.11))+geom_line(colour="blue")+ggtitle("CFC-11") +
  ylab("CFC-11")
scat_plot6<-  ggplot(climate_change_ymd, aes(year_month, CFC.12))+geom_line(colour="green")+ggtitle("CFC-12") +
  ylab("CFC-12")
scat_plot7<-  ggplot(climate_change_ymd, aes(year_month, TSI))+geom_line(colour="red")+ggtitle("TSI")
scat_plot8 <-  ggplot(climate_change_ymd, aes(year_month, Aerosols))+geom_line(colour="magenta")+ggtitle("Aerosols")
grapgh_arrange<-ggarrange(scat_plot5, scat_plot6, scat_plot7, scat_plot8+ rremove("x.text"),
                          labels = c("A", "B", "C", "D"),
                          ncol = 2, nrow = 2)
annotate_figure(grapgh_arrange,
                top = text_grob("Shows of CFC-11,CFC-12, TSI and Aerosols by year", color = "blue", face = "bold", size = 14)
)


fit=lm(Temp~CO2,data=climate_change)
summary(fit)
library(ggiraph)
library(ggiraphExtra)
ggPredict(fit,se=TRUE,interactive=TRUE)

climate_multreg <- filter(climate_change, Month == "1" | Month == "2")
fit1=lm(Temp~Year+Month,data=climate_multreg)
summary(fit1)
ggPredict(fit1,se=TRUE, interactive=TRUE)



#Animation
# 选择镜像
options(repos=structure(c(CRAN="https://cran.cnr.berkeley.edu/")))
# 安装pacman包
install.packages("installr")
library(installr)
library(stringr)
library(ggplot2)
library(gganimate)
p <- ggplot(climate_change, aes(x = Year, y = Temp, color = Month, group = Month)) +
  geom_path() +
  geom_point() +
  facet_wrap(~ Month) +
  theme(legend.position = 'none') +
  labs(title = 'Temperature Variation, Year: {frame_along}') +
  transition_reveal(along = Year) +
  ease_aes('linear')
p
anim_save(p)

library(tidyverse)
co2 <-
  read_table("https://raw.githubusercontent.com/espm-157/climate-template/master/assignment/co2_mm_mlo.txt",
             comment="#",
             col_names = c("year", "month", "decimal_date", "average",
                           "interpolated", "trend", "days"),
             na = c("-1", "-99.99"))
co2
ggplot(co2, aes(x = decimal_date, y = trend)) + geom_line()


temperature <- read_table(
  "http://climate.nasa.gov/system/internal_resources/details/original/647_Global_Temperature_Data_File.txt",
  skip = 5,
  col_names = c("year","temp","lowess5"))
temperature
temperature %>%
  ggplot(aes(x = year, y = temp)) + geom_line()


ice <- read_csv("http://climate.nasa.gov/system/internal_resources/details/original/499_GRN_ANT_mass_changes.csv",skip = 10,
                col_names = c("time","greenland_mass","antarctica_mass"))
ice
ice %>%
  ggplot(aes(x = time)) +
  geom_line(aes(y = greenland_mass), col="blue") +
  geom_line(aes(y = antarctica_mass), col = "red")
sea_level <- read_table("http://climate.nasa.gov/system/internal_resources/details/original/121_Global_Sea_Level_Data_File.txt",
                        skip = 50,
                        col_names = c("altimeter_type","merged_file_cycle","number_of_observations","number_of_weighted_observations",
                                      "gmsl_variation_adjustment_not_applied","standard_deviation_of_gmsl_variation_no_adjustment",
                                      "gmsl_variation_adjustment_applied","standard_deviation_of_gmsl_adjustment",
                                      "smoothed_gmsl_variation","smoothed_gmsl_variation_removed_signal"))
sea_level
sea_level %>%
  ggplot(aes(x = number_of_observations)) +
  geom_line(aes(y = smoothed_gmsl_variation),col= "blue") +
  geom_line(aes(y = smoothed_gmsl_variation_removed_signal),col= "red")
