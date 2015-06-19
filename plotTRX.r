library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
#Eye(breaks, labels)
TablaB100 <- read_excel("TableauBord_CHILI040615.xlsx", sheet = 1, skip = 4)
TablaB100$Fecha <- as.Date(TablaB100$Fecha)
f.str <- strptime(TablaB100$Fecha, "%Y-%m-%d")
TablaB100$Year <- f.str$year+1900

names(TablaB100) <- c("Fecha", "Primo_TRX", "TRX_Bus", "TRX_Metro", "KM_Bus", "KM_Metro", 
                      "TRX_Totales",  "Km_Totales", "Ingresos", "Ingresos/TRX_Totales",
                      "Ingresos/Primo_TRX", "Tarifa_media_publica", "Tarifa_media_publica", "Año")

TablaB100_actual <- subset(TablaB100, TablaB100$Año != 'NA' & TablaB100$Año < '2015' & 
                             TablaB100$Año >= '2012')

ggplot(TablaB100_actual, aes(x = Fecha, y = TRX_Totales, colour = Año)) + geom_line(size = 1.5) + 
  scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%m-%y")) + 
  xlab("Bimensual") + ylab("TRX (B100)") + stat_smooth(method = "lm", alpha = 0.5, colour = "black") + 
  theme_bw() + facet_grid(Año ~  ., margins = TRUE) + ggtitle("TRX\n B100")
 
eventos <- read_excel("TableauBord_CHILI040615.xlsx", sheet = 22, skip = 3)[, 1:6] %>% 
  filter(Fecha >= '2015-01-01')

gg <- ggplot(TablaB100_actual, aes(x = Fecha, y = TRX_Totales, colour = Año)) + geom_line(size = 1.5)
gg + scale_x_date(breaks = "2 month", minor_breaks = "1 month", labels=date_format("%m-%y"))
gg + xlab("Bimensual") + ylab("TRX (B100)") 
gg + stat_smooth()
gg2 <- gg + theme_bw() + facet_grid(Año ~  ., margins = TRUE) + ggtitle("TRX\n B100")
gg2
