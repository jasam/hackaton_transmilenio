get_dataframe <- function(dir, cols) {
  library(readr)
  directory <- dir
  setwd(directory)
  files <- list.files(pattern = "\\.csv$")
  mega_data <- data.frame()
  for (i in files) {
    print(file.path(directory,i))
    data <- read_csv(i)
    colnames(data) <- cols
    data <- subset(data, select = c(ESTACION, CANTIDAD, S, FECHAINICIAL, FECHAFINAL))
    mega_data = rbind(mega_data, data)
  }
  return(mega_data)
}

#may 2011
dir <- "F:/data/transmilenio/Datos/DATOS TRANSMILENIO/F1 Y F2 2011/MAYO"
cols <- c("FILA","CODIGO","ESTACION","NUMEROMOLINETE","FECHAINICIAL","FECHAFINAL","S","CANTIDAD","IDARTIFICIALTARIFA","DESCRIPCION")
data_may_2011 <- get_dataframe(dir, cols)
data_may_2011$mes_ano <- "may_2011"
#may 2012
dir <- "F:/data/transmilenio/Datos/DATOS TRANSMILENIO/F1 Y F2 2012/MAYO"
cols <- c("FILA","CODIGO","ESTACION","NUMEROMOLINETE","FECHAINICIAL","FECHAFINAL","S","CANTIDAD")
data_may_2012 <- get_dataframe(dir, cols)
data_may_2012$mes_ano <- "may_2012"
#may 2013
dir <- "F:/data/transmilenio/Datos/DATOS TRANSMILENIO/F1 Y F2 2013/MAYO"
cols <- c("FILA","CODIGO","ESTACION","NUMEROMOLINETE","FECHAINICIAL","FECHAFINAL","S","CANTIDAD")
data_may_2013 <- get_dataframe(dir, cols)
data_may_2013$mes_ano <- "may_2013"
#may 2014
dir <- "F:/data/transmilenio/Datos/DATOS TRANSMILENIO/F1 Y F2 2014/MAYO"
cols <- c("FILA","CODIGO","ESTACION","NUMEROMOLINETE","FECHAINICIAL","FECHAFINAL","S","CANTIDAD")
data_may_2014 <- get_dataframe(dir, cols)
data_may_2014$mes_ano <- "may_2014"
#save data
setwd("F:/data/transmilenio/Datos")
save(data_may_2014, file="data_may_2014.rda")
load("data_may_2014.rda")

#merge data
full_data <- rbind(data_may_2011,
                   data_may_2012,
                   data_may_2013,
                   data_may_2014)

#bar plot
library(dplyr)
library(ggplot2)
colnames(full_data)
colnames(full_data) <- c("estacion", "cantidad", "ent_sal", "mes_ano", "fechainicial", "fechafinal")
data_by_year <- full_data %>% 
                group_by(mes_ano, ent_sal) %>% 
                summarise(cantidad = sum(cantidad))

#save data
setwd("F:/data/transmilenio/Datos")
save(data_by_year, file="data_by_year.rda")
load("data_by_year.rda")

data_by_year$cantidad <- data_by_year$cantidad / 1000000
data_by_year$cantidad <- data_by_year$cantidad / 1000000

#multiply by -1, colado
sin_pago <- transform(data_by_year, cantidad = ifelse(ent_sal == "E", cantidad * -1, cantidad)) %>% 
  group_by(mes_ano) %>%
  summarise(cantidad = sum(cantidad)) %>%
  mutate(categoria = "Sin pago") 

#Category
con_pago <- subset(data_by_year, ent_sal == "E", select = c(mes_ano, cantidad)) %>%
  mutate(categoria = "Con pago") 

#final dataset           
users <- rbind(sin_pago, con_pago)

#acum
users <- users %>%
  mutate(acumulado = cumsum(users$cantidad))

#bar plot
title <- "Cantidad de usuarios con pago y sin pago años 2011 - 2014 mes mayo fase I y II de transmilenio"
x_lab <- "Mes-año"
y_lab <- "Cantidad viajes (millones)"
ggplot(data=users, aes(x=mes_ano, y=cantidad, fill=categoria)) + 
  geom_bar(stat = "identity", color="black") +
  geom_text(aes(label=signif(cantidad,4)),vjust=1.5, colour="white") +
  scale_y_continuous(limits=c(0,60)) +
  labs(title = title, x = x_lab, y = y_lab) +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, face="bold.italic"))

# Pasajes perdidos en dinero
((54.65511* 1000000) - (49.47844 * 1000000)) * 1800

# porcentaje de colados
(((54.65511* 1000000) - (49.47844 * 1000000)) / (54.65511* 1000000)) * 100

# histogram by hours
setwd("F:/data/transmilenio/Datos")
save(full_data, file="full_data.rda")
load("full_data.rda")
colnames(full_data)
head(full_data, 10)
unique(full_data$FECHAINICIAL)
data_may_2011$FECHAINICIAL
data_may_2012$FECHAINICIAL
data_may_2013$FECHAINICIAL
data_may_2014$FECHAINICIAL

unique(data_may_2014$ESTACION)

library(lubridate)
hour(dmy_hms(unique(full_data$FECHAINICIAL)))
minute(dmy_hms(unique(full_data$FECHAINICIAL)))
unique(data_may_2014$FECHAFINAL)

#summarize by hour - minute
data_by_hour_minute <- data_may_2014 %>% 
                       group_by(hour_min = hour(dmy_hms(FECHAINICIAL)), S) %>% 
                       summarise(cantidad = sum(CANTIDAD))

data_by_hour_minute <- transform(data_by_hour_minute, cantidad = ifelse(S == "E", cantidad * -1, cantidad))

data_by_hour_minute <- data_by_hour_minute %>% 
                       group_by(hour_min) %>% 
                       summarise(cantidad = sum(cantidad))

colnames(data_by_hour_minute)

title <- "Cantidad de usuarios colados por hora mayo 2014"
x_lab <- "Hora 24"
y_lab <- "Cantidad de colados (millones)"
ggplot(data = data_by_hour_minute, aes(x=hour_min, y=cantidad / 1000000)) +
  geom_line() + 
  scale_x_continuous(limits=c(0,23), breaks=c(1:24)) +
  labs(title = title, x = x_lab, y = y_lab) +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, face="bold.italic")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  geom_hline(yintercept=0, color="red")


#by days

str(data_may_2014)

data_may_2014$dia <- wday(dmy_hms(data_may_2014$fechainicial), abbr = FALSE, label = TRUE)
data_may_2014$hora <- hour(dmy_hms(data_may_2014$FECHAINICIAL))

data_by_hour_day <- data_may_2014 %>% 
                    group_by(hora, dia, S) %>% 
                    summarise(cantidad = sum(CANTIDAD))

data_by_hour_day <- transform(data_by_hour_day, cantidad = ifelse(S == "E", cantidad * -1, cantidad))

data_by_hour_day <- data_by_hour_day %>% 
                    group_by(dia, hora) %>% 
                    summarise(cantidad = sum(cantidad))

#plot with grid
title <- "Cantidad de usuarios colados por hora por días mayo 2014"
x_lab <- "Hora 24"
y_lab <- "Cantidad de colados (miles)"
ggplot(data = data_by_hour_day, aes(x=hora, y=cantidad/1000)) +
  geom_line() + 
  scale_x_continuous(limits=c(0,23), breaks=c(1:24)) +
  labs(title = title, x = x_lab, y = y_lab) +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, face="bold.italic")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  geom_hline(yintercept=0, color="red") +
  facet_grid(dia ~ ., scales="free_y", space="free_y") 


#plot with grid
library(ggplot2)
title <- "Cantidad de usuarios colados por hora por días mayo 2014"
x_lab <- "Hora 24"
y_lab <- "Cantidad de colados (miles)"
ggplot(data = data_by_hour_day, aes(x=hora, y=cantidad/1000)) +
  geom_line() + 
  scale_x_continuous(limits=c(0,23), breaks=c(1:24)) +
  labs(title = title, x = x_lab, y = y_lab) +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, face="bold.italic")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  geom_hline(yintercept=0, color="red") +
  facet_wrap(~ dia)

#plot polar
library(plyr)
data_by_hour_day <- ddply(data_by_hour_day, .(dia), transform, orden=seq_along(hora))

title <- "Cantidad de usuarios colados por hora por días mayo 2014"
x_lab <- "Hora 24"
y_lab <- "Cantidad de colados (miles)"
ggplot(data_by_hour_day, aes(y = cantidad / 1000, x = reorder(hora, orden), group = dia, colour = dia)) +
      coord_polar() + 
      geom_point() + 
      geom_path() + 
      labs(x = NULL) +
      labs(title = title, x = x_lab, y = y_lab) 