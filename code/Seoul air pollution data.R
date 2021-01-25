  #library
library(data.table)
library(dplyr)
library(tidyr)
library(imputeTS)
library(stringr)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
  
  # handling raw data. 
dat = fread("Measurement_info.csv")
colnames(dat) = c("date", "station", "Item", "Average", "Status")

dat$Average[dat$Status != 0] =NA    #data$Status means 0: Normal, 1: Need for calibration, 
                                    #2: Abnormal, 4: Power cut off, 8: Under repair, 9: abnormal data.
                                    #So need to change !=0 to NA.

dat_item = fread("Measurement_item_info.csv")
colnames(dat_item) = c("Item", "name", "unit", "Good_blue", "Normal_green", "Bad_yellow", "very_bad_red")

dat1 = 
  dat %>%
  spread(Item, Average)

colnames(dat1)[4:9] = c("SO2", "NO2", "CO", "O3", "PM10", "PM25")
str(dat1)

  # same NA, but location is different
mapply(summary, dat1[,4:9])
summary(dat1)

  # Missing value(NA) imputation
dat1[[4]] = na_kalman(dat1[[4]], model = "StructTS")
dat1[[5]] = na_kalman(dat1[[5]], model = "StructTS")
dat1[[6]] = na_kalman(dat1[[6]], model = "StructTS")
dat1[[7]] = na_kalman(dat1[[7]], model = "StructTS")
dat1[[8]] = na_kalman(dat1[[8]], model = "StructTS")
dat1[[9]] = na_kalman(dat1[[9]], model = "StructTS")

  # Handle for visualizatioin.
dat1$date1 = as.Date(substr(dat1$date, 1, 10))

dat_day = 
  dat1 %>%
  group_by(date1, station) %>%
  summarise(SO2_m = mean(SO2),
            NO2_m = mean(NO2),
            CO_m = mean(CO),
            O3_m = mean(O3),
            PM10_m = mean(PM10),
            PM25_m = mean(PM25))

dat_day$month = month(dat_day$date1)
dat_day$year = year(dat_day$date1)
dat_day1 = 
  dat_day %>%
  dplyr::select(date1, station, PM10_m, PM25_m, month) %>%
  filter(month %in% c(4,7,10,1)) %>%
  group_by(month, station) %>%
  summarise(min10 = min(PM10_m),
            min25 = min(PM25_m),
            max10 = max(PM10_m),
            max25 = max(PM25_m))

summary(dat_day1)
dat_station = fread("Measurement_station_info.csv")
dat_station1 = dat_station[,c(1,4,5)]
iid = c(11110,11140,11170,11200,11215,11230,11260,11290,11305,
        11320,11350,11380,11410,11440,11470,11500,11530,11545,
        11560,11590,11620,11650,11680,11710,11740)
dat_station1$iid = iid

colnames(dat_station1)[1] = "station"

dat_day2_station = 
  dat_day1 %>%
  left_join(dat_station1, by="station")

#Visualization with January, April, July, October
map <- shapefile("TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]

  ###Jan
seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==1) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=min10, alpha=0.5), color="black")+
  ggtitle("Jan, min, PM10")+
  scale_fill_gradient(low = "yellow", high = "red")


seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==1) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=max10, alpha=0.5), color="black")+
  ggtitle("Jan, max, PM10")+
  scale_fill_gradient(low = "yellow", high = "red")

seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==1) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=min25, alpha=0.5), color="black")+
  ggtitle("Jan, min, PM25")+
  scale_fill_gradient(low = "yellow", high = "red")

seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==1) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=max25, alpha=0.5), color="black")+
  ggtitle("Jan, max, PM25")+
  scale_fill_gradient(low = "yellow", high = "red")

  ###Apr
seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==4) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=min10, alpha=0.5), color="black")+
  ggtitle("Apr, min, PM10")+
  scale_fill_gradient(low = "yellow", high = "red")


seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==4) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=max10, alpha=0.5), color="black")+
  ggtitle("Apr, max, PM10")+
  scale_fill_gradient(low = "yellow", high = "red")

seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==4) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=min25, alpha=0.5), color="black")+
  ggtitle("Apr, min, PM25")+
  scale_fill_gradient(low = "yellow", high = "red")

seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==4) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=max25, alpha=0.5), color="black")+
  ggtitle("Apr, max, PM25")+
  scale_fill_gradient(low = "yellow", high = "red")

  ###Jul
seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==7) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=min10, alpha=0.5), color="black")+
  ggtitle("Jul, min, PM10")+
  scale_fill_gradient(low = "yellow", high = "red")


seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==7) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=max10, alpha=0.5), color="black")+
  ggtitle("Jul, max, PM10")+
  scale_fill_gradient(low = "yellow", high = "red")

seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==7) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=min25, alpha=0.5), color="black")+
  ggtitle("Jul, min, PM25")+
  scale_fill_gradient(low = "yellow", high = "red")

seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==7) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=max25, alpha=0.5), color="black")+
  ggtitle("Jul, max, PM25")+
  scale_fill_gradient(low = "yellow", high = "red")

  ###Oct
seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==10) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=min10, alpha=0.5), color="black")+
  ggtitle("Oct, min, PM10")+
  scale_fill_gradient(low = "yellow", high = "red")


seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==10) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=max10, alpha=0.5), color="black")+
  ggtitle("Oct, max, PM10")+
  scale_fill_gradient(low = "yellow", high = "red")

seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==10) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=min25, alpha=0.5), color="black")+
  ggtitle("Oct, min, PM25")+
  scale_fill_gradient(low = "yellow", high = "red")

seoul_map %>%
  left_join(dat_day2_station %>% rename(id=iid), by="id") %>%
  filter(month==10) %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group=group, fill=max25, alpha=0.5), color="black")+
  ggtitle("Oct, max, PM25")+
  scale_fill_gradient(low = "yellow", high = "red")

  #Forecasting
library(bsts)
