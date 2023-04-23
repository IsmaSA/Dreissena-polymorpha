#MGams models

#While I cant help you with (i), for (ii; the bubble plot) we need the coefficients, its direction (+ or -), and the p value for all (!) 8 (!) models:
1. rel abundance period 1 overall Europe
2. rel abundance period 2 overall Europe
3. occurrence period 1 overall Europe
4. occurrence period 2 overall Europe
5. rel abundance period 1 Central Europe
6. rel abundance period 2 Central Europe
7. occurrence period 1 Central Europe
8. occurrence period 2 Central Europe

#1.
setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial")
df<- read_excel("Spatial_Dp.xlsx")

head(df)
colnames(df)
str(df)
hist(df$Invaded)
Invaded <- gam(Invaded~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                 s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                 s(Temp,k=3)+ s(Precipitation,k=3)+
                 s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                 s(Slope,k=3)+s(Elevation,k=3)+
                 s(cat_190,k=3)+s(cat_210,k=3), 
               family = binomial, na.action = "na.fail", data = df)

summary(Invaded)

T1 <- visreg(Invaded,  scale='response', "cat_210", line.par = list(col = 'black'), plot=TRUE)

#2.
setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial")
df<- read_excel("Spatial_Dp2.xlsx")

colnames(df)
str(df)
hist(df$Invaded)
colnames(df)
Invaded <- gam(Invaded~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                 s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                 s(Temp,k=3)+ s(Precipitation,k=3)+
                 s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                 s(Slope,k=3)+s(Elevation,k=3)+
                 s(cat_190,k=3)+s(cat_210,k=3), 
               family = binomial, data = df)

summary(Invaded)

T1 <- visreg(Invaded,  scale='response', "cat_210", line.par = list(col = 'black'), plot=TRUE)


#3. 
df<- read_excel("Spatial_Dp.xlsx", sheet = "Ocurrence")
hist(df$Rel_abundance)
Ocurrence <- gam(Rel_abundance~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                   s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                   s(Temp,k=3)+ s(Precipitation,k=3)+
                   s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                   s(Slope,k=3)+s(Elevation,k=3)+
                   s(cat_190,k=3)+s(cat_210,k=3), 
                 family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "cat_210", line.par = list(col = 'black'), plot=TRUE)


#4.
df<- read_excel("Spatial_Dp2.xlsx", sheet = "Ocurrence")
  hist(df$Rel_abun)
Ocurrence <- gam(Rel_abun~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                   s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                   s(Temp,k=3)+ s(Precipitation,k=3)+
                   s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                   s(Slope,k=3)+s(Elevation,k=3)+
                   s(cat_190,k=3)+s(cat_210,k=3), 
                 family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "Strahler", line.par = list(col = 'black'), plot=TRUE)


#### Section using central Europe
## Germany netherlands Denmark 

setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial")

df<- read_excel("Spatial_Dp.xlsx")
unique(df$country)
df<-df %>% filter(!country=="Spain")
df<-df %>% filter(!country=="UK")
df<- df %>% filter(!country=="Latvia")
df<- df %>% filter(!country=="France")
df<- df %>% filter(!country=="Switzerland")

hist(df$Invaded)
Invaded <- gam(Invaded~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                 s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                 s(Temp,k=3)+ s(Precipitation,k=3)+
                 s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                 s(Slope,k=3)+s(Elevation,k=3)+
                 s(cat_190,k=3)+s(cat_210,k=3), 
               family = binomial, na.action = "na.fail", data = df)

summary(Invaded)

T1 <- visreg(Invaded,  scale='response', "cat_210", line.par = list(col = 'black'), plot=TRUE)


#6.
df<- read_excel("Spatial_Dp2.xlsx")
df<-df %>% filter(!country=="Spain")
df<-df %>% filter(!country=="UK")
df<- df %>% filter(!country=="Latvia")
df<- df %>% filter(!country=="France")
df<- df %>% filter(!country=="Switzerland")
head(df)
colnames(df)
str(df)
hist(df$Invaded)
colnames(df)
Invaded <- gam(Invaded~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                 s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                 s(Temp,k=3)+ s(Precipitation,k=3)+
                 s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                 s(Slope,k=3)+s(Elevation,k=3)+
                 s(cat_190,k=3)+s(cat_210,k=3), 
               family = binomial, data = df)

summary(Invaded)

T1 <- visreg(Invaded,  scale='response', "cat_210", line.par = list(col = 'black'), plot=TRUE)


#7.
df<- read_excel("Spatial_Dp.xlsx", sheet = "Ocurrence")
df<-df %>% filter(!country=="Spain")
df<-df %>% filter(!country=="UK")
df<- df %>% filter(!country=="Latvia")
df<- df %>% filter(!country=="France")
df<- df %>% filter(!country=="Switzerland")
unique(df$country)
hist(df$Rel_abundance)
Ocurrence <- gam(Rel_abundance~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                   s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                   s(Temp,k=3)+ s(Precipitation,k=3)+
                   s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                   s(Slope,k=3)+s(Elevation,k=3)+
                   s(cat_190,k=3)+s(cat_210,k=3), 
                 family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "Rich_alien", line.par = list(col = 'black'), plot=TRUE)


#8.
df<- read_excel("Spatial_Dp2.xlsx", sheet = "Ocurrence")
df<-df %>% filter(!country=="Spain")
df<-df %>% filter(!country=="UK")
df<- df %>% filter(!country=="Latvia")
df<- df %>% filter(!country=="France")
df<- df %>% filter(!country=="Switzerland")
hist(df$Rel_abun)
Ocurrence <- gam(Rel_abun~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                   s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                   s(Temp,k=3)+ s(Precipitation,k=3)+
                   s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                   s(Slope,k=3)+s(Elevation,k=3)+
                   s(cat_190,k=3)+s(cat_210,k=3), 
                 family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "cat_210", line.par = list(col = 'black'), plot=TRUE)




### Extract the relativa abundance
setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial")
df<- read_excel("Spatial_Dp.xlsx")
df<- read_excel("Spatial_Dp2.xlsx")

head(df)
df<- df %>% filter(Invaded=="1")

a<- read_excel("Global_dataset.xlsx")


df2<- semi_join(a,df, by="site_id")
head(df2)

df2$taxon[df2$taxon=="dreissenidae gen. sp."] <- "dreissena polymorpha"

df2<- df2 %>% subset(year > 2010 & year < 2016)

df_NO_dp<- df2 %>% group_by(site_id,year) %>% summarise(Total_abundance=sum(abundance))
df_NO_dp$Link <- str_c(df_NO_dp$site_id, sep = "_",df_NO_dp$year )

df3<- df2 %>% filter(taxon=="dreissena polymorpha")

df_dp<- df3 %>% group_by(site_id, year) %>% summarise(Abundace_DP=sum(abundance))
df_dp$Link <- str_c(df_dp$site_id, sep = "_",df_dp$year )


dfr<- right_join(df_dp,df_NO_dp, by="Link")
head(dfr)
dfr$Rel_abun <- dfr$Abundace_DP/dfr$Total_abundance

dfr<- dfr %>% group_by(site_id.x) %>% summarise(Rel_abun=mean(Rel_abun))
write.csv2(dfr,"Rel abundance2.csv")





#############################################################################################
##########################          REVIEWER COMMENTS          ##############################
#############################################################################################


# Ocurrence == Latitude, alien abundance, elevation, distance to the next barrier & native richness
# Rel abundance == Native richness and latitude



#1.Rel abundance
setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial")
df<- read_excel("Spatial_Dp.xlsx")

head(df)
colnames(df)
str(df)
hist(df$Invaded)
Invaded <- gam(Invaded~ 
                 s(Elevation)+ s(Latitude) + s(DistanceKm)+s(Rich_nat)+s(Abun_alien), 
               family = binomial, na.action = "na.fail", data = df)

summary(Invaded)

T1 <- visreg(Invaded,  scale='response', "Abun_alien", line.par = list(col = 'black'), plot=TRUE)
T2 <- visreg(Invaded,  scale='response', "Rich_nat", line.par = list(col = 'black'), plot=TRUE)



#2. Rel abundance
setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial")
df<- read_excel("Spatial_Dp2.xlsx")

colnames(df)
str(df)
hist(df$Invaded)
colnames(df)
Invaded2 <- gam(Invaded~ s(Elevation)+ s(Latitude) + s(DistanceKm)+s(Rich_nat)+s(Abun_alien), 
               family = binomial, data = df)

summary(Invaded2)

T3 <- visreg(Invaded2,  scale='response', "Abun_alien", line.par = list(col = 'black'), plot=TRUE)
T4 <- visreg(Invaded2,  scale='response', "Rich_nat", line.par = list(col = 'black'), plot=TRUE)






#3. Ocurrence
df<- read_excel("Spatial_Dp.xlsx", sheet = "Rel_abun")
hist(df$Rel_abundance)
Ocurrence <- gam(Rel_abundance~ 
                   s(Latitude) + s(Rich_nat), 
                 family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "Rich_nat", line.par = list(col = 'black'), plot=TRUE)
plot(Ocurrence, all.terms = TRUE, rug=TRUE, shade= TRUE, shade.col = "lightblue", seWithMean = TRUE, residuals=TRUE)






#4.
df<- read_excel("Spatial_Dp2.xlsx", sheet = "Rel_abun")
hist(df$Rel_abun)
Ocurrence <- gam(Rel_abun~ s(Latitude) + s(Rich_nat), 
                 family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "Rich_nat", line.par = list(col = 'black'), plot=TRUE)





#5.
#### Section using central Europe
## Germany netherlands Denmark 

setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial")

df<- read_excel("Spatial_Dp.xlsx")
unique(df$country)
df<-df %>% filter(!country=="Spain")
df<-df %>% filter(!country=="UK")
df<- df %>% filter(!country=="Latvia")
df<- df %>% filter(!country=="France")
df<- df %>% filter(!country=="Switzerland")

hist(df$Invaded)
Invaded <- gam(Invaded~ s(Elevation)+ s(Latitude) + s(DistanceKm)+s(Rich_nat)+s(Abun_alien), 
               family = binomial, na.action = "na.fail", data = df)

summary(Invaded)

T1 <- visreg(Invaded,  scale='response', "Rich_nat", line.par = list(col = 'black'), plot=TRUE)








#6.
df<- read_excel("Spatial_Dp2.xlsx")
df<-df %>% filter(!country=="Spain")
df<-df %>% filter(!country=="UK")
df<- df %>% filter(!country=="Latvia")
df<- df %>% filter(!country=="France")
df<- df %>% filter(!country=="Switzerland")
head(df)
colnames(df)
str(df)
hist(df$Invaded)
colnames(df)
Invaded <- gam(Invaded~ s(Elevation)+ s(Latitude) + s(DistanceKm)+s(Rich_nat)+s(Abun_alien), 
               family = binomial, data = df)

summary(Invaded)

T1 <- visreg(Invaded,  scale='response', "Abun_alien", line.par = list(col = 'black'), plot=TRUE)








#7.
df<- read_excel("Spatial_Dp.xlsx", sheet = "Rel_abun")
df<-df %>% filter(!country=="Spain")
df<-df %>% filter(!country=="UK")
df<- df %>% filter(!country=="Latvia")
df<- df %>% filter(!country=="France")
df<- df %>% filter(!country=="Switzerland")
unique(df$country)
hist(df$Rel_abundance)
Ocurrence <- gam(Rel_abundance~  s(Latitude) + s(Rich_nat), 
                 family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "Rich_nat", line.par = list(col = 'black'), plot=TRUE)






#8.
df<- read_excel("Spatial_Dp2.xlsx", sheet = "Rel_abun")
df<-df %>% filter(!country=="Spain")
df<-df %>% filter(!country=="UK")
df<- df %>% filter(!country=="Latvia")
df<- df %>% filter(!country=="France")
df<- df %>% filter(!country=="Switzerland")
hist(df$Rel_abun)
Ocurrence <- gam(Rel_abun~  s(Latitude) + s(Rich_nat), 
                 family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "Rich_nat", line.par = list(col = 'black'), plot=TRUE)









DATAFILE <- read.csv("heatmap.csv")
ggplot() +
  stat_density2d(data = DATAFILE, aes(x = Longitude, y = Latitude, fill = ..density..),
                 geom = 'tile', contour = F) 

map_bounds <- c(left = -2.8, bottom = 52.1059, right = -2.15, top = 52.4) 

coords.map1 <- get_stamenmap(map_bounds, zoom = 11, maptype = "terrain-background")
coords.map <- ggmap(coords.map1, extent="device", legend="none")
coords.map <- coords.map +   stat_density2d(data = DATAFILE, aes(x = Longitude, y = Latitude,
                            fill = ..density..), geom = 'tile', contour = F, alpha = .5)

coords.map <- coords.map +  scale_fill_gradient(low = "yellow", high = "red", na.value = NA,
                                                limits = c(0.1, 15000))

p1<- coords.map <- coords.map + theme_bw()    
p1

plot(newmap,col="white",
     bg="lightblue",border="grey40",
     xlim = c(-10, 30),
     ylim = c(35, 65),
     asp = 1)

DATAFILE<-DATAFILE[c(1:550),]
DATAFILE<-DATAFILE[!duplicated(DATAFILE$Longitude),]
points(DATAFILE$Longitude , DATAFILE$Latitude,  pch=19,cex=1, col=adjustcolor("purple",0.35))
points(data1$decimalLongitude , data1$decimalLatitude,  pch=19,cex=1, col=adjustcolor("purple",0.35))
sp <- occ_data(scientificName ="Schistocerca gregaria", hasCoordinate = TRUE,
               occurrenceStatus = "PRESENT", continent="europe",
               limit = 30000)
data1 <- sp[["data"]]






DATAFILE <- read.csv("heatmap.csv")
ggplot() +
  stat_density2d(data = DATAFILE, aes(x = Longitude, y = Latitude, fill = ..density..),
                 geom = 'tile', contour = F) #basic ggplot heatmap

map_bounds <- c(left = -2.75, bottom = 52.00, right = -2.18, top = 52.74000) #set the limits of the map
coords.map1 <- get_stamenmap(map_bounds, zoom = 13, maptype = "toner-background") # Here you can choose different map styles
# map styles: https://www.nceas.ucsb.edu/sites/default/files/2020-04/ggmapCheatsheet.pdf


coords.map <- ggmap(coords.map1, extent="device", legend="none")

coords.map <- coords.map +   stat_density2d(data = DATAFILE, aes(x = Longitude, y = Latitude,
                                                                 fill = ..density..), geom = 'tile', contour = F, alpha = .5)


coords.map <- coords.map + scale_fill_gradient(low = "yellow", high = "red", na.value = NA, limits = c(1, 15000))

p1<- coords.map <- coords.map + theme_bw()    
p1