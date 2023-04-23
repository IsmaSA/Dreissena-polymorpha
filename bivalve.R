### Bivalvia proyect

setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves")

df <- read_excel("Bivalves.xlsx")
head(df)

unique(df$site_id) # 192 time series

unique(df$taxon)
df1<- df %>% filter(taxon== "sinanodonta woodiana")
df2<- df %>% filter(taxon== "musculium transversum")
df3<- df %>% filter(taxon== "dreissena polymorpha")
df4<- df %>% filter(taxon== "dreissena rostriformis")

df3<- df3[!duplicated(df3$site_id),]
df3<- df3 %>% group_by(country) %>% summarise(entradas =n())
df3$Prop <- df3$entradas * 100/178
df3
sum(df3$entradas)
#####  Europe Map
unique(df$country)
ddf = read.table(text="
country value
'France' 94
'Spain' 94
'Germany' 94
'United Kingdom' 94
'Hungary' 94
'Denmark'94
'Latvia'94
'Switzerland'94
'Netherlands' 94", header=TRUE)

pal <- colorRampPalette(brewer.pal(7, 'Reds'))(length(ddf$value))
pal <- pal[with(ddf, findInterval(value, sort(unique(value))))]

newmap <- getMap(resolution = "high")
col <- rep("white", length(newmap@data$NAME))
col[match(ddf$country, newmap@data$NAME)] <- pal
par(mar=c(0,0,0,0))
plot(newmap,col=col,
     bg="lightblue",border="grey40",
     xlim = c(8, 10),
     ylim = c(33, 64),
     asp = 1)

df1<- df1[!duplicated(df1$site_id),]
df2<- df2[!duplicated(df2$site_id),]
df3<- df3[!duplicated(df3$site_id),]
df4<- df4[!duplicated(df4$site_id),]

points(df1$Longitude , df1$Latitude,  pch=19,cex=1, col=adjustcolor("chartreuse4",0.35))
points(df2$Longitude , df2$Latitude,  pch=19,cex=1, col=adjustcolor("black",0.35))
points(df3$Longitude , df3$Latitude,  pch=19,cex=1, col=adjustcolor("darkorchid4",0.35))
points(df4$Longitude , df4$Latitude,  pch=19,cex=1, col=adjustcolor("yellow",0.35))


setwd("C:/Users/isma-/OneDrive/Escritorio/Killer shrimp/QgisMap")
riversData <- readOGR("Europe_Hydrography.shp") # load the shapefile
plot(riversData, col= "steelblue2", add=T) 


#Check proportions
df1<- read_excel("Global_dataset.xlsx")
a <- semi_join(df1, df, by = "site_id")
unique(a$site_id)

b<- a%>% group_by(site_id,year) %>% summarise(Abundance= sum(abundance))
write.xlsx(b,"abundance.xlsx")

### ---- Meta-regression Dreissena polymorpha

head(df3)
unique(df3$site_id)

df3 <- df3 %>% filter(!site_id =="100000233")
df3 <- df3 %>% filter(!site_id =="100000309")
df3 <- df3 %>% filter(!site_id =="120000002")
df3 <- df3 %>% filter(!site_id =="107000201")




xy.list <- split(df3$abundance, df3$site_id) 
xy.list <- xy.list[lengths(xy.list) >= 3]  # 132
length(xy.list)

MK <-as.data.frame(do.call(rbind,lapply(xy.list[1:132],function(x)unlist(My.mmkh(x))))) #96 are time series,
head(MK)

write.csv2(MK, "Slopes.csv")


#Meta-regression with all-covariates
setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves")

df <- read_excel("Bivalves.xlsx", sheet = "Hoja4")
head(df)
df<- df[!duplicated(df$site_id), ]
colnames(df)
unique(df$country)
df$const = 1

df1 <- df %>% filter(country =="France")
df2 <- df %>% filter(country =="Germany")
df3 <- df %>% filter(country =="UK")
df4 <- df %>% filter(country =="Hungary")
df5 <- df %>% filter(country =="Netherlands")
df6 <- df %>% filter(country =="Switzerland")
df7 <- df %>% filter(country =="Latvia")


Dp1 <- rma.mv(S, Variance, random = ~ (Longitude + Latitude) | const, struct = "SPGAU",
              method = "REML", data = df)

Dp1 <- rma.mv(S, Variance, random = ~1| site_id, method = "REML", data = df4)


Dp1

W <- diag(1/df$Variance)
X <- model.matrix(Dp1)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
I2 <- 100 * Dp1$sigma2 / (sum(Dp1$sigma2) + (Dp1$k-Dp1$p)/sum(diag(P)))
I2

regplot(Dp1, refline=0,at=seq(-80,30,by=10), xlim = c(1970,2020),
        digits=1, las=1, bty="l",
         labsize=0.9)

####Meta-regression ocurrences 
head(df3)
unique(df3$country)

df4<- df3 %>% filter(country=="France")
df5<- df3 %>% filter(country=="Spain")
df6<- df3 %>% filter(country=="Denmark")
df7<- df3 %>% filter(country=="Germany")
df8<- df3 %>% filter(country=="UK")
df9<- df3 %>% filter(country=="Hungary")
df10<- df3 %>% filter(country=="Netherlands")
df11<- df3 %>% filter(country=="Switzerland")
df12<- df3 %>% filter(country=="Latvia")


a<- df12 %>% group_by(year) %>% summarise(Ocurrence=n())
head(a)

TS = ts(a$Ocurrence,
        frequency=1,
        start= 1990)

res <-  MannKendall(TS)
print(res)
summary(res)

setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves")

df <- read_excel("Bivalves.xlsx", sheet = "Meta-Ocurrence")
Dp1 <- rma.mv(S, Variance, method = "REML", data = df)
Dp1


## GAM model

hist(df3$Proportion)
colnames(df3)
df3$country <- as.factor(df3$country)

res<- gam(Proportion ~  s(Longitude, Latitude, bs="sos")+ s(year, bs="cr")+ s(country, bs = 're')+
            s(Temperature)+s(Precipitation)+s(DistanceKm)+s(Elevation)+s(Slope),
             family = nb(link = log), data=df3)
summary(res)  

T1 <- visreg(res,  scale='response', "year", line.par = list(col = 'black'), plot=TRUE)



# Plot meta-regression

Dp1 <- rma.mv(S, Variance, random = ~1| site_id, method = "REML", data = df)

model_results <- orchaRd::mod_results(Dp1, mod = "1", group = "site_id", data = df6)
print(model_results)

orchard_plot(Dp1, mod = "1",group = "site_id",data = df6,
             xlab = "Standardised mean difference", transfm = "none")


##Plot GAM model 
df3
unique(df3$country)
df31 <- df3 %>% filter(country=="France")
df32 <- df3 %>% filter(country=="Spain")
df33 <- df3 %>% filter(country=="Denmark")
df34 <- df3 %>% filter(country=="Germany")
df35 <- df3 %>% filter(country=="UK")
df36 <- df3 %>% filter(country=="Hungary")
df37 <- df3 %>% filter(country=="Netherlands")
df38 <- df3 %>% filter(country=="Switzerland")
df39 <- df3 %>% filter(country=="Latvia")


res9<- gam(Proportion ~   s(year, bs="cr")+
              s(Temperature)+ s(Precipitation)+s(Elevation)+s(Slope),
          family = nb(link = log), data=df39)
summary(res9)
res9<- gam(Proportion ~   s(year, bs="cr", k=3), #Spain, #Denmark #Swtizerland
        data=df39)

T2 <- visreg(res1,  scale='response', "year", line.par = list(col = 'blue'), plot=TRUE)
T3 <- visreg(res2,  scale='response', "year", line.par = list(col = 'blue'), plot=TRUE)
T4 <- visreg(res3,  scale='response', "year", line.par = list(col = 'blue'), plot=TRUE)
T5 <- visreg(res4,  scale='response', "year", line.par = list(col = 'blue'), plot=TRUE)
T6 <- visreg(res5,  scale='response', "year", line.par = list(col = 'blue'), plot=TRUE)
T7 <- visreg(res6,  scale='response', "year", line.par = list(col = 'blue'), plot=TRUE)
T8 <- visreg(res7,  scale='response', "year", line.par = list(col = 'blue'), plot=TRUE)
T9 <- visreg(res8,  scale='response', "year", line.par = list(col = 'blue'), plot=TRUE)
T10 <- visreg(res9,  scale='response', "year", line.par = list(col = 'blue'), plot=TRUE)

dplyr::bind_rows(
    dplyr::mutate(T1$fit, plt = "all"),
    dplyr::mutate(T2$fit, plt = "France"),
    dplyr::mutate(T3$fit, plt = "Spain"),
    dplyr::mutate(T4$fit, plt = "Denmark"),
    dplyr::mutate(T5$fit, plt = "Germany"),
    dplyr::mutate(T6$fit, plt = "Uk"),
    dplyr::mutate(T7$fit, plt = "Hungary"),
    dplyr::mutate(T8$fit, plt = "Netherlands"),
    dplyr::mutate(T9$fit, plt = "Switzerland"),
    dplyr::mutate(T10$fit, plt = "Latvia"),
    
) -> fits
ggplot() +
    geom_ribbon(
        data = fits,
        aes(year, ymin=visregLwr, ymax=visregUpr, group=plt), fill="gray90"
    ) +
    geom_line(data = fits, aes(year, visregFit, group=plt, color=plt),size=1.2) +
    scale_size_manual(values = c(1,1,1))+ 
    theme_classic()
dev.off()



##### Ocurrence model
setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves")
df <- read_excel("Bivalves.xlsx")
df<- df %>% filter(taxon=="dreissena polymorpha")
unique(df$country)
df<- df %>% mutate(Ocurrence =1)

a<- read_excel("Global_dataset.xlsx")

b<- semi_join(a,df1, by="site_id")
unique(df1$site_id) #178
unique(b$site_id)

b<- b %>% group_by(site_id,year) %>% summarise(entradas=n())
b<- b %>% mutate(Entradas=1)
b<-b %>% group_by(year) %>% summarise(Number=sum(Entradas))

d1<- df %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)

c<- right_join(d1,b, by="year")
d1<- c[c(1:5), ]

hist(d1$Ocurrence)
res<- gam(Ocurrence~ s(year, bs="cr"),family = nb, data=d1)
summary(res)
T1 <- visreg(France,  scale='response', "year", line.par = list(col = 'yellow'), plot=TRUE)


df1<- df %>% filter(country=="France")
df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)
France <- gam(Ocurrence~ s(year, bs="cr"),family=nb(link = log), data=d1)
summary(France)


df1<- df %>% filter(country=="Spain")
df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)
Spain <- gam(Ocurrence~ s(year, bs="cr", k=2),family=Gamma, data=d1)
summary(Spain)


df1<- df %>% filter(country=="Denmark")
df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)
Denmark <- gam(Ocurrence~ s(year, bs="cr",k=4),family=nb(link = log), data=d1)
summary(Denmark)



df1<- df %>% filter(country=="Germany")
df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)
Germany <- gam(Ocurrence~ s(year, bs="cr"),family=nb(link = log), data=d1)
summary(Germany)


df1<- df %>% filter(country=="UK")
df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)
UK <- gam(Ocurrence~ s(year, bs="cr"),family=nb(link = log), data=d1)
summary(UK)


df1<- df %>% filter(country=="Hungary")
df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)
Hungary <- gam(Ocurrence~ s(year, bs="cr"),family=nb(link = log), data=d1)
summary(Hungary)


df1<- df %>% filter(country=="Netherlands")
df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)
Netherlands <- gam(Ocurrence~ s(year, bs="cr"),family=nb(link = log), data=d1)
summary(Netherlands)



df1<- df %>% filter(country=="Switzerland")
df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)
Switzerland <- gam(Ocurrence~ s(year, bs="cr", k=3),family=Gamma, data=d1)
summary(Switzerland)


df1<- df %>% filter(country=="Latvia")
df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)
Latvia <- gam(Ocurrence~ s(year, bs="cr"),family=Gamma, data=d1)
summary(Latvia)


T1 <- visreg(res,  scale='response', "year", line.par = list(col = 'yellow'), plot=TRUE)
T2 <- visreg(France,  scale='response', "year", line.par = list(col = 'red'), plot=TRUE)
T3 <- visreg(Spain,  scale='response', "year", line.par = list(col = 'brown'), plot=TRUE)
T4 <- visreg(Denmark,  scale='response', "year", line.par = list(col = 'black'), plot=TRUE)
T5 <- visreg(Germany,  scale='response', "year", line.par = list(col = 'purple'), plot=TRUE)
T6 <- visreg(UK,  scale='response', "year", line.par = list(col = 'purple'), plot=TRUE)
T7 <- visreg(Hungary,  scale='response', "year", line.par = list(col = 'purple'), plot=TRUE)
T8 <- visreg(Netherlands,  scale='response', "year", line.par = list(col = 'purple'), plot=TRUE)
T9 <- visreg(Switzerland,  scale='response', "year", line.par = list(col = 'purple'), plot=TRUE)
T10 <- visreg(Latvia,  scale='response', "year", line.par = list(col = 'purple'), plot=TRUE)


dplyr::bind_rows(
    dplyr::mutate(T1$fit, plt = "res"),
    dplyr::mutate(T2$fit, plt = "France"),
    dplyr::mutate(T3$fit, plt = "Spain"),
    dplyr::mutate(T4$fit, plt = "Denmark"),
    dplyr::mutate(T5$fit, plt = "Germany"),
    dplyr::mutate(T6$fit, plt = "UK"),
    dplyr::mutate(T7$fit, plt = "Hungary"),
    dplyr::mutate(T8$fit, plt = "Netherlands"),
    dplyr::mutate(T9$fit, plt = "Switzerland"),
    dplyr::mutate(T10$fit, plt = "Latvia")
) -> fits

ggplot() +
    geom_ribbon(
        data = fits,
        aes(year, ymin=visregLwr, ymax=visregUpr, group=plt), fill="gray95"
    ) +
    geom_line(data = fits, aes(year, visregFit, group=plt, color=plt),size=1.2) +
    scale_size_manual(values = c(1,1,1))+ 
    theme_classic2() + theme_cleveland() +scale_x_continuous(limits = c(1972, 2020))+
    scale_y_continuous(limits = c(0, 120))




#Spatial analysis

df <- read_excel("Spatial.xlsx")
unique(df$country)
head(df)
min(df$year) #1968
max(df$year)  #2019
df<- df %>% filter(taxon=="dreissena polymorpha")
a<- df %>% group_by(year) %>% summarise(entradas=n())
max(a$entradas)
1998-2002 / 2006-2012

df<- df %>% subset(year > 2010 & year < 2016)
unique(df$site_id) # 968 
df <- df[!duplicated(df$site_id), ]

df # todas las series temporales
a #series unicas
b #series temporales con dp


b<- df %>% filter(taxon =="dreissena polymorpha")
unique(b$site_id) # 89 con dp

b<- semi_join(a,b, by="site_id")
b<- b%>% mutate(Invaded=1)

c<- df %>% group_by(site_id) %>% summarise(abun=sum(Abundance))
c
write.xlsx(b, "nada.xlsx", overwrite = T)

#abundance and richness
a

d<- semi_join(a, df, by = "site_id")
unique(d$site_id) 
unique(df$site_id)

d<- d %>% subset(year>2010 & year< 2016)
e<- d %>% filter(Alien=="N") #native
f<- d %>% filter(Alien=="Y") #alien

nada <- f %>% group_by(site_id) %>% summarise(abundance=sum(abundance))

e<- e %>% group_by(site_id,taxon) %>% summarise(rich=n()) %>% mutate(Rich=1)
e<- e %>% group_by(site_id) %>% summarise(Rich=sum(Rich))
write.csv2(e,"native_rich.csv")



setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial/nada")
df <- read.csv2("Temperature100.csv")
df1<- read.csv2("Temperature200.csv")
df2<- read.csv2("Temperature600.csv")
df3<- read.csv2("Temperature800.csv")
df4<- read.csv2("Prec_data700.csv")

a<- bind_rows(df,df1,df2,df3)
a <- read.csv2("Full_prec_Data.csv")

max(a$Year)

a<- a %>% subset(Year > 2010 & Year < 2016)

new_temp3<- new_temp3 %>% filter(Year < 2016)
a<- a %>% group_by(Site) %>% summarise(temp=mean(Mean_temp,na.rm=T))
head(a)
new_temp3$Mean_temp<- as.numeric(new_temp3$Mean_temp)
a<- new_temp3 %>% group_by(Site) %>% summarise(temp=mean(Mean_temp, na.rm=T))
write.csv2(a,"Total_Temp22222.csv")


a <- read_excel("Global_dataset.xlsx")
a<-  a %>% filter(Alien=="Y")

a<- a %>% group_by(site_id,taxon) %>% summarise(Abundance=sum(abundance))
a<- a %>% mutate(entradas=1)
a<- a %>% group_by(site_id) %>% summarise(alien=sum(entradas))
write.csv2(a,"Alien_species.csv")


###### Real spatial analysis 
setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial")

df<- read_excel("Spatial_Dp.xlsx")
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

df<- read_excel("Spatial_Dp.xlsx", sheet = "Ocurrence")
hist(df$Ocurrence)
Ocurrence <- gam(Ocurrence~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                   s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                   s(Temp,k=3)+ s(Precipitation,k=3)+
                   s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                   s(Slope,k=3)+s(Elevation,k=3)+
                   s(cat_190,k=3)+s(cat_210,k=3), 
               family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "Abun_alien", line.par = list(col = 'black'), plot=TRUE)
T2 <- visreg(Ocurrence,  scale='response', "Rich_alien", line.par = list(col = 'green'), plot=TRUE)
T3 <- visreg(Ocurrence,  scale='response', "Abun_nat", line.par = list(col = 'blue'), plot=TRUE)
T4 <- visreg(Ocurrence,  scale='response', "Rich_nat", line.par = list(col = 'blue'), plot=TRUE)
T5 <- visreg(Ocurrence,  scale='response', "Abun_nat_sp", line.par = list(col = 'red'), plot=TRUE)
T6 <- visreg(Ocurrence,  scale='response', "Strahler", line.par = list(col = 'blue'), plot=TRUE)




df<- read_excel("Spatial_Dp2.xlsx")
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

df<- read_excel("Spatial_Dp2.xlsx", sheet = "Ocurrence")
hist(df$Ocurrence)
Ocurrence <- gam(Ocurrence~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                     s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                     s(Temp,k=3)+ s(Precipitation,k=3)+
                     s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                     s(Slope,k=3)+s(Elevation,k=3)+
                     s(cat_190,k=3)+s(cat_210,k=3), 
                 family = tw, data = df)

summary(Ocurrence)

T1 <- visreg(Ocurrence,  scale='response', "Rich_alien", line.par = list(col = 'black'), plot=TRUE)
T2 <- visreg(Ocurrence,  scale='response', "Temp", line.par = list(col = 'green'), plot=TRUE)
T3 <- visreg(Ocurrence,  scale='response', "Precipitation", line.par = list(col = 'blue'), plot=TRUE)
T4 <- visreg(Ocurrence,  scale='response', "Outlet", line.par = list(col = 'blue'), plot=TRUE)
T5 <- visreg(Ocurrence,  scale='response', "cat_210", line.par = list(col = 'red'), plot=TRUE)
T6 <- visreg(Ocurrence,  scale='response', "Strahler", line.par = list(col = 'blue'), plot=TRUE)



#### Section using central Europe

## Germany netherlands Denmark France and switzerland

setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/spatial")

df<- read_excel("Spatial_Dp.xlsx")
unique(df$country)
df<-df %>% filter(!country=="Spain")
df<-df %>% filter(!country=="UK")
df<- df %>% filter(!country=="Latvia")

mod_selec <- dredge(Invaded, trace=2)
subset(mod_selec, delta <= 2, recalc.weights=FALSE)

#Period

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
               family = binomial, na.action = "na.fail", data = df)

summary(Invaded)

df<- read_excel("Spatial_Dp.xlsx", sheet = "Ocurrence")
hist(df$Ocurrence)
Ocurrence <- gam(Ocurrence~ s(Abun_alien,k=3)+ s(Rich_alien,k=3)+s(Abun_nat, k=3)+
                     s(Rich_nat,k=3)+ s(Abun_nat_sp,k=3)+ s(Abun_alien_sp,k=3)+
                     s(Temp,k=3)+ s(Precipitation,k=3)+
                     s(DistanceKm,k=3)+ s(Outlet,k=3)+ s(Strahler,k=3)+
                     s(Slope,k=3)+s(Elevation,k=3)+
                     s(cat_190,k=3)+s(cat_210,k=3), 
                 family = tw, data = df)

summary(Ocurrence)



##### MULTIVARIATE RDA

setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves")

df <- read_excel("RDA.xlsx")
head(df)
df<- df %>% filter(Invaded =="1")

Resp <- df[,c("Invaded","Rel_abundance","Period")]

Expl <- df[,c("Abun_alien","Rich_alien","Abun_nat","Rich_nat","Abun_nat_sp",
                  "Abun_alien_sp","Temp","Precipitation","DistanceKm","Outlet",
                  "Strahler","Slope","Elevation","cat_190","cat_210"),]

Resp$Rel_abundance <- log1p (Resp$Rel_abundance)  # species data are in percentage scale which is strongly rightskewed, better to transform them
spe.hell <- decostand (spe.log, 'hell')

myrda <- rda(Resp,Expl,scale=T)

plot(myrda,scaling=2)
plot(myrda,scaling=1,p.max = 0.05)


plot(myrda, type="t")
p1<-ordiplot (myrda)


type_num <- as.numeric (as.factor (df$Period))

ordiplot (p1, type = 'n')
points (p1, display = 'sites', col = type_num, pch = type_num)

ordiplot (p1, type = 'n')
points(p1, pch=1,cex=df$Period, col="blue") #sites with different shape and color
text(p1,'species', col="red") #species names
points (p1, col = Expl$Period, pch =Expl$Period)



#db-RDA
str(df)
colnames(df)
dbRDA <-  capscale(Resp ~ Abun_alien+Rich_alien+Abun_nat+Rich_nat+Abun_nat_sp+
                  Abun_alien_sp+Temp+Precipitation+DistanceKm+Outlet+
                Strahler+Slope+Elevation+cat_190+cat_210 + Latitude+ Longitude
                , data = df,dist="bray",
                qrt.dist = FALSE, dfun = vegdist)

plot(dbRDA) 
plot(dbRDA, type="t")




### Plot lenght time series

setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves/RDA/")

df<- read_excel("Spatial_Dp.xlsx")
df <- read_excel("RDA.xlsx")
df<- df[!duplicated(df$site_id), ]
unique(df$country)

b<- read_excel("Global_dataset.xlsx")

a <- semi_join(b, df, by = "site_id")
unique(df$site_id)
unique(a$site_id)

df1 <- a %>% group_by(site_id, country) %>% mutate(End = max(year),
                                                     star=min(year))
df1$site_id <- as.factor(df1$site_id)
df1 <- df1[order(df1$star),]

mycolors <- brewer.pal(9, name = 'Paired')
mycolors2 <- data.frame(cbind(colB=mycolors, country= unique(as.character(df1$country))))
df1 <- merge(df1, mycolors2, by="country")
df1 <- df1[order(-df1$star),]
par(mar=c(4, 4, 1, 1)) 

plot(df1$star,c(1:length(df1$star)), xlim=c(1968, 2020), type="n", 
     las=1, axes=F, xlab="Year", ylab= "Time series")
segments(x0=df1$star, x1=df1$End, y0=c(1:length(df1$star)),
         y1=c(1:length(df1$star)), col=as.character(df1$colB),lwd=1.7)

abline(v=c(1970,1980,1990,2000,2010,2020), col=c("black"), lty=c(2), lwd=c(1))
axis(1)
legend("bottomleft", lty=1,lwd=2, col=unique(as.character(df1$colB)), 
       legend=c("Spain", 
                "England", "Germany", "Hungary",
                 "Denmark","France","Netherlands", "Latvia","Switzerland"))



###### d cohen for Phillip

setwd("C:/Users/isma-/OneDrive/Escritorio/Bivalves")
df <- read_excel("Bivalves.xlsx")
df<- df %>% filter(taxon== "dreissena polymorpha")
df1<- read_excel("Global_dataset.xlsx")

#Invaded
df1<- read_excel("Global_dataset.xlsx")

unique(df1$country)

{df1 <- df1 %>% filter(!country=="Estonia")
    df1 <- df1 %>% filter(!country=="Cyprus")
    df1 <- df1 %>% filter(!country=="Luxembourg")
    df1 <- df1 %>% filter(!country=="Bulgaria")
    df1 <- df1 %>% filter(!country=="Norway")
    df1 <- df1 %>% filter(!country=="Ireland")
    df1 <- df1 %>% filter(!country=="CzechRep")
    df1 <- df1 %>% filter(!country=="Italy")
    df1 <- df1 %>% filter(!country=="Portugal")
    df1 <- df1 %>% filter(!country=="Austria")
    df1 <- df1 %>% filter(!country=="Finland")
    df1 <- df1 %>% filter(!country=="Belgium")
    df1 <- df1 %>% filter(!country=="Sweden")
}

df2<- df1 %>% filter(taxon =="dreissena polymorpha")
df2 %>% group_by(country) %>% summarise(fdsd=min(year))
unique(df1$site_id) #1491
unique(df2$site_id) #164

df3<- semi_join(df1,df2, by="site_id")
unique(df3$site_id) #164

df4<- anti_join(df3,s, by="site_id")
unique(df4$site_id) #131

df4<-df4 %>% 
    filter(!str_detect(taxon, 'gen. sp.'))
df4<-df4 %>% 
    filter(!str_detect(taxon, 'sp.'))

df4<- read_excel("Invaded2.xlsx", sheet = "Sheet 1")
df4<- df4 %>% group_by(taxon, year,country) %>% summarise(Abun=mean(abundance))
write.xlsx(df4, "Invaded3.xlsx", overwrite = T)


#Uninvaded
unique(df$country)
df1<- read_excel("Global_dataset.xlsx")

unique(df1$country)

{df1 <- df1 %>% filter(!country=="Estonia")
df1 <- df1 %>% filter(!country=="Cyprus")
df1 <- df1 %>% filter(!country=="Luxembourg")
df1 <- df1 %>% filter(!country=="Bulgaria")
df1 <- df1 %>% filter(!country=="Norway")
df1 <- df1 %>% filter(!country=="Ireland")
df1 <- df1 %>% filter(!country=="CzechRep")
df1 <- df1 %>% filter(!country=="Italy")
df1 <- df1 %>% filter(!country=="Portugal")
df1 <- df1 %>% filter(!country=="Austria")
df1 <- df1 %>% filter(!country=="Finland")
df1 <- df1 %>% filter(!country=="Belgium")
df1 <- df1 %>% filter(!country=="Sweden")
}

df2<- df1 %>% filter(taxon =="dreissena polymorpha")

c<- anti_join(df1, df2, by = "site_id")
unique(df2$site_id)
unique(df1$site_id)
unique(c$site_id)


c<-c %>% 
    filter(!str_detect(taxon, 'gen. sp.'))
c<-c %>% 
    filter(!str_detect(taxon, 'sp.'))


unique(c$site_id)
options(scipen = 999)


{df11<- df1 %>% filter(site_id=="100000001")
df12<- df1 %>% filter(site_id=="100000308")
df13<- df1 %>% filter(site_id=="103000046")
df14<- df1 %>% filter(site_id=="103000213")
df15<- df1 %>% filter(site_id=="107000213")
df16<- df1 %>% filter(site_id=="107000224")
df17<- df1 %>% filter(site_id=="108000028")
df18<- df1 %>% filter(site_id=="108000053")
df19<- df1 %>% filter(site_id=="108000162")
df111<- df1 %>% filter(site_id=="109000049")
df112<- df1 %>% filter(site_id=="109000169")
df113<- df1 %>% filter(site_id=="109000201")
df114<- df1 %>% filter(site_id=="109000242")
df115<- df1 %>% filter(site_id=="109000244")
df116<- df1 %>% filter(site_id=="109000254")
df117<- df1 %>% filter(site_id=="109000261")
df118<- df1 %>% filter(site_id=="109000262")
df119<- df1 %>% filter(site_id=="109000264")
df1111<- df1 %>% filter(site_id=="109000290")
df1112<- df1 %>% filter(site_id=="109000321")
df1113<- df1 %>% filter(site_id=="109000364")
df1114<- df1 %>% filter(site_id=="114000005")
df1115<- df1 %>% filter(site_id=="114000041")
df1116<- df1 %>% filter(site_id=="114000046")
df1117<- df1 %>% filter(site_id=="114000051")
df1118<- df1 %>% filter(site_id=="114000052")
df1119<- df1 %>% filter(site_id=="114000074")
df1120<- df1 %>% filter(site_id=="114000084")
df1121<- df1 %>% filter(site_id=="114000086")
df1122<- df1 %>% filter(site_id=="114000087")
df1123<- df1 %>% filter(site_id=="117000009")
df1124<- df1 %>% filter(site_id=="117000022")
df1125<- df1 %>% filter(site_id=="120000003")
}

s<- bind_rows(df11,df12,df13,df14,df15,df16,df17,df18,df19,df111,df112,df113,df114,df115,df116,df117,df118,
          df119,df1111,df1112,df1113,df1114,df1115,df1116,df1117,df1118,df1119,df1120,df1121,df1122,
          df1123,df1124,df1125)

c<- bind_rows(c,s)
c<-c %>% 
    filter(!str_detect(taxon, 'gen. sp.'))
c<-c %>% 
    filter(!str_detect(taxon, 'sp.'))

c<- c %>% group_by(taxon, year,country) %>% summarise(Abun=mean(abundance))
write.xlsx(c, "No_invaded2.xlsx", overwrite = T)



df <- read_excel("No_invaded.xlsx")
df<- df %>% group_by(taxon, year,country) %>% summarise(Abun=mean(abundance))
df<- df %>% pivot_wider(names_from = country, values_from = Abun, values_fill = 0)
write.xlsx(df, "D_cohen.xlsx", overwrite = T)



df<- read_excel("D_Cohen.xlsx", sheet = "Sheet 1")
df<- read_excel("D_cohen.xlsx", sheet = "No invaders")
colnames(df)

df<- df %>% filter(Group=="Insects")
df1<- df %>% group_by(taxa,year) %>% summarise(Avg=mean(Germany),
                                    Avg2=mean(Germany_NI))

unique(df1$year)
df2 <- df1 %>% filter(year=="2012")
#df2 <- df1 %>% filter(year=="2019")
df2
cohensD(df2$Avg, df2$Avg2)




df<- read_excel("D_cohen.xlsx", sheet = "Hoja1")

df$D<- as.numeric(df$D)

df <- na.omit(df$D) 
df<- df %>% group_by(Country) %>% summarise(D_Cohen=mean(D, na.rm=T),
                                       sd=sd(D, na.rm = T),
                                       n=n()) %>% mutate(index=1:8)



ggplot(data=df, aes(y=index, x=D_Cohen, xmin=D_Cohen-sd, xmax=D_Cohen+sd)) +
    geom_point(size=3,aes(colour = factor(Country))) + 
    geom_errorbarh(height=.1) +
    scale_y_continuous(breaks=1:nrow(df), labels=df$Country) +
    labs( x='D Cohen', y = 'Country') +
    geom_vline(xintercept=0, color='black', linetype='dashed', alpha=.5) +
    theme_classic() + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","red","blue",
                                                  "yellow","pink","red"))      
           


ggplot(data=df, aes(year,D,group=Country)) + geom_point(aes(color=Country)) + 
                                                            geom_line(aes(color=Country))+
    theme_classic() + theme_cleveland()+ scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","red","blue",
                                                  "yellow","pink","red"))  +
    facet_wrap(vars(Country),nrow = 4) + geom_smooth(method = "loess", aes(colour=Country), alpha=1/8, se=F)


df$`D_no invader`<- as.numeric(df$`D_no invader`)
ggplot(data=df, aes(year,D,group=Country)) + geom_point(aes(color=Country)) + 
    geom_line(aes(color=Country))+
    theme_classic() + theme_cleveland()+ scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","red","blue",
                                                                     "yellow","pink"))  +
    facet_wrap(vars(Country),nrow = 4) + geom_smooth(method = "loess", aes(colour=Country), alpha=1/8, se=F)


#Treds

df<- read_excel("D_cohen.xlsx", sheet = "Avg")
df<- read_excel("D_cohen.xlsx", sheet = "Crustacean")
df<- read_excel("D_cohen.xlsx", sheet = "Extra_Europe")
str(df)
df$`Etiquetas de fila`<-as.factor(df$`Etiquetas de fila`)
MannKendallTESTresponse <- function(data){
    rbind(MannKendallTEST(df$`Promedio de Europe`, "Europe"),
          MannKendallTEST(df$`Promedio de Europe_NI`, "Europe_NI"))}

MannKendallTEST <- function(variable, name) cbind(variable=name, data.frame(as.list(My.mmkh(variable))))
MannKendallTESTresponse <- function(data){
    rbind(MannKendallTEST(df$`Promedio de Netherland`, "Netherland"),
          MannKendallTEST(df$`Promedio de Netherlands_NI`, "Netherlands_NI"),
          MannKendallTEST(df$`Promedio de Denmark`, "Denmark"),
          MannKendallTEST(df$`Promedio de Denmark_NI`, "Denmark_NI"),
          MannKendallTEST(df$`Promedio de Hungary`, "Hungary"),
          MannKendallTEST(df$`Promedio de Hungary_NI`, "Hungary_NI"),
          MannKendallTEST(df$`Promedio de UK`, "UK"),
          MannKendallTEST(df$`Promedio de UK_NI`, "UK_NI"),
          MannKendallTEST(df$`Promedio de France`, "France"),
          MannKendallTEST(df$`Promedio de France_NI`, "France_NI"),
          MannKendallTEST(df$`Promedio de Germany`, "Germany"),
          MannKendallTEST(df$`Promedio de Germany_NI`, "Germany_NI"),
          MannKendallTEST(df$`Promedio de Latvia`, "Latvia"),
          MannKendallTEST(df$`Promedio de Latvia_NI`, "Latvia_NI"))
}

MKresults.response <- MannKendallTESTresponse(df)
colnames(MKresults.response)[2:6] <- c("Z.corr", "p.value", "N.N", "Z.original", "p.value.old")
MKresults.response <- MKresults.response[c(nrow(MKresults.response):1), ]

plotMANNKENDALL <- function(data, labels, axiscolors="black",title=NULL){
    plot(data$Tau, c(1:length(data$Tau)), 
         main = "", frame.plot = TRUE, 
         xlab = "", ylab = "", axes = F,
         col = ifelse(data$p.value < 0.05, "black", "darkgrey"), 
         lty = ifelse(data$p.value < 0.05, 'solid', 'dashed'),
         pch = 15, lwd = 5) #if we want to code specific colours to a point use col = "column name"
    axis(1, seq(-0.8, 0.8, 0.2), labels = T)
    axis(2, at = c(1:length(data$Tau)), labels = T,
         rownames(data), las = 1, cex.axis = 1)
    abline(v = 0, lty = 2)
    segments(x0 = 0, x1 = data$Tau, col = ifelse(data$p.value < 0.05, "black", "darkgrey"), 
             lty = ifelse(data$p.value < 0.05, 'solid', 'dashed'),
             y0 = c(1:length(data$Tau)), 
             y1 = c(1:length(data$Tau)))
    mtext("Mann-Kendall Tau", side = 1, outer = T, at = 0.73, las = 1, cex = 1, font = 2)
    Map(axis, side = 2, at = 1:32, col.axis = axiscolors, labels = labels, las = 1)
    title(title)
}
plotMANNKENDALL(MKresults.response)

df<- read_excel("D_cohen.xlsx", sheet = "Hoja1")

unique(df$Country)
df1<- df %>% filter(Country =="Latvia")
str(df1)
df1$D_Crustacea <- as.numeric(df1$D_Crustacea)
MannKendallTEST(df1$D_Crustacea, "Netherlands_NI")



df<- read_excel("D_cohen.xlsx", sheet = "Sheet 1")
head(df)
unique(df$Group)

df<- df %>% filter(Group=="Crustacea")

df1<- df %>% group_by(taxa,year) %>% summarise(Avg=mean(Europe),
                                               Avg2=mean(Europe_NI))


unique(df1$year)
df2 <- df1 %>% filter(year=="2020")
df2
cohensD(df2$Avg, df2$Avg2)





#Tonda plot
df<- df %>% filter(taxon== "dreissena polymorpha")
head(df)

a <- semi_join(b, df, by = "site_id")
unique(a$site_id)

df$site_id <- as.factor(df$site_id)
df$year <- as.factor(df$year)
df$country<- df$country
str(a)
a$site_id <- as.factor(a$site_id)
a$year <- as.factor(a$year)
a$country <- as.factor(a$country)
ggplot() +    
    geom_point(data = a, aes(year,site_id), # Total sampled
               fill = "black", color = "black", 
               size = 1, shape = 1)+
    geom_point(data = df, aes(year,site_id), #Ocurrence
               fill = "black", color ="black",
               size = 1, shape = 21) + theme_bw()+ 
    theme(text = element_text(size = 5),element_line(size =1))


options(repr.plot.width =2, repr.plot.height =5)
ggplot(df, aes(year,site_id,color=country)) + geom_point() + theme_bw() + 
    scale_color_manual(values = c("red", "blue", "green","purple","grey","yellow",
                                  "pink","darkred","tan3"))+theme(axis.text.x = element_text(angle = 90))+
    theme(text = element_text(size = 5),element_line(size =1))



c<-  semi_join(b,df, by="site_id")
unique(c$site_id) 

c$site_id<- as.factor(c$site_id)
c$year<- as.factor(c$year)

d<- c %>% filter(country=="France")
df1<- df %>% filter(country=="France")

ggplot(c, aes(year,site_id,color=country)) + geom_point() + theme_bw() + 
    scale_color_manual(values = c("red", "blue", "green","purple","grey","yellow",
                                  "pink","darkred","tan3")) 


df$site_id<-as.factor(df$site_id)
df$year<-as.factor(df$year)

ggplot() +               
    geom_point(data = d, aes(year,site_id), #sampled
               fill = "dark green", color = "black",
               size = 2, shape = 21)+
    geom_point(data = df1, aes(year,site_id), # Ocurrences
               fill = "red", color = "black", 
               size = 1.3, shape = 21)





head(df3)

df4<- df3 %>% group_by(site_id,year) %>% summarise(entradas=n())
df5<- df4%>% group_by(year) %>% summarise(time_series=sum(entradas))


df6<- df3 %>% group_by(year) %>% summarise(abundance= mean(abundance))

d<- right_join(df6,df5)
d$standarized <- d$abundance/d$time_series

hist(d$standarized)

s <- gam(standarized~ s(year, k=3), family=nb,data=d)
summary(s)
T1 <- visreg(s,  scale='response', "year", line.par = list(col = 'black'), plot=TRUE)

df3 %>% group_by(country) %>% summarise(abundance=mean(abundance))


r<- semi_join(b,df3, by= "site_id")
unique(r$site_id)
r<-r %>% filter(country=="Germany")
r<- r[!duplicated(r$site_id),]
r<-r %>%group_by(site_id) %>% mutate(year2=max(year))
z<- r$year2
DATA1 <- r[order(r$year2),]
DATA1$year2



##### Another plot for Phillip

head(df3)
df3<- df3 %>% filter(country=="Germany")
df31 <- df3 %>% group_by(year) %>% summarise(Proportion= mean(Proportion))
df32 <- df3 %>% group_by(year) %>% summarise(abundance= mean(abundance))

ggplot() +
    #geom_line(data = df31, aes(x = year, y = Proportion),size=1, color = "purple") + 
    geom_line(data = df32, aes(x = year, y = abundance),size=1, color="cornflowerblue") + 
    theme_classic2() + theme_cleveland()+
    theme(axis.text=element_text(size=12))



#D_cohe
df<- read_excel("D_Cohen.xlsx", sheet = "Hoja1")
df<- df %>% filter(Country=="Germany")

head(df)

df0<- df[,c(2,5)]
df1<-df[,c(2,6)]
df2<- df[,c(2,7)]
str(df)
df$D_Mollusk<- as.numeric(df$D_Mollusk)
df$D_Crustacea<- as.numeric(df$D_Crustacea)
df$D_Insects<- as.numeric(df$D_Insects)

ggplot() +
    geom_line(data = df0, aes(x = year, y = D_Mollusk),size=1, color = "purple") + 
    geom_line(data = df1, aes(x = year, y = D_Crustacea),size=1, color="cornflowerblue") + 
    geom_line(data = df2, aes(x = year, y = D_Insects),size=1, color="orange")+
    theme_classic2() + theme_cleveland()+
    theme(axis.text=element_text(size=12))
    
df2






