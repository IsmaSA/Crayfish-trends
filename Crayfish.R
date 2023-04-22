### Crayfish proyect
setwd("C:/Users/isma-/OneDrive/Escritorio/Crayfish")
df <- read_excel("Crayfish.xlsx")

head(df)
glimpse(df)

df1 <-  read_excel("Global_dataset.xlsx")

a<- semi_join(df1, df, by = "site_id")
unique(a$site_id)
df <- a

#Characteristics
unique(df$site_id) # -> total time series

df1 <- df %>% group_by(site_id,year) %>% summarise(count = n())
df2 <- df1 %>% group_by(site_id,year,count) %>% summarise(count=n())
df3 <- df2 %>% group_by(site_id) %>% summarise(sumar= sum(count))
mean(df3$sumar) #->mean sampling years
sd(df3$sumar)

df <- select(df, site_id, year)
df1 <-df %>% group_by(site_id) %>% mutate(total = max(year)- min(year))
mean(df1$total) # -> mean o total years
sd(df1$total)

#####  Europe Map
ddf = read.table(text="
country value
'France' 94
'Spain' 94
'Sweden' 94
'Luxembourg' 94
'Germany' 94
'United Kingdom' 94
'Hungary' 94
'Portugal' 94
'Netherlands' 94", header=TRUE)

pal <- colorRampPalette(brewer.pal(8, 'Reds'))(length(ddf$value))
pal <- pal[with(ddf, findInterval(value, sort(unique(value))))]

df<- df[!duplicated(df$site_id), ]

newmap <- getMap(resolution = "high")
col <- rep("white", length(newmap@data$NAME))
col[match(ddf$country, newmap@data$NAME)] <- pal
par(mar=c(0,0,0,0))
plot(newmap,col=col,
     bg="lightblue",border="grey40",
     xlim = c(8, 10),
     ylim = c(33, 64),
     asp = 1)

df1 <- df %>% filter(df$taxon == "procambarus clarkii")
df2 <- df %>% filter(df$taxon == "orconectes limosus")
df3 <- df %>% filter(df$taxon == "pacifastacus leniusculus")

points(df1$Longitude , df1$Latitude,  pch=19,cex=1, col=adjustcolor("chartreuse4",0.35))
points(df2$Longitude , df2$Latitude,  pch=19,cex=1, col=adjustcolor("darkorange4",0.35))
points(df3$Longitude , df3$Latitude,  pch=19,cex=1, col=adjustcolor("darkorchid4",0.35))



riversData <- readOGR("Europe_Hydrography.shp") # load the shapefile
plot(riversData, col= "steelblue2", add=T) 



##### META_REGRESSION MODELS
library(metafor)
#Overall
df <- df %>% filter(!site_id =="114000086")
df <- df %>% filter(!site_id =="117000012")
df <- df %>% filter(!site_id =="103000530")
df <- df %>% filter(!site_id =="104000031")
df <- df %>% filter(!site_id =="109000030")

xy.list <- split(df$abundance, df$site_id) 
xy.list <- xy.list[lengths(xy.list) >= 3]  # 56
length(xy.list)


MK <-as.data.frame(do.call(rbind,lapply(xy.list[1:52],function(x)unlist(My.mmkh(x))))) #96 are time series,
head(MK)
Ove <- rma.mv(`S statistic`, old.variance, method = "REML", data = MK)
Ove
forest(Ove)
model_results <- orchaRd::mod_results(Ove)
devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)


# Procambarus clarkii
unique(df1$site_id) #18
head(df1)
df5 <-  read_excel("Global_dataset.xlsx")


xy.list <- split(df1$abundance, df1$site_id) 
xy.list <- xy.list[lengths(xy.list) >= 3]  # 8
length(xy.list)


MK <-as.data.frame(do.call(rbind,lapply(xy.list[1:8],function(x)unlist(My.mmkh(x))))) #96 are time series,
head(MK)

Pc <- rma(`S statistic`, old.variance, method = "REML", data = MK)
Pc
forest(Pc)
plot_model(Pc, type = "est")

min(df1$abundance)
max(df1$abundance)
mean(df1$abundance)
sd(df1$abundance)


# orconectes limosus
unique(df2$site_id) #66
head(df2)
df2 <- df2 %>% filter(!site_id =="114000086")
df2 <- df2 %>% filter(!site_id =="117000012")


xy.list <- split(df2$abundance, df2$site_id) 
xy.list <- xy.list[lengths(xy.list) >= 3]  # 11
length(xy.list)

MK <-as.data.frame(do.call(rbind,lapply(xy.list[1:11],function(x)unlist(My.mmkh(x))))) #96 are time series,
head(MK)

colnames(df2)
Ol <- rma.mv(`S statistic`, old.variance, method = "REML", data = MK)
Ol

forest(Ol)

min(df2$abundance)
max(df2$abundance)
mean(df2$abundance)
sd(df2$abundance)


# Pacifastacus leniusculus
unique(df3$site_id) #85
head(df3)
df3 <- df3 %>% filter(!site_id =="103000530")
df3 <- df3 %>% filter(!site_id =="104000031")
df3 <- df3 %>% filter(!site_id =="109000030")



a<- semi_join(df5, df3, by = "site_id")
unique(a$site_id)
df3 <- a

xy.list <- split(df3$abundance, df3$site_id) 
xy.list <- xy.list[lengths(xy.list) >= 3]  # 33
length(xy.list)

MK <-as.data.frame(do.call(rbind,lapply(xy.list[1:33],function(x)unlist(My.mmkh(x))))) #96 are time series,
head(MK)

colnames(df3)
Pl <- rma(`S statistic`, old.variance, method = "REML", data = MK)

plot_model(Pl, type = "est")
forest(Pl)
MK$`S statistic`

min(df3$abundance)
max(df3$abundance)
mean(df3$abundance)
sd(df3$abundance)

#################### ---------------- GLM -----------------------######

#Procambarus clarkii
head(df1)

d1<- df1 %>% group_by(site_id) %>% summarise(N= n())
d1 <- d1 %>% filter(N >2)

Pc <- semi_join(df1, d1, by = "site_id")
colnames(Pc)
unique(Pc$season)
Pc1 <- glm.nb(Proportion~ year+ Temp+ Prec+Elevation+DistanceKM+ Slope
              + Dist+strahler,  data=Pc)
summary(Pc1)
deviance(Pc1)
anova(Pc1)

a<-Pc %>% group_by(year) %>% summarise(N=n())
Pc <- right_join(a, Pc, by = "year")

s <- 1- (33.96/43.64)
s
library(blme)
Pc1$N<- as.factor(Pc1$N)
library(rcompanion)

nagelkerke(Pc1)

min(Pc$Proportion)
max(Pc$Proportion)
mean(Pc$Proportion)
sd(Pc$Proportion)
## O limosus ----------------
head(df2)

d2<- df2 %>% group_by(site_id) %>% summarise(N= n())
d2 <- d2 %>% filter(N >2)

Ol <- semi_join(df2, d2, by = "site_id")

colnames(Ol)

Ol1 <- glm.nb(Proportion~ year+ Temp+ Prec+Elevation+DistanceKM+ 
                Slope+ Dist+strahler, data=Ol)
summary(Ol1)
s <- 1- (79.735/105.401)
s
deviance(Ol1)

a<-Ol %>% group_by(year) %>% summarise(N=n())
Ol <- right_join(a, Ol, by = "year")

min(Ol$Proportion)
max(Ol$Proportion)
mean(Ol$Proportion)
sd(Ol$Proportion)



## P. leniusculus ------------

d3<- df3 %>% group_by(site_id) %>% summarise(N= n())
d3 <- d3 %>% filter(N >2)

Pl <- semi_join(df3, d3, by = "site_id")

colnames(Pl)
Pl$season <- as.factor(Pl$season)
Pl1 <- glm.nb(Proportion~ year+ Temp+ Prec+Elevation+DistanceKM+ Slope
              + Dist+strahler,  data=Pl)
summary(Pl1)
s <- 1- (143.06/204.94)
s
deviance(Pl1)
summary(Pl1)$null.deviance

a<-Pl %>% group_by(year) %>% summarise(N=n())
Pl <- right_join(a, Pl, by = "year")

a<- log1p(Pl$Proportion)
hist(a)
min(Pl$Proportion)
max(Pl$Proportion)
mean(Pl$Proportion)
sd(Pl$Proportion)

### GLM MODEL -- trend over time 
T1 <- visreg(Pc1,  scale='response', "year", line.par = list(col = 'yellow'), plot=TRUE)
T2 <- visreg(Ol1,  scale='response', "year", line.par = list(col = 'brown'), plot=TRUE)
T3 <- visreg(Pl1,  scale='response', "year", line.par = list(col = 'purple'), plot=TRUE)
T4 <- visreg(Sp,  scale='response', "year", line.par = list(col = 'purple'), plot=TRUE)
dplyr::bind_rows(
  dplyr::mutate(T1$fit, plt = "P. clarkii"),
  dplyr::mutate(T2$fit, plt = "O. limosus"),
  dplyr::mutate(T3$fit, plt = "P. leniusculus"),
  dplyr::mutate(T4$fit, plt = "total")

) -> fits


ggplot() +
  geom_ribbon(
    data = fits,
    aes(year, ymin=visregLwr, ymax=visregUpr, group=plt), fill="gray95"
  ) +
  geom_line(data = fits, aes(year, visregFit, group=plt, color=plt),size=1.2) +
  scale_size_manual(values = c(1,1,1))+ 
  theme_classic2() + theme_cleveland() 

## ALL SPECIES
head(df)
d4<- df %>% group_by(site_id, country) %>% summarise(N= n())
d4 <- d4 %>% filter(N >2)

a<- semi_join(df, d4, by = "site_id")

Sp <- glm.nb(Proportion~ year+ Temp+ Prec+Elevation+DistanceKM+  Slope
             + Dist+strahler ,link = log, data=a)

summary(Sp)
s <- 1- (289.49/335)
s
deviance(Sp)


#### Mean + SD

min(Pl$abundance)
max(Pl$abundance)
mean(Pl$abundance)
sd(Pl$abundance)


min(Ol$abundance)
max(Ol$abundance)
mean(Ol$abundance)
sd(Ol$abundance)


min(Pc$abundance)
max(Pc$abundance)
mean(Pc$abundance)
sd(Pc$abundance)


### SPATIAL GLM MODEL CLUSTER

head(df)

setwd("C:/Users/isma-/OneDrive/Escritorio/Crayfish")
df <- read_excel("Crayfish.xlsx")
head(df)

#Spain

df1<- df %>% filter(country=="Spain")
unique(df1$site_id)


df1 <- df1 %>% mutate(Ocurrence =1)
d1<- df1 %>% group_by(year,taxon) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence, breaks = 5)

Spain <- glm.nb(Ocurrence~ year, data=d1)
summary(Spain)
s <- 1- (19.226/21.319)
s
#Germany
unique(df$country)
df2<- df %>% filter(!country=="France")
df2<- df2 %>% filter(!country=="Spain")
df2<- df2 %>% filter(!country=="Sweden")
df2<- df2 %>% filter(!country=="UK")
df2<- df2 %>% filter(!country=="Hungary")
df2<- df2 %>% filter(!country=="Portugal")

unique(df2$country)


df2 <- df2 %>% mutate(Ocurrence =1)
d2<- df2 %>% group_by(year, taxon) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d2$Ocurrence)

Germany <- glm.nb(Ocurrence~ year, data=d2)
summary(Germany)
s <- 1- (35.789/36.012)
s
#Uk
df3<- df %>% filter(country=="UK")
unique(df3$site_id)


df3 <- df3 %>% mutate(Ocurrence =1)
d3<- df3 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d3$Ocurrence)

UK <- glm.nb(Ocurrence~ year, data=d3)
summary(UK)
s <- 1- (18.046/60.053)
s
#Hungary 
df4<- df %>% filter(country=="Hungary")
unique(df4$site_id)


df4 <- df4 %>% mutate(Ocurrence =1)
d4<- df4 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d4$Ocurrence)
a<- df4 %>% group_by(taxon) %>% summarise(Ocurrence=sum(Ocurrence))

Hungary <- glm.nb(Ocurrence~ year, data=d4)
summary(Hungary)
s <- 1- (7.9288/10.5364)
s


T1 <- visreg(Spain,  scale='response', "year", line.par = list(col = 'yellow'), plot=TRUE)
T2 <- visreg(Germany,  scale='response', "year", line.par = list(col = 'brown'), plot=TRUE)
T3 <- visreg(UK,  scale='response', "year", line.par = list(col = 'black'), plot=TRUE)
T4 <- visreg(Hungary,  scale='response', "year", line.par = list(col = 'purple'), plot=TRUE)
dplyr::bind_rows(
  dplyr::mutate(T1$fit, plt = "Espa?a"),
  dplyr::mutate(T2$fit, plt = "Germany"),
  #dplyr::mutate(T3$fit, plt = "UK"),
  #dplyr::mutate(T4$fit, plt = "Hungary")
  
) -> fits


ggplot() +
  geom_ribbon(
    data = fits,
    aes(year, ymin=visregLwr, ymax=visregUpr, group=plt), fill="gray95"
  ) +
  geom_line(data = fits, aes(year, visregFit, group=plt, color=plt),size=1.2) +
  scale_size_manual(values = c(1,1,1))+ 
  theme_classic2() + theme_cleveland()+ scale_y_continuous(breaks = seq(0, 25, by = 5))


#### Temperature plot
setwd("C:/Users/isma-/OneDrive/Escritorio/Crayfish")
df <- read_excel("Crayfish.xlsx")
head(df)

colnames(df)
hist(df$Temp)

temp<- gam(Temp ~ s(year, bs="cr", k= 5),family=Gamma(link = log), data= df)
Prec<- gam(Prec ~ s(year, bs="cr", k = 5), family=Gamma(link = log), data= df)
op <- par(mfrow=c(2,2), mar=c(4,4,1,1))
gam.check(Prec)
par(op)
summary.gam(Prec)


T1 <- visreg(temp,  scale='response', "year", line.par = list(col = 'black'), plot=TRUE)
T2 <- visreg(Prec,  scale='response', "year", line.par = list(col = 'green'), plot=TRUE)


dplyr::bind_rows(
  dplyr::mutate(T1$fit, plt = "Temp"),
  dplyr::mutate(T2$fit, plt = "Prec")
) -> fits


ggplot() +
  geom_ribbon(
    data = fits,
    aes(year, ymin=visregLwr, ymax=visregUpr, group=plt), fill="gray95"
  ) +
  geom_line(data = fits, aes(year, visregFit, group=plt, color=plt),size=1.2) +
  scale_size_manual(values = c(1,1,1))+ 
  theme_classic2() + theme_cleveland() 



#df3
a<- df %>% group_by(site_id) %>% summarise(N=n())
a<- a[!duplicated(a$site_id),]
table(a$country)

df1 <- df[duplicated(df$LINK),]
head(df1)

df1 <- a[duplicated(a$site_id),]

b<-a %>% group_by(site_id)%>% mutate(minimi =min(year))






### bar plot sampling methods
head(df)

a<- read_excel("Global_dataset.xlsx")
b<- df

b<- b[,c(1,4,5,7,19)]
b<- b %>% mutate(todos ="todo")

b<- b[!duplicated(b$site_id),]

ggplot(b, aes(todos, ..count..)) + 
  geom_bar(aes(fill = Method), ) + theme_clean()

unique(b$Method)


b$Method[b$Method== "Kicknet, 0.5mm mesh"] <- "Kicknet"
b$Method[b$Method== "Kicknet 0.5mm"] <- "Kicknet"
b$Method[b$Method== "Kicknet 1.5mm"] <- "Kicknet"
b$Method[b$Method== "UK RIVPACS protocol"] <- "RIVPACS"
b$Method[b$Method== "AQEM/STAR"] <- "AQEM"
b$Method[b$Method== "NA"] <- "AQEM"

a<- read_excel("Global_dataset.xlsx")
df
b<- df %>% mutate(todos ="todo")
c<- anti_join(a,b, by="site_id")

unique(b$country)
unique(c$country)

c<- c %>% filter(!country=="Estonia")
c<- c %>% filter(!country=="Cyprus")
c<- c %>% filter(!country=="Bulgaria")
c<- c %>% filter(!country=="Denmark")
c<- c %>% filter(!country=="Ireland")
c<- c %>% filter(!country=="Norway")
c<- c %>% filter(!country=="CzechRep")
c<- c %>% filter(!country=="Italy")
c<- c %>% filter(!country=="Austria")
c<- c %>% filter(!country=="Switzerland")
c<- c %>% filter(!country=="Latvia")
c<- c %>% filter(!country=="Belgium")
c<- c %>% filter(!country=="Finland")

write.csv2(c,"without.csv")
b<- read.csv2("without.csv")

b<- b %>% mutate(todos ="todo")

b<- b[!duplicated(b$site_id),]

b$Method[b$Method== "Standard 3 min kick sample + 30 secs hand search"] <- "Kicknet"
b$Method[b$Method== "Multi-habitat Kicknet (Mykra et al., 2006)"] <- "Kicknet"
b$Method[b$Method== "NA"] <- "AQEM"




b$Method <- as.factor(b$Method)
b<- b %>% group_by(Method) %>% summarise(counts=n())
b$counts2 <- b$counts/sum(b$counts)

ggplot(b, aes(fill=Method, Method, counts2)) + 
  geom_bar(position="stack", stat="identity") + theme_clean()


### Site specific trend // cluster

df1<- df %>% filter(country=="Spain")
unique(df1$taxon)

df2<- df1 %>% filter(taxon=="procambarus clarkii")

df3<- df1 %>% filter(taxon=="pacifastacus leniusculus")

df2 <- df2 %>% mutate(Ocurrence =1)
d1<- df3 %>% group_by(year) %>% summarise(Ocurrence=sum(Ocurrence))
hist(d1$Ocurrence)

Spain <- glm(Ocurrence~ year, data=d1)
summary(Spain)


T1 <- visreg(Spain,  scale='response', "year", line.par = list(col = 'black'), plot=TRUE)
T2 <- visreg(Spain,  scale='response', "year", line.par = list(col = 'green'), plot=TRUE)


dplyr::bind_rows(
  dplyr::mutate(T1$fit, plt = "Ol"),
  dplyr::mutate(T2$fit, plt = "Pl")
) -> fits


ggplot() +
  geom_ribbon(
    data = fits,
    aes(year, ymin=visregLwr, ymax=visregUpr, group=plt), fill="gray95"
  ) +
  geom_line(data = fits, aes(year, visregFit, group=plt, color=plt),size=1.2) +
  scale_size_manual(values = c(1,1,1))+ 
  theme_classic2() + theme_cleveland() 


###
df1 <- df %>% group_by(site_id,year) %>% summarise(count = n())
df2 <- df1 %>% group_by(site_id,year) %>% summarise(sumar= sum(count))
mean(df2$sumar) 
sd(df2$sumar)


#Tonda plot
head(df)
df$site_id <- as.factor(df$site_id)
df$year <- as.factor(df$year)
str(df)

ggplot(df, aes(year,site_id,color=country)) + geom_point() + theme_bw() + 
  scale_color_manual(values = c("red", "blue", "green","purple","grey","yellow",
                                "pink","darkred","tan3"))






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



unique(c$country)




df<- df %>% filter(country=="Spain")

f<- semi_join(b,df, by="site_id")
unique(df$site_id)
unique(f$site_id)

f<-f %>% group_by(site_id,year) %>% summarise(abundance=n())

str(f)
f$year<- as.factor(f$year)

f <- f %>% group_by(site_id,year) %>% summarise()
f$site_id <- as.factor(f$site_id)
str(f)

f<- f %>% group_by(site_id,year) %>% summarise(abundance=sum(abundance))
                 
str(f)                          
ggplot(f, aes(year,abundance, group=site_id, color=site_id)) + geom_line()+
  geom_point() + scale_color_brewer(palette="Paired") + theme_classic2() + theme_cleveland()
 


colnames(df3)
hist(df3$Proportion)
s<- gam(Proportion~s(strahler,k=3)+s(Dist, k=3), family=nb,  data=df3)
summary(s)

T1 <- visreg(s,  scale='response', "Dist", line.par = list(col = 'black'), plot=TRUE)
T2 <- visreg(s,  scale='response', "strahler", line.par = list(col = 'green'), plot=TRUE)

