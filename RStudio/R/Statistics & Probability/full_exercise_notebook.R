#================================================================================#
#  _____           _   _                                        _              
# |  ___|  _   _  | | | |     ___  __  __   ___   _ __    ___  (_)  ___    ___ 
# | |_    | | | | | | | |    / _ \ \ \/ /  / _ \ | '__|  / __| | | / __|  / _ \
# |  _|   | |_| | | | | |   |  __/  >  <  |  __/ | |    | (__  | | \__ \ |  __/
# |_|      \__,_| |_| |_|    \___| /_/\_\  \___| |_|     \___| |_| |___/  \___|
#================================================================================#
# exercise.R: Con este unico fichero, se pretende aplicar los conocimientos aprendidos
# en el curso de estadistica, para un dataset real
#================================================================================#



rm(list=ls())

#Carga los datos de inmuebles que se encuentran en la carpeta data_in
load("./data_in/inmuebles.Rdata")

#Visualiza previamente los datos
View(data_scrap)

#-----------------------------------------------#
#--------------- Primera parte -----------------#
#-----------------------------------------------#


#Podrias aproximar cual es la media poblacional de la localidad de Arroyomolinos (Madrid)?
df_temp<-data_scrap[which(data_scrap$level5 == "Arroyomolinos (Madrid)"),]
summary(df_temp$price)
df_temp<-df_temp[which(df_temp$price>0),]

p<-df_temp$price


hist(df_temp$price, prob=T)
lines(density(df_temp$price))

lillie.test(df_temp$price)

mean(p)

t.test(p,alternative = "two.sided",mu=295000)


#-----------------------------------------------#
#--------------- Segunda parte -----------------#
#-----------------------------------------------#


#Seleccione unicamente el cinturón sur de Madrid ("Fuenlabrada","Leganés","Getafe","Alcorcón")

data_scrap<-data_scrap[data_scrap$level5 %in% c("Fuenlabrada","Leganés","Getafe","Alcorcón"),]



# Calcula la media y varianza muestral de las variables precio, habitaciones, superficie y banos
sapply(data_scrap[,c("price","rooms","surface","bathrooms")],function(x){round(mean(x, na.rm=T),2)})

#Realice el calculo por localidad
aggregate(data_scrap[,c("price","rooms","surface","bathrooms")], by = list(loc = data_scrap$level5), FUN = function(x){round(mean(x, na.rm=T),2)})


#Cual es la vivienda mas cara de cada localidad?
aggregate(data_scrap[,c("price")], by = list(localidad=data_scrap$level5), FUN = function(x){round(max(x),2)})




#-----------------------------------------------#
#--------------- Tercera parte -----------------#
#-----------------------------------------------#


#Grafica un mapa con los data_scrap que tienes


pal <- colorFactor(
  palette = 'Dark2',
  domain = data_scrap$level5
)



my_map <- leaflet() %>%
  addTiles() %>%  # use the default base map which is OpenStreetMap tiles
  addCircleMarkers(lng=data_scrap$longitude,lat=data_scrap$latitude, radius = 5,  popup=data_scrap$id_realEstates ,color = pal(NULL))
my_map


#que observas? soluciona el problema (si lo hay). Latitud y longitud cero, son outliers

data_scrap<-data_scrap[which(data_scrap$latitude !=0 | data_scrap$longitude!=0),]

my_map <- leaflet() %>%
  addTiles() %>%  # use the default base map which is OpenStreetMap tiles
  addCircleMarkers(lng=data_scrap$longitude,lat=data_scrap$latitude, radius = 5,  popup=factor(data_scrap$id_realEstates) ,color = pal(data_scrap$level5))
my_map



#que puedes decir de las variables de precio y superficie en metros cuadrados?


cor(data_scrap$surface,data_scrap$price)


#soluciona el problema (si lo hay)

data_scrap<-data_scrap[which(!is.na(data_scrap$surface) & !is.na(data_scrap$price)),]

cor(data_scrap$surface,data_scrap$price)


plot(data_scrap$surface,data_scrap$price)

lm_model<-lm(price ~ surface,data=data_scrap)

abline(lm_model,col="red",lwd=3)

summary(lm_model)


cor.test(data_scrap$surface,data_scrap$price)


#Hay algo que te llame la atención en el plot anterior?

data_scrap<-data_scrap[which(data_scrap$price !=0),]

price_norm<-(data_scrap$price-mean(data_scrap$price))/sd(data_scrap$price)

lm_model<-lm(price_norm ~ data_scrap$surface)
plot(data_scrap$surface,price_norm)
abline(lm_model,col="red",lwd=3)
summary(lm_model)


#-----------------------------------------------#
#---------------- Cuarta parte -----------------#
#-----------------------------------------------#


#esta tu conjunto libre de outliers para las variables precio, habitaciones, superficie y banos?


p1<-plot_ly(data_scrap,y=~price,type = "box",name="price")
p2<-plot_ly(data_scrap,y=~rooms,type = "box",name="rooms")
p3<-plot_ly(data_scrap,y=~bathrooms,type = "box",name="bathrooms")
p4<-plot_ly(data_scrap,y=~surface,type = "box",name="surface")


p <- subplot(p1, p2,p3,p4,nrows=2)
p

#Intenta de nuevo eliminando NA's


na.omit(data_scrap)
data_scrap[rowSums(is.na(data_scrap)) < 2, ]


data_scrap$zipCode<-NULL
data_scrap$customZone<-NULL

nrow(na.omit(data_scrap))

data_scrap<-na.omit(data_scrap)





#-----------------------------------------------#
#---------------- Quinta parte -----------------#
#-----------------------------------------------#




#Como eliminarias los outliers?


plot_ly(NULL, y = ~NULL, x = ~NULL, type = "box",color = ~level5)

plot_ly(NULL, y = ~NULL, x = ~NULL, type = "box",color = ~level5)


by(data = data_scrap,INDICES = data_scrap$level5,FUN = function(x){ boxplot.stats(x$price)$out})


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}



library(dplyr)
data_scrap <- data_scrap %>%
  group_by(level5) %>%
  mutate(price = remove_outliers(price))


data_scrap <- data_scrap %>%
  group_by(level5) %>%
  mutate(surface = remove_outliers(surface))

data_scrap<-na.omit(as.data.frame(data_scrap))




#-----------------------------------------------#
#----------------- Sexta parte -----------------#
#-----------------------------------------------#



#Algo que puedas decir de la distribucion de la variable precio por cada localidad?

par(mfrow = c(2,2))
qqnorm(data_scrap[data_scrap$level5 == NULL,"price"], main = "Fuenlabrada")
qqline(data_scrap[data_scrap$level5 == NULL,"price"])
qqnorm(data_scrap[data_scrap$level5 == NULL,"price"], main = "Leganés")
qqline(data_scrap[data_scrap$level5 == NULL,"price"])
qqnorm(data_scrap[data_scrap$level5 == NULL,"price"], main = "Getafe")
qqline(data_scrap[data_scrap$level5 == NULL,"price"])
qqnorm(data_scrap[data_scrap$level5 == NULL,"price"], main = "Alcorcón")
qqline(data_scrap[data_scrap$level5 == NULL,"price"])



#Realiza un t-test para determinar si la media de precios en Getafe es 250000
df_temp<-data_scrap[NULL,]


#hallamos el t-estadistico y otra informacion
getafeTtest<-t.test(NULL,alternative = "two.sided",mu=NULL)
getafeTtest

randT<-rt(30000,df=NROW(df_temp)-1)

#graficamos
ggplot(data.frame(x=randT))+
  geom_density(aes(x=x),fill="grey",color="grey") +
  geom_vline(xintercept = getafeTtest$statistic) + #El estadístico T es la linea continua
  geom_vline(xintercept = mean(randT) + c(-2,2)*sd(randT),linetype=2) #Media más o menos una desviación



#-----------------------------------------------#
#--------------- Septima parte -----------------#
#-----------------------------------------------#


#Que puedes decir del precio por metro cuadrado entre las localidades de Getafe y Alcorcón

df_temp<-data_scrap[NULL,]
df_temp$pricem2<-NULL/NULL
t.test(NULL~NULL,data=df_temp,var.equal=F) 





#-----------------------------------------------#
#---------------- Octava parte -----------------#
#-----------------------------------------------#

#Realiza un ANalysis Of VAriance para la media de todas las localidades


data_scrap$pricem2<-NULL/NULL
aggregate(NULL~NULL,data_scrap,function(x){mean(x,na.rm=T)})
anova <- aov(NULL~NULL,data=NULL)
summary(anova)
plot(anova)




#-----------------------------------------------#
#---------------- Novena parte -----------------#
#-----------------------------------------------#


#Vuelve a cargar los datos, es igual la media de precios de Valdemorillo y Galapagar?


rm(list=ls())


load("./data_in/inmuebles.Rdata")
data_scrap<-data_scrap[NULL,]




aggregate(NULL~NULL,NULL,function(x){mean(x,na.rm=T)})

ansari.test(NULL ~NULL,NULL)

t.test(NULL~NULL,data=NULL,var.equal=T)





#Que pasa si hacemos el mismo analisis para precio/m2

data_scrap$pricem2<-NULL

aggregate(NULL~NULL,NULL,function(x){mean(x,na.rm=T)})

ansari.test(NULL ~NULL,NULL)

t.test(NULL~NULL,data=NULL,var.equal=T)



#================================================================================#
#  _____               _     _____                               _              
# | ____|  _ __     __| |   | ____| __  __   ___   _ __    ___  (_)  ___    ___ 
# |  _|   | '_ \   / _` |   |  _|   \ \/ /  / _ \ | '__|  / __| | | / __|  / _ \
# | |___  | | | | | (_| |   | |___   >  <  |  __/ | |    | (__  | | \__ \ |  __/
# |_____| |_| |_|  \__,_|   |_____| /_/\_\  \___| |_|     \___| |_| |___/  \___|
#================================================================================#                                                              


