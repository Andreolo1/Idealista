library('arules')
library('leaflet')

vendidos = read.csv('pisosR.csv', sep = ',')

names(vendidos)[names(vendidos) == 'ad_characteristics_bathNumber'] <- 'baños'

names(vendidos)[names(vendidos) == 'ad_characteristics_constructedArea'] <- 'm2'

names(vendidos)[names(vendidos) == 'ad_characteristics_hasGarden'] <- 'jardin'

names(vendidos)[names(vendidos) == 'ad_characteristics_hasLift'] <- 'ascensor'

names(vendidos)[names(vendidos) == 'ad_characteristics_hasParking'] <- 'parking'

names(vendidos)[names(vendidos) == 'ad_characteristics_hasSwimmingPool'] <- 'piscina'

names(vendidos)[names(vendidos) == 'ad_characteristics_hasTerrace'] <- 'terraza'

names(vendidos)[names(vendidos) == 'ad_characteristics_roomNumber'] <- 'habitaciones'

names(vendidos)[names(vendidos) == 'ad_condition_isGoodCondition'] <- 'buenas_condiciones'

names(vendidos)[names(vendidos) == 'ad_condition_isNeedsRenovating'] <- 'a_reformar'

names(vendidos)[names(vendidos) == 'ad_owner_type'] <- 'tipo_vendedor'

names(vendidos)[names(vendidos) == 'ad_typology'] <- 'tipo_inmueble'

names(vendidos)[names(vendidos) == 'price'] <- 'precio'

names(vendidos)[names(vendidos) == 'price_last'] <- 'precio_final'

names(vendidos)[names(vendidos) == 'tiempo_venta'] <- 'dias_venta'



View(vendidos)
summary(vendidos$m2)

m = vendidos$m2
vendidos$pricem2 = discretize(m, method='frequency', breaks=6, labels=c('30-70', '71-86', '87-99', '100-118', '119-148', '149-750'))

la = vendidos$latitude
lo = vendidos$longitude

valencia = data.frame(lat = la, lng = lo)


pale = rainbow(6)
leaflet(valencia) %>%
  addTiles() %>%
  addCircles(weight = 3 # grosor de circulos
             ,radius = 4, # radio de los circulos proporcional a la clase perteneciente
             color = pale[vendidos$pricem2], # color diferenciador de clases
             popup = as.character(vendidos$pricem2))%>% 
  addLegend(position = "bottomleft" ,colors = pale,
            labels = paste("-->", c('30-70', '71-86', '87-99', '100-118', '119-148', '149-750')),title = "Etiqueta") 

# distrito por rango de valores
rango1 = table(vendidos$distrito[vendidos$pricem2 == '30-70'])
rango2 = table(vendidos$distrito[vendidos$pricem2 == '71-86'])
rango3 = table(vendidos$distrito[vendidos$pricem2 == '87-99'])
rango4 = table(vendidos$distrito[vendidos$pricem2 == '100-118'])
rango5 = table(vendidos$distrito[vendidos$pricem2 == '119-148'])
rango6 = table(vendidos$distrito[vendidos$pricem2 == '149-750'])

numero = data.frame(table(vendidos$distrito))
numero1 = data.frame(rango1)
numero2 = data.frame(rango2)
numero3 = data.frame(rango3)
numero4 = data.frame(rango4)
numero5 = data.frame(rango5)
numero6 = data.frame(rango6)

# juntamos todo
f = merge(x = numero, y = numero1, by = 'Var1', all = TRUE)
i = merge(x = f, y = numero2, by = 'Var1', all = TRUE)
colnames(numero3) = c('Var1', 'f')
n = merge(x = i, y = numero3, by = 'Var1', all =TRUE)
colnames(numero4) = c('Var1', 'f2')
a = merge(x = n, y = numero4, by = 'Var1', all = TRUE)
colnames(numero5) = c('Var1', 'f3')
l = merge(x = a, y = numero5, by = 'Var1', all = TRUE)
colnames(numero6) = c('Var1', 'f4')
final = merge(x = l, y = numero6, by = 'Var1', all = TRUE)


colnames(final) = c('distrito','total', '30-70', '71-86', '87-99', '100-118', '119-148', '149-750')
final[is.na(final)] <- 0

# sacamos probabilidades
final_p = data.frame('distrito' = final$distrito)
final_p$`30-70` = final$`30-70`/final$total
final_p$`71-86` = final$`71-86` / final$total
final_p$`87-99` = final$`87-99` / final$total
final_p$`100-118` = final$`100-118` / final$total
final_p$`119-148` = final$`119-148` / final$total
final_p$`149-750` = final$`149-750` / final$total

View(final_p)





