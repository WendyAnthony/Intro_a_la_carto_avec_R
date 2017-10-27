# libraries
library(sf)
library(readxl)
library(cartography)

# Téléchargement
# download.file(url = "https://raw.githubusercontent.com/riatelab/basemaps/master/World/countries.geojson",
#               destfile = "data/country.geojson")
# download.file(url = "https://raw.githubusercontent.com/riatelab/basemaps/master/World/graticule30.geojson",
#               destfile = "data/graticule.geojson")
# download.file(url = "https://esa.un.org/unpd/wup/CD-ROM/WUP2014_XLS_CD_FILES/WUP2014-F12-Cities_Over_300K.xls", 
#               destfile = "data/citypop.xls")
# download.file(url = "https://esa.un.org/unpd/wup/CD-ROM/WUP2014_XLS_CD_FILES/WUP2014-F14-Growth_Rate_Cities.xls", 
#               destfile = "data/cityevo.xls")

# import
country <- st_read(dsn = "data/country.geojson")
graticule <- st_read(dsn = "data/graticule.geojson")
citypop <- data.frame(read_excel("data/citypop.xls", skip = 16))
cityevo <- data.frame(read_excel("data/cityevo.xls", skip = 16))

# jointure entre les 2 tableaux de données
city <- merge(citypop, cityevo[, c(4,9:24)], by = "City.Code")

# construction de la couche city
city <- st_as_sf(city, coords = c("Longitude","Latitude"), crs = "+init=epsg:4326")

# transformation de la projection WGS84 => Robinson
projRobinson <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
country <- st_transform(x = country, crs = projRobinson)
graticule <- st_transform(x = graticule, crs = projRobinson)
city <- st_transform(x = city, crs = projRobinson)

# Cartes
plot(st_geometry(graticule))
plot(st_geometry(country), add = TRUE)
plot(st_geometry(city), add = TRUE)
title(main ="Agglomérations urbaines de plus de 300 000 habitants en 2014", 
      sub = "Source : WUP 2014")


par(mar = c(0,0,1.2,0), bg = "ivory1")
plot(st_geometry(graticule), col = "lightblue", border = "white", lwd = 0.2)
plot(st_geometry(country), col = "ivory4", border ="ivory3", lwd = 0.5, add=TRUE)
plot(st_geometry(city), pch = 21, cex = 0.5, lwd = 1, col = "white", bg = "red", add = TRUE)
mtext(text = "Agglomérations urbaines de plus de 300 000 habitants en 2014", side = 3, line = -0.75)
mtext(text = "Source : WUP 2014", side = 1, line = -1, adj = 0.99, cex = 0.75)



par(mar = c(0,0,1.2,0), bg = "ivory1")
plot(st_geometry(graticule), col = "lightblue", border = "white", lwd = 0.2)
plot(st_geometry(country), col = "ivory4", border ="ivory3", lwd = 0.5, add=TRUE)
propSymbolsLayer(x = city, var = "X2015")
layoutLayer(title = "Population des agglomérations urbaines en 2015", 
            sources = "WUP, 2014", author = "T. Giraud, 2017", col = "ivory1", 
            coltitle = "black", scale = NULL)



par(mar = c(0,0,1.2,0), bg = "ivory1")
plot(st_geometry(graticule), col = "lightblue", border = "white", lwd = 0.2)
plot(st_geometry(country), col = "ivory4", border ="ivory3", lwd = 0.5, add=TRUE)
propSymbolsLayer(x = city, var = "X2015", col = "#921010", border = "white", 
                 lwd = 0.5, symbols = "circle", inches = 0.2, 
                 legend.pos = "topleft", legend.title.cex = 0.7,
                 legend.title.txt = "Population en 2015\n(en milliers)")
layoutLayer(title = "Population des agglomérations urbaines en 2015", 
            sources = "WUP, 2014", author = "T. Giraud, 2017", col = "ivory2", 
            coltitle = "black", scale = NULL, boxtitle = "small", frame = FALSE)

getFigDim(x = graticule, width = 1200, mar = c(0,0,1.2,0), res = 120)




png(filename = "fig/P2015.png", width = 1200, height = 637, res = 120)
par(mar = c(0,0,1.2,0), bg = "white")
plot(st_geometry(graticule), col = "lightblue", border = "white", lwd = 0.2, bg = "cornsilk2")
plot(st_geometry(country) + c(100000,-100000), add=T, col = "grey30", border = NA)
plot(st_geometry(country), col = "ivory4", border ="ivory3", lwd = 0.5, add=TRUE)
propSymbolsLayer(x = city, var = "X2015", col = "#721010", border = "white", 
                 lwd = 0.5, symbols = "circle", inches = 0.2, 
                 legend.pos = "n")
legendCirclesSymbols(pos = c(-13317499, -6284211 ), 
                     title.txt = "Population en 2015\n(en milliers)", 
                     title.cex = 0.6, cex = 0.75, col = "#921010",
                     var = c(300, 5000, 20000, 38000),
                     inches = 0.2, style = "e")
layoutLayer(title = "Population des agglomérations urbaines en 2015", 
            sources = "WUP, 2014", author = "T. Giraud, 2017", col = "black", 
            coltitle = "white", scale = NULL, frame = FALSE, boxtitle = "small", 
            aligntitle = "left")
dev.off()





png(filename = "fig/P2015evo.png", width = 1200, height = 637, res = 120)
par(mar = c(0,0,1.2,0), bg = "white")
plot(st_geometry(graticule), col = "lightblue", border = "white", lwd = 0.2, bg = "cornsilk2")
plot(st_geometry(country) + c(100000,-100000), add=T, col = "grey30", border = NA)
plot(st_geometry(country), col = "ivory4", border ="ivory3", lwd = 0.5, add=TRUE)
bks <- c(min(city$X2010.2015), 0, 1, 2, 3, 4, max(city$X2010.2015))
cols <- carto.pal("wine.pal", 1, "green.pal", 5)
propSymbolsChoroLayer(x = city, var = "X2015", border = "white",legend.title.cex = 0.6,
                 lwd = 0.5, symbols = "circle", inches = 0.2,
                 var2 = "X2010.2015", breaks = bks, col = cols,
                 legend.var.title.txt = "Population en 2015\n(en milliers)",
                 legend.var2.title.txt = "Taux de croissance\nannuel moyen\n(2000-2015)",
                 legend.var.pos = "topright", 
                 legend.var2.pos = "topleft", legend.values.cex = 0.6)
layoutLayer(title = "Population des agglomérations urbaines en 2015", 
            sources = "WUP, 2014", author = "T. Giraud, 2017", col = "black", 
            coltitle = "white", scale = NULL, frame = FALSE, boxtitle = "small", 
            aligntitle = "left")
dev.off()


png(filename = "fig/P2015evoCHIIND.png", width = 861, height = 700, res = 120)
par(mar = c(0,0,1.2,0), bg = "white")
lest <- st_bbox(country[country$ISO3 %in% c('CHN', "IND", "JPN"),])
plot(st_geometry(graticule), col = "lightblue", border = "white", lwd = 0.2, 
     bg = "cornsilk2", xlim = lest[c(1,3)], ylim = lest[c(2,4)])
plot(st_geometry(country) + c(20000,-20000), add=T, col = "grey30", border = NA)
plot(st_geometry(country), col = "ivory4", border ="ivory3", lwd = 0.5, add=TRUE)
propSymbolsChoroLayer(x = city, var = "X2015", border = "white",legend.title.cex = 0.6,
                      lwd = 0.5, symbols = "circle", inches = 0.2,
                      var2 = "X2010.2015", breaks = bks, col = cols,
                      legend.var.title.txt = "Population en 2015\n(en milliers)",
                      legend.var2.title.txt = "Taux de croissance\nannuel moyen\n(2000-2015)",
                      legend.var.pos = "topright", legend.var.frame = T,legend.var2.frame = TRUE,
                      legend.var2.pos = "bottomright", legend.values.cex = 0.6)
layoutLayer(title = "Population des agglomérations urbaines en 2015", 
            sources = "WUP, 2014", author = "T. Giraud, 2017", col = "black", 
            coltitle = "white", scale = NULL, frame = FALSE, boxtitle = "small", 
            aligntitle = "left")
dev.off()




# c3 <- c2
# st_geometry(c3) <- NULL
# x <- t(apply(c3[, 7:23], 1, FUN = function(x){approx(y = x, x = seq(1950,2030,5), xout = (1950:2030))$y}))
# dimnames(x) <- c(list(c2$City.Code), list(paste0('PX', 1950:2030)))
# c4 <- cbind(c2,x)
# library(animation)
# x <- function(){
#   for (i in 24:104){
#     par(mar = c(0,0,1.2,0))
#     plot(g2$geometry, col = "lightblue", border = "white", lwd = 0.5)
#     plot(w2$geometry, add=T, col = "ivory4", border ="ivory3", lwd = 0.5)
#     propSymbolsLayer(x = c4[c4[[names(c4)[i]]]>100,], var = names(c4)[i], col = "#921010", legend.pos = "left",fixmax = 38000,
#                      border = "white", lwd = 0.8, symbols = "circle", inches = 0.4)
#   }
# }
# ani.options( ani.height = 828, interval = 0.1,
#              ani.width = 1600)
# saveGIF(expr = x(),   movie.name = "animation.gif", img.name = "Rplot")

