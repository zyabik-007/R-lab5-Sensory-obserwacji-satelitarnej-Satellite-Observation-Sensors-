# install.packages("rgdal")
# install.packages("raster")
library(rgdal)
library(raster)

getCV = function(x)
{
  sdX = sd(x)
  meanX = mean(x)
  CV = sdX / meanX
  return(CV * 100)
}

punktyTest = data.frame(lon = c(18.703433, 18.616147, 18.643683), lat = c(54.426767, 54.415559, 54.389409), id = as.factor(c("woda", "las", "miasto")))
gdansk = stack('gdansk_2015.tif')

png("gdansk_2015.5.png")
plot(gdansk$gdansk_2015.5)
dev.off()

png("gdansk rgb.png")
plotRGB(gdansk, r = 4, g = 3, b = 2, scale = 2^16)
dev.off()

png("gdansk infrared.png")
plotRGB(gdansk, r = 5, g = 4, b = 3, scale = 2^16)
dev.off()


reflGdansk = (gdansk / 0.00002) - 0.1
png("gdansk refl.png")
plot(reflGdansk)
dev.off()

test = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
cv = getCV(test)
print(cv)
с = calc(reflGdansk, getCV)
plot(c)

newReflGdansk = coordinates(punktyTest) = ~lon + lat
print(newReflGdansk)
print(punktyTest)

proj4string(punktyTest) = CRS("+init=epsg:4326")

punktyUTM = spTransform(punktyTest, proj4string(punktyTest))
print(punktyUTM)
print(reflGdansk)
# pos = points(gdansk)######
# pritnt(pos)
extractVal = extract(reflGdansk, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
print(extractVal)
png("gdansk extractVal.png")
plot(extractVal)
dev.off()

getMask = function(reflRaster, prog = .1)
{
  # wyświetlanie rastra reflRaster z reflektancją funkcją plotRGB()
  png("getMask_rgb.png")
  plotRGB(reflRaster, r = 4, g = 3, b = 2)
  dev.off()
  # (już zaimplementowane)
  # coord = data.frame(do.call("cbind", locator(1)))
  x = 348314
  y = 6031570
  coord = data.frame(x = x, y = y)
  # nadanie odpowiedniego układu współrzędnych obiektowi coord
  # UWAGA nazwy kolumn w obiekcie coord to "x" i "y" a nie "lon" i "lat"
  # jak było w przypadku punktyTest

  coordinates(coord) = ~x + y
  # proj4string(coord) = CRS("+init=epsg:4326")
  proj4string(coord) = proj4string(reflRaster)
  punktyUTM = spTransform(coord, proj4string(coord))
  # print(punktyUTM)

  # pobranie wartości pikseli w miejscach wskazanych przez coord
  # i zapisanie wyniku jako nowy obiekt o nazwie wzor


  # wykorzystanie wcześniej zdefiniowanej funkcji sam()
  # aby obliczyć kąt spektralny między obiektem wzor,
  # a każdym pikselem zobrazowania reflRaster
  # i zapisanie wyniku jako nowy obiekt wynikSAM
  # UWAGA aby przekazać wzor jako argument
  # utworzono funkcję sam0, nie przejmuj się tym
  # uzyj w calc funkcji sam z jednym argumentm
  sam = function(x) { sam0(x, vecWzor = wzor[1,]) }

  sam0 = function(..., vecWzor)
  {
    a = 1 * ...
    b = vecWzor
    sam = acos(sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))) #podmień NULL na właściwy wzór
    return(sam)
  }

  wzor = extract(reflRaster, punktyUTM)
  print(wzor)
  wynikSAM = calc(reflRaster, fun = sam)
  #tu zacznij calc:
  wynikMask = wynikSAM < prog
  # wykonanie wyrażenia logicznego
  # które wskaże obiekty o kącie mniejszym
  # niż zdefiniowana przez użytkownika wartość prog
  # wynik wyrażenia zapisujemy jako obiekt wynikMask

  # plot(wynikMask)
  return(wynikMask)
}


ret = getMask(reflGdansk)

png("getMask.png")
plot(ret)
dev.off()