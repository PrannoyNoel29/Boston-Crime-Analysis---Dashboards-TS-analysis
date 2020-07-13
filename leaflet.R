library(maps)
library(dplyr)
library(leaflet)
library(geojsonR)
library(sf)

df = read.csv("crime.csv")
df = na.omit(df)
df = df[df$Lat > -1,]
cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
"))

leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(Pop) * 30, popup = ~City
  )

# for all district, get center (long and lat):

df$Lat = as.numeric(df$Lat)
df$Long = as.numeric(df$Long)

districts = aggregate(x = df[,c('Lat','Long')], by = list(df$DISTRICT), function(x) mean(as.numeric(as.character(x))))
districts

summary(df$Lat)
summary(df$Long)
summary(df$DISTRICT)

districts = districts[-1,]
districts$size = 1
colnames(districts)[1] = "DISTRICT"


get_map <- function(districts) {
  leaflet(districts) %>% addTiles() %>%
    addCircles(lng = ~Long, lat = ~Lat, weight = 1,
               radius = ~sqrt(size) * 1000, popup = ~DISTRICT
    )
}

get_map(districts)


boundaries = FROM_GeoJson(url_file_string = "Boston_Neighborhoods.geojson")
boundaries_polygones = data.frame(length(boundaries$features))


#CartoDB.Positron

leaflet() %>% addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(data = boundaries[["features"]][[2]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[3]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[4]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[5]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[6]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[7]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[8]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[9]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[10]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[11]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[12]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  #  addPolygons(data = boundaries[["features"]][[14]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
  #              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[15]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[16]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[17]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[18]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[19]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[20]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[21]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  #  addPolygons(data = boundaries[["features"]][[22]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
  #              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[23]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  #  addPolygons(data = boundaries[["features"]][[24]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
  #              opacity = 1.0) %>%
  addPolygons(data = boundaries[["features"]][[25]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
              opacity = 1.0) %>%
  #  addPolygons(data = boundaries[["features"]][[26]][["geometry"]][["coordinates"]],stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0, weight = 1,
  #              opacity = 1.0)
  addCircles(data = districts, lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(size) * 1000, popup = ~DISTRICT
  )


leaflet(boundaries$features[[1]]$geometry$coordinates[[1]]) %>% addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1, weight = 1,
              opacity = 1.0)#,
#fillColor = ~pal(log10(pop)),
#label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
#addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
#labFormat = labelFormat(transform = function(x) round(10^x)))


leaflet(nycounties) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop)),
              label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
  addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x)))

centers_shooting


police_districts = st_read("/Users/michaelullah/Documents/DSBA/ESSEC/Big\ Data\ Analytics/crimes-in-boston/Police_Districts/Police_Districts.shp")
st_geometry_type(police_districts$geometry)

st_geometry_type(police_districts$geometry[1])