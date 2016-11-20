library(maptools)
library(rgdal)
library(sp)
library(rgeos)
library(data.table)

house = readShapePoly("/media/data_drive/gis_data/USA/tl_2014_us_cd114.shp")

#Senate races
states = readOGR("/media/data_drive/gis_data/USA/states.shp",
                 layer = "states", stringsAsFactors = FALSE)

#Alaska & Hawaii screw up plotting
states = states[!states$STATE_NAME %in% c("Alaska", "Hawaii"), ]

cols = c(Arizona = "darkgreen", Mississippi = "goldenrod1",
         Nebraska = "goldenrod1", Nevada = "darkgreen",
         Tennessee = "orangered", Texas = "goldenrod1",
         Utah = "orangered", Wyoming = "red")

cols[setdiff(states$STATE_NAME, names(cols))] = "white"

pdf("~/Desktop/maps/senate.pdf")
plot(states, col = cols[states$STATE_NAME], 
     main = "Republican Seats in 2018 Senate Elections")
dev.off()

states = gBuffer(gUnaryUnion(states), byid = TRUE, width = 0)

inside = gIntersects(states, house, byid = TRUE)

house = SpatialPolygonsDataFrame(
  gIntersection(states, house, byid = TRUE),
  data = house@data[inside[ , 1L], ], match.ID = FALSE)

house@data = setDT(copy(house@data))

fp_match = data.table(state = c('AZ', 'CA', 'CO', 'FL', 'IL', 'IA', 'MI',
                                'MN', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY',
                                'OH', 'PA', 'TX', 'VA', 'WA', 'WI'),
                      fips = c('04', '06', '08', '12', '17', '19', '26',
                               '27', '31', '32', '33', '34', '35', '36',
                               '39', '42', '48', '51', '53', '55'))

house@data[fp_match, state := i.state, on = c(STATEFP = "fips")]

house@data[ , dist_no := 
              as.integer(gsub("Congressional District ", "", NAMELSAD))]

house@data[ , ord := seq_len(.N)]

competitive = 
  list(list(st = "AZ", di = 2), list(st = "CA", di = c(10, 25, 39, 49)),
       list(st = "CO", di = c(3, 6)),
       list(st = "FL", di = c(6, 15, 18, 26, 27)),
       list(st = "IL", di = c(6, 12, 13, 14)),
       list(st = "IA", di = c(1, 3)),
       list(st = "MI", di = c(1, 7, 8, 11)),
       list(st = "MN", di = c(2, 3)), list(st = "NE", di = 2),
       list(st = "NJ", di = 7),
       list(st = "NY", di = c(1, 19, 22, 23)),
       list(st = "PA", di = c(6, 8, 16)),
       list(st = "TX", di = 23), list(st = "VA", di = c(5, 7, 10)),
       list(st = "WA", di = 4))

for (sts in competitive) {
  with(sts, {
    png(paste0("~/Desktop/maps/", st, ".png"))
    house@data[state == st, 
               plot(house[ord, ], 
                    col = ifelse(dist_no %in% di, "red", "white"))]
    dev.off()})
}
    