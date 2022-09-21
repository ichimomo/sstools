areadef.default <-
function (lon, lat, lats, lons, char = 1) 
{
    areadef <- as.numeric(lat >= lats[1] & lat < lats[2] & lon >= 
        lons[1] & lon < lons[2])
    areadef <- ifelse(areadef == 1, char, 0)
    areadef
}
