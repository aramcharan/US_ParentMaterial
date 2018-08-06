library(plotKML)
library(rgdal)
library(raster)
library(plotKML)
#This RSAGA version has been tested with SAGA GIS versions 2.0.4 - 2.2.2.
library(RSAGA)


if(.Platform$OS.type=="windows"){
  saga_cmd = "C:\\Program Files\\saga_2.2.2_win32/saga_cmd.exe"
}
saga_cmd


#### Converting polygon to raster maps
data("eberg_grid25")
gridded(eberg_grid25) <- ~x+y
proj4string(eberg_grid25) <- CRS("+init=epsg:31467")
r <- raster(eberg_grid25)
r
data(eberg_zones)
eberg_zones_r <- rasterize(eberg_zones, r, field="ZONES")
names(eberg_zones)
eberg_zones_r <- rasterize(eberg_zones, r, field="ZONES")
plot(eberg_zones_r)

eberg_zones$ZONES_int <- as.integer(eberg_zones$ZONES)
writeOGR(eberg_zones["ZONES_int"], "eberg_zones.shp", ".", "ESRI Shapefile")
pix = 25


##grid_gridding does not work
system(paste0(saga_cmd, ' grid_gridding 0 -INPUT \"eberg_zones.shp\" ',
              '-FIELD \"ZONES_int\" -GRID \"eberg_zones.sgrd\" -GRID_TYPE 0 ',
              '-TARGET_DEFINITION 0 -TARGET_USER_SIZE ', pix, ' -TARGET_USER_XMIN ', 
              extent(r)[1]+pix/2,' -TARGET_USER_XMAX ', extent(r)[2]-pix/2, 
              ' -TARGET_USER_YMIN ', extent(r)[3]+pix/2,' -TARGET_USER_YMAX ', 
              extent(r)[4]-pix/2))
eberg_zones_r2 <- readGDAL("eberg_zones.sdat")


data(eberg_grid)
gridded(eberg_grid) <- ~x+y
proj4string(eberg_grid) <- CRS("+init=epsg:31467")
names(eberg_grid)


writeGDAL(eberg_grid["TWISRT6"], "eberg_grid_TWISRT6.tif")
system(paste0('gdalwarp eberg_grid_TWISRT6.tif',
              ' eberg_grid_TWISRT6_25m.tif -r \"cubicspline\" -te ', 
              paste(as.vector(extent(r))[c(1,3,2,4)], collapse=" "),
              ' -tr ', pix, ' ', pix, ' -overwrite'))



#### Create DEM derivatives
saga_DEM_derivatives <- function(INPUT, MASK=NULL, sel=c("SLP","TWI","CRV","VBF","VDP","OPN","DVM")){
  if(!is.null(MASK)){
    ## Fill in missing DEM pixels:
    suppressWarnings( system(paste0(saga_cmd, 
                                    ' grid_tools 25 -GRID=\"', INPUT, 
                                    '\" -MASK=\"', MASK, '\" -CLOSED=\"', 
                                    INPUT, '\"')) )
  }
  ## Slope:
  if(any(sel %in% "SLP")){
    try( suppressWarnings( system(paste0(saga_cmd, 
                                         ' ta_morphometry 0 -ELEVATION=\"', 
                                         INPUT, '\" -SLOPE=\"', 
                                         gsub(".sgrd", "_slope.sgrd", INPUT), 
                                         '\" -C_PROF=\"', 
                                         gsub(".sgrd", "_cprof.sgrd", INPUT), '\"') ) ) )
  }
  ## TWI:
  if(any(sel %in% "TWI")){
    try( suppressWarnings( system(paste0(saga_cmd, 
                                         ' ta_hydrology 15 -DEM=\"', 
                                         INPUT, '\" -TWI=\"', 
                                         gsub(".sgrd", "_twi.sgrd", INPUT), '\"') ) ) )
  }
  ## MrVBF:
  if(any(sel %in% "VBF")){
    try( suppressWarnings( system(paste0(saga_cmd, 
                                         ' ta_morphometry 8 -DEM=\"', 
                                         INPUT, '\" -MRVBF=\"',
                                         gsub(".sgrd", "_vbf.sgrd", INPUT),
                                         '\" -T_SLOPE=10 -P_SLOPE=3') ) ) )
  }
  ## Valley depth:
  if(any(sel %in% "VDP")){
    try( suppressWarnings( system(paste0(saga_cmd, 
                                         ' ta_channels 7 -ELEVATION=\"', 
                                         INPUT, '\" -VALLEY_DEPTH=\"', 
                                         gsub(".sgrd", "_vdepth.sgrd", 
                                              INPUT), '\"') ) ) )
  }
  ## Openess:
  if(any(sel %in% "OPN")){
    try( suppressWarnings( system(paste0(saga_cmd, 
                                         ' ta_lighting 5 -DEM=\"', 
                                         INPUT, '\" -POS=\"', 
                                         gsub(".sgrd", "_openp.sgrd", INPUT), 
                                         '\" -NEG=\"', 
                                         gsub(".sgrd", "_openn.sgrd", INPUT), 
                                         '\" -METHOD=0' ) ) ) )
  }
  ## Deviation from Mean Value:
  if(any(sel %in% "DVM")){
    suppressWarnings( system(paste0(saga_cmd, 
                                    ' statistics_grid 1 -GRID=\"', 
                                    INPUT, '\" -DEVMEAN=\"', 
                                    gsub(".sgrd", "_devmean.sgrd", INPUT), 
                                    '\" -RADIUS=11' ) ) )
  }
}

#### Doesn't work
writeGDAL(eberg_grid["DEMSRT6"], "DEMSRT6.sdat", "SAGA")
saga_DEM_derivatives("DEMSRT6.sgrd")
dem.lst <- list.files(pattern=glob2rx("^DEMSRT6_*.sdat"))
plot(stack(dem.lst), col=SAGA_pal[[1]])

