############################################
##                                        ##
##  1. Open each inundation map file      ##
##  2. Convert to vector polygons         ##
##  3. Remove all polygons < 5000 sq.m    ##
##  4. Calculate water area per habitat   ##
##  5. Output each date map as poly shp   ##
##  6. Run conditionals                   ##
##  7. Run landscape metrics              ##
##  8. Run lateral connectivity metrics   ##
##  9. Still developing volume estimates  ##
##  Developed by Shruti Khanna on 12/5/23 ##
##       last updated: March, 2026        ##
##                                        ##
############################################

# libraries needed
library("ggplot2")
library("tidyverse")
library("terra")
library("readr")
library("inundation")
library("sf")
library("dplyr")
library("lubridate")
library("landscapemetrics")
library("googledrive")

######  GOOGLE FOLDERS IDs WITH WATER CLASS FILES  ######

fid.s1.wy2019 = "1c14hUh1QsH2CB3cbnlAqvzzw4mfpudXu"
fid.s1.wy2020 = "1hPJrw_p9IKSZ13UhmHk0uYl-tmTVDDi_"
fid.s1.wy2021 = "1UDRkI-lnp7TXenZlc7CUrotMaFfjgXTe"
fid.s1.wy2022 = "1UnJ83v10yAW-ZWc0mptIbymHVZISt1WA"
fid.s1.wy2023 = "1GI4xaY3nLAvL_l5FaBNyc1GE-Lh_DrDA"
fid.s1.wy2024 = "1G71Cs-571B3-1tUj1YlP5aHnDmpX9oVQ"

fid.s2.wy2019 = "1NaP7Dx6RC0fLSnXz6qUUzFee6Fq1NNH6"
fid.s2.wy2020 = "1wPcyfGX3JYwJWtUxKR4-kClaDbEkX6Kr"
fid.s2.wy2021 = "1QY0hP_Vwzw5L1YakXVJMJlFNREz1DvcD"
fid.s2.wy2022 = "1aHgdDRCARsWUh8XzFPDOhYPMJtlV-PZZ"
fid.s2.wy2023 = "12vQGq3ys4Aj7DL-GqKFgDwadRD4CVZRe"
fid.s2.wy2024 = "1HjU4UmkNAa7o4V77k26tdF9wszeoxSfl"

####################    CHANGE    ######################

# which water year is being analyzed
WY = "2023"
y1y2 = "2022_23"

# the input files.
indir.s2 = fid.s2.wy2023
indir.s1 = fid.s1.wy2023

# Authenticate
drive_auth()

# List all .tif files in the folder
files.s1 <- drive_ls(as_id(indir.s1), pattern = "\\.tif$")
files.s2 <- drive_ls(as_id(indir.s2), pattern = "\\.tif$")

# input suffix for S1 and S2 files
insuf = ".tif"

# system date
strdate = Sys.Date()

# if version needs to change
ver = "_v5"

# the output files - inundation per polygon
s1.outfname1 = paste0("tables/SH_WY", WY, "_S1_LeeS_B_", strdate, ver, "_habArea.csv")
s1.outfname2 = paste0("tables/SH_WY", WY, "_S1_LeeS_B_", strdate, ver, "_lmAll.csv")
s2.outfname1 = paste0("tables/SH_WY", WY, "_S2_AWEIsh_", strdate, ver, "_habArea.csv")
s2.outfname2 = paste0("tables/SH_WY", WY, "_S2_AWEIsh_", strdate, ver, "_lmAll.csv")

# shapefile for yolo bypass habitat polygons
shp.nm = "vectors/YoloBypassForSH_UTM_wat.shp"
# shapefile for yolo bypass parallel lines
ln.nm  = "vectors/parellel_lines.shp"
# shapefile for E-W slices of Yolo Bypass for water volume calculations
dsc.nm = "vectors/polygon_slices.shp"
# bathymetry raster to calculate water depth histograms
#bathy.nm = "X:/delta_sav/raster/Bathymetry/dem_yolo_2m_20200505/dem_yolo_2m_20200505_envi_sub.img"

# list of landscape metrics to be calculated for each flooded image
list.lsm.all = list_lsm()
lm.c.list = c("lsm_c_clumpy",  "lsm_c_cohesion")
lm.p.list = c("lsm_p_enn", "lsm_p_frac")
var.cnames = c("Ymd", "ttlAreaHa", "latConn_mn", "latConn_sd",
               "area.mn", "area.sd", "core.mn", "core.sd", 
               "para.mn", "para.sd", "clumpiness", "cohesion", 
               # "enn.mn", "enn.sd", "frac.mn", "frac.sd",
               "area.p10", "area.p50", "area.p90", "area.max", 
               "core.p10", "core.p50", "core.p90", "core.max",
               "para.p10", "para.p50", "para.p90", "para.max"
               #  "enn.p10",  "enn.p50",  "enn.p90",  "enn.max",
               # "frac.p10", "frac.p50", "frac.p90", "frac.max"
              )
hec.cnames = c("habitat", "Ymd", "ttlAreaHab_ha", "iflagf", "iflagl", "ttlAreaHa", "percWater", "iflag")

#################### END CHANGE ########################

# get list of S1 files
# images.s1 <- list.files(indir.s1, pattern = insuf.s1, full.names = TRUE)
# images.s1 <- subset(images.s1, !grepl(".aux", images.s1))
# images.s1 <- subset(images.s1, !grepl(".ovr", images.s1))
# images.s1 <- subset(images.s1, !grepl(".vat", images.s1))

# get list of S2 files
# images.s2 <- list.files(indir.s2, pattern = insuf.s2, full.names = TRUE)
# images.s2 <- subset(images.s2, !grepl(".aux", images.s2))
# images.s2 <- subset(images.s2, !grepl(".ovr", images.s2))
# images.s2 <- subset(images.s2, !grepl(".vat", images.s2))

# count of files for both sensors
nfiles.s1 <- length(files.s1[[1]])
nfiles.s2 <- length(files.s2[[1]])

# set vars for S1 files
s1.forDates <- files.s1[[1]]
s1.extDates = sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", s1.forDates)
# s1.outshpdir = "X:/delta_sentinel/vector/S1S2_waterclass/S1_LeeSigma/"

# set vars for S2 files
s2.forDates <- files.s2[[1]]
s2.extDates = sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", s2.forDates)

# read the yolo bypass habitat polygons shapefile
fd.poly <- terra::vect(shp.nm)
fd.poly$area_ha = round((terra::expanse(fd.poly)*0.0001), digits = 4)
fd.attr <- as.data.frame(values(fd.poly)) %>% rename(habitat = NAME)
no.poly <- nrow(fd.poly)

# code for flood start trigger
habArea  = fd.attr %>% group_by(habitat) %>% summarise(ttlAreaHa = sum(area_ha))
first    = fd.attr %>% filter(habitat == "First")
first_ha = first$area_ha
lastf    = fd.attr %>% filter(habitat == "Last")
lastf_ha = lastf$area_ha

# read the parallel lines in yolo bypass
pl.lns  <- terra::vect(ln.nm)
no.lns  <- nrow(pl.lns)
# read the polygon discs in yolo bypass
ew.poly <- terra::vect(dsc.nm)

# initialize previous iteration flood flag to 0 i.e. no flood
pre.iflag = 0

# Function to extract endpoints and return as spatVector points
extract_line_endpoints <- function(line_spat, raster) {
  n_lines <- nrow(line_spat)
  
  # Get line attributes
  line_attrs <- as.data.frame(line_spat)
  
  all_points <- list()
  
  for(a in 1:n_lines) {
    # Get coordinates for this line (only 2 points)
    coords_i <- crds(line_spat[a])
    
    # Create point spatVector for start point
    start_point <- vect(coords_i[1, , drop=FALSE], type="points", crs=crs(line_spat))
    start_attrs <- line_attrs[a, , drop=FALSE]
    start_attrs$endpoint <- "start"
    values(start_point) <- start_attrs
    
    # Create point spatVector for end point
    end_point <- vect(coords_i[2, , drop=FALSE], type="points", crs=crs(line_spat))
    end_attrs <- line_attrs[a, , drop=FALSE]
    end_attrs$endpoint <- "end"
    values(end_point) <- end_attrs
    
    # Combine start and end points
    all_points[[a]] <- rbind(start_point, end_point)
  }
  
  # Combine all endpoint points
  endpoint_points <- do.call(rbind, all_points)
  
  # Extract raster values at endpoints
  extracted_values <- extract(raster, endpoint_points, ID=FALSE)
  
  # Add raster values to the point spatVector
  endpoint_points$wlevel <- extracted_values[, 1]
  
  return(endpoint_points)
}


# calculate #pixels classed as water within each polygon for all S1 files
for (i in 1:nfiles.s1) {
  
  # initialize number of polygons to 0
  no_poly_trn = 0
  
  # open the from google drive folder
  tmp <- tempfile(fileext = insuf)
  drive_download(as_id(files.s1$id[i]), path = tmp, overwrite = TRUE)
  img.cls <- rast(tmp)
  
  # CONVERT THE RASTER CLASSIFICATION INTO CLASS POLYGONS
  # creates a SpatVector with just two multi-part polygons with values 0 & 1
  poly.cls = as.polygons(img.cls, dissolve = TRUE, values = TRUE)
  
  # disaggregate multi-part polygons so all are single-part polygons
  poly.sngl.cls = terra::disagg(poly.cls)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## INTERSECT THE WATER POLYGONS LAYER WITH THE HABITAT TYPE LAYER ##
  ## TO CALCULATE FLOODED AREA PER HABITAT AND APPLY DECISION TREE  ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
  # intersect with the yolo bypass polygons and crop to extent
  poly.int = terra::intersect(poly.sngl.cls, fd.poly)
  
  # dis-aggregate again to split multi-part polygons
  poly.int.sngl = terra::disagg(poly.int)
  
  # select only for water pixels and force all CommWater to be water
  poly.wat = poly.int.sngl[poly.int.sngl$class == 1 | poly.int.sngl$NAME == "CommWater", ]
  # say it is all water
  poly.wat$class = 1
  
  # dissolve adjacent polygons before calculating area
  # first aggregate the polygons on class
  poly.temp = terra::aggregate(poly.wat, by = "NAME", fun = "mean")
  # now disaggregate the polygons to make all polygons single-part
  poly.wat.hab = terra::disagg(poly.temp)
  
  # calculate area of each polygon in square meters
  poly.wat.hab$area_m2 = terra::expanse(poly.wat.hab, unit = "m")

  # filter out polygons smaller than 500 sq. meters
  poly.wat.hab.sub = poly.wat.hab[poly.wat.hab$area_m2 > 500,]

  # add date to vector attributes
  poly.wat.hab.sub$Ymd = s1.extDates[i]
  poly.attr = as.data.frame(values(poly.wat.hab.sub))
  
  # calculate total inundated area per habitat type: natural, agriculture, channel
  poly.attr = poly.attr %>% rename(habitat = NAME, class = mean_class) %>% 
    select("habitat", "class", "area_m2", "Ymd") %>% 
    mutate(area_ha = area_m2*0.0001)
  # update the attributes of the filtered water polygons to include above calculated
  values(poly.wat.hab.sub) = poly.attr
  
  # calculate total inundated area per habitat for the entire image
  z = poly.attr %>% group_by(habitat, Ymd) %>% 
    summarize(ttlAreaHab_ha = sum(area_ha)) %>% 
    # set the flag for start of a flood
    mutate(iflagf = ifelse((habitat == "First" & (ttlAreaHab_ha/first_ha) > 0.5),  1, 0)) %>% 
    mutate(iflagl = ifelse((habitat == "Last"  & (ttlAreaHab_ha/lastf_ha) > 0.33), 1, 0))
  # calculate percent cover of water for each habitat
  z = left_join(z, habArea, by = "habitat") %>% mutate(percWater = (ttlAreaHab_ha/ttlAreaHa)*100) %>% 
    # if the first polygon is flooded (>50% cover), then flood flag is TRUE
    # if the first polygon is NOT flooded, then if last polygon is flooded, then flood flag is TRUE, else FALSE
    # in essence, flood starts when the first polygon floods and then ends when the last polygon drains (< 33%)
    mutate(iflag = ifelse(iflagf == 1, 1, ifelse(pre.iflag == 1 & iflagl == 1, 1, 0)))
  
  if(sum(z$iflag) == 0) {
    # if area not in flood cycle, remove agricultural and natural land 
    poly.fldwat = poly.wat.hab.sub[(poly.wat.hab.sub$habitat != "Agriculture") & 
                                     (poly.wat.hab.sub$habitat != "Natural") & 
                                     (poly.wat.hab.sub$habitat != "Last"),]
    # adjust acreage values in z data frame accordingly (i.e. artificially set this water area to 0)
    z = z %>% mutate(ttlAreaHab_ha = ifelse((habitat == "Agriculture")|(habitat == "Natural")|(habitat == "Last"), 0, ttlAreaHab_ha),
                     percWater     = ifelse((habitat == "Agriculture")|(habitat == "Natural")|(habitat == "Last"), 0, percWater))
  } else {
    # during floods, all water polygons represent flood polygons
    poly.fldwat = poly.wat.hab.sub
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## CALCULATE LANDSCAPE METRICS THAT CAN BE CALCULATED OFF VECTORS ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
  # calculate perimeter of each polygon in meters
  poly.fldwat$perim_m = terra::perim(poly.fldwat)
  
  # calculate para (perimeter to area ratio)
  poly.fldwat$para = poly.fldwat$perim_m/poly.fldwat$area_m2
  
  # calculate core polygons with an inner buffer of 20m (1-2 pixels of S1)
  edge_dist <- -20 # Inward buffer of 2 units (pixels) i.e. 20m
  core_geoms <- terra::buffer(poly.fldwat, width = edge_dist)
  # calculate area of core polygons
  poly.fldwat$core_m2 = terra::expanse(core_geoms, unit = "m")
  
  # calculate total water inundation in entire floodplain
  poly.attr = as.data.frame(values(poly.fldwat))
  ha.each = data.frame(Ymd = z$Ymd[1], ttlArea_ha = sum(poly.attr$area_ha))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## CALCULATE LATERAL CONNECTIVITY BY INTERSECTING PARELLEL LINES  ##
  ## WITH THE FLOODED POLYGON VECTOR LAYER AND MEASURING LINEWIDTH  ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

  # aggregate on class before doing line segments
  # meaning all flooded polygons are one multipart polygon (no land polygons in poly.fldwat)
  poly.temp = terra::aggregate(poly.fldwat, by = "class", fun = "mean")
  # intersect parallel lines with inundated habitat to measure lateral connectivity
  ln.int = terra::intersect(pl.lns, poly.temp)
  # plot(ln.int)
  ln.attr = values(ln.int) %>% select("RNAME", "class", "Ymd", "agg_n", "habitat")
  # measure the length of all line segments and get mean and sd
  ln.attr$length_m = perim(ln.int)
  # update line attributes
  values(ln.int) = ln.attr
  y = ln.attr %>% select("RNAME", "length_m") %>% group_by(RNAME) %>% 
    summarize(maxlen = max(length_m)) %>% mutate(yolo = 1) %>% group_by(yolo) %>% 
    summarize(mean = mean(maxlen), sd = sd(maxlen))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## CALCULATE LANDSCAPE METRICS THAT CAN ONLY BE CALCULATED OFF THE RASTER ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
  # calculate landscape metrics for this image
  lm.c.all = calculate_lsm(img.cls, what = lm.c.list, count_boundary = FALSE, full_name = TRUE) %>% 
             filter(class == 1) %>% select(metric, value)
  # lm.p.all = calculate_lsm(img.cls, what = lm.p.list, count_boundary = FALSE, full_name = TRUE) %>% 
  #            filter(class == 1) %>% select(metric, value)
  
  # also calculate patch size and core size at patch level so quantiles can be calculated
  # lm.p.enn  <- calculate_lsm(img.cls, level = "patch", metric = "enn" ) %>% filter(class == 1)
  # lm.p.frac <- calculate_lsm(img.cls, level = "patch", metric = "frac") %>% filter(class == 1)
  
  # assign mean and sd for all metrics for each date to x
  x = ha.each %>% mutate(
                   latcn_mn = y$mean,         # mean of lateral connectivity
                   latcn_sd = y$sd,           # stdv of lateral connecitvity
                   area_mn = mean(poly.attr$area_ha), # mean of patch area
                   area_sd = sd(  poly.attr$area_ha), # std dev of patch area
                   core_mn = mean(poly.attr$core_m2), # mean of patch area
                   core_sd = sd(  poly.attr$core_m2), # std dev of patch area
                   para_mn = mean(poly.attr$para),    # mean of patch area
                   para_sd = sd(  poly.attr$para),    # std dev of patch area
                   clumpy   = lm.c.all$value[1],      # clumpiness
                   cohesion = lm.c.all$value[2],      # cohesion
                   # enn_mn = mean(lm.p.enn$value),     # mean of euclidian nearest neighbor distance
                   # enn_sd = sd(  lm.p.enn$value),     # stdv of euclidian nearest neighbor distance
                   # frac_mn = mean(lm.p.enn$value),    # mean of fractal dimension index
                   # frac_sd = sd(  lm.p.enn$value),    # stdv of fractal dimension index
                   # calculate required quantiles
                   area.p10 = quantile(poly.attr$area_ha, probs = 0.10, na.rm = TRUE),
                   area.p50 = quantile(poly.attr$area_ha, probs = 0.50, na.rm = TRUE),
                   area.p90 = quantile(poly.attr$area_ha, probs = 0.90, na.rm = TRUE),
                   area.max = quantile(poly.attr$area_ha, probs = 1,    na.rm = TRUE),
                   core.p10 = quantile(poly.attr$core_m2, probs = 0.10, na.rm = TRUE),
                   core.p50 = quantile(poly.attr$core_m2, probs = 0.50, na.rm = TRUE),
                   core.p90 = quantile(poly.attr$core_m2, probs = 0.90, na.rm = TRUE),
                   core.max = quantile(poly.attr$core_m2, probs = 1   , na.rm = TRUE),
                   para.p10 = quantile(poly.attr$para,    probs = 0.10, na.rm = TRUE),
                   para.p50 = quantile(poly.attr$para,    probs = 0.50, na.rm = TRUE),
                   para.p90 = quantile(poly.attr$para,    probs = 0.90, na.rm = TRUE),
                   para.max = quantile(poly.attr$para,    probs = 1,    na.rm = TRUE),
                   # enn.p10  = quantile(lm.p.enn$value,    probs = 0.10, na.rm = TRUE),
                   # enn.p50  = quantile(lm.p.enn$value,    probs = 0.50, na.rm = TRUE),
                   # enn.p90  = quantile(lm.p.enn$value,    probs = 0.90, na.rm = TRUE),
                   # enn.max  = quantile(lm.p.enn$value,    probs = 1,    na.rm = TRUE),
                   # frac.p10 = quantile(lm.p.frac$value,   probs = 0.10, na.rm = TRUE),
                   # frac.p50 = quantile(lm.p.frac$value,   probs = 0.50, na.rm = TRUE),
                   # frac.p90 = quantile(lm.p.frac$value,   probs = 0.90, na.rm = TRUE),
                   # frac.max = quantile(lm.p.frac$value,   probs = 1,    na.rm = TRUE)
                  )

  # create complete data.frame containing area information per date
  if(i == 1) {
    # if this is the first instance of the loop
    # create ha.date
    ha.date = as.data.frame(z)
    # create lm.date
    lm.date = x
    # save whether previous iteration was a flood or not
    # CHECK WHETHER NEEDED OR NOT - WASN'T THERE BEFORE
    pre.iflag = sum(z$iflag)
  } else {
    # add a row (new date) to ha.date
    ha.date = rbind(ha.date, as.data.frame(z))
    # add a row (new date) to lm.date
    lm.date = rbind(lm.date, x)
    # save whether previous iteration was a flood or not
    pre.iflag = sum(z$iflag)
  }
  
  # status report
  print(i)
  
}  # end i for loop

# give column names to the habitat area file
colnames(lm.date) = var.cnames
# give column names to the landscape metrics file
colnames(ha.date) = hec.cnames
# write all inundated area by habitat to a file
write_csv(ha.date, s1.outfname1)
# write all landscape metrics to a file
write_csv(lm.date, s1.outfname2)

# calculate #pixels classed as water within each polygon for all S2 files
for (i in 1:nfiles.s2) {

  # initialize number of polygons to 0
  no_poly_trn = 0

  # open the from google drive folder
  tmp <- tempfile(fileext = insuf)
  drive_download(as_id(files.s2$id[i]), path = tmp, overwrite = TRUE)
  img.cls <- rast(tmp)
  
  # CONVERT THE RASTER CLASSIFICATION INTO CLASS POLYGONS
  # creates a SpatVector with just two multi-part polygons with values 0 & 1
  poly.cls = as.polygons(img.cls, dissolve = TRUE, values = TRUE)
  
  # disaggregate multi-part polygons so all are single-part polygons
  poly.sngl.cls = terra::disagg(poly.cls)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## INTERSECT THE WATER POLYGONS LAYER WITH THE HABITAT TYPE LAYER ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
  # intersect with the yolo bypass polygons and crop to extent
  poly.int = terra::intersect(poly.sngl.cls, fd.poly)
  
  # dis-aggregate again to split multi-part polygons
  poly.int.sngl = terra::disagg(poly.int)
  
  # select only for water pixels
  poly.wat = poly.int.sngl[poly.int.sngl$aweish_cls == 1 | poly.int.sngl$NAME == "CommWater", ]
  # say it is all water
  poly.wat$class = 1
  
  # dissolve adjacent polygons before calculating area
  # first aggregate the polygons on class
  poly.temp = terra::aggregate(poly.wat, by = "NAME", fun = "mean")
  # now disaggregate the polygons on NAME
  poly.wat.hab = terra::disagg(poly.temp)
  
  # calculate area of each polygon in square meters
  poly.wat.hab$area_m2 = terra::expanse(poly.wat.hab, unit = "m")
  
  # filter out polygons smaller than 500 sq. meters
  poly.wat.hab.sub = poly.wat.hab[poly.wat.hab$area_m2 > 500,]
  
  # add date to vector attributes
  poly.wat.hab.sub$Ymd = s2.extDates[i]
  poly.attr = as.data.frame(values(poly.wat.hab.sub))
  
  # calculate total inundated area per habitat type: natural, agriculture, channel
  poly.attr = poly.attr %>% rename(habitat = NAME, class = mean_class) %>% 
    select("habitat", "class", "area_m2", "Ymd") %>% 
    mutate(area_ha = area_m2*0.0001)
  values(poly.wat.hab.sub) = poly.attr
  
  # calculate total inundated area per habitat for the entire image
  z = poly.attr %>% group_by(habitat, Ymd) %>% 
    summarize(ttlAreaHab_ha = sum(area_ha)) %>% 
    # set the flag for start of a flood
    mutate(iflagf = ifelse((habitat == "First" & (ttlAreaHab_ha/first_ha) > 0.5),  1, 0)) %>% 
    mutate(iflagl = ifelse((habitat == "Last"  & (ttlAreaHab_ha/lastf_ha) > 0.33), 1, 0))
  # calculate percent cover of water for each habitat
  z = left_join(z, habArea, by = "habitat") %>% mutate(percWater = (ttlAreaHab_ha/ttlAreaHa)*100) %>% 
    mutate(iflag = ifelse(iflagf == 1, 1, ifelse(pre.iflag == 1 & iflagl == 1, 1, 0)))
  
  if(sum(z$iflag) == 0) {
    # if area not in flood cycle, remove agricultural and natural land
    poly.fldwat = poly.wat.hab.sub[(poly.wat.hab.sub$habitat != "Agriculture") & 
                                     (poly.wat.hab.sub$habitat != "Natural") & 
                                     (poly.wat.hab.sub$habitat != "Last"),]
    # adjust acreage values in z data frame accordingly
    z = z %>% mutate(ttlAreaHab_ha = ifelse((habitat == "Agriculture")|(habitat == "Natural")|(habitat == "Last"), 0, ttlAreaHab_ha),
                     percWater     = ifelse((habitat == "Agriculture")|(habitat == "Natural")|(habitat == "Last"), 0, percWater))
  } else {
    poly.fldwat = poly.wat.hab.sub
  }
  
  # calculate perimeter of each polygon in meters
  poly.fldwat$perim_m = terra::perim(poly.fldwat)
  
  # calculate para (perimeter to area ratio)
  poly.fldwat$para = poly.fldwat$perim_m/poly.fldwat$area_m2
  
  # calculate core polygons with an inner buffer of 20m (1-2 pixels of S2)
  edge_dist <- -20 # Inward buffer of 2 units (pixels) i.e. 20m
  core_geoms <- terra::buffer(poly.fldwat, width = edge_dist)
  # calculate area of core polygons
  poly.fldwat$core_m2 = terra::expanse(core_geoms, unit = "m")
  
  # calculate total water inundation in entire floodplain
  poly.attr = as.data.frame(values(poly.fldwat))
  ha.each = data.frame(Ymd = z$Ymd[1], ttlArea_ha = sum(poly.attr$area_ha))
  
  # aggregate on class before doing line segments
  poly.temp = terra::aggregate(poly.fldwat, by = "class", fun = "mean")
  # intersect parallel lines with inundated habitat to measure lateral connectivity
  ln.int = terra::intersect(pl.lns, poly.temp)
  # plot(ln.int)
  ln.attr = values(ln.int)
  # measure the length of all line segments and get mean and sd
  ln.attr$length_m = perim(ln.int)
  y = ln.attr %>% select("RNAME", "length_m") %>% group_by(RNAME) %>% 
    summarize(maxlen = max(length_m)) %>% mutate(yolo = 1) %>% group_by(yolo) %>% 
    summarize(mean = mean(maxlen), sd = sd(maxlen))
  
  
  # calculate landscape metrics for this image
  lm.c.all = calculate_lsm(img.cls, what = lm.c.list, count_boundary = FALSE, full_name = TRUE) %>% 
    filter(class == 1) %>% select(metric, value)
  # lm.p.all = calculate_lsm(img.cls, what = lm.p.list, count_boundary = FALSE, full_name = TRUE) %>% 
  #            filter(class == 1) %>% select(metric, value)
  
  # also calculate patch size and core size at patch level so quantiles can be calculated
  # lm.p.enn  <- calculate_lsm(img.cls, level = "patch", metric = "enn" ) %>% filter(class == 1)
  # lm.p.frac <- calculate_lsm(img.cls, level = "patch", metric = "frac") %>% filter(class == 1)

  # assign mean and sd for all metrics for each date to x
  x = ha.each %>% mutate(
    latcn_mn = y$mean,                 # mean of lateral connectivity
    latcn_sd = y$sd,                   # stdv of lateral connecitvity
    area_mn = mean(poly.attr$area_ha), # mean of patch area
    area_sd = sd(  poly.attr$area_ha), # std dev of patch area
    core_mn = mean(poly.attr$core_m2), # mean of patch area
    core_sd = sd(  poly.attr$core_m2), # std dev of patch area
    para_mn = mean(poly.attr$para),    # mean of patch area
    para_sd = sd(  poly.attr$para),    # std dev of patch area
    clumpy   = lm.c.all$value[1],      # clumpiness
    cohesion = lm.c.all$value[2],      # cohesion
    # enn_mn = mean(lm.p.enn$value),     # mean of euclidian nearest neighbor distance
    # enn_sd = sd(  lm.p.enn$value),     # stdv of euclidian nearest neighbor distance
    # frac_mn = mean(lm.p.enn$value),    # mean of fractal dimension index
    # frac_sd = sd(  lm.p.enn$value),    # stdv of fractal dimension index
    # calculate required quantiles
    area.p10 = quantile(poly.attr$area_ha, probs = 0.10, na.rm = TRUE),
    area.p50 = quantile(poly.attr$area_ha, probs = 0.50, na.rm = TRUE),
    area.p90 = quantile(poly.attr$area_ha, probs = 0.90, na.rm = TRUE),
    area.max = quantile(poly.attr$area_ha, probs = 1,    na.rm = TRUE),
    core.p10 = quantile(poly.attr$core_m2, probs = 0.10, na.rm = TRUE),
    core.p50 = quantile(poly.attr$core_m2, probs = 0.50, na.rm = TRUE),
    core.p90 = quantile(poly.attr$core_m2, probs = 0.90, na.rm = TRUE),
    core.max = quantile(poly.attr$core_m2, probs = 1   , na.rm = TRUE),
    para.p10 = quantile(poly.attr$para,    probs = 0.10, na.rm = TRUE),
    para.p50 = quantile(poly.attr$para,    probs = 0.50, na.rm = TRUE),
    para.p90 = quantile(poly.attr$para,    probs = 0.90, na.rm = TRUE),
    para.max = quantile(poly.attr$para,    probs = 1,    na.rm = TRUE),
    # enn.p10  = quantile(lm.p.enn$value,    probs = 0.10, na.rm = TRUE),
    # enn.p50  = quantile(lm.p.enn$value,    probs = 0.50, na.rm = TRUE),
    # enn.p90  = quantile(lm.p.enn$value,    probs = 0.90, na.rm = TRUE),
    # enn.max  = quantile(lm.p.enn$value,    probs = 1,    na.rm = TRUE),
    # frac.p10 = quantile(lm.p.frac$value,   probs = 0.10, na.rm = TRUE),
    # frac.p50 = quantile(lm.p.frac$value,   probs = 0.50, na.rm = TRUE),
    # frac.p90 = quantile(lm.p.frac$value,   probs = 0.90, na.rm = TRUE),
    # frac.max = quantile(lm.p.frac$value,   probs = 1,    na.rm = TRUE)
  )
  
  # create complete data.frame containing area information per date
  if(i == 1) {
    # if this is the first instance of the loop
    # create ha.date
    ha.date = as.data.frame(z)
    # create lm.date
    lm.date = x
  } else {
    # add a row (new date) to ha.date
    ha.date = rbind(ha.date, as.data.frame(z))
    # add a row (new date) to lm.date
    lm.date = rbind(lm.date, x)
    # save whether previous iteration was a flood or not
    pre.iflag = sum(z$iflag)
  }
  
  # status report
  print(i)

}  # end i for loop

# give column names to the habitat area file
colnames(lm.date) = var.cnames
# give column names to the landscape metrics file
colnames(ha.date) = hec.cnames
# write all inundated area by habitat to a file
write_csv(ha.date, s2.outfname1)
# write all landscape metrics to a file
write_csv(lm.date, s2.outfname2)

############## Read back the files that were written ###############

# # devtools::install_github("goertler/inundation")
# fre <- get_fre() %>% filter(value >= 33.5) %>% filter(year(datetime) > 2017) %>% 
#        mutate(Ymd = date(datetime))
# 
# fl.plt = ggplot(fre, aes(x = Ymd, y = value))
# 
# fl.plt + geom_line(col = "blue", linewidth = 1) + scale_y_continuous(limits = c(33.5, 35)) +
#   scale_x_date(date_labels = "%d-%m", limits = c(as.Date("2018-10-01"), as.Date("2019-09-30"))) +
#   labs(x = "", y = expression("Sacramento River stage")) + 
#   theme_classic(base_size = 14)
# 
# ggsave(paste0(indir, "SummaryResults/GEE_v3/Hydrograph_", WY, ".tiff"), width = 7, height = 2, units = "in", dpi = 300)

infile.s1.ha = s1.outfname1
infile.s1.lm = s1.outfname2
infile.s2.ha = s2.outfname1
infile.s2.lm = s2.outfname2

s1.ha = read_csv(infile.s1.ha) %>% mutate(sensor = "S1") %>%
  mutate(WY = ifelse(month(Ymd) <= 9, year(Ymd), (year(Ymd)+1)))

s1.lm = read_csv(infile.s1.lm) %>% mutate(sensor = "S1") %>%
  mutate(WY = ifelse(month(Ymd) <= 9, year(Ymd), (year(Ymd)+1)))

s2.ha = read_csv(infile.s2.ha) %>%
  mutate(Ymd = as.Date(as.character(Ymd), format = "%Y-%m-%d"), sensor = "S2") %>%
  mutate(WY = ifelse(month(Ymd) <= 9, year(Ymd), (year(Ymd)+1)))

s2.lm = read_csv(infile.s2.lm) %>%
  mutate(Ymd = as.Date(as.character(Ymd), format = "%Y-%m-%d"), sensor = "S2") %>%
  mutate(WY = ifelse(month(Ymd) <= 9, year(Ymd), (year(Ymd)+1)))

# merge s1 and s2 tables
s1s2 = rbind(s1.ha, s2.ha) %>% group_by(Ymd, sensor) %>% summarise(ttlAreaHab_ha = sum(ttlAreaHab_ha), percWater = mean(percWater))

plt      = ggplot(s1.ha, aes(x = Ymd, y = ttlAreaHab_ha, col = habitat))
plt.perc = ggplot(s1.ha, aes(x = Ymd, y = percWater,     col = habitat))
plt.both = ggplot(s1s2,  aes(x = Ymd, y = ttlAreaHab_ha, col = sensor))
plt.tper = ggplot(s1s2,  aes(x = Ymd, y = percWater,     col = sensor))

plt + geom_point(size = 1.5) + theme_classic(base_size = 14) +
    theme(legend.title = element_blank()) + labs(x = "", y = "Flooded area (ha)") +
    scale_y_continuous(limits = c(0, 9000)) +
    scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y")

ggsave(paste0(indir, "figures/S1_spatio_hydrograph_habitat_", WY, ver, ".tiff"), width = 8, height = 4, units = "in", dpi = 300)

plt.perc + geom_point(size = 1.5) + theme_classic(base_size = 14) +
  theme(legend.title = element_blank()) + labs(x = "", y = "Percent area flooded") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y")

ggsave(paste0(indir, "figures/S1_spatio_hydrograph_percwat_", WY, ver, ".tiff"), width = 8, height = 4, units = "in", dpi = 300)

plt.both + geom_point(size = 1.5) + theme_classic(base_size = 14) + 
    theme(legend.title = element_blank()) + labs(x = "", y = "Flooded area (ha)") + 
    scale_y_continuous(limits = c(0, 18000)) +
    scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y")

ggsave(paste0(indir, "figures/comb_spatio_hydrograph_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

plt.tper + geom_point(size = 1.5) + theme_classic(base_size = 14) +
  theme(legend.title = element_blank()) + labs(x = "", y = "Percent area flooded") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y")

ggsave(paste0(indir, "figures/S1_spatio_hydrograph_tperwat_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

###################    LATERAL CONNECTIVITY VISUALIZATION     ####################

s1.latcn = s1.lm %>% select(Ymd, latConn_mn, latConn_sd) %>% 
  mutate(upper = latConn_mn + latConn_sd, lower = latConn_mn - latConn_sd) 

ggplot(s1.latcn, aes(x = Ymd, y = latConn_mn, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = .8) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  scale_y_continuous(limits = c(0, 4500)) +
  labs(x = "", y = expression("Lateral connectivity (m)")) + 
  theme_classic(base_size = 14)

ggsave(paste0(indir, "figures/s1_latConn_", strdate, "_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

s2.latcn = s2.lm %>% select(Ymd, latConn_mn, latConn_sd) %>% 
  mutate(upper = latConn_mn + latConn_sd, lower = latConn_mn - latConn_sd) 

ggplot(s2.latcn, aes(x = Ymd, y = latConn_mn, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = .8) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  scale_y_continuous(limits = c(0, 4500)) +
  labs(x = "", y = expression("Lateral connectivity (m)")) + 
  theme_classic(base_size = 14)

ggsave(paste0(indir, "figures/s2_lat_conn_", strdate, "_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

###################         PATCH SIZE VISUALIZATION        ####################

ggplot(s1.lm, aes(x = Ymd, y = area.p50, ymin = area.p10, ymax = area.p90)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = 1) +
  geom_point(aes(x = Ymd, y = area.max), color = "blue", size = 1) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  labs(title = "Patch size distribution", x = "Date", y = expression("Patch Area Distribution (" * m^2 * ")")) + 
  theme_bw(base_size = 14)

ggsave(paste0(indir, "figures/s1_PatchAreaPile_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

ggplot(s2.lm, aes(x = Ymd, y = area.p50, ymin = area.p10, ymax = area.p90)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = 1) +
  geom_point(aes(x = Ymd, y = area.max), color = "blue", size = 1) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  labs(title = "Patch size distribution", x = "Date", y = expression("Patch Area Distribution (" * m^2 * ")")) + 
  theme_bw(base_size = 14)

ggsave(paste0(indir, "figures/s2_PatchAreaPile_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

###################         CORE AREA VISUALIZATION        ####################

ggplot(s1.lm, aes(x = Ymd, y = core.p50, ymin = core.p10, ymax = core.p90)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = 1) +
  geom_point(aes(x = Ymd, y = core.max), color = "blue", size = 1) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  labs(title = "Core area distribution", x = "Date", y = expression("Core Area Distribution (" * m^2 * ")")) + 
  theme_bw(base_size = 14)

ggsave(paste0(indir, "figures/s1_CoreAreaPile_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

ggplot(s2.lm, aes(x = Ymd, y = core.p50, ymin = core.p10, ymax = core.p90)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = 1) +
  geom_point(aes(x = Ymd, y = core.max), color = "blue", size = 1) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  labs(title = "Core area distribution", x = "Date", y = expression("Core Area Distribution (" * m^2 * ")")) + 
  theme_bw(base_size = 14)

ggsave(paste0(indir, "figures/s2_CoreAreaPile_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

###################         PARA VISUALIZATION        ####################

ggplot(s1.lm, aes(x = Ymd, y = para.p50, ymin = para.p10, ymax = para.p90)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = 1) +
  geom_point(aes(x = Ymd, y = para.max), color = "blue", size = 1) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  labs(title = "PARA distribution", x = "Date", y = expression("PARA Distribution (" * m^2 * ")")) + 
  theme_bw(base_size = 14)

ggsave(paste0(indir, "figures/s1_PARA_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

ggplot(s2.lm, aes(x = Ymd, y = para.p50, ymin = para.p10, ymax = para.p90)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = 1) +
  geom_point(aes(x = Ymd, y = para.max), color = "blue", size = 1) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  labs(title = "Core area distribution", x = "Date", y = expression("PARA Distribution (" * m^2 * ")")) + 
  theme_bw(base_size = 14)

ggsave(paste0(indir, "figures/s2_PARA_", WY, ver, ".tiff"), width = 6, height = 4, units = "in", dpi = 300)

