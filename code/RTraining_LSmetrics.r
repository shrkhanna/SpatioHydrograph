#####################################################
##                                                 ##
##  1. Open raster class images                    ##
##  2. Convert to vector polygons                  ##
##  3. Remove all polygons < 5000 sq.m             ##
##  4. Calculate water area per habitat            ##
##  5. Calculate landscape metrics using polygons  ##
##  6. Calculate landscape metrics using rasters   ##
##  7. Summarize and write outputs                 ##
##  8. Visualize outputs                           ##
##  Written by Shruti Khanna on 2/3/2026           ##
##  Last modified by Shruti Khanna on 2/12/2026    ##
##                                                 ##
#####################################################

# working directory with the R program file
dir_progr = "X:/delta_sentinel/code/SpatioHydrograph/"

# set working directory
setwd(dir_progr)

# libraries needed
library("ggplot2")
library("tidyverse")
library("terra")
library("readr")
library("sf")
library("lubridate")
library("landscapemetrics")

####################   BEGIN CHANGE   ########################

# input parent directory and output directory
indir = "X:/delta_sentinel/raster/Water_Class/"
s2.outshpdir = "X:/delta_sentinel/vector/S1S2_waterclass/S2_AWEI/"

# which water year is being analyzed
WY = "2019"
y1y2 = "2018_19"

# the specific input directory we are analyzing
indir.s2 = paste0(indir, "S1S2_waterclass/S2_AWEI/S2_classmaps/202509_v3/WY_", y1y2, "/")

# input suffix for S1 and S2 files
insuf.s2 = ".tif"

# system date
strdate = Sys.Date()

# the output files - inundated area per polygon
s2.outfname1 = paste0(indir, "SummaryResults/GEE_v3/SH_WY", WY, "_S2_AWEIsh_", strdate, "_habArea.csv")
# the output files - landscape metrics
s2.outfname2 = paste0(indir, "SummaryResults/GEE_v3/SH_WY", WY, "_S2_AWEIsh_", strdate, "_lmAll.csv")

# shapefile for yolo bypass habitat polygons
shp.nm = "X:/delta_sentinel/vector/projects/YoloBypassForSH_UTM_wat.shp"

# shapefile for yolo bypass parallel lines
ln.nm  = "X:/delta_sentinel/vector/parellel_lines.shp"

# list of landscape metrics to be calculated for each flooded image
list.lsm.all = list_lsm()
lm.c.list = c("lsm_c_clumpy",  "lsm_c_cohesion")
lm.p.list = c("lsm_p_enn", "lsm_p_frac")
var.cnames = c("Ymd", "ttlAreaHa", "latConn_mn", "latConn_sd",
               "area.mn", "area.sd", "core.mn", "core.sd", 
               "para.mn", "para.sd", "clumpiness", "cohesion", 
               "enn.mn", "enn.sd", "frac.mn", "frac.sd",
               "area.p10", "area.p50", "area.p90", "area.max", 
               "core.p10", "core.p50", "core.p90", "core.max",
               "para.p10", "para.p50", "para.p90", "para.max",
               "enn.p10",  "enn.p50",  "enn.p90",  "enn.max",
               "frac.p10", "frac.p50", "frac.p90", "frac.max"
              )
hec.cnames = c("habitat", "Ymd", "ttlAreaHab_ha", "ttlAreaHa", "percWater")

#################### END CHANGE ########################

# get list of S2 files
images.s2 <- list.files(indir.s2, pattern = insuf.s2, full.names = TRUE)
images.s2 <- subset(images.s2, !grepl(".aux", images.s2))
images.s2 <- subset(images.s2, !grepl(".ovr", images.s2))
images.s2 <- subset(images.s2, !grepl(".vat", images.s2))

# count of files
nfiles.s2 <- length(images.s2)
# start.s2 = 1
# end.s2 = nfiles.s2
start.s2 <- 5
end.s2 <- 20

# set vars for S2 files
s2.forDates <- basename(images.s2)
s2.extDates = sub(".*_(\\d{4}-\\d{2}-\\d{2})\\.tif$", "\\1", s2.forDates)

# read the yolo bypass habitat polygons shapefile
fd.poly <- terra::vect(shp.nm)
# calculate area per habitat and convert to hectares
fd.poly$area_ha = round((terra::expanse(fd.poly)*0.0001), digits = 4)
# remove the first and last trigger areas for flood events and rename NAME to habitat
fd.attr <- as.data.frame(values(fd.poly)) %>% 
           mutate(habitat = ifelse(NAME == "Last", "Agriculture", ifelse(NAME == "First", "Natural", NAME))) %>% 
           select(-NAME)
# assign modified attributes back to vector file
values(fd.poly) = fd.attr
# calculate total area of each habitat
habArea = fd.attr %>% group_by(habitat) %>% summarise(ttlAreaHa = sum(area_ha))
no.poly <- nrow(fd.poly)

# read the parallel lines in yolo bypass
pl.lns  <- terra::vect(ln.nm)
no.lns  <- nrow(pl.lns)

# calculate #pixels classed as water within each polygon for all S2 files
for (i in start.s2:end.s2) {

  # initialize number of polygons to 0
  no_poly_trn = 0

  # read the image and mask files
  img.cls = terra::rast(images.s2[i])

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ##     CONVERT THE RASTER CLASSIFICATION INTO CLASS POLYGONS      ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  # creates a SpatVector with just two multi-part polygons with values 0 & 1
  poly.cls = terra::as.polygons(img.cls, dissolve = TRUE, values = TRUE)
  
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
  poly.wat = poly.int.sngl[poly.int.sngl$aweish_cls == 1 | poly.int.sngl$habitat == "CommWater", ]
  # say it is all water
  poly.wat$class = 1
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ##             SUMMARIZE FLOODED AREA PER HABITAT TYPE            ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
  # dissolve adjacent polygons before calculating area
  # first aggregate the polygons on class
  poly.temp = terra::aggregate(poly.wat, by = "habitat", fun = "mean")
  
  # now disaggregate to convert multi-part polygons into single-part
  poly.wat.hab = terra::disagg(poly.temp)
  
  # calculate area of each polygon in square meters
  poly.wat.hab$area_m2 = terra::expanse(poly.wat.hab, unit = "m")
  
  # filter out polygons smaller than 500 sq. meters
  poly.wat.hab.sub = poly.wat.hab[poly.wat.hab$area_m2 > 500,]
  
  # add date to vector attributes
  poly.wat.hab.sub$Ymd = s2.extDates[i]
  poly.attr = as.data.frame(values(poly.wat.hab.sub))
  
  # rename column and calculate area in ha per polygon
  poly.attr = poly.attr %>% rename(class = mean_class) %>% 
    select("habitat", "class", "area_m2", "Ymd") %>% 
    mutate(area_ha = area_m2*0.0001)
  values(poly.wat.hab.sub) = poly.attr
  
  # calculate total inundated area per habitat for the entire image
  z = poly.attr %>% group_by(habitat, Ymd) %>% summarize(ttlAreaHab_ha = sum(area_ha))
  
  # calculate percent cover of water for each habitat
  z = left_join(z, habArea, by = "habitat") %>% mutate(percWater = (ttlAreaHab_ha/ttlAreaHa)*100)
  
  # calculate total water inundation in entire floodplain
  poly.attr = as.data.frame(values(poly.fldwat))
  ha.each = data.frame(Ymd = z$Ymd[1], ttlArea_ha = sum(poly.attr$area_ha))
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## CALCULATE LANDSCAPE METRICS WITH VECTOR DATA WHENEVER POSSIBLE ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
  poly.fldwat = poly.wat.hab.sub

  # calculate PERIMETER of each polygon in meters
  poly.fldwat$perim_m = terra::perim(poly.fldwat)
  
  # calculate PARA (perimeter to area ratio)
  poly.fldwat$para = poly.fldwat$perim_m/poly.fldwat$area_m2
  
  # calculate CORE polygons with an inner buffer of 20m (1-2 pixels of S2)
  edge_dist <- -20 # Inward buffer of 2 units (pixels) i.e. 20m
  core_geoms <- terra::buffer(poly.fldwat, width = edge_dist)
  
  # calculate area of core polygons
  poly.fldwat$core_m2 = terra::expanse(core_geoms, unit = "m")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ##      CALCULATE LATERAL CONNECTIVITY ACROSS THE FLOODPLAIN      ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
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
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ##          CALCULATE LANDSCAPE METRICS WITH RASTER DATA          ##
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
  # calculate landscape metrics for this image
  lm.c.all = calculate_lsm(img.cls, what = lm.c.list, count_boundary = FALSE, full_name = TRUE) %>% 
             filter(class == 1) %>% select(metric, value)
  # also calculate patch size and core size at patch level so quantiles can be calculated
  lm.p.enn  <- calculate_lsm(img.cls, level = "patch", metric = "enn" ) %>% filter(class == 1)
  lm.p.frac <- calculate_lsm(img.cls, level = "patch", metric = "frac") %>% filter(class == 1)

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
    clumpy  = lm.c.all$value[1],       # clumpiness
    cohesion= lm.c.all$value[2],       # cohesion
    enn_mn  = mean(lm.p.enn$value),    # mean of euclidian nearest neighbor distance
    enn_sd  = sd(  lm.p.enn$value),    # stdv of euclidian nearest neighbor distance
    frac_mn = mean(lm.p.frac$value),   # mean of fractal dimension index
    frac_sd = sd(  lm.p.frac$value),   # stdv of fractal dimension index
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
    enn.p10  = quantile(lm.p.enn$value,    probs = 0.10, na.rm = TRUE),
    enn.p50  = quantile(lm.p.enn$value,    probs = 0.50, na.rm = TRUE),
    enn.p90  = quantile(lm.p.enn$value,    probs = 0.90, na.rm = TRUE),
    enn.max  = quantile(lm.p.enn$value,    probs = 1,    na.rm = TRUE),
    frac.p10 = quantile(lm.p.frac$value,   probs = 0.10, na.rm = TRUE),
    frac.p50 = quantile(lm.p.frac$value,   probs = 0.50, na.rm = TRUE),
    frac.p90 = quantile(lm.p.frac$value,   probs = 0.90, na.rm = TRUE),
    frac.max = quantile(lm.p.frac$value,   probs = 1,    na.rm = TRUE)
  )
  
  # create complete data.frame containing area information per date
  if(i == start.s2) {
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
  }
  
  # # output the flooded area as a polygon shapefile
  # vecname = paste0(s2.outshpdir, "S2_AWEIsh_cls_", s2.extDates[i], ".shp")
  # if(!file.exists(vecname)) {
  #   # write the vector file if it doesn't exist
  #   writeVector(poly.fldwat, filename = vecname, filetype = "ESRI Shapefile", overwrite = TRUE)
  # }
  
  # status report
  print(s2.extDates[i])

}  # end i for loop

# give column names to the habitat area file
colnames(lm.date) = var.cnames
# give column names to the landscape metrics file
colnames(ha.date) = hec.cnames
# write all inundated area by habitat to a file
write_csv(ha.date, s2.outfname1)
# write all landscape metrics to a file
write_csv(lm.date, s2.outfname2)


#################### Read back the files that were written #####################

infile.s2.ha = "X:/delta_sentinel/raster/Water_Class/SummaryResults/GEE_v3/SH_WY2019_S2_AWEIsh_2026-02-04_habArea.csv"
infile.s2.lm = "X:/delta_sentinel/raster/Water_Class/SummaryResults/GEE_v3/SH_WY2019_S2_AWEIsh_2026-02-04_lmAll.csv"

# infile.s2.ha = s2.outfname1
# infile.s2.lm = s2.outfname2

s2.ha = read_csv(infile.s2.ha) %>%
  mutate(Ymd = as.Date(as.character(Ymd), format = "%Y-%m-%d"), sensor = "S2") %>%
  mutate(WY = ifelse(month(Ymd) <= 9, year(Ymd), (year(Ymd)+1)))

s2.lm = read_csv(infile.s2.lm) %>%
  mutate(Ymd = as.Date(as.character(Ymd), format = "%Y-%m-%d"), sensor = "S2") %>%
  mutate(WY = ifelse(month(Ymd) <= 9, year(Ymd), (year(Ymd)+1)))

plt      = ggplot(s2.ha, aes(x = Ymd, y = ttlAreaHab_ha, col = habitat))
plt.perc = ggplot(s2.ha, aes(x = Ymd, y = percWater,     col = habitat))

plt + geom_point(size = 1.5) + theme_classic(base_size = 14) +
    theme(legend.title = element_blank()) + labs(x = "", y = "Flooded area (ha)") +
    scale_y_continuous(limits = c(0, 9000)) +
    scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y")

ggsave(paste0(indir, "SummaryResults/GEE_v3/S2_spatioHydrograph_habitat_", WY, "_trn.tiff"), width = 8, height = 4, units = "in", dpi = 300)

plt.perc + geom_point(size = 1.5) + theme_classic(base_size = 14) +
  theme(legend.title = element_blank()) + labs(x = "", y = "Percent area flooded") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y")

ggsave(paste0(indir, "SummaryResults/GEE_v3/S2_spatioHydrograph_percwat_", WY, "_trn.tiff"), width = 8, height = 4, units = "in", dpi = 300)


###################    LATERAL CONNECTIVITY VISUALIZATION     ####################

s2.latcn = s2.lm %>% select(Ymd, latConn_mn, latConn_sd) %>% 
  mutate(upper = latConn_mn + latConn_sd, lower = latConn_mn - latConn_sd) 

ggplot(s2.latcn, aes(x = Ymd, y = latConn_mn, ymin = lower, ymax = upper)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = .8) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  scale_y_continuous(limits = c(0, 4500)) +
  labs(x = "", y = expression("Lateral connectivity (m)")) + 
  theme_classic(base_size = 14)

ggsave(paste0(indir, "SummaryResults/GEE_v3/s2_latConn_", strdate, "_", WY, "_trn.tiff"), width = 6, height = 4, units = "in", dpi = 300)

###################         PATCH SIZE VISUALIZATION        ####################

ggplot(s2.lm, aes(x = Ymd, y = area.p50, ymin = area.p10, ymax = area.p90)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = 1) +
  geom_point(aes(x = Ymd, y = area.max), color = "blue", size = 1) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  labs(title = "Patch size distribution", x = "Date", y = expression("Patch Area Distribution (" * m^2 * ")")) + 
  theme_bw(base_size = 14)

ggsave(paste0(indir, "SummaryResults/GEE_v3/s2_PatchAreaPile_", WY, "_trn.tiff"), width = 6, height = 4, units = "in", dpi = 300)

###################         CORE AREA VISUALIZATION        ####################

ggplot(s2.lm, aes(x = Ymd, y = core.p50, ymin = core.p10, ymax = core.p90)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = 1) +
  geom_point(aes(x = Ymd, y = core.max), color = "blue", size = 1) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  labs(title = "Core area distribution", x = "Date", y = expression("Core Area Distribution (" * m^2 * ")")) + 
  theme_bw(base_size = 14)

ggsave(paste0(indir, "SummaryResults/GEE_v3/s2_CoreAreaPile_", WY, "_trn.tiff"), width = 6, height = 4, units = "in", dpi = 300)

###################         PARA VISUALIZATION        ####################

ggplot(s2.lm, aes(x = Ymd, y = para.p50, ymin = para.p10, ymax = para.p90)) +
  geom_pointrange(color = "firebrick", size = 0.3, linewidth = 1) +
  geom_point(aes(x = Ymd, y = para.max), color = "blue", size = 1) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%Y") +
  labs(title = "Core area distribution", x = "Date", y = expression("PARA Distribution (" * m^2 * ")")) + 
  theme_bw(base_size = 14)

ggsave(paste0(indir, "SummaryResults/GEE_v3/s2_PARA_", WY, "_trn.tiff"), width = 6, height = 4, units = "in", dpi = 300)

################################################################################
###    Create a short video showing one entire flood cycle using S2 images   ###
################################################################################

# Create a directory to store the PNG frames for the video
dir.vid = paste0(indir, "SummaryResults/GEE_v3/video_frames/")
dir.create(dir.vid, showWarnings = FALSE)

# Define custom colors and labels for your classes
class_colors <- c("black", "white", "blue")
class_labels <- c("", "Land", "Water")
plot_title <- "Flood cycle"

dir.create(dir.vid, showWarnings = FALSE)

# Loop through each raster file
for (i in 7:18) {
  
  # Read the raster file
  current_raster <- rast(images.s2[i])
  
  # Extract the year from the filename for the plot title (optional but nice)
  # This uses gsub to extract the numbers from the filename
  year <- sub("^(\\d{8})_.*", "\\1", basename(images.s2[i]))
  
  # Define the output filename for this frame
  # sprintf creates filenames with leading zeros (e.g., frame_001.png, frame_002.png)
  # which ensures they are ordered correctly.
  frame_filename <- file.path(dir.vid, sprintf("frame_%03d.png", i))
  
  # --- Open a PNG graphics device ---
  png(frame_filename, width = 300, height = 700, res = 100)
  
  # --- Create the plot ---
  # Using type = "classes" is important for categorical rasters
  plot(
    current_raster,
    type = "classes",
    col  = class_colors,
    main = paste(plot_title, "\nDate:", year),
    plg  = list(legend = class_labels) # plg is plot legend arguments
  )
  
  # --- Close the PNG device to save the file ---
  dev.off()
}

# List the frames we just created
list.files(dir.vid)

# Get the list of PNG files we just created
png_files <- list.files(dir.vid, pattern = "frame_.*\\.png$", full.names = TRUE)

# Define the output video file
output_video_file <- "S2_raster_timeseries_trn.mp4"

# Set the frame rate (frames per second)
# A lower number means each frame is visible for longer (slower video)
fps <- 0.8

# Encode the video
av::av_encode_video(
  input = png_files,
  output = output_video_file,
  framerate = fps,
  # vcodec = "libx264" # Standard codec for MP4, highly compatible
)

print(paste("Video saved to:", output_video_file))

# Remove the individual frame images
unlink(dir.vid, recursive = TRUE)

# Remove the dummy rasters
unlink("temp_rasters", recursive = TRUE)
