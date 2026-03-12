# Install necessary packages if you haven't already
# install.packages("tidyverse")

# Load the libraries
library(readr)
library(dplyr)
library(ggplot2)

# --- 1. Read in the Dayflow Data ---

# URL for the Dayflow data covering 1997-2023
# This makes the script reproducible without manual downloads
dayflow_url <- "https://data.cnra.ca.gov/dataset/dayflow/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2023.csv"

# Read the CSV directly from the URL
dayflow_raw <- read_csv(dayflow_url)

# Inspect the data to make sure it loaded correctly
glimpse(dayflow_raw)

# --- 2. Process and Filter the Data ---

# Define the start and end dates for Water Year 2019
wy_2019_start <- as.Date("2018-10-01")
wy_2019_end <- as.Date("2019-09-30")

df_wy2019 <- dayflow_raw %>%
  # Select and rename columns for clarity
  select(Date, SAC, YOLO) %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  mutate(WY = ifelse(month(date) <= 9, year(date), (year(date)+1))) %>% 
  # Filter for the 2019 Water Year
  filter(WY == 2023) %>% 
  pivot_longer(cols = c(SAC, YOLO), names_to = "location", values_to = "flow_cfs")

df_wy2019_yolo <- dayflow_raw %>%
  # Select and rename columns for clarity
  select(Date, YOLO) %>%
  mutate(date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  mutate(WY = ifelse(month(date) <= 9, year(date), (year(date)+1))) %>% 
  # Filter for the 2019 Water Year
  filter(WY == 2023)
  

# --- 3. Plot the Sacramento River Flow ---

ggplot(df_wy2019, aes(x = date, y = flow_cfs, colour = location)) +
   geom_line(size = 1) +
   labs(
     # title = "Sacramento River Flow (Freeport) for Water Year 2019",
     # subtitle = "Source: California Department of Water Resources (Dayflow)",
     x = "Date",
     y = "Mean Daily Flow (cfs)"
   ) +
   # Use the scales package for nice formatting of the y-axis labels
   scale_y_continuous(labels = scales::comma) +
   # Format the x-axis to show month abbreviations
   scale_x_date(date_breaks = "2 months", date_labels = "%m-%y") +
   theme_classic(base_size = 14) +
   theme(
     plot.title = element_text(face = "bold"),
     axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for readability
   )

ggsave(paste0(indir, "SummaryResults/GEE_v3/Dayflow_SAC_YOLO_WY", WY, ".tiff"), width = 8, height = 4, units = "in", dpi = 300)

ggplot(df_wy2019_yolo, aes(x = date, y = YOLO)) +
  geom_line(size = 1, col = "blue") +
  labs(
    # title = "Sacramento River Flow (Freeport) for Water Year 2019",
    # subtitle = "Source: California Department of Water Resources (Dayflow)",
    x = "Date",
    y = "Mean Daily Flow (cfs)"
  ) +
  # Use the scales package for nice formatting of the y-axis labels
  scale_y_continuous(labels = scales::comma) +
  # Format the x-axis to show month abbreviations
  scale_x_date(date_breaks = "2 months", date_labels = "%m-%y") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels for readability
  )

ggsave(paste0(indir, "SummaryResults/GEE_v3/Dayflow_justYOLO_WY", WY, ".tiff"), width = 8, height = 4, units = "in", dpi = 300)

# --- PLOT DUAL-AXIS SHOWING BOTH AREA AND FLOW ON SAME GRAPH

infile.s2.lm = "X:/delta_sentinel/raster/Water_Class/SummaryResults/GEE_v3/SH_WY2023_S2_AWEIsh_2025-09-28_lmAll.csv"

s2.lm = read_csv(infile.s2.lm) %>%
  mutate(Ymd = as.Date(as.character(Ymd), format = "%Y-%m-%d"), sensor = "S2") %>%
  mutate(WY = ifelse(month(Ymd) <= 9, year(Ymd), (year(Ymd)+1)))

# This command opens the file device. All subsequent plotting will be
# directed to this file until dev.off() is called.
tiff(
  filename = paste0(indir, "SummaryResults/GEE_v3/yolo_bypass_flor.tiff"),
  width = 8,         # Width of the plot in inches
  height = 4,        # Height of the plot in inches
  units = "in",      # Specify the units for width and height
  res = 300,         # Resolution in pixels per inch (DPI)
  compression = "lzw" # Use a lossless compression
)

# --- Set up plot parameters ---
# We need to increase the right margin to make space for the second axis
# The numbers are c(bottom, left, top, right). Default is c(5.1, 4.1, 4.1, 2.1)
par(mar = c(5, 5, 4, 5) + 0.1)

# --- 1. Plot the FIRST data series (Flow) ---
plot(df_wy2019_yolo$date, df_wy2019_yolo$YOLO,
     type = "l",                          # "l" for line plot
     col = "blue",                        # Line color
     lwd = 2,                             # Line width
     xlab = "Date",                       # X-axis label
     ylab = "Yolo Bypass Flow (cfs)",     # Y-axis label
     ylim = c(0, max(df_wy2019_yolo$YOLO)),  # Set y-limits
     main = "Yolo Bypass Flow and Flooded Area", # Main title
     col.lab = "blue"                     # Color for the axis label
)

# --- 2. Tell R to overlay the next plot ---
par(new = TRUE)

# --- 3. Plot the SECOND data series (Area) ---
# IMPORTANT: We suppress the axes and labels so they don't overwrite the first plot
plot(s2.lm$Ymd, s2.lm$ttlAreaHa,
     type = "p",           # "p" for points
     pch = 16,             # Point character type (16 = solid circle)
     cex = 0.75,           # Point size (Character EXpansion, <1 is smaller)
     col = "firebrick",
     xaxt = "n",                          # Suppress x-axis
     yaxt = "n",                          # Suppress y-axis
     xlab = "",                           # No x-label
     ylab = ""                            # No y-label
)

# --- 4. Add the second (right-side) Y-axis ---
# 'side = 4' means the right side.
# 'pretty()' calculates nice-looking tick mark locations.
axis(side = 4, at = pretty(range(s2.lm$ttlAreaHa)), col.axis = "firebrick")

# --- 5. Add the label for the second Y-axis ---
# 'mtext()' writes text in the plot margins
mtext("Flooded Area (ha)", side = 4, line = 3, col = "firebrick")

# --- 6. Add a legend ---
legend("topright",
       legend = c("Flow (cfs)", "Area (ha)"),
       col = c("blue", "firebrick"),
       lty = 1,                           # Line type (1 = solid)
       lwd = 2,                           # Line width
       bty = "n"                          # No box around the legend
)

dev.off()
