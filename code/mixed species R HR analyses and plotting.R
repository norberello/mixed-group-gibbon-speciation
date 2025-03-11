
library(readxl)
library(anytime)
library(tidyverse)
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(ggmap)
library(RColorBrewer)
library(sp)
library(adehabitatHR)
library(adehabitatLT)
library(mapview)
library(raster)
library(rgdal)
library(move)
library(tmap)

mix.data <- read_excel("mixed lar pil 5m locations.xlsx")
names(mix.data)

# Assuming your X and Y variables are named X and Y
#new_data <- mix.data[complete.cases(mix.data$X, mix.data$Y),
#                     c("X", "Y")]#this is just to keep x and y

# Remove the row at index 713 from the original dataframe
#new_data <- new_data[-713, , drop = FALSE]

#names(new_data)
# Remove the row at index 713



#new_data <- mix.data[complete.cases(mix.data), ]

# Assuming your dataframe is named df
#new_data <- mix.data %>% drop_na(X, Y)
#new_data <- new_data[-713, ]
#names(new_data)

mix.data <- mix.data[-c(4154,4153,4152,4156,4155,4151,6350,6349,
                        6569,6571,6570), ]
#remove duplicate rows from data frame
mix.data[!duplicated(mix.data), ]




#convert to simple feature
library(sf)
mysf <- sf::st_as_sf(mix.data, coords = c("x", "y"), 
                     crs = 32647)
head(mysf)
plot(mysf)

###################los shapefiles
# Load the shapefile
sf.streams <- st_read("KS_streams.shp")
sf.roads <- st_read("KS_roads.shp")
sf.contours <- st_read("KS_contours.shp")

#plot for visual inspection
mapview::mapview(mysf)
#can I have the layer as raster of whater window?

# Create separate convex hulls for each group
#convex_hulls <- st_union(st_convex_hull(mysf[mysf$grouping == "lar group", ]),
 #                        st_convex_hull(mysf[mysf$grouping == "pileated group", ]),
  #                       # Add more lines for additional groups if needed
   #                      crs = st_crs(mysf))

# You can also create a new column in 'mysf' indicating the group for each point
# mysf$group <- factor(mysf$group)  # Ensure 'group' is a factor if not already

# Plot the convex hulls
#plot(st_geometry(mysf), col = c(1,2,3))
#plot(convex_hulls, add = TRUE, border = "black", lty = 2, lwd = 2)


# Calculate convex hulls for each group
convex_hulls <- st_convex_hull(mysf, group_by = "grouping")

convex_hulls <- st_convex_hull(mysf)

# Print the resulting sf object
print(convex_hulls)
st_area(convex_hulls) #Ta

?st_convex_hull

?st_area

library(GeoRange)
CHullArea(mix.data$x,mix.data$y)

# Assuming mix.data contains columns x, y, and grouping
area_grouped <- mix.data %>%
  group_by(grouping) %>%
  summarise(area = CHullArea(x, y),
            ha = area/10000)

area_grouped


# Convert mix.data to sf
mysf <- st_as_sf(mix.data, coords = c("x", "y"), crs = 32647)



# Calculate convex hulls for each group
convex_hulls <- mysf %>%
  group_by(grouping) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()

# Calculate the area of each convex hull
area <- st_area(convex_hulls)

# Add the area information to the convex_hulls sf object
convex_hulls$area <- area
area/10000


############
# Convert mix.data to sf
mysf <- st_as_sf(mix.data, coords = c("x", "y"), crs = 32647)

# Calculate convex hulls for each group
convex_hulls <- mysf %>%
  group_by(grouping) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()

# Calculate the area of each convex hull in square meters
area_sqm <- st_area(convex_hulls)

# Add the area information to the convex_hulls sf object
convex_hulls$area_sqm <- area_sqm

# Convert the area from square meters to hectares (if necessary)
convex_hulls$area_hectares <- convex_hulls$area_sqm / 10000

# Print the resulting sf object with area information
print(convex_hulls)

#################





###########


head(mix.data)
mix.data$grouping <- as.factor(mix.data$grouping)
levels(mix.data$grouping)

# Specify the desired order of levels
desired_order <- c("mixed-species group", "lar group", "pileated group")

# Reorder the levels of the "phase" variable
mix.data$grouping <- factor(mix.data$grouping, levels = desired_order)
levels(mix.data$grouping)


ggplot(mix.data,aes(x,y))+
  geom_point()+
  facet_grid(~grouping)

# Plot using ggplot2
ggplot(mix.data, aes(x, y, col = grouping)) +
  geom_point()+
  theme_bw()

ggplot(mix.data, aes(x,y, colour=grouping,fill =grouping)) + 
  geom_point() + 
  geom_density2d(alpha=.5) + 
  labs(x = "northing", y = "easting") + 
  geom_polygon(data=mix.data, alpha=.2)+
  facet_grid(~grouping)

ggplot(mix.data, aes(x,y, colour=grouping,fill =grouping)) + 
  geom_density2d(alpha=.5) + 
  labs(x = "northing", y = "easting") + 
  geom_polygon(data=mix.data, alpha=.2)+
  facet_grid(~grouping)

ggplot(mix.data, aes(x,y, colour=grouping,fill =grouping)) + 
  labs(x = "northing", y = "easting") + 
  geom_polygon(data=mix.data, alpha=.2)+
  facet_grid(~grouping)

# 2d histogram with default option
ggplot(mix.data, aes(x=x, y=y) ) +
  geom_bin2d() +
  theme_bw()+
  facet_grid(~grouping)


# Bin size control + color palette
ggplot(mix.data, aes(x=x, y=y) ) +
  geom_bin2d(bins = 15) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  facet_grid(~grouping)

# Hexbin chart with default option
ggplot(mix.data, aes(x=x, y=y) ) +
  geom_hex() +
  theme_bw()+
  facet_grid(~grouping)

# Bin size control + color palette
ggplot(mix.data, aes(x=x, y=y) ) +
  geom_hex(bins = 12) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()+
  facet_grid(~grouping)

# Show the contour only
ggplot(mix.data, aes(x=x, y=y) ) +
  geom_density_2d()+
  facet_grid(~grouping)

# Show the area only
ggplot(mix.data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+
  facet_grid(~grouping)

# Area + contour
ggplot(mix.data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")+
  facet_grid(~grouping)+
  geom_point(alpha=.5)+
  theme_bw()

# Using raster
ggplot(mix.data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position='none'
  )+facet_grid(~grouping)


ggplot(mix.data, aes(x = x, y = y)) +
  geom_point(alpha=.2) + 
  geom_density_2d()+
  facet_grid(~grouping)+
  theme_bw()

ggplot(mix.data, aes(x = x, y = y)) +
  geom_point(alpha=.2)+
  geom_density_2d_filled(alpha = 0.2) +
  geom_density_2d(linewidth = 0.25, colour = "black")+
  facet_grid(~grouping)+
  theme(
    legend.position='none'
  )

ggplot(mix.data, aes(x = x, y = y))+ 
  stat_density_2d(geom = "point", aes(size = after_stat(density)), n = 20, contour = FALSE)+
  facet_grid(~grouping)+
  theme(
    legend.position='none'
  )

ggplot(mix.data,aes(x=x, y= y)) + 
  stat_density_2d(aes(fill = stat(level)),
                  geom = "polygon", 
                  n = 100 ,
                  bins = 10) + 
  facet_wrap(grouping~.) +
    theme_bw()+
  theme(
    legend.position='none'
  )

ggplot(mix.data,aes(x=x,y=y))+#it could b Z
    stat_density_2d(geom = "raster",
                    aes(fill = ..density..),
                    contour = FALSE)+
  theme(
    legend.position='none'
  )
  
ggplot(mix.data,aes(x=x,y=y))+#it could b Z
    stat_density_2d(geom = "raster",
                    aes(fill = ..density..),
                    contour = F)+ 
    facet_wrap(grouping~.) +
    theme_bw()+
  theme(
    legend.position='none'
  )
  
ggplot(mix.data,aes(x=x,y=y,color=grouping))+
    stat_density_2d(geom = "point",
                    aes(size = ..density..),
                    n = 15, contour = FALSE)+
    facet_wrap(grouping~.) +
    theme_bw()+
  theme(
    legend.position='none'
  )
  
##################################
  
  # Load the raster file
  file.name <- raster("bacground 18.tif")
  
  # Convert the raster data to a data frame
  df <- as.data.frame(file.name, xy = TRUE)
  
  # Rename the columns to x, y, and value
  colnames(df) <- c("x", "y", "value")
  
  # Create a ggplot2 plot
  p <- ggplot() +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    coord_quickmap()
  
  p#funciona, pero fondo azul
  
  
# Load necessary libraries
  library(raster)
  library(ggplot2)
  
  # Load the raster file
  file.name <- raster("bacground 18.tif")
  
  # Convert the raster data to a data frame
  df <- as.data.frame(file.name, xy = TRUE)
  
  # Rename the columns to x, y, and value
  colnames(df) <- c("x", "y", "value")
  
  # Create a ggplot2 plot
  p <- ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    coord_quickmap()
  
  p
  

 

  
  # Load the raster file
  file.name <- raster("bacground 18.tif")
  
  # Convert the raster data to a data frame
  df <- as.data.frame(file.name, xy = TRUE)
  
  # Rename the columns to x, y, and value
  colnames(df) <- c("x", "y","value")
  
  # Create a ggplot2 plot
  p <- ggplot(df, aes(x = x, y = y,fill=value)) +
    geom_raster() +
    coord_quickmap()
  
  p
  
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    coord_quickmap() +
    theme_void()  # You can also use theme_minimal() for a cleaner look
  
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    coord_quickmap() +
    scale_fill_gradient(low = "black", high = "white")  # Adjust the colors as needed
  
  # 

  
  # Load the raster file
  file.name <- raster("bacground 18.tif")
  
  # Convert the raster data to a data frame
  df <- as.data.frame(file.name, xy = TRUE)
  
  # Rename the columns to x, y, and value
  colnames(df) <- c("x", "y", "value")

# Create a ggplot2 plot
p <- ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradient(low = "black", high = "white") +
  geom_density_2d(data = mix.data, aes(x = x, y = y), linewidth = 1, color = "red") +
  coord_quickmap() +
  facet_grid(~grouping) +
  theme(legend.position = "none")+
  theme_bw()
p




p <- ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradient(low = "black", high = "white") +
  geom_point(data = mix.data, aes(x = x, y = y,alpha=.4),color="blue") +
  coord_quickmap() +
  facet_grid(~grouping) +
  theme(legend.position = "none")
p
###################################


# Load the raster file
file.name <- raster("bacground 18.tif")

# Convert the raster data to a data frame
df <- as.data.frame(file.name, xy = TRUE)

# Rename the columns to x, y, and value
colnames(df) <- c("x", "y", "value")

# Load the raster file
file.name <- raster("background2.tif")

# Convert the raster data to a data frame
df <- as.data.frame(file.name, xy = TRUE)
head(df)

# Rename the columns to x, y, and value
colnames(df) <- c("x", "y", "value")

ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = value))+
  scale_fill_gradient(low = "black", high = "white")+
  geom_point(data = mix.data, aes(x = x, y = y,color=grouping))+
  coord_quickmap() +
  facet_grid(~grouping)+
  theme(legend.position = "none")

########
###https://docs.ropensci.org/terrainr/reference/geom_spatial_rgb.html
###########
##########THIS IS THE ONE!!!!

library(terrainr)

merged_stack <- terra::rast("background2.tif")

ggplot() +
  geom_spatial_rgb(
    data = merged_stack,
    mapping = aes(
      x = x,
      y = y,
      r = red,
      g = green,
      b = blue
    )
  ) +
  geom_point(data = mix.data, aes(x = x, y = y,color=grouping))+
  coord_quickmap() +
  facet_grid(~grouping)+
  theme(legend.position = "none")


####]]]****
##########################using all sf shapefiles

# Define the extent of the map
extent <- st_bbox(mysf)


#
ggplot() +
  geom_sf(data = mysf) + 
  geom_sf(data = sf.contours) +
  geom_sf(data = sf.roads) +
  geom_sf(data=sf.streams)+
  theme(legend.position = "none")+
  xlim(c(extent$xmin, extent$xmax)) +
  ylim(c(extent$ymin, extent$ymax))+
  facet_wrap(~grouping)+
  theme_bw()


#######
ggplot() +
  geom_sf(data = sf.contours, color = "grey", size = 0.5) + # Contours as grey thin lines
  geom_sf(data = sf.roads, color = "black") + # Roads as black lines
  geom_sf(data = sf.streams, color = "blue") +
  geom_sf(data = mysf) + # Streams as blue lines
  theme(legend.position = "none") +
  xlim(c(extent$xmin, extent$xmax)) +
  ylim(c(extent$ymin, extent$ymax)) +
  facet_wrap(~grouping) +
  theme_bw()



ggplot() +
  geom_sf(data = sf.contours, color = "grey", size = 0.5) + # Contours as grey thin lines
  geom_sf(data = sf.roads, color = "black") + # Roads as black lines
  geom_sf(data = sf.streams, color = "blue") +
  geom_sf(data = mysf) + # Streams as blue lines
  theme(legend.position = "none") +
  xlim(c(extent$xmin, extent$xmax)) +
  ylim(c(extent$ymin, extent$ymax)) +
  facet_wrap(~grouping) +
  theme_bw()



# Your existing ggplot code
ggplot() +
  geom_spatial_rgb(
    data = merged_stack,
    mapping = aes(
      x = x,
      y = y,
      r = red,
      g = green,
      b = blue
    )
  ) +
  geom_point(data = mix.data, aes(x = x, y = y, color = grouping), size = 3.5,
             alpha=.2,shape=16) +
  coord_quickmap() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("mixed-species group" = "chocolate", 
                                "lar group" = "green",
                                "pileated group" = "red")) +
  labs(x="Easting",y="Northing") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 14),
    strip.background = element_rect(fill = "lightgray")
  )+facet_grid(~grouping)


################HULLSSSS
library(dplyr)
hull_data <- 
  mix.data %>%
  drop_na() %>%
  group_by(grouping) %>% 
  slice(chull(x, y))
head(hull_data)


ggplot() +
  geom_spatial_rgb(
    data = merged_stack,
    mapping = aes(
      x = x,
      y = y,
      r = red,
      g = green,
      b = blue
    )
  ) +
  geom_polygon(data = hull_data, aes(x = x, y = y, color = grouping), linewidth = 1,
             alpha=.1) +
  coord_quickmap() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("mixed-species group" = "chocolate", 
                                "lar group" = "green",
                                "pileated group" = "red")) +
  labs(x="Easting",y="Northing") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 14),
    strip.background = element_rect(fill = "lightgray")
  )+
  geom_point(data = mix.data, aes(x = x, y = y, color = grouping), size = 2,
             alpha=.2,shape=3)+
  facet_grid(~grouping)



ggplot() +
  geom_spatial_rgb(
    data = merged_stack,
    mapping = aes(
      x = x,
      y = y,
      r = red,
      g = green,
      b = blue
    )
  ) +
  geom_polygon(data = hull_data, aes(x = x, y = y, color = grouping), linewidth = 1,
               alpha=.05) +
  coord_quickmap() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("mixed-species group" = "chocolate", 
                                "lar group" = "green",
                                "pileated group" = "red")) +
  labs(x="Easting",y="Northing") +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1,size=8),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "lightgray")
  )+
  geom_point(data = mix.data, aes(x = x, y = y), size = 1.75,
             alpha=.35,shape=3)+
  facet_grid(~grouping)

#"Spatial Distribution and Convex Hull Home Ranges of Gibbon Groups in Khao Yai National Park"

#Subtitle:
#  "Mixed Gibbon Species Group, Lar Gibbon Group, and Pileated Gibbon Group"

#Caption:
#  "Locations marked with crosses and home ranges outlined with polygons.


# Assuming `mysf` is your `sf` object containing the convex hulls
library(sf)


ggplot() +
  geom_spatial_rgb(
    data = merged_stack,
    mapping = aes(
      x = x,
      y = y,
      r = red,
      g = green,
      b = blue
    )
  ) +
  geom_polygon(data = hull_data, aes(x = x, y = y, color = grouping), linewidth = 1,
               alpha=.05) +
  coord_quickmap() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("mixed-species group" = "chocolate", 
                                "lar group" = "green",
                                "pileated group" = "red")) +
  labs(x="Easting",y="Northing") +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1,size=8),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "lightgray")
  )+
  geom_point(data = mix.data, aes(x = x, y = y), size = 1.2,
             alpha=.35,shape=19)+
  facet_grid(~grouping)






ggplot() +
  geom_spatial_rgb(
    data = merged_stack,
    mapping = aes(
      x = x,
      y = y,
      r = red,
      g = green,
      b = blue
    )
  ) +
  geom_point(data = mix.data, aes(x = x, y = y, color = grouping), size = 2,
             alpha=.3,shape=16)+
  geom_polygon(data = hull_data, aes(x = x, y = y, color = grouping), linewidth = 1,
               alpha=.01) +
  coord_quickmap() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("mixed-species group" = "chocolate", 
                                "lar group" = "green",
                                "pileated group" = "red")) +
  labs(x="Easting",y="Northing") +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1,size=8),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "lightgray")
  )+
  facet_grid(~grouping)


##########################################shapefiles
##########################################
##########################################THIS IS THE ONE!!!!

# Specify the desired order of levels
desired_order <- c("mixed-species group", "lar group", "pileated group")

# Reorder the levels of the "phase" variable
mysf$grouping <- factor(mix.data$grouping, levels = desired_order)
levels(mysf$grouping)



ggplot() +
  geom_sf(data = sf.contours, color = "grey", size = 0.5) + # Contours as grey thin lines
  geom_sf(data = sf.roads, color = "black") + # Roads as black lines
  geom_sf(data = sf.streams, color = "blue") +
  geom_sf(data = mysf,alpha=.3) +
  geom_polygon(data = hull_data, aes(x = x, y = y, color = grouping), linewidth = 1,
               alpha=.01) +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1,size=8),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "lightgray")
  )+
  xlim(c(extent$xmin, extent$xmax)) +
  ylim(c(extent$ymin, extent$ymax)) +
  facet_wrap(~grouping) +
  theme_bw()+
  theme(legend.position = "none")








#################


####this what I want
l <- layout(matrix(c(1, 1,  # First, second
                     2, 3), # and third plot
                   nrow = 2,
                   ncol = 2,
                   byrow = TRUE))

layout.show(l)

#########################

library(ggplot2)
library(ggpubr) # For ggarrange

library(ggplot2)
library(ggpubr) # For ggarrange

# Function to create individual ggplot plots
create_plot <- function(data, hulls, grouping) {
  ggplot() +
    geom_sf(data = sf.contours, color = "grey", size = 0.5) +
    geom_sf(data = sf.roads, color = "black") +
    geom_sf(data = sf.streams, color = "blue") +
    geom_sf(data = data[data$grouping == grouping,], alpha = .3) +
    geom_polygon(data = hulls[hulls$grouping == grouping,], aes(x = x, y = y), color = "red", fill = NA) +
    labs(x = "Longitude", y = "Latitude") +
    theme(
      axis.title = element_text(size = 20),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      strip.text = element_text(size = 20),
      plot.title = element_text(size = 20),
      strip.background = element_rect(fill = "lightgray")
    ) +
    xlim(c(extent$xmin, extent$xmax)) +
    ylim(c(extent$ymin, extent$ymax)) +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(grouping) + annotate("text", x = 756907, y = 1592546, label = "Road to Khao Kiew", hjust = 0, vjust = 1,fontface = "italic")+
    ggtitle(grouping) + annotate("text", x = 757007, y = 1593650, label = "stream", hjust = 0, vjust = 1,fontface = "italic",color="blue")+ 
    theme(plot.title = element_text(size = 16))# Add title with grouping name
}

# Get unique groupings
unique_groupings <- unique(mysf$grouping)

# Create and arrange plots
plots <- lapply(unique_groupings, function(group) create_plot(mysf, hull_data, group))
#ggarrange(plotlist = plots, nrow = 2, ncol = 2)

plots[1]
plots[2]
plots[3]

# Arrange plots using ggarrange
ggarrange(
  plots[[2]], 
  ggarrange(plots[[1]], plots[[3]], ncol = 2, nrow = 2), 
  ncol = 1
)

ggarrange(
  plots[[2]],                # First row with line plot
  # Second row with box and dot plots
  ggarrange(plots[[1]], plots[[3]], ncol = 2), 
  nrow = 2       # Label of the line plot
) 

# Arrange plots using ggarrange
ggarrange(
  plots[[2]], 
  ggarrange(plots[[1]], plots[[3]], ncol = 1), 
  ncol = 2
)





#############################################
ggplot() +
  geom_sf(data = sf.contours, color = "grey", size = 0.5) + # Contours as grey thin lines
  geom_sf(data = sf.roads, color = "black") + # Roads as black lines
  geom_sf(data = sf.streams, color = "blue") +
  geom_sf(data = mysf,alpha=.3) +
  geom_polygon(data = hull_data, aes(x = x, y = y, color = grouping), linewidth = 1,
               alpha=.01) +
  labs(x = "Longitude", y = "Latitude") +
  scale_y_continuous(breaks = seq(floor(min(hull_data$y)), ceiling(max(hull_data$y)), by = 0.5)) + # Setting breaks on y-axis
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "lightgray")
  ) +
  xlim(c(extent$xmin, extent$xmax)) +
  ylim(c(extent$ymin, extent$ymax)) +
  facet_wrap(~ grouping) +
  theme_bw() +
  theme(legend.position = "none")


##################
# Convert sf object to data.frame
mysf_df <- st_sf(mysf)

# Plot using ggplot
ggplot() +
  geom_sf(data = sf.contours, color = "grey", size = 0.5) + # Contours as grey thin lines
  geom_sf(data = sf.roads, color = "black") + # Roads as black lines
  geom_sf(data = sf.streams, color = "blue") +
  geom_sf(data = mysf_df) + # Use the converted data.frame
  geom_polygon(data = hull_data, aes(x = x, y = y, color = grouping), linewidth = 1,
               alpha = .01) +
  labs(x = "", y = "") +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 8, angle = 0), # Setting angle to 0 for y-axis labels
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "lightgray")
  ) +
  xlim(c(extent$xmin, extent$xmax)) +
  ylim(c(extent$ymin, extent$ymax)) +
  facet_wrap(~ grouping) +
  theme_bw() +
  theme(legend.position = "none")




# Plot using ggplot
ggplot() +
  geom_sf(data = sf.contours, color = "grey", size = 0.5) + # Contours as grey thin lines
  geom_sf(data = sf.roads, color = "black") + # Roads as black lines
  geom_sf(data = sf.streams, color = "blue") +
  geom_sf(data = mysf_df, aes(x = longitude, y = latitude)) + # Specify x and y variables
  geom_polygon(data = hull_data, aes(x = x, y = y, color = grouping), linewidth = 1,
               alpha = .01) +
  labs(x = "Longitude", y = "Latitude") + # Naming x and y axes
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 8, angle = 0), # Setting angle to 0 for y-axis labels
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "lightgray")
  ) +
  xlim(c(extent$xmin, extent$xmax)) +
  ylim(c(extent$ymin, extent$ymax)) +
  facet_wrap(~ grouping) +
  theme_bw() +
  theme(legend.position = "none")






#########
library(ggplot2)
library(sf)

# Assuming sf.streams is an sf object containing stream data
# and sf.streams has the 'name' attribute

ggplot() +
  geom_sf(data = sf.contours, color = "grey", size = 0.5) + # Contours as grey thin lines
  geom_sf(data = sf.roads, color = "black") + # Roads as black lines
  geom_sf(data = sf.streams, color = "blue") + # Streams as blue lines
  geom_sf_label(data = sf.streams, aes(label = name), size = 3) + # Adding stream names as labels
  geom_sf(data = mysf) +
  geom_polygon(data = hull_data, aes(x = x, y = y, color = grouping), linewidth = 1,
               alpha = 0.01) +
  labs(x = "", y = "") +
  theme(
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 10),
    strip.background = element_rect(fill = "lightgray")
  ) +
  xlim(c(extent$xmin, extent$xmax)) +
  ylim(c(extent$ymin, extent$ymax)) +
  facet_wrap(~grouping) +
  theme_bw() +
  theme(legend.position = "none")


#############################merged stream

#

##################################################
##################################################

data.soc <- read.csv("LP18_may2017.csv")
names(data.soc)
str(data.soc)


hours <- data.soc %>%
  group_by(PHASE) %>%
  drop_na(Time)%>%
  summarise(hours=sum(Time)/60) 

hours

# Assuming your date column is named "Date" and PHASE is your grouping variable
day.count <- data.soc %>%
  group_by(PHASE) %>%
  drop_na(Day) %>%
  summarise(total_days = length(unique(as.Date(Day, format = "%d-%b-%y"))))

day.count




# Assuming your date column is named "Date" and PHASE is your grouping variable
result <- data.soc %>%
  group_by(PHASE) %>%
  drop_na(Time, Day) %>%
  summarise(
    total_hours = sum(Time)/60,
    total_days = length(unique(as.Date(Day, format = "%d-%b-%y")))
  )

result






# Assuming your date column is named "Day" and PHASE is your grouping variable
totals <- data.soc %>%
  drop_na(Time, Day) %>%
  summarise(
    total_hours = sum(Time) / 60,
    total_days = length(unique(as.Date(Day, format = "%d-%b-%y")))
  )

totals

names(data.soc)

encounter.result <- data.soc %>%
  group_by(PHASE) %>%
  drop_na(Time, Day)%>%
  filter(Behavior == "IE") %>%
  summarise(
    number.of.ie = n(),
    total_hours = sum(Time) / 60,
    encounter.rate=number.of.ie/total_hours
  )

encounter.result



groom.result <- data.soc %>%
  group_by(PHASE) %>%
  drop_na(Time, Day)%>%
  filter(Behavior == "G") %>%
  summarise(
    number.of.ie = n(),
    total_hours = sum(Time) / 60,
    groomming.rate=number.of.ie/total_hours
  )

groom.result


#grooming interactions/directions
#"Actor"             "target"        
groom.result <- data.soc %>%
  group_by(PHASE) %>%
  filter(Behavior == "G") %>%
  summarise(
    number.of.ie = n()
  )
groom.result

interaction_matrix <- data.soc %>%
  filter(Behavior == "G") %>%
  group_by(PHASE, Actor, target) %>%
  summarise(number_of_interactions = n()) %>%
  spread(target, number_of_interactions, fill = 0) %>%
  ungroup()

interaction_matrix


#let´s do quartet phase only
interaction_matrix_quartet <- data.soc %>%
  filter(PHASE == "quartet", Behavior == "G") %>%
  group_by(Actor, target) %>%
  summarise(number_of_interactions = n()) %>%
  spread(target, number_of_interactions, fill = 0) %>%
  ungroup()
interaction_matrix_quartet

# Reshape the data from wide to long format
interaction_long <- tidyr::pivot_longer(interaction_matrix_quartet, -Actor, names_to = "Target", values_to = "Number_of_interactions")

# Repeat each interaction according to the number of times it occurred
interaction_long_repeated <- interaction_long[rep(row.names(interaction_long), interaction_long$Number_of_interactions),]

# Create the relations data frame
relations <- data.frame(from = interaction_long_repeated$Actor, to = interaction_long_repeated$Target)

# Print the relations data frame
print(relations)

g <- graph_from_data_frame(relations, directed=TRUE)
plot(g)




# Convert character matrix to numeric matrix
numeric_matrix <- as.matrix(as.data.frame(lapply(interaction_matrix_quartet[, -1], as.numeric)))

# Set row and column names
rownames(numeric_matrix) <- interaction_matrix_quartet$Actor
colnames(numeric_matrix) <- interaction_matrix_quartet$Actor

# Create igraph object
graph <- graph_from_adjacency_matrix(numeric_matrix, mode = "directed", weighted = TRUE)

# Plot the graph
plot(graph, layout = layout_with_fr(graph), vertex.label.dist = 2, 
     vertex.label.cex = 1.2,
     main = "Interactions during quartet phase")



interaction_matrix_trio <- data.soc %>%
  filter(PHASE == "trio", Behavior == "G") %>%
  group_by(Actor, target) %>%
  summarise(number_of_interactions = n()) %>%
  spread(target, number_of_interactions, fill = 0) %>%
  ungroup()
interaction_matrix_trio

# Convert character matrix to numeric matrix
numeric_matrix_trio <- as.matrix(as.data.frame(lapply(interaction_matrix_trio[, -1], as.numeric)))
numeric_matrix_trio


# Set row and column names
rownames(numeric_matrix_trio) <- interaction_matrix_trio$Actor
colnames(numeric_matrix_trio) <- interaction_matrix_trio$Actor

# Create igraph object
graph_trio <- graph_from_adjacency_matrix(numeric_matrix_trio, mode = "directed", weighted = TRUE)

# Plot the graph
plot(graph_trio, layout = layout_with_fr(graph_trio), vertex.label.dist = 2, 
     vertex.label.cex = 1.2,
     main = "Interactions during trio phase")



# Plot the graph
plot(graph_trio, layout = layout_with_fr(graph_trio), vertex.label.dist = 2, 
     vertex.label.cex = 1.2,
     main = "Interactions during trio phase")







interaction_matrix_fisfus <- data.soc %>%
  filter(PHASE == "quartet fis-fus", Behavior == "G") %>%
  group_by(Actor, target) %>%
  summarise(number_of_interactions = n()) %>%
  spread(target, number_of_interactions, fill = 0) %>%
  ungroup()
interaction_matrix_fisfus


#################################
