library(sf)
library(tidyverse)
library(tmap)
library(igraph)
library(tigris)
#install.packages("devtools") #to download R package on GitHub
#devtools::install_github("friendlycities-gatech/SSNtools")
library(SSNtools)

# --------------------------------------------------#
# ------- 3.1 Convert Coordinates to Points ------- # 
# --------------------------------------------------#

#MafiaNodes is a built-in dataset in SSNtools R package

data(MafiaNodes)
MafiaSpatial = MafiaNodes %>% 
  st_as_sf(coords=c("LonX", "LatY"), crs = 4326) 

head(MafiaSpatial, 3)

# --------------------------------------------------#
# ------- 3.2 Visualize Nodes --------------------- # 
# --------------------------------------------------#

#Convert coordinate system
MafiaSpatial = MafiaSpatial %>%
  st_transform(MafiaSpatioal, crs="ESRI:102010")

#states is a function in tigris to download U.S. state boundary shapefile
us_states = states(cb=TRUE, progress_bar = FALSE) %>%
  filter(!STUSPS %in% c('PR','AS', 'AK', 'GU','MP','VI', 'HI')) %>%
  st_transform(crs="ESRI:102010")

tmap_mode('plot') # set tmap plotting mode to static map 
tm_shape(us_states) +  
  tm_polygons() + #draw U.S. state shapefile first as polygons
  tm_shape(MafiaSpatial) + 
  tm_symbols() #overlay nodes as points

# --------------------------------------------------#
# ------- 3.3 Visualize Nodes by Node Color ------- # 
# --------------------------------------------------#

#MafiaEdges is a built-in dataset in SSNtools
data(MafiaEdges)
#Construct a network and calculate the degree for each node. 
g = graph_from_data_frame(MafiaEdges, directed = FALSE, vertices=MafiaSpatial)
MafiaSpatial$degree = degree(g)

tmap_mode('plot')
tm_shape(us_states) +
  tm_polygons(alpha=0) +
  tm_shape(MafiaSpatial) +
  tm_symbols(
    size=1, #constant node size
    col='degree', palette = 'YlGnBu', #define which numeric column to vary by color and the color palette
    style='quantile', n = 4, #define the color breaks using quantile, and set the number of classes to 4.
    alpha=0.5, border.col='black', border.alpha = 0.5,
    title.col=c('Degree of Connections'))

# --------------------------------------------------#
# ------- 3.3 Visualize Nodes by Node Size -------- # 
# --------------------------------------------------#

# Visualize 
tmap_mode('plot')
tm_shape(us_states) +
  tm_polygons(alpha=0) + #turn the background color of the polygons to transparent
  tm_shape(MafiaSpatial) +
  tm_symbols(size="degree", scale=2, #scale up the node size 
             col='lightblue', border.col='#6698CC', 
             title.size=c('Degree of Connections'))

quantile(MafiaSpatial$degree)

# Create a new 'degree_bracket' column that contains the node size for each group of nodes. 
MafiaSpatial = MafiaSpatial %>%
  mutate(degree_brackets = case_when(
    degree >= 2 & degree < 8 ~ 0.1,
    degree >= 8 & degree < 12 ~ 0.3, 
    degree >= 12 & degree < 20 ~ 0.5,
    degree >= 20 & degree <= 154 ~ 1
  ))

# Visualize
tmap_mode('plot')
tm_shape(us_states) +
  tm_polygons(alpha=0) +
  tm_shape(MafiaSpatial) +
  tm_symbols(
    size="degree_brackets", size.max = 1, scale = 1.5,
    sizes.legend = c(0.1, 0.3, 0.5, 1)/1*1.5,
    sizes.legend.labels = c('2-8','8-12','12-20','20-154'),
    col='lightblue', border.col='#6698CC', #blue grey
    alpha=0.5, border.alpha = 0.5, 
    title.size=c('Degree of Connections'))

# ------------------------------------------------------#
# ------- 3.4 Visualize Nodes by Size and Color ------- # 
# ------------------------------------------------------#
tmap_mode('plot')
map = tm_shape(us_states) +
  tm_polygons(alpha=0) +
  tm_shape(MafiaSpatial) +
  tm_symbols(
    #arguments that define point sizes 
    size="degree_brackets", size.max = 1, scale = 1.5,
    legend.size.show = FALSE,
    #arguments that define point colors 
    col='degree_brackets', palette = 'YlGnBu', n = 4,
    alpha=0.5, 
    border.col='black', border.alpha = 0.5,
    legend.col.show = FALSE) +
  tm_add_legend(type=c("symbol"),
                #copy from RColorBrewer::brewer.pal(4, "YlGnBu")
                col = c("#FFFFCC", "#A1DAB4", "#41B6C4", "#225EA8"), 
                alpha = 0.5, is.portrait = FALSE, # legend becomes horizontal
                # size defined here should be the square root of the normalized point size * scale. 
                # The square root is taken since the area is proportional to the data, not the radius.
                size = (c(0.1,0.3,0.5,1)/1)^0.5*1.5, 
                labels = c('2-8','8-12','12-20','20-154'), #break labels 
                border.col = 'black', border.alpha = 0.2,
                title = c("Degree of Connections"))

map

# --------------------------------------------------#
# ------- Full codes for 3.4 ---------------------- # 
# --------------------------------------------------#

# complete codes from start to end 
library(readr)
library(igraph)
library(tmap)
library(sf)
library(tidyverse)
library(SSNtools)

#read data
MafiaSpatial = MafiaNodes %>% 
  st_as_sf(coords=c("LonX", "LatY"), crs = 4326) 

us_states = states(cb=TRUE, progress_bar=FALSE) %>%
  filter(!STUSPS %in% c('PR','AS', 'AK', 'GU','MP','VI', 'HI')) %>%
  st_transform(crs="ESRI:102010")

# Construct a network and calculate the degree for each node.
g = graph_from_data_frame(MafiaEdges, directed = FALSE, vertices=MafiaSpatial)
MafiaSpatial$degree = degree(g)

# Create a new column that assign point size to points with varying degree.
MafiaSpatial = MafiaSpatial %>%
  mutate(degree_brackets = case_when(
    degree >= quantile(MafiaSpatial$degree)[1] & degree < quantile(MafiaSpatial$degree)[2] ~ 0.1,
    degree >= quantile(MafiaSpatial$degree)[2] & degree < quantile(MafiaSpatial$degree)[3] ~ 0.3,
    degree >= quantile(MafiaSpatial$degree)[3] & degree < quantile(MafiaSpatial$degree)[4] ~ 0.5,
    degree >= quantile(MafiaSpatial$degree)[4] & degree <= quantile(MafiaSpatial$degree)[5] ~ 1
  ))

# Visualize
tmap_mode('plot')
tm_shape(us_states) +
  tm_polygons(alpha=0) +
  tm_shape(MafiaSpatial) +
  tm_symbols(
    #arguments that define point sizes 
    size="degree_brackets", size.max = 1, scale = 1.5,
    legend.size.show = FALSE,
    #arguments that define point colors 
    col='degree_brackets', palette = 'YlGnBu', n = 4,
    alpha=0.5, 
    border.col='black', border.alpha = 0.5,
    legend.col.show = FALSE) +
  tm_add_legend(type=c("symbol"),
                #copy from RColorBrewer::brewer.pal(4, "YlGnBu")
                col = c("#FFFFCC", "#A1DAB4", "#41B6C4", "#225EA8"), 
                alpha = 0.5, is.portrait = FALSE, # legend becomes horizontal
                # size defined here should be the square root of the normalized point size * scale. 
                # The square root is taken since the area is proportional to the data, not the radius.
                size = (c(0.1,0.3,0.5,1)/1)^0.5*1.5, 
                labels = c('2-8','8-12','12-20','20-154'), #break labels 
                border.col = 'black', border.alpha = 0.2,
                title = c("Degree of Connections"))

#tmap_save(map, filename='YOUR_LOCAL_FOLDER_PATH/map.png')