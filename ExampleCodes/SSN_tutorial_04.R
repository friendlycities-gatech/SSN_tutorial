library(sf) #for using spatial objects 
library(tidyverse) #for using tidy syntax 
library(tmap) #for visualizing maps 
library(tigris) #for downloading TIGER boundary shapefiles 
library(stplanr) #for using od2line function to convert points to lines
library(igraph) #for calculating node degree
library(SSNtools) #load sample datasets MafiaNodes and MafiaEdges

# ------- 4.1 Convert Points to Lines ------- # 

# ---- 4.1.1 # Method 1: Use `od2line` function in `stplanr` package. 
#library(SSNtools)
#library(tidyverse)
#library(stplanr)

data(MafiaNodes)
data(MafiaEdges)

# convert MafiaNodes to an sf geometry object (shapefile)
MafiaSpatial = MafiaNodes %>% 
  st_as_sf(coords=c("LonX", "LatY"), crs = 4326) 

# create line geometry 
MafiaEdges_toLine = od2line(MafiaEdges, MafiaSpatial)

# print first three rows 
MafiaEdges_toLine[c(1:3),]


# ---- 4.1.2 # Method 2: Group by lineID and Summarize Points into Line  

#library(SSNtools)
#library(tidyverse)
#library(sf)

data(MafiaNodes)
data(MafiaEdges)

# convert MafiaNodes to an sf geometry object (shapefile)
MafiaSpatial = MafiaNodes %>% 
  st_as_sf(coords=c("LonX", "LatY"), crs = 4326) 

# transform MafiaEdges to a format where each node in the edge is coded with an edge ID
MafiaEdges2 = MafiaEdges %>% mutate(ID = row_number()) %>% 
  pivot_longer(cols = c("Source", "Target"), names_to = "type", values_to = "NODE") %>% 
  left_join(MafiaSpatial, by=c('NODE'), copy=FALSE) 

# print first three rows of the data
MafiaEdges2[c(1:3),]

# ---- 4.1.3 # Method 3: Join two point geometry into one row and unite into line

#library(SSNtools)
#library(tidyverse)
#library(sf)

data(MafiaNodes)
data(MafiaEdges)

# attach point geometry to MafiaEdges for both Source and Target nodes 
MafiaEdges_toLine = MafiaEdges %>% 
  left_join(MafiaNodes, by=c('Source' = 'NODE'), copy=FALSE) %>% 
  left_join(MafiaNodes, by=c('Target' = 'NODE'), copy=FALSE) %>% 
  #LonX.x and LatY.x are coordinates for Source; 
  #LonX.y and LatY.y are coordinates for Target 
  select(c(Source, Target, LonX.x, LatY.x, LonX.y, LatY.y)) %>% 
  # this is an optional step to remove points that are at the same location
  filter(LonX.x != LonX.y & LatY.x != LatY.y) 

# this helper function converts a row with four coordinates into a 
# two by two matrix and cast it into a linestring.
st_segment = function(r){st_linestring(t(matrix(unlist(r), 2, 2)))}

# loop through each row and cast it into a linestring
MafiaEdges_toLine$geometry = st_sfc(
  sapply(1:nrow(MafiaEdges_toLine), 
         function(i){
           st_segment(MafiaEdges_toLine[i,][c('LonX.x', 'LatY.x', 'LonX.y', 'LatY.y')])},
         simplify=FALSE))

# ensure the output is an sf object and set the crs 
MafiaEdges_toLine = MafiaEdges_toLine %>% st_as_sf() %>% st_set_crs(4326)

MafiaEdges_toLine[c(1:3),]

# ------- 4.2 Visualizing Edges ------- # 

#library(SSNtools)
#library(tidyverse)
#library(sf)
#library(tmap)
#library(tigris)

# prepare data 
data(MafiaNodes)
data(MafiaEdges)

# convert coordinates to sf point geometries 
MafiaSpatial = MafiaNodes %>% 
  st_as_sf(coords=c("LonX", "LatY"), crs = 4326) 

# convert point geometries to lines 
MafiaEdges_toLine = od2line(MafiaEdges, MafiaSpatial)

# states is a function in tigris to download U.S. state boundary shapefile
us_states = states(cb=TRUE, progress_bar = FALSE) %>%
  filter(!STUSPS %in% c('PR','AS', 'AK', 'GU','MP','VI', 'HI')) 

# tmap functions to visualize maps
tmap_mode('plot')
tm_shape(us_states) + 
  tm_polygons() + 
  tm_shape(MafiaEdges_toLine) + 
  tm_lines()

# ------- 4.3 Visualizing Edges by Line Width ------- # 

# create a line weight column based on edge distance
MafiaEdges_toLine = MafiaEdges_toLine %>% mutate(weight = as.numeric(st_length(geometry)))

#library(tmap)
tmap_mode('plot')
tm_shape(us_states) + 
  tm_polygons(alpha=0, border.col = 'grey') + 
  tm_shape(MafiaEdges_toLine) + 
  #define line width with column `weight` and properties associated with lines 
  tm_lines(lwd='weight', scale=2, alpha=0.2, legend.lwd.is.portrait = TRUE,
           title.lwd = c('Distance (m)')) + 
  tm_layout(legend.position = c('right', 'bottom'))

# We create a column called flow_breaks that stores relative line width 
brks = round(quantile(MafiaEdges_toLine$weight, probs=c(0, 0.5, 0.9, 0.99, 1)), 0)

MafiaEdges_toLine = MafiaEdges_toLine %>% mutate(
  line_width = case_when(
    weight >= brks[1] & weight <= brks[2] ~ 0.1,
    weight > brks[2] & weight <= brks[3] ~ 0.3,
    weight > brks[3] & weight <= brks[4] ~ 0.5,
    weight > brks[4] & weight <= brks[5] ~ 1
  )
)

tmap_mode('plot')
tm_shape(us_states) + 
  tm_polygons(alpha=0, border.col = 'grey') + 
  tm_shape(MafiaEdges_toLine) + 
  #define line width with column `weight` and properties associated with lines 
  tm_lines(lwd='line_width', scale=2, alpha=0.2, 
           legend.lwd.is.portrait = TRUE,
           lwd.legend = c(0.1, 0.3, 0.5, 1)*2, 
           lwd.legend.labels=c('0-14','14-1630','1630-4000','4000-4150'),
           title.lwd = c('Distance (km)')) + 
  tm_layout(legend.position = c('right', 'bottom'))

# ------- 4.4 Visualizing Edges by Color ------- # 

tmap_mode('plot')
tm_shape(us_states) + 
  tm_polygons(alpha=0, border.col = 'grey') + 
  #reorder edges so that long distance edges are drawn first and short-ranged edges drawn last
  tm_shape(arrange(MafiaEdges_toLine, desc(weight))) + 
  #define line color with column `weight` and properties associated with lines 
  tm_lines(col='weight', scale=2, alpha=0.2, 
           breaks = round(quantile(MafiaEdges_toLine$weight, probs=c(0, 0.5, 0.9, 0.99, 1)), 0),
           style="fixed", n = 4, 
           labels=c('0-14','14-1630','1630-4000','4000-4150'),
           palette=c('#CCEBC5', '#7BCCC4', '#2B8CBE', '#094081'), 
           title.col = c('Distance (km)')) + 
  tm_layout(legend.position = c('right', 'bottom'))

# visualize edges by family groups (categorical color)

top_5 = c('Genovese', 'Lucchese', 'Gambino', 'Detroit', 'Chicago')
MafiaNodes = MafiaNodes %>% mutate(Family = ifelse(Family %in% top_5, Family, 'Others'))
MafiaEdges_toLine = MafiaEdges_toLine %>% 
  left_join(MafiaNodes %>% select(c(NODE, Family)), by=c('Source' = 'NODE'), copy=FALSE) %>% 
  left_join(MafiaNodes %>% select(c(NODE, Family)), by=c('Target' = 'NODE'), copy=FALSE) %>% 
  #the two left join above will automatically create Family.x and Family.y to differentiate having `Family` twice. 
  mutate(edge_family = ifelse(Family.x %in% top_5, Family.x, 'Others')) %>% 
  mutate(edge_family = ifelse(Family.y %in% top_5, Family.y, edge_family))

tmap_mode('plot')
tm_shape(us_states) + 
  tm_polygons(alpha=0, border.col = 'grey') + 
  tm_shape(arrange(MafiaEdges_toLine, desc(edge_family))) + 
  tm_lines(col='edge_family', style='cat', alpha=0.5, lwd=1,
           palette=c('#57B897', '#F7774F', '#7A8CC1', '#E072B5', '#FAD324', 'lightgrey'),
           title.col = c('Edges by Families')) + 
  tm_layout(legend.position = c('right', 'bottom'))

# ------- 4.5 Visualizing Edges by Width and Color ------- # 

tmap_mode('plot')

map = tm_shape(us_states) + 
  tm_polygons(alpha=0, border.col = 'grey') + 
  #reorder edges so that long distance edges are drawn first and short-ranged edges drawn last
  tm_shape(arrange(MafiaEdges_toLine, desc(weight))) + 
  tm_lines(
    #arguments that define the styles for color 
    col="weight", alpha=0.2, 
    breaks = round(quantile(MafiaEdges_toLine$weight, probs=c(0, 0.5, 0.9, 0.99, 1)), 0),
    style="fixed", n = 4, 
    palette=c('#CCEBC5', '#7BCCC4', '#2B8CBE', '#094081'),
    legend.col.show = FALSE,
    #arguments that define the styles for line width 
    lwd='line_width', scale=2,
    legend.lwd.show = FALSE
  ) + 
  #add manual legends to combine color and line width schema
  tm_add_legend(
    type=c('line'), 
    col=c('#CCEBC5', '#7BCCC4', '#2B8CBE', '#094081'), 
    lwd=c(0.1, 0.3, 0.5, 1)*2, 
    labels=c('0-14','14-1630','1630-4000','4000-4150'),
    title='Distance (km)') + 
  tm_layout(legend.position = c('right', 'bottom'))

map

# Visualize both nodes and edges 

#library(igraph)
g = graph_from_data_frame(MafiaEdges, directed = FALSE, vertices=MafiaSpatial)
MafiaSpatial$degree = degree(g)

map = map + 
  tm_shape(MafiaSpatial) +
  tm_symbols(size="degree", scale=2, #scale up the node size 
             col='orange', border.col='darkorange',
             alpha=0.2, border.alpha = 0.2,
             title.size=c('Degree'))

map

# full codes 

library(sf)
library(tidyverse)
library(tmap)
library(tigris)
library(stplanr)
library(igraph)

data(MafiaNodes)
data(MafiaEdges)

# convert MafiaNodes to an sf geometry object (shapefile)
MafiaSpatial = MafiaNodes %>% 
  st_as_sf(coords=c("LonX", "LatY"), crs = 4326) 

# create line geometry 
MafiaEdges_toLine = od2line(MafiaEdges, MafiaSpatial)

# states is a function in tigris to download U.S. state boundary shapefile
us_states = states(cb=TRUE, progress_bar = FALSE) %>%
  filter(!STUSPS %in% c('PR','AS', 'AK', 'GU','MP','VI', 'HI')) 

# create weight column for each edge 
MafiaEdges_toLine = MafiaEdges_toLine %>% mutate(weight = as.numeric(st_length(geometry)))
brks = round(quantile(MafiaEdges_toLine$weight, probs=c(0, 0.5, 0.9, 0.99, 1)), 0)

# create line_width column for each edge
MafiaEdges_toLine = MafiaEdges_toLine %>% mutate(
  line_width = case_when(
    weight >= brks[1] & weight <= brks[2] ~ 0.1,
    weight > brks[2] & weight <= brks[3] ~ 0.3,
    weight > brks[3] & weight <= brks[4] ~ 0.5,
    weight > brks[4] & weight <= brks[5] ~ 1
  )
)

# create degree column for each node
g = graph_from_data_frame(MafiaEdges, directed = FALSE, vertices=MafiaSpatial)
MafiaSpatial$degree = degree(g)

tmap_mode('plot')

map = tm_shape(us_states) + 
  tm_polygons(alpha=0, border.col = 'grey') + 
  #reorder edges so that long distance edges are drawn first and short-ranged edges drawn last
  tm_shape(arrange(MafiaEdges_toLine, desc(weight))) + 
  tm_lines(
    #arguments that define the styles for color 
    col="weight", alpha=0.2, 
    breaks = round(quantile(MafiaEdges_toLine$weight, probs=c(0, 0.5, 0.9, 0.99, 1)), 0),
    style="fixed", n = 4, 
    palette=c('#CCEBC5', '#7BCCC4', '#2B8CBE', '#094081'),
    legend.col.show = FALSE,
    #arguments that define the styles for line width 
    lwd='line_width', scale=2,
    legend.lwd.show = FALSE
  ) + 
  #add manual legends to combine color and line width schema
  tm_add_legend(
    type=c('line'), 
    col=c('#CCEBC5', '#7BCCC4', '#2B8CBE', '#094081'), 
    lwd=c(0.1, 0.3, 0.5, 1)*2, 
    labels=c('0-14','14-1630','1630-4000','4000-4150'),
    title='Distance (km)') + 
  tm_shape(MafiaSpatial) +
  tm_symbols(size="degree", scale=2, #scale up the node size 
             col='orange', border.col='darkorange',
             alpha=0.2, border.alpha = 0.2,
             title.size=c('Degree')) + 
  tm_layout(legend.position = c('right', 'bottom')) 

#tmap_save(map, filename='YOUR_LOCAL_FOLDER_PATH/map.png')
map
