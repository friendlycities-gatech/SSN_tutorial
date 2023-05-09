# ----------------------------------------------------------------------------------------------#
# ----- 6.1.1 Calculate SSN hotspots using radius or K-nearest neighbor window sizes -----------#
# ----------------------------------------------------------------------------------------------#

library(SSNtools)
  
data(NYCMafiaNodes)
data(NYCMafiaEdges)

nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

# we will use NDScanRadius as an example
result = NDScanRadius(nodes, edges, 500, min=3)
heat = result[[1]]
edgeWithin = result[[2]]

# print example NDSCanRadius outputs at selected rows
heat[c(1,93,102), ]
edgeWithin[c(1,254,259), ]

# other SSN hotspots functions; may take a few minutes to run!! 
# calculates network density within 10 nearest neighbors of each node in a network
# heat = NDScanKNearest(nodes, edges, 10)[[1]]

# calculates network density within a radius (500 meters - Manhattan distance) of each node in a network
# heat = NDScanManhattan(nodes, edges, 500)[[1]]

# calculates the density of edges within 10 nearest neighbors of each node in a network
# heat = edgeScanKNearest(nodes, edges, 10)[[1]]

# calculates the density of edges within a radius (500 meters - Manhattan distance) of each node in a network
# heat = edgeScanManhattan(nodes, edges, 500)[[1]]

# ----- 6.1.2 Visualize SSN hotspots -----------#  
library(tmap)
library(basemaps)
library(sf)
library(tidyverse)
library(stplanr)
library(SSNtools)

# we will use NDScanRadius as an example
result = NDScanRadius(nodes, edges, 500, min=3)
heat = result[[1]]
edgeWithin = result[[2]]

# convert heat dataframe to a spatial sf object
MafiaSpatial = heat %>% 
  left_join(NYCMafiaNodes, by=c('label'), copy=FALSE) %>% st_as_sf(coords=c('LonX', 'LatY'), crs=32118)

# convert edgeWithin dataframe to line geometry and filter those that are within the window radius.
NYCMafiaEdges_shp = od2line(edgeWithin, MafiaSpatial) %>% 
  filter(WithinWindow == 1)

# create basemap with functions from basemaps library
set_defaults(map_service = "carto", map_type = "light")
bgbox = st_bbox(MafiaSpatial) %>% st_as_sfc()
bg = basemap_stars(bgbox)

# create hotspot map with functions from tmap library
tmap_mode('plot')
g = 
  tm_shape(bg) +
  tm_rgb(alpha=0.8) +
  #map edges within searching window 
  tm_shape(NYCMafiaEdges_shp) + 
  tm_lines(col='black', lwd=1) + 
  #map nodes with NA values 
  tm_shape(MafiaSpatial %>% filter(is.na(heat))) + 
  tm_symbols(col='white', size=0.05) +  
  #map nodes with heat values, allowing nodes with higher heat values to be mapped on top
  tm_shape(MafiaSpatial %>% drop_na(heat) %>% arrange(heat)) + 
  tm_symbols(col='heat', size=0.1, style='fixed', legend.col.show = FALSE, palette = c('#FBD977', '#F78D3D', '#E3211B', '#800F26'), breaks=c(0, 0.00001, 0.10, 0.25, 0.667)) +
  #customize legend and layout
  tm_add_legend(type='symbol', labels=c('0', '(0, 0.10]', '(0.10, 0.25]', '(0.25, 0.667]', 'NA (Less than MinPts)'), col=c('#FBD977', '#F78D3D', '#E3211B', '#800F26', 'white'), is.portrait = T, title=c('Network Density'), size=0.5) + 
  tm_add_legend(type='line', labels=c(paste0('Edges within window size')), col='black', lwd=1) + 
  tm_scale_bar(breaks=c(0, 5, 10), text.size = 1) +  
  tm_layout(legend.title.size = 1.3, legend.text.size = 1, legend.width = 1) + 
  tm_layout(main.title = 'NYC 1960s Mafia SSN Hotspots',
            main.title.position = c('center'),
            main.title.size=1.5)
g

# Plot Inset Map 
# create a bounding box in crs=32118 to retrieve basemap 
Inset = st_bbox(data.frame(lon=c(-74.00434, -73.98125), lat=c(40.73080, 40.71109)) %>% 
                  st_as_sf(coords=c('lon', 'lat'), crs=4326) %>% st_transform(crs=32118)) 

# create a basemap for the inset map
Inset_bg = basemap_stars(Inset)

# create the inset map
InsetMap = tm_shape(Inset_bg) + 
  tm_rgb() + 
  tm_shape(NYCMafiaEdges_shp ) + 
  tm_lines(col='black', lwd=1) + 
  tm_shape(MafiaSpatial %>% filter(is.na(heat))) + 
  tm_symbols(col='white', size=0.05) +
  tm_shape(MafiaSpatial %>% drop_na(heat) %>% arrange(heat)) + 
  tm_symbols(col='heat', size=0.2, style='fixed', 
             breaks=c(0, 0.00001, 0.10, 0.25, 0.667), legend.col.show = FALSE, 
             palette = c('#FBD977', '#F78D3D', '#E3211B', '#800F26')) + 
  tm_layout(frame=c('black'), frame.lwd = 2, main.title = c('Little Italy'), 
            main.title.position = c('center'),
            main.title.size = 0.8, fontface = 2)

# create the bounding box map
box = tm_shape(st_bbox(Inset) %>% st_as_sfc()) + tm_polygons(alpha=0, border.col='black', lwd=1)

# create aspect ratio to preserve the height and width ratio in the inset map
aspect_ratio = unname((Inset$ymax - Inset$ymin)/(Inset$xmax - Inset$xmin))

# --- uncomment below export the background map g with the inset map ----
library(grid)
tmap_save(g + box, insets_tm=InsetMap,
          insets_vp = viewport(0.21, 0.54, width = 0.3, height = aspect_ratio*0.5),
          filename='PATH', dpi=600)

# --- uncomment below to print maps in R Studio Plots window ---- 
# library(grid)
# g + box
# print(InsetMap, vp=viewport(0.21, 0.54, width = 0.3, height = aspect_ratio*0.5))

# ------------------------------------------------------------------#
# ----- 6.1.3 Find optimal window sizes for SSN hotspots -----------#
# ------------------------------------------------------------------#

library(SSNtools)
library(ggplot2)
library(ggdist)
library(fpc)

nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

# create an empty dataframe to store values in the loop
df = data.frame(matrix(ncol = 3, nrow = 0))
colnames(df) = c('cluster', 'Average Network Density', 'Window Size')

# loop through each window size (meter)
for (i in seq(100, 2000, 100)) { 
  # calculate the heat values 
  heat = NDScanRadius(nodes, edges, i, min=3)[[1]]
  # attach spatial information to nodes whose heat is not NA
  MafiaSpatial = heat %>% drop_na(heat) %>% left_join(NYCMafiaNodes, by=c('label'), copy=FALSE)
  # assign the DBSCAN cluster ID back to each node; 
  # DBSCAN eps argument takes in the same reachability distance value (window size) and reachability minimum no. of points as the NDScanRadius()
  MafiaSpatial$cluster = fpc::dbscan(MafiaSpatial %>% select(c(LonX, LatY)), eps = i, MinPts = 3)$cluster
  # filter noise (fpc::dbscan classified noise into cluster 0) and calculate the average network density by DBSCAN cluster 
  MafiaSpatial = MafiaSpatial %>% filter(cluster != 0) %>% group_by(cluster) %>% 
    summarise(`Average Network Density`=mean(heat)) %>%
    mutate(`Window Size` = i)
  # merge output from one window size into the final dataframe 
  df = rbind(df, MafiaSpatial)
}

# visualize through a raincloud plot 
g = ggplot(df, aes(x=`Window Size`, y=`Average Network Density`, group=`Window Size`)) + 
  geom_boxplot(colour = "lightgrey", outlier.color = NA, width=0.2) + 
  ggdist::stat_dots(side='left', justification=1.1, binwidth=0.01, alpha=0.5, col='black', dotsize=1) + 
  ylab('Avg. Network Density per DBSCAN Cluster') + 
  xlab('Window Size (Euclidean Distance Meter)') + 
  ggtitle(paste0('New York Optimal Window Size for NDScan')) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size=20),
        legend.text = element_text(size=15))

g

# --------------------------------------------------------------------------------------------------------------#
# ----- 6.1.4 Calculate SSN hotspots using a user-defined walking distance matrix extracted from OSM -----------#  
# --------------------------------------------------------------------------------------------------------------#

# create a dataframe that contains all non-repetitive combinations of nodes
library(tidyverse)
# create a dataframe that contains all non-repetitive combinations of nodes
allEdgesTable = as.data.frame(t(combn(NYCMafiaNodes$label, 2))) 
colnames(allEdgesTable) <- c('Source', 'Target')

edgeDistance = allEdgesTable %>% 
  #attach coordinates to allEdgesTable
  left_join(NYCMafiaNodes, by=c('Source' = 'label'), copy=FALSE) %>% 
  left_join(NYCMafiaNodes, by=c('Target' = 'label'), copy=FALSE) %>% 
  #again, dist is in the unit of meter
  mutate(dist = sqrt((LonX.x - LonX.y)**2 + (LatY.x - LatY.y)**2)) %>% 
  filter(dist <= 2000) 

# print example outputs 
edgeDistance[c(1:3), ]

# create origin and destination table
origin = edgeDistance %>% 
  select(c(Source, LonX.x, LatY.x)) %>% st_as_sf(coords=c('LonX.x', 'LatY.x'), crs=32118) %>%
  st_transform(crs=4326) %>% 
  mutate(LonX = sf::st_coordinates(.)[,1],
         LatY = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

des = edgeDistance %>% 
  select(c(Target, LonX.y, LatY.y)) %>% st_as_sf(coords=c('LonX.y', 'LatY.y'), crs=32118) %>%
  st_transform(crs=4326) %>% 
  mutate(LonX = sf::st_coordinates(.)[,1],
         LatY = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

# print example outputs 
origin[c(1:3), ]

origin_sub = origin[c(1:10),]
des_sub = des[c(1:10),]

# distances contains a list of outputs. 
distances <- osrmTable(
  src = origin_sub[c("LonX", "LatY")],
  dst = des_sub[c("LonX", "LatY")],
  osrm.profile = "foot",
  measure = c("distance")
)

# distances$distances returns a full distance matrix between all nodes in origin_sub and des_sub. We are only interested in the pairwise result and thus only take the diagonal values. Distance values are meters. 
Distance_m = diag(distances$distances)
Distance_m

Distance_m = sample.int(2000, nrow(origin), replace=TRUE)

dist = data.frame(Source = origin$Source, Target = des$Target, Distance_m = Distance_m)

# create an edge table with walking distance 
edgeDistance = edgeDistance %>% left_join(dist, by=c('Source' = 'Source', 'Target' = 'Target'), copy=FALSE) %>% 
  select(c(Source, Target, Distance_m)) %>%
  mutate(Distance_m = ifelse(Distance_m == 0, 0.001, Distance_m))

library(igraph)
# create input full matrix from the edge table. as_adjacency_matrix() fills empty sell with zeros and zero has practical meaning in our example. Thus we convert 0 to NA and we converted those that have actual 0 in distance (converted to 0.001 in the code above) back to 0. 
g = graph_from_data_frame(edgeDistance, directed=FALSE, vertices=NYCMafiaNodes)
mat = as_adjacency_matrix(g, sparse=F, attr="Distance_m") 
mat[mat==0]<-NA
mat[mat==0.001]<-0

# find SSN hotspots within 1 mile (~1600m) walking distance! 
nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

heat = edgeScanMatrix(nodes, edges, 1600, mat, min=3)[[1]]

# -----------------------------------------------------------------------#
# ----- 6.1.4 Application to a weighted and bipartite network -----------#  
# -----------------------------------------------------------------------#

library(SSNtools)

data(POINodes)
data(POIEdges)

nodes = processNode(POINodes, 'label', 'LonX', 'LatY', 'Bipartite')
edges = processEdge(POIEdges, 'Source', 'Target', 'Weight')

# calculate edgeScan values for KNN = 5 in a weighted, bipartite network
# this takes about 3 mins to run
result = edgeScanKNearest(nodes, edges, 5, weighted=TRUE, bipartite=TRUE)
heat = result[[1]]
edgeWithin = result[[2]]

heat[c(1:3),]
edgeWithin[c(1:3),]

# calculate NDScan values for KNN = 5 in an undirected, bipartite network
# this takes about 3 mins to run
result = NDScanKNearest(nodes, edges, k=5, directed=FALSE, bipartite=TRUE)
heat = result[[1]]
edgeWithin = result[[2]]

heat[c(1:3),]
edgeWithin[c(1:3),]

# visualize POI visits hotspots
library(tmap)
library(tidyverse)
library(basemaps)
library(sf)

POISpatial = heat %>% left_join(POINodes, copy=FALSE) %>% st_as_sf(coords=c('LonX', 'LatY'), crs=26967)
edgeWithin = edgeWithin %>% left_join(POINodes, by=c('Source' = 'label'), copy=FALSE) %>% 
  left_join(POINodes, by=c('Target' = 'label'), copy=FALSE) 
st_segment = function(r){st_linestring(t(matrix(unlist(r), 2, 2)))}
edgeWithin$geometry = st_sfc(sapply(1:nrow(edgeWithin), 
                                    function(i){st_segment(edgeWithin[i,][c('LonX.x', 'LatY.x', 'LonX.y', 'LatY.y')])},simplify=FALSE)) 

edgeWithin = edgeWithin %>% st_as_sf() %>% st_set_crs(26967)

set_defaults(map_service = "carto", map_type = "light")
bgbox = st_bbox(POISpatial)

bg = basemap_stars(bgbox, map_service = 'carto')

legend_name = 'Network Density'
k = '5'
breaks = c(0.07, 0.30, 0.40, 0.50, 1)
labels = c('[0.07, 0.30)', '[0.3, 0.4)', '[0.4, 0.50)', '[0.50, 0.75]')
output = c('Atlanta POI Visits NDScan')

tmap_mode('plot')
g = tm_shape(bg) + 
  tm_rgb(alpha=0.8) + 
  tm_shape(edgeWithin %>% filter(WithinWindow == 1)) + 
  tm_lines(alpha=0.5) + 
  tm_shape(POISpatial) + 
  tm_symbols(col='heat', size=0.05, style='fixed', legend.col.show = FALSE, 
             palette = c('#FBD977', '#F78D3D', '#E3211B', '#800F26'), breaks=breaks) +
  tm_add_legend(type='symbol', labels=labels, col=c('#FBD977', '#F78D3D', '#E3211B', '#800F26'), title=paste0(legend_name, ' (KNN = ', k, ')'), size=0.5) +
  tm_add_legend(type='line', labels=c(paste0('Edges within window size')), col='black', lwd=1) + 
  tm_scale_bar(breaks=c(0, 1, 2, 4), text.size = 1) +  
  tm_layout(legend.title.size = 1.3, legend.text.size = 1, legend.width = 1) + 
  tm_layout(legend.bg.color = 'white', legend.bg.alpha = 0.7,
            main.title = output,
            main.title.position = c('center')) 
g

#tmap_save(g, 'PATH')
