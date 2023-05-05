# ---- 6.3.1 Calculate Global Flattening Ratio ----- #

# the sum of the Euclidean distance between every pair of connected nodes in the network G- 
# (each node with degree D connects to its nearest D neighbor in Euclidean space) 
# compared to the sum of the Euclidean distance between every pair of connected nodes in G. 
library(SSNtools)

data(NYCMafiaNodes)
data(NYCMafiaEdges)

nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

# run both lines to get consistent GlobalFlatteningRatio results
set.seed(1234, "Mersenne-Twister", sample.kind="Rounding")
GlobalFlatteningRatio(nodes, edges, 100)

# ---- 6.3.2 Calculate Local Flattening Ratio ------ #

# This metric (adapted from Sarkar et al. 2019) is defined as the ratio of a 
# node’s minimized distance (d_opt) needed to connect to any k nearest neighbors 
# to the total actual distance (d_act) of its connections. 
# Nodes with low values prioritize distant connections. 

library(SSNtools)

data(NYCMafiaNodes)
data(NYCMafiaEdges)

nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

results = LocalFlatteningRatio(nodes, edges)
nodelist = results[[1]]
edgelist = results[[2]]


# ---- 6.3.3 Visualize Local Flattening Ratio ------ #

# packages for wrangling data and map visualization
library(tmap)
library(tidyverse)
library(sf)
library(stplanr)
library(basemaps)

# convert heat dataframe to a spatial sf object
MafiaSpatial = nodelist %>% 
  left_join(NYCMafiaNodes, by=c('label'), copy=FALSE) %>% st_as_sf(coords=c('LonX', 'LatY'), crs=32118)

# convert edgelist dataframe to line geometry
NYCMafiaEdges_shp = od2line(edgelist, MafiaSpatial) 

# create basemap with functions from basemaps library
set_defaults(map_service = "carto", map_type = "light")
bgbox = st_bbox(MafiaSpatial) %>% st_as_sfc()
bg = basemap_stars(bgbox)

tmap_mode('plot')
g = 
  # map base map
  tm_shape(bg) +
  tm_rgb(alpha=0.8) +
  # map all edges in grey
  tm_shape(NYCMafiaEdges_shp) + 
  tm_lines(alpha=0.5, col='grey') + 
  # map edges that also connect to K-nearest neighbors in black 
  tm_shape(NYCMafiaEdges_shp %>% filter(is_K_nearest_neighbor == 1)) + 
  tm_lines(alpha=0.5, col='black') + 
  # map nodes colored by Local_flattening_ratio. Nodes with higher Local_flattening_ratio values mapped last. 
  tm_shape(MafiaSpatial %>% arrange(Local_flattening_ratio)) + 
  tm_symbols(col='Local_flattening_ratio', size=0.1, style='fixed', 
             breaks=c(0, 0.05, 0.1, 0.15, 1),
             palette = c('#FBD977', '#F78D3D', '#E3211B', '#800F26'),
             legend.col.show = FALSE) + 
  #customize legend and layout
  tm_add_legend(type='symbol', 
                labels=c('(0, 0.05]', '(0.05, 0.10]', '(0.10, 0.15]', '(0.15, 1]', 'NA (Less than minK)'), 
                col=c('#FBD977', '#F78D3D', '#E3211B', '#800F26', 'grey'), 
                is.portrait = T, title=c('Local Flattening Ratio'), size=0.5) + 
  tm_add_legend(type='line', labels=c('Edges also Connect to KNN', 'All Edges'), 
                col=c('black', 'grey'), lwd=1) + 
  tm_scale_bar(breaks=c(0, 5, 10), text.size = 0.7, position=c(0.78,0.9)) +  
  tm_layout(legend.title.size = 0.9, legend.text.size = 0.7, legend.width = 1,
            main.title = 'NYC Mafia SSN Local Flattening Ratio',
            main.title.position = c('center'),
            main.title.size = 1.2)

g

# ---- 6.3.4 Application to a bipartite network ------ #  
library(SSNtools)

data(POINodes)
data(POIEdges)

nodes = processNode(POINodes, 'label', 'LonX', 'LatY', 'Bipartite')
edges = processEdge(POIEdges, 'Source', 'Target', 'Weight')

results = LocalFlatteningRatio(nodes, edges, minK=2, bipartite=TRUE)
nodelist = results[[1]]
edgelist = results[[2]]

nodelist[c(1:3),]
edgelist[c(1:3),]
