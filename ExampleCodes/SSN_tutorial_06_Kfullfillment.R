# --------------------------------------------------#
# ---- 6.2.1 Calculate K-fullfillment values -------#
# --------------------------------------------------#

library(SSNtools)

data(NYCMafiaNodes)
data(NYCMafiaEdges)

nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

results = Kfullfillment(nodes, edges, minK=1, bipartite=FALSE)
nodelist = results[[1]]
edgelist = results[[2]]

nodelist[c(1:3), ]
edgelist[c(1:3), ]

# --------------------------------------------------#
# ----- 6.2.2 Visualize K-fullfillment -------------# 
# --------------------------------------------------#

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
  # map nodes colored by K_fullfillment. Nodes with higher K_fullfillment values mapped last. 
  tm_shape(MafiaSpatial %>% arrange(K_fullfillment)) + 
  tm_symbols(col='K_fullfillment', size=0.1, style='fixed', 
             breaks=c(0, 0.00001, 0.10, 0.25, 1),
             palette = c('#FBD977', '#F78D3D', '#E3211B', '#800F26'),
             legend.col.show = FALSE) + 
  # customize legend and layout
  tm_add_legend(type='symbol', 
                labels=c('0', '(0, 0.10]', '(0.10, 0.25]', '(0.25, 1]', 'NA (Less than minK)'), 
                col=c('#FBD977', '#F78D3D', '#E3211B', '#800F26', 'grey'), 
                is.portrait = T, title=c('K-fullfillment'), size=0.5) + 
  tm_add_legend(type='line', labels=c('Edges also Connect to KNN', 'All Edges'), 
                col=c('black', 'grey'), lwd=1) + 
  tm_scale_bar(breaks=c(0, 5, 10), text.size = 0.7, position=c(0.78,0.9)) +  
  tm_layout(legend.title.size = 0.9, legend.text.size = 0.7, legend.width = 1) + 
  tm_layout(main.title = 'NYC Mafia SSN K-fullfillment',
            main.title.position = c('center'),
            main.title.size = 1.5)

g

# ----------------------------------------------------------#
# ----- 6.2.3 Application to a bipartite network -----------#  
# ----------------------------------------------------------#

library(SSNtools)
data(POINodes)
data(POIEdges)

nodes = processNode(POINodes, 'label', 'LonX', 'LatY', 'Bipartite')
edges = processEdge(POIEdges, 'Source', 'Target', 'Weight')

results = Kfullfillment(nodes, edges, minK=1, bipartite=TRUE)
nodelist = results[[1]]
edgelist = results[[2]]

nodelist[c(1:3),]
edgelist[c(1:3),]
