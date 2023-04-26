library(SSNtools)

data(NYCMafiaNodes)
data(NYCMafiaEdges)

nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

results = Kfullfillment(nodes, edges, minK=2, bipartite=FALSE)
nodelist = results[[1]]
edgelist = results[[2]]

library(tmap)
library(tidyverse)
library(sf)
library(stplanr)
library(basemaps)

MafiaSpatial = nodelist %>% 
  left_join(NYCMafiaNodes, by=c('label'), copy=FALSE) %>% st_as_sf(coords=c('LonX', 'LatY'), crs=32118)

NYCMafiaEdges_shp = od2line(edgelist, MafiaSpatial) %>% 
  filter(is_k_nearest_neighbor == 1)

set_defaults(map_service = "carto", map_type = "light")
bgbox = st_bbox(MafiaSpatial) %>% st_as_sfc()
bg = basemap_stars(bgbox)
  
tmap_mode('plot')
g = 
  tm_shape(bg) +
  tm_rgb(alpha=0.8) +
  tm_shape(MafiaSpatial %>% arrange(K_fullfillment)) + 
  tm_symbols(col='K_fullfillment', size=0.1, style='fixed', 
             breaks=c(0, 0.00001, 0.10, 0.25, 1),
             palette = c('#FBD977', '#F78D3D', '#E3211B', '#800F26'),
             legend.col.show = FALSE) + 
  tm_shape(NYCMafiaEdges_shp) + 
  tm_lines(alpha=0.5) + 
  #customize legend and layout
  tm_add_legend(type='symbol', 
                labels=c('0', '(0, 0.10]', '(0.10, 0.25]', '(0.25, 1]', 'NA (Less than min_degree)'), 
                col=c('#FBD977', '#F78D3D', '#E3211B', '#800F26', 'white'), 
                is.portrait = T, title=c('K-fullfillment'), size=0.5) + 
  tm_add_legend(type='line', labels=c(paste0('Connections to K-nearest Neigbhors')), 
                col='black', lwd=1) + 
  tm_scale_bar(breaks=c(0, 5, 10), text.size = 0.7) +  
  tm_layout(legend.title.size = 0.9, legend.text.size = 0.7, legend.width = 1) + 
  tm_layout(main.title = 'NYC 1960s Mafia SSN K-fullfillment',
            main.title.position = c('center'),
            main.title.size = 1.5)

g

#Application to a bipartite network
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
