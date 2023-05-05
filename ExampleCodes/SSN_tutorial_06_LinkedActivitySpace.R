# ----- 6.4.1 Visualize Linked Activity Spaces -----------#  

library(SSNtools)
library(ggplot2)
library(basemaps)
library(tidyverse)
library(sf)

data(EmergencyNodes)
data(EmergencyEdges)

# filter edges that contain the ego
ego_label = 'P1'
ego_alters_pairs = EmergencyEdges %>% filter(Source == ego_label | Target == ego_label)
ego_alters_pairs[c(1:3),]

# filter emergency locations associated with ego and its alters in node list  
ego_alters_locations = EmergencyNodes %>% filter(label %in% ego_alters_pairs$Source | label %in% ego_alters_pairs$Target) %>% 
  mutate(role = ifelse(label == ego_label, 'Primary Node', 'Linked Node'))

ego_alters_locations[c(1,2,46),]

# visualize locations visited by the primary nodes and linked nodes
ego_alters_locations = ego_alters_locations %>% 
  st_as_sf(coords=c('LonX', 'LatY'), crs=4326) %>% 
  st_transform(crs=3857) %>% #basemap_gglayer assumes data comes in with crs=3857
  mutate(LonX = sf::st_coordinates(.)[,1],
         LatY = sf::st_coordinates(.)[,2]) %>% 
  mutate(role = factor(role, levels=c('Primary Node', 'Linked Node')))

set_defaults(map_service = "carto", map_type = "light")
bgbox = st_bbox(ego_alters_locations %>% st_as_sf(coords=c('LonX', 'LatY'), crs=3857)) %>% st_as_sfc()

g1 = ggplot() + 
  basemap_gglayer(bgbox) + 
  scale_fill_identity() +
  geom_point(data = ego_alters_locations, 
             aes(x =LonX, y=LatY, color=role, size=role)) +
  scale_size_manual(values = c(`Primary Node` = 5, `Linked Node` = 2), name='Emergency Locations Visited by') +
  scale_color_manual(values = c(`Primary Node` = 'darkred', `Linked Node` = 'orange'), name='Emergency Locations Visited by') +
  coord_sf() + 
  theme_void() + 
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) 

g1

# visualize standard deviation ellipses for locations visited by the primary node and linked nodes.
g2 = 
  # map basemap 
  ggplot() + 
  basemap_gglayer(bgbox) + 
  scale_fill_identity() +
  # map locations associated with both the primary node and linked nodes. 
  geom_point(data = ego_alters_locations, 
             aes(x =LonX, y=LatY, color=role, size=role)) +
  scale_size_manual(values = c(`Primary Node` = 5, `Linked Node` = 2), name='Activity Space by') +
  scale_color_manual(values = c(`Primary Node` = 'darkred', `Linked Node` = 'orange'), name='Activity Space by') +
  # map activity space in the form of ellipses 
  stat_ellipse(data = ego_alters_locations, 
               geom = "polygon", aes(x=LonX, y=LatY, group=role, color=role),
               type = "norm", alpha=0, linetype='dashed', level = 0.8) + 
  coord_sf() + 
  theme_void() + 
  ggtitle('A Pair of Activity Spaces') + 
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title = element_text(hjust = 0.5)) 

g2 

# visualize standard deviation ellipses of locations visited by each unique responders 
g3 = suppressMessages(
  # map basemap 
  ggplot() + 
  basemap_gglayer(bgbox) + 
  scale_fill_identity() +
  # map locations associated with both the primary node and linked nodes. 
  geom_point(data = ego_alters_locations, 
             aes(x =LonX, y=LatY, color=role, size=role)) +
  scale_size_manual(values = c(`Primary Node` = 5, `Linked Node` = 2), name='Activity Space by') +
  scale_color_manual(values = c(`Primary Node` = 'darkred', `Linked Node` = 'orange'), name='Activity Space by') +
  # map activity space in the form of ellipses 
  stat_ellipse(data = ego_alters_locations, 
               geom = "polygon", aes(x=LonX, y=LatY, group=label, color=role),
               type = "norm", alpha=0, linetype='dashed', level = 0.8) + 
  coord_sf() + 
  theme_void() + 
  ggtitle('Linked Activity Spaces') + 
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        plot.title = element_text(hjust = 0.5))  
)
g3

# ----- 6.4.2 Quantify Linked Activity Spaces -----------#  

#reference: https://stackoverflow.com/questions/34650441/how-to-get-the-points-inside-of-the-ellipse-in-ggplot2

# retrieve points and ellipse data. 
build <- ggplot_build(g2)$data
points <- build[[2]]
ellipse <- build[[3]]

# convert ellipse data to sf polygons; ellipse_sf is an sf object with two geometries 
ellipse_sf <- ellipse %>%
  st_as_sf(coords = c("x", "y"), crs = 3857) %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

ellipse_sf

# check if each point in the ego_alters_locations dataset is within the respective ellipses
points_in_ellipses <- st_within(ego_alters_locations, ellipse_sf)

# example outputs in points_in_ellipses
points_in_ellipses[c(1,3,12)]

# calculate the number of points that are in both ellipses
sum(lengths(points_in_ellipses) > 1)

# retrieve points and ellipse data
build <- ggplot_build(g3)$data
points <- build[[2]]
ellipse <- build[[3]]

# convert ellipse data to sf polygons; ellipse_sf is an sf object with twenty-two geometries 
ellipse_sf <- ellipse %>%
  st_as_sf(coords = c("x", "y"), crs = 3857) %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

nrow(ellipse_sf)

# loop through each linked node's ellipse (starting from group 2) and count the number of locations in the intersection.
cnt = c()
for (grp in c(2:nrow(ellipse_sf))) {
  points_in_ellipses <- st_within(ego_alters_locations, ellipse_sf %>% filter(group %in% c(1, grp)))
  n_nodes_in_both_ellipses <- sum(lengths(points_in_ellipses) > 1)
  cnt = c(cnt, n_nodes_in_both_ellipses)
}

#pair up nodes and the number of nodes in P1's ellipse and this node's ellipse. 
data.frame(label = unique(ego_alters_locations$label), n_nodes_in_both_ellipses = c(NA, cnt))
