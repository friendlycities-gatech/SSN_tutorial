## Hotspot Detection for Spatial Social (Non-planar) Networks

**SSNtools** is an R package that provides metrics for analyzing and
visualizing spatial social networks.

**SSNtools** is currently equipped with functions to calculate hotspots
(i.e., heat) of spatial social networks. Traditional GIS hotspot
detection methods (e.g. the Getis-Ord GI\* statistic or Ripley’s
K-function) only apply to point patterns, and yet, clustered nodes in
network may not be connected.

The goal for the current function of **SSNtools** is to detect the
number of non-planar edges and the network density of a subset of a
social network contained within a focal window. In another words, the
algorithms return hotspots where nodes are not only densely located but
also connected.

See paper [Spatial Social Networks (SSN) Hot Spot Detection: Scan
Methods for Non-planar Networks](https://arxiv.org/pdf/2011.07702.pdf)
for detailed methodology.

#### Installation

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("friendlycities-gatech/SSNtools")
```

#### Example

This is a basic example which shows you how to use the main functions of
SSN hotspot detections in SSNtools. The package comes with an example
dataset, which is a spatial social network of Mafia members in New York
City in the 1960s. See [To racketeer among neighbors: spatial features
of criminal collaboration in the American
Mafia](https://drive.google.com/file/d/1guVURnryYUyXaJ3A7SoMFMpkv7CUx6He/view)
for more details about the dataset. You can call **NYCMafiaNodes**
(n=298) and **NYCMafiaEdges**(n=946) to directly access the sample
dataset. The coordinate unit of the sample dataset is **meter**. 

``` r
library(SSNtools)
# ----process dataframe into a list of lists 
# params:
#     nodes - a R dataframe containing node label, longitude, and latitude
#     label_name - the name of the column for node label
#     lon_name - the name of the column for node longitude 
#     lat_name - the name of the column for node latitude
nodes = processNode(NYCMafiaNodes, 'label', 'LonX', 'LatY')
# params:
#     edges - a R dataframe containing source node label and target node label
#     source_name - the name of the column for source node label
#     target_name - the name of the column for target node label
edges = processEdge(NYCMafiaEdges, 'Source', 'Target')

#-----calculates network density within a radius (500 meters - Euclidean distance) of each node in a network
# params:
#     nodes - a list of named lists. Each sublist contains a node.
#     edges - a list of list. Each sublist contains an edge.
#     maxRadius - radius (in the coordinate unit) of search window. 
result = NDScanRadius(nodes, edges, 500)
```

#### Other Available Functions

Currently available functions in SSNtools for SSN hotspot detection:

``` r
library(SSNtools)
#-----calculates network density within 10 nearest neighbors of each node in a network
result = NDScanKNearest(nodes, edges, 10)

#-----calculates network density within a radius (500 meters - Manhattan distance) of each node in a network
result = NDScanManhattan(nodes, edges, 500)

#----calculate the density of edges within a radius (500 meters - Euclidean distance) of every node in a graph
result = edgeScanRadius(nodes, edges, 500)

#-----calculates the density of edges within 10 nearest neighbors of each node in a network
result = edgeScanKNearest(nodes, edges, 10)

#-----calculates the density of edges within a radius (500 meters - Manhattan distance) of each node in a network
result = edgeScanManhattan(nodes, edges, 500)
```

#### Future Updates

This package is developed under NSF Career Grant: A Research and
Educational Framework for Incorporating Spatial Heterogeneity into
Social Network Analysis. More spatial social network metrics will be
incorporated in the package in the near future.