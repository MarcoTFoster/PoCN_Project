#' ---
#' title: "R Notebook"
#' output: html_notebook
#' ---
#' 
#' In this project we will create networks representing different cities in the United Kingdom that are made up of all public transport options that stay within that city. We utilise a dataset created by Riccardo Gallotti and Marc Barthelemy that integrates data from the timetables of all different types of public transport options in the UK to create a complete picture of all public transport options for a time period of a week in October 2010.
#' 
#' We begin by getting dplyr, designed to make data manipulation in R easy and intuitive, and geosphere, to be able to read the coordinates of each transport stop to both determine which city the stop belongs to and its position in the network relative to other stops in the same city.
#' 
#' We also read the nodes and edges csv files - the nodes representing transport 'stops' and edges representing a journey from one stop to another. We also read population data of each city in the UK as we are only interested in recreating transport maps for UK cities with a population above 50,000.
#' 
## ------------------------------------------------------------------------------------------------------
library(geosphere)
library(dplyr)

nodes <- read.csv("C:\\Users\\Student\\Downloads\\nodes.csv")
edges <- read.csv("C:\\Users\\Student\\Downloads\\edges.csv")
population_data <- read.csv("C:\\Users\\Student\\Downloads\\country-cities-data.csv")
reasonable_distance <- 10000

#' 
#' With the datasets read we continue with the following steps:
#' 
#' cities_above_500k - this step uses the population dataset to filter out any UK cities with a population below 50,000 and keep those cities with a population above this.
#' 
#' assign_city - this is a function to assign each node to a city, where we use the longitude and latitude of a node and the longitude and latitude of each city to determine the distances from that node to each city. We then select the one with the shortest distance - the city centre that the node is closest too, and if the distance from the city centre to the node is below our reasonable distance value, 10,000m, we determine that the node is inside that city and we return that city (if it is too far away we return NA).
#' 
## ------------------------------------------------------------------------------------------------------
cities_above_50k <- population_data %>%
  filter(pop2024 >= 50000) %>%
  pull(city)

assign_city <- function(node_lon, node_lat, city_data, max_distance) {
  distances <- distHaversine(
    matrix(c(node_lon, node_lat), ncol = 2),
    matrix(c(city_data$longitude, city_data$latitude), ncol = 2)
  )
  nearest_city <- city_data$city[which.min(distances)]
  min_distance <- min(distances)
  
  if (min_distance <= max_distance) {
    return(nearest_city)
  } else {
    return(NA)
  }
}

#' 
#' We now carry out our assign_city function to add a column in the nodes dataset called 'city', which states which city the node is in (or NA if it is not in a city above 50k population).
#' 
## ------------------------------------------------------------------------------------------------------
nodes$city <- apply(
  nodes, 1,
  function(row) assign_city(as.numeric(row["lon"]), as.numeric(row["lat"]), population_data, reasonable_distance)
)

#' 
## ------------------------------------------------------------------------------------------------------
print(nodes)

#' 
#' With each city having their transport stops assigned to them we cycle through each relevant city and create a dataset of nodes and a dataset of edges for that city. We achieve this by doing the following:
#' 
#' 1) first we run an if statement to only consider nodes where the city value is not NA.
#' 
#' 2) we then filter the nodes to include only those that are in the city we are looking at
#' 
#' 3) We also must get the edges that are within this city - these would be the edges that link between two nodes that are both within the relevant city. You can see we filter only for the edges where both the origin and destination nodes are both in the 'city_nodes' dataset.
#' 
#' 4) With all the nodes and edges in the city accounted for we write two new .csv files for them both. We do this for very single city.
#' 
## ------------------------------------------------------------------------------------------------------
for (city in cities_above_50k) {
  if (!is.na(city)) {
    city_nodes <- nodes %>%
      filter(city == !!city)
    
    city_edges <- edges %>%
      filter(ori_node %in% city_nodes$node & des_node %in% city_nodes$node)
    
    if (nrow(city_nodes) < 1) {
      cities_above_50k <- cities_above_50k[cities_above_50k != city]
    } else {
    # Write the node and edge data to CSV files
    write.csv(city_nodes, paste0("nodes_", city, ".csv"), row.names = FALSE)
    write.csv(city_edges, paste0("edges_", city, ".csv"), row.names = FALSE)
    }
  } 
}

#' 
#' 
#' ## PART 2: Build The Networks
#' 
#' With the data separated into different .csv files dependent on which city the nodes reside in we can move on to using these datasets to construct networks showing these journeys.
#' 
#' We begin by installing igraph, which is now needed as we wish to construct and visualise these networks. We also create a vector of colours to apply to the edges to represent the different transport options. These are
#' 
#' YELLOW - Bus
#' 
#' CYAN - Coach
#' 
#' GREEN - Metro
#' 
#' BLUE - Rail
#' 
#' PURPLE - Ferry
#' 
#' RED - Air
#' 
#' 
#' We also create a new function called 'calculate_relative_coordinates'. This function takes values of longitude and latitude for a node and determines the displacement of the node from the city centre. We then convert this into cartesian coordinates so that it can be used effectively in the plotting.
#' 
## ------------------------------------------------------------------------------------------------------
library(igraph)

layer_colours <- c("red", "purple", "blue", "green", "cyan", "yellow")


calculate_relative_coordinates <- function(node_lon, node_lat, city_lon, city_lat) {
  # Check if any of the coordinates are NA
  if (is.na(node_lon) | is.na(node_lat) | is.na(city_lon) | is.na(city_lat)) {
    return(c(NA, NA))
  }
  
  # Calculate the distance and bearing from the city centre
  distance <- distHaversine(c(node_lon, node_lat), c(city_lon, city_lat))
  bearing <- bearingRhumb(c(city_lon, city_lat), c(node_lon, node_lat))
  
  # Convert to Cartesian coordinates relative to the city centre
  x <- distance * cos(bearing * pi / 180)
  y <- distance * sin(bearing * pi / 180)
  
  return(c(x, y))
}

#' 
#' Finally we use everything we have done to plot the networks for each city.
#' 
#' We cycle through every relevant city and save the values for the longitude and latitude of the city centre. We then also load the city nodes and edges whilst also filtering out the edges that loop back to the same destination node as its origin node.
#' 
#' We run into an issue where the same node is used for each layer that it exists in (each layer represents the network of a singular transport option) so contracting all layers into one network would result in overlapping nodes where a stop/station services multiple transport options. To prevent this we aggregate all nodes with the same coordinates into one and and have all relevant edges connect to this node.
#' 
#' We now determine the relative coordinates of the nodes to the city centre and use these to plot our network.
#' 
#' The networks for the cities can be visualised below.
#' 
#' 
## ------------------------------------------------------------------------------------------------------
for (city_name in cities_above_50k) {
  city_center <- population_data %>%
    filter(city == city_name)
  
  city_lon <- city_center$longitude
  city_lat <- city_center$latitude
  
  # Load the node and edge files for the city
  nodes <- read.csv(paste0("nodes_", city_name, ".csv"))
  edges <- read.csv(paste0("edges_", city_name, ".csv"))
  
  edges <- edges %>%
    filter(edges$ori_node != edges$des_node)
  
  # Aggregate nodes by nodeID (unique physical locations)
  nodes_unique <- nodes %>%
    group_by(node) %>%
    summarize(latitude = first(lat),
              longitude = first(lon),
              tranID = min(layer)+1) %>%
    ungroup()
  
  # Calculate relative coordinates for each unique node
  relative_coords <- t(apply(nodes_unique, 1, function(row) {
    calculate_relative_coordinates(as.numeric(row["longitude"]),
                                   as.numeric(row["latitude"]),
                                   city_lon, city_lat)
  }))
  
  # Add the relative coordinates to the unique nodes data frame + colour of highest ranked transport type
  nodes_unique$colour <- layer_colours[nodes_unique$tranID]
  nodes_unique <- cbind(nodes_unique, x = relative_coords[, 1], y = relative_coords[, 2])
  
  nodes_unique <- nodes_unique %>%
    mutate(
      x = ifelse(is.na(x), 1, x),
      y = ifelse(is.na(y), 1, y)
    )
  
  # Create the graph object using igraph
  graph <- graph_from_data_frame(d = edges, vertices = nodes_unique, directed = FALSE)
  
  # Color edges and nodes by transport layer (assuming `ori_layer` in edges corresponds to layers)
  E(graph)$color <- layer_colours[as.numeric(edges$ori_layer) + 1]
  V(graph)$color <- layer_colours[nodes_unique$tranID ]
  
  # Optionally, visualize the network
  plot(graph, layout = as.matrix(nodes_unique[, c("x", "y")]), vertex.label = NA, vertex.size = 4, edge.color = E(graph)$colour, main = city_name)
  
  # Save the graph as an R object or any other format you need
  save(graph, file = paste0("graph_", city_name, ".RData"))
}

#' 
#' 
#' 
