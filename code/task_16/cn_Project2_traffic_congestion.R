#' ---
#' title: "R Notebook"
#' output: html_notebook
#' ---
#' 
## ------------------------------------------------------------------------------------------------------
library(igraph)

#' 
#' 
## ------------------------------------------------------------------------------------------------------
generate_tree <- function(z, m) {
  nodes <- c(1)
  parentID <- 1
  edges <- data.frame(parent = integer(), child = integer())
  count <- 2
  
  
  for (level in 1:m) {
    num_nodes <- z^level
    num_parents <- z^(level-1)
    nodes <- c(nodes, count:(count + num_nodes - 1))
    
    level_edges <- data.frame(
      parent = rep(parentID:(parentID + num_parents - 1), each = z),
      child = count:(count + num_nodes - 1)
    )
    
    edges <- rbind(edges, level_edges)
    count <- count + num_nodes
    parentID <- parentID + num_parents
  }
  return(list(nodes = nodes, edges = edges))
}

z <- 3 #Branching factor
m <- 2 #Num. levels - excluding the root
tree <- generate_tree(z, m)  # Example: z = 2, m = 3

#' 
## ------------------------------------------------------------------------------------------------------
graph <- graph_from_data_frame(tree$edges, directed = FALSE)

plot(graph,
     layout = layout_as_tree(graph, root=1, circular = FALSE),
     vertex.size = 20, 
     vertex.label.cex = 0.7, 
     vertex.label.color = "black", 
     vertex.color = "lightblue", 
     edge.arrow.size = 0.5, 
     main = "Tree Structure (z = 3, m = 3)")


#' 
#' 
## ------------------------------------------------------------------------------------------------------
initialize_packets <- function(graph, p) {
  nodes <- as.numeric(V(graph))  # Extract numeric node IDs
  
  packets <- lapply(nodes, function(node) {
    if (runif(1) < p) {
      list(origin = node, destination = sample(nodes[nodes != node], 1))
    } else {
      NULL
    }
  })
  
  packets <- Filter(Negate(is.null), packets)  # Remove NULL packets
  return(packets)
}


packets <- initialize_packets(graph, p = 0.1)

str(packets)

#' 
## ------------------------------------------------------------------------------------------------------
fMOVE <- function(n, y) {
  if (n == 0) {
    return(1)
  } else {
    return(n^(-y))
  }
}

k_ij <- function(j_ab, n_a, y) {
  return(j_ab * fMOVE(n_a, y))
}

q_ij <- function(k_ij, k_ji) {
  return(sqrt(k_ij * k_ji))
}

# Example for a pair of nodes i and j
j_ab <- runif(1)
n_a <- length(packets)  # Number of packets at node a
y <- 0.5  # Example value of y

k_ij_value <- k_ij(j_ab, n_a, y)
k_ji_value <- k_ij(j_ab, n_a, y)  # Assuming symmetry
q_ij_value <- q_ij(k_ij_value, k_ji_value)

#' 
## ------------------------------------------------------------------------------------------------------
fMOVE <- function(n, y) {
  if (n == 0) {
    return(1)
  } else {
    return(n^(-y))
  }
}

movePackets <- function(packets, graph, y) {
  updatedPackets <- vector("list", length(packets))  # Preallocate
  
  for (i in seq_along(packets)) {
    current_node <- packets[[i]]$origin
    destination_node <- packets[[i]]$destination
    
    path <- shortest_paths(graph, from = current_node, to = destination_node)$vpath[[1]]
    target_node <- as.numeric(path[2])
    
    # Count number of packets at current and target nodes
    n_i <- sum(vapply(packets, function(p) p$origin == current_node, logical(1)))
    n_j <- sum(vapply(packets, function(p) p$origin == target_node, logical(1)))
    
    # Communication quality calculation
    e_ij <- runif(1)
    e_ji <- runif(1)
    k_ij <- e_ij * fMOVE(n_i, y)
    k_ji <- e_ji * fMOVE(n_j, y)
    q_ij <- sqrt(k_ij * k_ji)
    
    # Move the packet with probability q_ij
    if (runif(1) < q_ij) {
      packets[[i]]$origin <- target_node
    }
    
    # If packet reached destination, mark for removal
    if (packets[[i]]$origin == packets[[i]]$destination) {
      updatedPackets[[i]] <- NULL
    } else {
      updatedPackets[[i]] <- packets[[i]]
    }
  }
  
  # Remove NULLs (i.e., delivered packets)
  updatedPackets <- Filter(Negate(is.null), updatedPackets)
  return(updatedPackets)
}


packets <- initialize_packets(graph, p = 0.2)
str(packets)

for (step in 1:10) {
  packets <- movePackets(packets, graph, y = 1)
  cat("Step", step, "- Remaining packets:", length(packets), "\n")
  str(packets)
}


#' 
## ------------------------------------------------------------------------------------------------------
simulation <- function(p, y, graph, steps) {
  NumPackets <- numeric(steps)
  packets <- list()
  
  for (step in 1:steps) {
    new_packets <- initialize_packets(graph, p)
    packets <- c(packets, new_packets)
    
    packets <- movePackets(packets, graph, y)
    packets <- Filter(Negate(is.null), packets)
    
    NumPackets[step] <- length(packets)
    
    if (length(packets) >= 500) {
      message(sprintf("Simulation stopped at step %d due to packet overflow (%d packets).", 
                      step, length(packets)))
      NumPackets <- NumPackets[1:step]
      break
    }
  }
  
  return(NumPackets)
}

#' 
#' 
## ------------------------------------------------------------------------------------------------------
steps <- 800
p <- 0.1
y <- 0.9

packet_counts <- simulation(p, y, graph, steps)

#tried to run with current parameters but I assume that it reached the point of criticality and too many packets were being added without then being removed.

#' 
#' 
## ------------------------------------------------------------------------------------------------------
library(ggplot2)

plot_packet_counts <- function(packet_counts) {
  df <- data.frame(
    time = 1:length(packet_counts),
    packets = packet_counts
  )
  
  ggplot(df, aes(x = time, y = packets)) +
    geom_line() +
    theme_minimal() +
    labs(
      title = bquote("Active Packets Over Time" ~ "(" * p == .(p) * "," ~ gamma == .(y) * ")"),
      x = "Time (steps)",
      y = "Number of Active Packets"
    )
}

plot_packet_counts(packet_counts)

#' 
## ------------------------------------------------------------------------------------------------------
#critical probability calculation

pc <- function(z, m) {
  numerator <- sqrt(z)
  denominator <- (z * (z^(m - 1) - 1)^2) / (z^m - 1) + 1
  return(numerator / denominator)
}

p_c <- pc(z,m+1)
print(p_c)

## ------------------------------------------------------------------------------------------------------
#to explore the effects of varying probability, especially as it transitions past the point of criticality p_c, we rewrite the move_packets function so that e_ij always equals 1.

movePackets_CRIT <- function(packets, graph, y) {
  updatedPackets <- vector("list", length(packets))  # Preallocate
  
  for (i in seq_along(packets)) {
    current_node <- packets[[i]]$origin
    destination_node <- packets[[i]]$destination
    
    path <- shortest_paths(graph, from = current_node, to = destination_node)$vpath[[1]]
    target_node <- as.numeric(path[2])
    
    # Count number of packets at current and target nodes
    n_i <- sum(vapply(packets, function(p) p$origin == current_node, logical(1)))
    n_j <- sum(vapply(packets, function(p) p$origin == target_node, logical(1)))
    
    # Communication quality calculation (removing e_ij)
    k_ij <- fMOVE(n_i, y)
    k_ji <- fMOVE(n_j, y)
    q_ij <- sqrt(k_ij * k_ji)
    
    # Move the packet with probability q_ij
    if (runif(1) < q_ij) {
      packets[[i]]$origin <- target_node
    }
    
    # If packet reached destination, mark for removal
    if (packets[[i]]$origin == packets[[i]]$destination) {
      updatedPackets[[i]] <- NULL
    } else {
      updatedPackets[[i]] <- packets[[i]]
    }
  }
  
  # Remove NULLs (i.e., delivered packets)
  updatedPackets <- Filter(Negate(is.null), updatedPackets)
  return(updatedPackets)
}

#' 
## ------------------------------------------------------------------------------------------------------
simulation_CRIT <- function(p, y, graph, steps) {
  NumPackets <- numeric(steps)
  packets <- list()
  
  for (step in 1:steps) {
    new_packets <- initialize_packets(graph, p)
    packets <- c(packets, new_packets)
    
    packets <- movePackets_CRIT(packets, graph, y)
    packets <- Filter(Negate(is.null), packets)
    
    NumPackets[step] <- length(packets)
    
    if (length(packets) >= 500) {
      message(sprintf("Simulation stopped at step %d due to packet overflow (%d packets).", 
                      step, length(packets)))
      NumPackets <- NumPackets[1:step]
      break
    }
  }
  
  return(NumPackets)
}

#' 
## ------------------------------------------------------------------------------------------------------
steps <- 1000
p <- 0.28
y <- 1.05

packet_counts <- simulation_CRIT(p, y, graph, steps)

#' 
## ------------------------------------------------------------------------------------------------------
library(ggplot2)

plot_packet_counts <- function(packet_counts) {
  df <- data.frame(
    time = 1:length(packet_counts),
    packets = packet_counts
  )
  
  ggplot(df, aes(x = time, y = packets)) +
    geom_line() +
    theme_minimal() +
    labs(
      title = bquote("Active Packets Over Time" ~ "(" * p == .(p) * "," ~ gamma == .(y) * ")"),
      x = "Time (steps)",
      y = "Number of Active Packets"
    )
}

plot_packet_counts(packet_counts)

#' 
