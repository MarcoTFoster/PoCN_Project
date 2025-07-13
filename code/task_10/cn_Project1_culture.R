#' zt\b ---
#' title: "R Notebook"
#' output: html_notebook
#' ---
#' 
#' This project examines the process of different cultures influencing each other to become more alike. We explore the effect of changing the complexity of a cultural make-up and the size of the grid we are working on. 
#' 
#' We carry this out by working on a 10x10 grid of nodes - which we can call 'villages'. Each village is assigned a code of 5 numbers from 0-9. This code represents the village's culture, with each number being a cultural trait and the values 0-9 for these numbers representing different features of that trait. If two neighbouring villages share a trait in common they are able to influence one another - a successful influence event involves one village changing the trait of a target village to match theirs. The more similar two villages' cultures are the more likely an influence event will occur. This simulation will ideally run until there are no more villages that can influence each other.
#' 
#' Below we first write the function to generate a culture - as we can see we are making a 5 number code of numbers from 0-9. Later on when we are experimenting with changing culture complexity the number of features and traits can vary.
#' 
#' We then initialise our experiment by creating the grid, all the nodes on it and the associated culture for each node.
#' 
## ------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(igraph)
library(gridExtra)

# Function to generate a node's culture (5 random numbers between 0 and 9)
generate_culture <- function() {
  sample(0:9, 5, replace = TRUE)
}

# Initialize a 10x10 grid of nodes
grid_size <- 10
nodes <- matrix(nrow = grid_size, ncol = grid_size)
cultures <- matrix(list(), nrow = grid_size, ncol = grid_size)
sequences <- matrix("", nrow = grid_size, ncol = grid_size)

# Function to calculate common culture considering position
common_culture <- function(culture1, culture2) {
  sum(culture1 == culture2)
}

for (i in 1:grid_size) {
  for (j in 1:grid_size) {
    culture <- generate_culture()
    cultures[i, j] <- list(culture)
    sequences[i, j] <- paste(culture, collapse = "")
  }
}

#' 
#' We create a small function that determines the similarity of two cultures
#' 
## ------------------------------------------------------------------------------------------------------
# Create a graph
g <- make_empty_graph(directed = FALSE)

# Add vertices with coordinates
layout_matrix <- matrix(0, nrow = grid_size^2, ncol = 2)
for (i in 1:grid_size) {
  for (j in 1:grid_size) {
    g <- add_vertices(g, 1, name = paste(i, j, sep = "_"))
    layout_matrix[(i-1)*grid_size + j, ] <- c(j, grid_size - i + 1)
  }
}

#' 
## ------------------------------------------------------------------------------------------------------
# Add edges based on common culture considering position
for (i in 1:grid_size) {
  for (j in 1:grid_size) {
    current_culture <- cultures[[i, j]]
    if (i < grid_size) {
      # Neighbor to the right
      right_culture <- cultures[[i + 1, j]]
      common <- common_culture(current_culture, right_culture)
      if (common > 0) {
        g <- add_edges(g, c(paste(i, j, sep = "_"), paste(i + 1, j, sep = "_")), weight = common)
      }
    }
    if (j < grid_size) {
      # Neighbor below
      below_culture <- cultures[[i, j + 1]]
      common <- common_culture(current_culture, below_culture)
      if (common > 0) {
        g <- add_edges(g, c(paste(i, j, sep = "_"), paste(i, j + 1, sep = "_")), weight = common)
      }
    }
  }
}

#' 
## ------------------------------------------------------------------------------------------------------
# Create a table of node sequences
table_plot <- tableGrob(sequences)

# Plot the graph with a fixed layout
plot(g, layout = layout_matrix, vertex.label = NA, edge.label = E(g)$weight, vertex.size = 5, edge.width = E(g)$weight, edge.arrow.size = 0.5)

# Arrange table and network plot side by side
grid.arrange(table_plot, ncol = 1)

#' 
## ------------------------------------------------------------------------------------------------------


#' 
#' 
## ------------------------------------------------------------------------------------------------------
# Function to perform cultural influence
cultural_influence <- function() {
  # Select a random node A
  node_A <- sample(V(g), 1)
  A_coords <- as.integer(strsplit(V(g)$name[node_A], "_")[[1]])
  A_culture <- cultures[[A_coords[1], A_coords[2]]]
  
  # Find the neighbors of node A
  neighbors <- adjacent_vertices(g, node_A)
  if (length(neighbors[[1]]) == 0) return()
  
  # Select a random neighbor B
  node_B <- sample(neighbors[[1]], 1)
  B_coords <- as.integer(strsplit(V(g)$name[node_B], "_")[[1]])
  B_culture <- cultures[[B_coords[1], B_coords[2]]]
  
  # Calculate similarity
  S <- common_culture(A_culture, B_culture)
  N <- length(A_culture)
  similarity <- S / N
  
  # Only proceed if they have at least one trait in common but not all
  if (S > 0 && S < N) {
    # With probability S/N, have one trait of A change to one from B
    if (runif(1) < similarity) {
      # Select a random position to change
      pos <- sample(which(A_culture != B_culture), 1)
      A_culture[pos] <- B_culture[pos]
      cultures[[A_coords[1], A_coords[2]]] <<- A_culture
      sequences[A_coords[1], A_coords[2]] <<- paste(A_culture, collapse = "")
    }
  }
}

#' 
#' 
#' 
## ------------------------------------------------------------------------------------------------------
total_steps <- 100000
segment_steps <- 20000
num_segments <- total_steps / segment_steps

# Initialize an empty list to store plots
plots <- vector("list", num_segments)

for (i in 1:num_segments) {
  for (j in 1:segment_steps) {
    cultural_influence()
  }

  # After each segment, update the graph and save the plot
  g <- graph.empty(directed = FALSE)
  
  # Add vertices with coordinates
  for (x in 1:grid_size) {
    for (y in 1:grid_size) {
      g <- add_vertices(g, 1, name = paste(x, y, sep = "_"))
    }
  }
  
  # Add edges based on updated cultures
  for (x in 1:grid_size) {
    for (y in 1:grid_size) {
      current_culture <- cultures[[x, y]]
      if (x < grid_size) {
        # Neighbor to the right
        right_culture <- cultures[[x + 1, y]]
        common <- common_culture(current_culture, right_culture)
        if (common > 0) {
          g <- add_edges(g, c(paste(x, y, sep = "_"), paste(x + 1, y, sep = "_")), weight = common)
        }
      }
      if (y < grid_size) {
        # Neighbor below
        below_culture <- cultures[[x, y + 1]]
        common <- common_culture(current_culture, below_culture)
        if (common > 0) {
          g <- add_edges(g, c(paste(x, y, sep = "_"), paste(x, y + 1, sep = "_")), weight = common)
        }
      }
    }
  }

  # Create a table of updated node sequences
  table_plot <- tableGrob(sequences)
  
  # Save the plot to the list
  plots[[i]] <- arrangeGrob(
    grobs = list(table_plot, 
                 plot(g, layout = layout_matrix, vertex.label = NA, edge.label = E(g)$weight, 
                      vertex.size = 5, edge.width = E(g)$weight, edge.arrow.size = 0.5)),
    ncol = 1
  )
}

# Display all the plots
grid.arrange(grobs = plots, ncol = 1)

#' 
#' 
## ------------------------------------------------------------------------------------------------------
##the problem with the above simulation is that it spends a long long while filtering between two very very similar regions bu still slightly different

#' 
#' 
#' 
#' 
#' 
#' ## PART 2: COUNTING NUMBER OF STABLE REGIONS
#' 
## ------------------------------------------------------------------------------------------------------
generate_culture <- function(num_features, num_traits) {
  sample(0:(num_traits - 1), num_features, replace = TRUE)
}

grid_size <- 10
nodes <- matrix(nrow = grid_size, ncol = grid_size)
cultures <- matrix(list(), nrow = grid_size, ncol = grid_size)
sequences <- matrix("", nrow = grid_size, ncol = grid_size)


#' 
#' 
#' 
#' 
## ------------------------------------------------------------------------------------------------------
for (i in 1:3) {
  for (j in 1:3) {
    num_features <- 5 * i
    num_traits <- 5 * j
    
    # Initialize cultures and sequences matrices
    for (k in 1:grid_size) {
      for (l in 1:grid_size) {
        culture <- generate_culture(num_features, num_traits)
        cultures[k, l] <- list(culture)
        sequences[k, l] <- paste(culture, collapse = "")
      }
    }
    
    g <- graph.empty(directed = FALSE)

    # Add vertices with coordinates
    layout_matrix <- matrix(0, nrow = grid_size^2, ncol = 2)
    for (m in 1:grid_size) {
      for (n in 1:grid_size) {
        g <- add_vertices(g, 1, name = paste(m, n, sep = "_"))
        layout_matrix[(m-1)*grid_size + n, ] <- c(n, grid_size - m + 1)
      }
    }
    
    # Add edges based on initial cultures
    for (m in 1:grid_size) {
      for (n in 1:grid_size) {
        current_culture <- cultures[[m, n]]
        if (m < grid_size) {
          # Neighbor to the right
          right_culture <- cultures[[m + 1, n]]
          common <- common_culture(current_culture, right_culture)
          if (common > 0) {
            g <- add_edges(g, c(paste(m, n, sep = "_"), paste(m + 1, n, sep = "_")), weight = common)
          }
        }
        if (n < grid_size) {
          # Neighbor below
          below_culture <- cultures[[m, n + 1]]
          common <- common_culture(current_culture, below_culture)
          if (common > 0) {
            g <- add_edges(g, c(paste(m, n, sep = "_"), paste(m, n + 1, sep = "_")), weight = common)
          }
        }
      }
    }
    
    total_steps <- 100000
    segment_steps <- 20000
    num_segments <- total_steps / segment_steps

    for (seg in 1:num_segments) {
      for (step in 1:segment_steps) {
        cultural_influence()
      }

      # Update the graph after cultural influence
      g <- graph.empty(directed = FALSE)
      for (m in 1:grid_size) {
        for (n in 1:grid_size) {
          g <- add_vertices(g, 1, name = paste(m, n, sep = "_"))
        }
      }
      
      for (m in 1:grid_size) {
        for (n in 1:grid_size) {
          current_culture <- cultures[[m, n]]
          if (m < grid_size) {
            # Neighbor to the right
            right_culture <- cultures[[m + 1, n]]
            common <- common_culture(current_culture, right_culture)
            if (common > 0) {
              g <- add_edges(g, c(paste(m, n, sep = "_"), paste(m + 1, n, sep = "_")), weight = common)
            }
          }
          if (n < grid_size) {
            # Neighbor below
            below_culture <- cultures[[m, n + 1]]
            common <- common_culture(current_culture, below_culture)
            if (common > 0) {
              g <- add_edges(g, c(paste(m, n, sep = "_"), paste(m, n + 1, sep = "_")), weight = common)
            }
          }
        }
      }
    }
  
    # Calculate the number of stable regions in the final graph
    clusters <- components(g)
    stable_regions <- sum(clusters$csize > 1)

    # Print the number of stable regions
    cat("Number of stable regions (Features:", num_features, ", Traits:", num_traits, "):", stable_regions, "\n")
  }
}

#' 
#' ## PART 3: VARYING GRID SIZE
#' 
## ------------------------------------------------------------------------------------------------------
# Range of grid sizes (from 5x5 to 50x50, in steps of 5)
grid_sizes <- seq(5, 50, by = 5)

# Fixed number of features and traits
num_features <- 5
num_traits <- 15

for (grid_size in grid_sizes) {
  # Initialize cultures and sequences matrices
  cultures <- matrix(list(), nrow = grid_size, ncol = grid_size)
  sequences <- matrix("", nrow = grid_size, ncol = grid_size)
  
  for (k in 1:grid_size) {
    for (l in 1:grid_size) {
      culture <- generate_culture(num_features, num_traits)
      cultures[k, l] <- list(culture)
      sequences[k, l] <- paste(culture, collapse = "")
    }
  }
  
  g <- graph.empty(directed = FALSE)

  # Add vertices with coordinates
  layout_matrix <- matrix(0, nrow = grid_size^2, ncol = 2)
  for (m in 1:grid_size) {
    for (n in 1:grid_size) {
      g <- add_vertices(g, 1, name = paste(m, n, sep = "_"))
      layout_matrix[(m-1)*grid_size + n, ] <- c(n, grid_size - m + 1)
    }
  }
  
  # Add edges based on initial cultures
  for (m in 1:grid_size) {
    for (n in 1:grid_size) {
      current_culture <- cultures[[m, n]]
      if (m < grid_size) {
        # Neighbor to the right
        right_culture <- cultures[[m + 1, n]]
        common <- common_culture(current_culture, right_culture)
        if (common > 0) {
          g <- add_edges(g, c(paste(m, n, sep = "_"), paste(m + 1, n, sep = "_")), weight = common)
        }
      }
      if (n < grid_size) {
        # Neighbor below
        below_culture <- cultures[[m, n + 1]]
        common <- common_culture(current_culture, below_culture)
        if (common > 0) {
          g <- add_edges(g, c(paste(m, n, sep = "_"), paste(m, n + 1, sep = "_")), weight = common)
        }
      }
    }
  }
  
  total_steps <- 10000 * grid_size
  segment_steps <- 2000 * grid_size
  num_segments <- total_steps / segment_steps

  for (seg in 1:num_segments) {
    for (step in 1:segment_steps) {
      cultural_influence()
    }

    # Update the graph after cultural influence
    g <- graph.empty(directed = FALSE)
    for (m in 1:grid_size) {
      for (n in 1:grid_size) {
        g <- add_vertices(g, 1, name = paste(m, n, sep = "_"))
      }
    }
    
    for (m in 1:grid_size) {
      for (n in 1:grid_size) {
        current_culture <- cultures[[m, n]]
        if (m < grid_size) {
          # Neighbor to the right
          right_culture <- cultures[[m + 1, n]]
          common <- common_culture(current_culture, right_culture)
          if (common > 0) {
            g <- add_edges(g, c(paste(m, n, sep = "_"), paste(m + 1, n, sep = "_")), weight = common)
          }
        }
        if (n < grid_size) {
          # Neighbor below
          below_culture <- cultures[[m, n + 1]]
          common <- common_culture(current_culture, below_culture)
          if (common > 0) {
            g <- add_edges(g, c(paste(m, n, sep = "_"), paste(m, n + 1, sep = "_")), weight = common)
          }
        }
      }
    }
  }

  # Calculate the number of stable regions in the final graph
  clusters <- components(g)
  stable_regions <- sum(clusters$csize > 1)

  # Print the number of stable regions
  cat("Number of stable regions for grid size", grid_size, "x", grid_size, ":", stable_regions, "\n")
}

#' 
#' ## JANUARY UPDATE
#' Work on making the above section more efficient
#' -NEW: task to vary the degree k of each node.
#' 
## ------------------------------------------------------------------------------------------------------
cultural_influence <- function(g, cultures) {
  node_A <- sample(V(g), 1)
  neighbors <- neighbors(g, node_A)
  if (length(neighbors) == 0) return(cultures)
  
  node_B <- sample(neighbors, 1)
  
  A_culture <- cultures[[node_A]]
  B_culture <- cultures[[node_B]]
  
  # Calculate similarity
  S <- sum(A_culture == B_culture)
  N <- length(A_culture)
  similarity <- S / N
  
  if (S > 0 && S < N && runif(1) < similarity) {
    differing <- which(A_culture != B_culture)
    change_idx <- sample(differing, 1)
    A_culture[change_idx] <- B_culture[change_idx]
    cultures[[node_A]] <- A_culture
  }
  
  cultures
}

#' 
## ------------------------------------------------------------------------------------------------------
# Simulation parameters
num_nodes <- 30
num_features <- 5
num_traits <- 10
k_values <- seq(1, 10) # k-nearest neighbors to test
steps <- 100000          # Number of cultural influence steps per simulation

# Store results
stable_regions_results <- numeric(length(k_values))

#' 
## ------------------------------------------------------------------------------------------------------
# Perform simulations
for (idx in seq_along(k_values)) {
  k <- k_values[idx]
  cat("Running simulation for k =", k, "\n")
  
  # Create lattice graph
  g <- graph.lattice(dimvector = num_nodes, nei = k, directed = FALSE, periodic = TRUE)
  
  # Initialize cultures
  cultures <- replicate(vcount(g), generate_culture(num_features, num_traits), simplify = FALSE)
  
  # Run cultural influence
  for (step in seq_len(steps)) {
    cultures <- cultural_influence(g, cultures)
  }
  
  # Determine stable regions
  # Two nodes are in the same region if their cultures are identical
  culture_strings <- sapply(cultures, paste, collapse = "")
  unique_cultures <- unique(culture_strings)
  stable_regions_results[idx] <- length(unique_cultures)
}

#' 
## ------------------------------------------------------------------------------------------------------
library(ggplot2)

# Create results dataframe
results <- data.frame(
  k = k_values,
  stable_regions = stable_regions_results
)

# Print results
print(results)

# Plot the results
ggplot(results, aes(x = k, y = stable_regions)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Stable Regions vs. Node Degree",
    x = "Node Degree (k)",
    y = "Number of Stable Regions"
  ) +
  theme_minimal()

#' 
## ------------------------------------------------------------------------------------------------------
# Simulation parameters
num_features <- 5
num_traits <- 10
node_values <- seq(10, 150, by = 10) # Number of nodes to test
k <- 2                               # Fixed k-nearest neighbors
steps <- 100000                      # Number of cultural influence steps per simulation

# Store results
stable_regions_results <- numeric(length(node_values))

# Perform simulations
for (idx in seq_along(node_values)) {
  num_nodes <- node_values[idx]
  cat("Running simulation for num_nodes =", num_nodes, "\n")
  
  # Create lattice graph
  g <- graph.lattice(dimvector = num_nodes, nei = k, directed = FALSE, periodic = TRUE)
  
  # Initialize cultures
  cultures <- replicate(vcount(g), generate_culture(num_features, num_traits), simplify = FALSE)
  
  # Run cultural influence
  for (step in seq_len(steps)) {
    cultures <- cultural_influence(g, cultures)
  }
  
  # Determine stable regions
  culture_strings <- sapply(cultures, paste, collapse = "")
  unique_cultures <- unique(culture_strings)
  stable_regions_results[idx] <- length(unique_cultures)
}

#' 
## ------------------------------------------------------------------------------------------------------
print(stable_regions_results)

#' 
#' 
## ------------------------------------------------------------------------------------------------------
library(ggplot2)

# Create results dataframe
results <- data.frame(
  num_nodes = node_values,
  stable_regions = stable_regions_results
)

# Print results
print(results)

# Plot the results
ggplot(results, aes(x = num_nodes, y = stable_regions)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Stable Regions vs. Number of Nodes",
    x = "Number of Nodes",
    y = "Number of Stable Regions"
  ) +
  theme_minimal()

#' 
