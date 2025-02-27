library(ggplot2)

coords <- read.csv("C:\\Users\\mrran\\Downloads\\Untitled spreadsheet - Sheet1 (2).csv")
n <- nrow(coords)

distances <- as.matrix(
  dist(coords, method = "euclidean"),
  diag = TRUE,
  upper = TRUE
)

# -------------------------------------------------------------------------
# 3) Helper Functions
# -------------------------------------------------------------------------

# (A) Compute total distance of a tour (closing the loop)
tour_length <- function(tour, distmat) {
  n <- length(tour)
  total <- 0
  for (i in seq_len(n - 1)) {
    total <- total + distmat[tour[i], tour[i+1]]
  }
  total <- total + distmat[tour[n], tour[1]]  # close loop
  return(total)
}

# (B) Nearest-neighbor approach to build an initial tour
nearest_neighbor_tour <- function(start_city, distmat) {
  n <- nrow(distmat)
  visited <- logical(n)
  visited[start_city] <- TRUE
  tour <- integer(n)
  tour[1] <- start_city
  
  current <- start_city
  for (i in 2:n) {
    dists <- distmat[current, ]
    dists[visited] <- Inf
    next_city <- which.min(dists)
    tour[i] <- next_city
    visited[next_city] <- TRUE
    current <- next_city
  }
  
  return(tour)
}

# (C) Try multiple NN starts, pick the best
get_initial_tour <- function(distmat, tries = 3) {
  n <- nrow(distmat)
  best_tour <- NULL
  best_len <- Inf
  for (t in seq_len(tries)) {
    start <- sample.int(n, 1)
    cand  <- nearest_neighbor_tour(start, distmat)
    length_cand <- tour_length(cand, distmat)
    if (length_cand < best_len) {
      best_len   <- length_cand
      best_tour  <- cand
    }
  }
  return(best_tour)
}

# (D) Convert a tour into a list of edges (pairs)
tour_edges <- function(tour) {
  n <- length(tour)
  edges <- matrix(nrow = n, ncol = 2)
  for (i in 1:(n - 1)) {
    edges[i, ] <- c(tour[i], tour[i+1])
  }
  # close tour
  edges[n, ] <- c(tour[n], tour[1])
  return(edges)
}

# Check if edge (a,b) is in the set of edges (undirected check)
edge_in_tour <- function(a, b, edge_list) {
  idx1 <- which(edge_list[,1] == a & edge_list[,2] == b)
  idx2 <- which(edge_list[,1] == b & edge_list[,2] == a)
  return(length(idx1) + length(idx2) > 0)
}

# (E) Rebuild the tour after removing/adding edges
rebuild_tour_from_edges <- function(edges_added, edges_removed, current_tour) {
  old_edges <- tour_edges(current_tour)
  old_list  <- split(old_edges, seq(nrow(old_edges)))
  
  # Helper to compare edges ignoring direction
  same_edge <- function(e1, e2) {
    (e1[1] == e2[1] && e1[2] == e2[2]) ||
    (e1[1] == e2[2] && e1[2] == e2[1])
  }
  
  # Remove edges in edges_removed
  for (rem in edges_removed) {
    to_remove_idx <- NULL
    for (i in seq_along(old_list)) {
      e <- as.integer(old_list[[i]])
      if (same_edge(e, rem)) {
        to_remove_idx <- i
        break
      }
    }
    if (!is.null(to_remove_idx)) {
      old_list[[to_remove_idx]] <- NULL
    }
  }
  
  # Combine with edges_added
  new_list <- old_list
  for (add in edges_added) {
    new_list[[length(new_list) + 1]] <- add
  }
  
  # Build adjacency
  n <- length(unique(current_tour))
  adjacency <- vector("list", n)
  for (i in seq_len(n)) {
    adjacency[[i]] <- integer(0)
  }
  
  for (p in new_list) {
    p <- as.integer(p)
    a <- p[1]
    b <- p[2]
    if (a < 1 || a > n || b < 1 || b > n) {
      stop(sprintf("Invalid city index in new edges: a=%d, b=%d, n=%d", a, b, n))
    }
    adjacency[[a]] <- c(adjacency[[a]], b)
    adjacency[[b]] <- c(adjacency[[b]], a)
  }
  
  # Rebuild a cycle by traversing adjacency
  keys <- which(sapply(adjacency, length) > 0)
  if (length(keys) == 0) {
    return(current_tour)
  }
  
  start <- keys[1]
  visited <- logical(n)
  new_tour <- integer(0)
  current <- start
  
  repeat {
    new_tour <- c(new_tour, current)
    visited[current] <- TRUE
    neighbors <- adjacency[[current]]
    next_city <- neighbors[!visited[neighbors]]
    if (length(next_city) == 0) {
      break
    }
    current <- next_city[1]
    if (length(new_tour) > n) {
      break
    }
  }
  
  if (length(unique(new_tour)) == n) {
    return(new_tour)
  } else {
    return(current_tour)
  }
}

# (F) Lin–Kernighan "search chain" (depth-limited partial edge swaps)
lk_search_chain <- function(current_tour, distmat, max_depth = 5) {
  edges_in_tour <- tour_edges(current_tour)
  best_tour <- current_tour
  best_gain <- 0
  
  do_lk_dfs <- function(tour_in, removed_edges, added_edges,
                        partial_gain, depth, t_start, last_city) {
    best_local_tour <- tour_in
    best_local_gain <- partial_gain
    
    # If partial_gain > 0, see if this yields an overall improvement
    if (partial_gain > 0) {
      candidate_tour <- rebuild_tour_from_edges(added_edges, removed_edges, tour_in)
      old_len <- tour_length(tour_in, distmat)
      new_len <- tour_length(candidate_tour, distmat)
      actual_gain <- old_len - new_len
      if (actual_gain > best_local_gain) {
        best_local_gain <- actual_gain
        best_local_tour <- candidate_tour
      }
    }
    
    if (depth >= max_depth) {
      return(list(tour = best_local_tour, gain = best_local_gain))
    }
    
    # Next: pick an edge from last_city to remove
    edges_in <- tour_edges(tour_in)
    candidates_remove <- edges_in[
      (edges_in[,1] == last_city) | (edges_in[,2] == last_city),
      , drop = FALSE
    ]
    
    for (rr in seq_len(nrow(candidates_remove))) {
      rem_edge <- candidates_remove[rr, ]
      # the "other city"
      if (rem_edge[1] == last_city) {
        new_city <- rem_edge[2]
      } else {
        new_city <- rem_edge[1]
      }
      # approximate partial gain
      d_old <- distmat[last_city, t_start]
      d_new <- distmat[last_city, new_city]
      gain_inc <- d_old - d_new
      partial_gain_new <- partial_gain + gain_inc
      
      removed_new <- removed_edges
      removed_new[[length(removed_new) + 1]] <- rem_edge
      
      # Now pick a city to connect from new_city
      for (cand_add in seq_len(nrow(distmat))) {
        if (cand_add == new_city) next
        
        e_in <- edge_in_tour(new_city, cand_add, edges_in)
        e_add <- FALSE
        if (length(added_edges) > 0) {
          for (ae in added_edges) {
            if ((ae[1] == new_city && ae[2] == cand_add) ||
                (ae[2] == new_city && ae[1] == cand_add)) {
              e_add <- TRUE
              break
            }
          }
        }
        if (e_in || e_add) {
          next
        }
        
        add_edge <- c(new_city, cand_add)
        gain_inc2 <- distmat[new_city, t_start] - distmat[new_city, cand_add]
        partial_gain2 <- partial_gain_new + gain_inc2
        
        removed2 <- removed_new
        added2   <- added_edges
        added2[[length(added2) + 1]] <- add_edge
        
        dfs_result <- do_lk_dfs(
          tour_in       = tour_in,
          removed_edges = removed2,
          added_edges   = added2,
          partial_gain  = partial_gain2,
          depth         = depth + 1,
          t_start       = t_start,
          last_city     = cand_add
        )
        
        if (dfs_result$gain > best_local_gain) {
          best_local_gain <- dfs_result$gain
          best_local_tour <- dfs_result$tour
        }
      }
    }
    return(list(tour = best_local_tour, gain = best_local_gain))
  }
  
  # Try each edge in the tour as the potential start
  for (e_idx in seq_len(nrow(edges_in_tour))) {
    e <- edges_in_tour[e_idx, ]
    t1 <- e[1]
    t2 <- e[2]
    removed_edges <- list(e)
    added_edges   <- list()
    partial_gain  <- 0
    
    res <- do_lk_dfs(
      tour_in       = current_tour,
      removed_edges = removed_edges,
      added_edges   = added_edges,
      partial_gain  = partial_gain,
      depth         = 1,
      t_start       = t1,
      last_city     = t2
    )
    
    if (res$gain > best_gain) {
      best_gain <- res$gain
      best_tour <- res$tour
    }
  }
  
  if (best_gain > 1e-9) {
    return(list(tour = best_tour, improved = TRUE))
  } else {
    return(list(tour = current_tour, improved = FALSE))
  }
}

# (G) Main Lin–Kernighan driver: repeated improvement
lin_kernighan <- function(distmat, max_rounds = 10, max_depth = 5) {
  current_tour <- get_initial_tour(distmat, tries = 5)
  current_len  <- tour_length(current_tour, distmat)
  cat("Initial tour length:", current_len, "\n")
  
  for (round_i in seq_len(max_rounds)) {
    res <- lk_search_chain(current_tour, distmat, max_depth = max_depth)
    if (!res$improved) {
      cat("No improvement found in round", round_i, "- stopping.\n")
      break
    } else {
      new_len <- tour_length(res$tour, distmat)
      if (new_len < current_len) {
        current_tour <- res$tour
        current_len  <- new_len
        cat("Round", round_i, "- improved tour length to", current_len, "\n")
      } else {
        cat("Round", round_i, "- no net improvement, stopping.\n")
        break
      }
    }
  }
  
  return(list(tour = current_tour, length = current_len))
}

# -------------------------------------------------------------------------
# 4) Run the Lin–Kernighan TSP
# -------------------------------------------------------------------------
result <- lin_kernighan(distances, max_rounds = 20, max_depth = 5)
cat("Final tour length:", result$length, "\n")
cat("Tour (city indices):", result$tour, "\n")

# -------------------------------------------------------------------------
# 5) Plot the Final Route in the Requested Style
# -------------------------------------------------------------------------
# We convert 'result$tour' into segments for geom_segment,
# and highlight the starting point in red.

# Make a closed version of the tour
tour <- result$tour
tour_closed <- c(tour, tour[1])

# Build a data frame of segments
segments_df <- data.frame(
  xstarts = coords[tour_closed[-length(tour_closed)], "xcoord"],
  ystarts = coords[tour_closed[-length(tour_closed)], "ycoord"],
  xends   = coords[tour_closed[-1], "xcoord"],
  yends   = coords[tour_closed[-1], "ycoord"]
)

# The 'startingpoint' is the first city in 'tour'
startingpoint <- tour[1]

plot2 <- ggplot() +
  # 1) Plot all cities in blue
  geom_point(
    data = coords,
    aes(x = xcoord, y = ycoord),
    color = "blue",
    size = 2
  ) +
  # 2) Draw the route as black segments
  geom_segment(
    data = segments_df,
    aes(x = xstarts, y = ystarts, xend = xends, yend = yends),
    color = "black"
  ) +
  # 3) Highlight the first city in red
  geom_point(
    aes(
      x = coords[startingpoint, "xcoord"],
      y = coords[startingpoint, "ycoord"]
    ),
    size = 4,
    color = "red"
  )

print(plot2)
