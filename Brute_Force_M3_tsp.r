library(ggplot2)

# read in a list of coordinates 
coords <- read.csv("C:\\Users\\mrran\\Downloads\\Untitled spreadsheet - Sheet1 (2).csv")
n <- nrow(coords)

#to set the number of salesmen (itll be 1 because to fix that we can just split the regions up)

plot1 <- ggplot(data = coords, mapping = aes(xcoord, ycoord)) + 
    geom_point()
#print(plot1)

#matrix of distances 
distances <- as.matrix(dist(coords, method = "euclidean"), diag = TRUE, upper = TRUE)


#-----------------------------------------------------------------------------------------

# to bash out every route - would only be helpful in small cases - 9 is kinda already pushing it 
permute <- function(k) {
#to stop getting annoying ass errors out of this thing 
  if (!is.numeric(k) || length(k) != 1 || k < 1 || floor(k) != k) {
    stop("bad")
  }
  
  if (k == 1) {
    return(list(c(1)))
  }
  
  previous_perms <- permute(k - 1)
  result <- list()
  
  for (perm in previous_perms) {
    for (pos in 0:(k - 1)) {
      new_perm <- append(perm, k, after = pos)
      result[[length(result) + 1]] <- new_perm
    }
  }
  
  return(result)
}

#to rearrange the permutations so that the starting point is at the beginning and end 
perms <- permute(n)
for (i in 1:length(perms)) {
    vec <- perms[[i]]
    perms[[i]] <- vec[! vec %in% c(1)]
    perms[[i]] <- append(perms[[i]], 1, after = 0)
    perms[[i]] <- append(perms[[i]], 1)
}

#find summed distances for each route 
min <- Inf
minperm <- c()
for (i in 1:length(perms)) {
    rt <- perms[[i]]
    x <- 0
    for (j in 1:n) {
        x <- x + distances[strtoi(rt[j]), strtoi(rt[j+1])]
    }
    #adding endpoint distances
    x <- x + distances[1, strtoi(rt[1])] + distances[rt[length(rt)-1],1]
    if (x < min) {
        min <- x
        minperm <- rt
    }
}
print(min)
print(minperm)

#initializing the segment things 
xstarts <- c()
xends <- c()
ystarts <- c()
yends <- c()

for (i in 1:n) {
    xstarts <- append(xstarts, coords[minperm[i],1])
    xends <- append(xends, coords[minperm[i+1],1])
    ystarts <- append(ystarts, coords[minperm[i],2])
    yends <- append(yends, coords[minperm[i+1],2])
}
segments <- data.frame(
    xstarts,
    xends, 
    ystarts, 
    yends
)
#plotting the graph
plot2 <-    ggplot() +
    geom_point(
        data = coords,
        aes(x = xcoord, y = ycoord),
        color = "blue",
        size = 2
    ) +
    geom_segment(
        data = segments,
        aes(x = xstarts, y = ystarts, xend = xends, yend = yends),
        color = "black"
    ) + 
    geom_point( 
        data = coords,
        aes(x = xstarts[1], y = ystarts[1]), 
        size = 4,
        color = "red"
    )

print(plot2)