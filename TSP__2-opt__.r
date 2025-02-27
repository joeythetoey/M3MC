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


#----------------------------------------------------

#simpler and less ai generated 2-opt for smaller size data 

#get an initial path with nearest neighbor: 

findclosest <- function(k, listofvisited) {
    closest <- NA
    mindist <- Inf

    for (j in 1:n) {
        if (!(j %in% listofvisited) && distances[k, j] < mindist) {
            mindist <- distances[k, j]
            closest <- j
        }
    }

    return(closest)
}


# run through all possible starting points 
smallestloop <- 1e10
startingpoint <- NA
for (i in 1:n) {
    start <- c(coords[i,1],coords[i,2])
    visitedalready <- c()
    totaldist <- 0
    closest <- i
    for (j in 1:n) {
        found <- findclosest(closest, visitedalready)
        totaldist <- totaldist + distances[closest, found]
        closest <- found
        visitedalready <- append(visitedalready, found)
    }
    totaldist <- totaldist + distances[closest,i]
    visitedalready <- append(visitedalready, i)
    if (totaldist < smallestloop) {
        smallestloop <- totaldist
        startingpoint <- i
    }
}
print("initial min")
print(smallestloop)
initialpath <- visitedalready

twoopt <- function(route, first, second) {
    new_route <- c()
    for (i in 1:length(route)) {
        new_route <- append(new_route, 0)
    }
    new_route[1:first] <- route[1:first]
    new_route[first+1:second] <- rev(route[first+1:second])
    new_route[second+1:end] <- route[second+1:end]
    return(new_route)
}

minneddistancecost <- function(distances, path) {
    cost = 0 
    for (i in path) {
        cost <- cost + distances[path[i], path[i+1]]
    }
    return(cost)
}

new_distance <- Inf 
best_distance <- minneddistancecost(distances, initialpath)
curr <- initialpath 
for (i in 1:n-2) {
    for (j in i+1:n-1) { 
        new_route <- twoopt(curr, i, j)
        new_distance <- minneddistancecost(distances, new_route)
        if (new_distance < best_distance) {
            curr <- new_route 
            best_distance = new_distance
        }
    }
    print(best_distance)
    print(curr)
}
