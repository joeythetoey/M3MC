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

#for the nearest neighbor strategy - self explanatory - greedy algo or whatever

#function to find closest neighbor 
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
print(smallestloop)
print(visitedalready)


#initializing the segment things 
xstarts <- c()
xends <- c()
ystarts <- c()
yends <- c()

for (i in 1:n) {
    xstarts <- append(xstarts, coords[visitedalready[i],1])
    xends <- append(xends, coords[visitedalready[i+1],1])
    ystarts <- append(ystarts, coords[visitedalready[i],2])
    yends <- append(yends, coords[visitedalready[i+1],2])
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
        aes(x = coords[startingpoint, 1], y = coords[startingpoint, 2]), 
        size = 4,
        color = "red"
    )

print(plot2)
