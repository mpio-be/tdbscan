# make toybird -----
    require(rgdal)
    require(sp)
    require(trajectories)
    require(magrittr)

    toybird = readOGR("x.geojson")   %>%
    spacetime::STIDF(time = seq(Sys.time(), by = 'hour', along.with = . ),
                     data = data.frame(ID = 1:length(.))) %>%
    trajectories::Track()

    proj4string(toybird) = '+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs'

    save(toybird, file = 'data/toybird.RData')

# make pesa56511----

    require(sdb)
    require(sp)
    require(spacetime)
    require(trajectories)
    require(data.table)
    require(anytime)

    x = dbq(user = 'mihai', q = 'select distinct latitude, longitude,datetime_,quality from ARGOS.2014_PESA where tagID = 56511')
    x[, datetime_ := anytime(datetime_,asUTC = TRUE, tz = 'UTC')]

    x = STIDF(SpatialPoints(x[, .(longitude, latitude)], CRS("+proj=longlat +ellps=WGS84") ), x$datetime_, x[, .(quality)])
    x = Track(x)
    is.projected(x)

    x =spTransform(x, CRS('+proj=laea +lat_0=90 +lon_0=-156.653428 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))
    is.projected(x)
    plot(x)

    pesa56511 = x
    save(pesa56511, file = 'data/pesa56511.RData')

# rann speed----
    require(dbscan)
    require(magrittr)
    require(data.table)
    require(tdbscan)
    require(ggrepel)

    x = data.table(x = rnorm(1000000, mean = 10), y = rnorm(1000000 , mean = 10))

    started.at=proc.time()

    z = nn2(x, k = 5, radius = 0.5)$nn.dists %>% melt %>% data.table
    dim(z)
    timetaken(started.at)

# using DT--------
    x = data.table(x=c(2, 3.1, 4.2, 4.5, 5.56, 6.2, 6.6, 6.6, 2.2,2.4,2.3, 6.7),
                   y=c(2, 3.1, 4.2, 4, 6.63, 6.6, 6.3, 6.8, 6.5,6.5,6.1, 6.1))

    cl = dbscan(x, eps = 1, minPts = 3)
    cc = cbind(x, cluster = dbscan(x, eps = 1, minPts = 3)$cluster)[, id := 1:.N]
    ggplot(cc, aes(x,y,color = factor(cluster))) + geom_point( ) + geom_text_repel(aes(label = id))

    #--
    eps = 1
    minPts = 3

    k =  dbscan::frNN(x, eps = eps,  sort = FALSE)

    ids = sapply(k$dist, length); ids = rep(1:nrow(x), times = ids)
    z = data.table(id = ids, ngb = unlist(k$id), dist = unlist(k$dist) )


    # max n by id
    z[, n :=  .N + 1, by = id] # +1 because a pt is always in its own eps neighborhood

    # eps neighborhood
    z[, iscore := n >= minPts]

    # find valid border points (points in the neighborhood of a core point)
    z[ !(iscore), iscore :=  z[!(iscore), ngb] %in% z[(iscore), id] ]
    z = z[(iscore)][,iscore := NULL]

    # find clusters
    z[, uid := paste0( sort(c(id, ngb)) , collapse = ''), by = 1:nrow(z) ] # unique id
    z = unique(z, by = 'uid')[, uid := NULL]

    z[, id := as.character(id)] # so it will be  interpreted as symbolic vertex name by graph_from_edgelist
    z[, ngb := as.character(ngb)] # so it will be  interpreted as symbolic vertex name by graph_from_edgelist

    g = graph_from_edgelist( z[, .(id, ngb)] %>% as.matrix, directed = FALSE)

    gr = components(g) %>% groups

    ids = sapply(gr, length); ids = rep(names(ids), times = ids)%>% as.integer
    o = data.table(id = unlist(gr), clust = ids)
    o[, id := as.numeric(id)]

    # expand o to the original data
    o = merge(data.table(id = 1:nrow(x) ), o, by = 'id', sort = FALSE, all.x = TRUE)

# benchmark-----
    requireNamespace("fpc", quietly = TRUE)
    requireNamespace("microbenchmark", quietly = TRUE)

    set.seed(665544)
        n <- 10000
        x <- cbind(
        x = runif(10, 0, 10) + rnorm(n, sd = 0.2),
        y = runif(10, 0, 10) + rnorm(n, sd = 0.2)
        )


    t_dbscan <- microbenchmark::microbenchmark(dbscan::dbscan(x, .3, 3), times = 5, unit = "ms")
    t_fpc    <- microbenchmark::microbenchmark(fpc::dbscan(x, .3, 3), times = 5, unit = "ms")
    t_DBSCAN <- microbenchmark::microbenchmark(tdbscan::DBSCAN(x, .3, 3), times = 5, unit = "ms")


    ## speedup of the kd-tree-based version compared to the fpc implementation
    median(t_fpc$time) / median(t_dbscan$time)
    median(t_DBSCAN$time) / median(t_dbscan$time)

# stdbscan------
    sapply(c('ggplot2','ggrepel', 'dbscan', 'magrittr', 'data.table'), require, character.only = TRUE)


    x = data.table(
        x=c(2,3.1,4.2,4.5,5.56,6.2,6.6,6.6,2.2,2.4,2.3,6.7, 6.5, 6.2, 5.9, 6.1, 6.3,2.18, 2.31),
        y=c(2,3.1,4.2,4,6.63,6.6,6.3,6.8,6.5,6.5,6.1,6.1,6.5, 6.9, 6.4, 5.9, 6.2,6.33, 5.51),
        dt = seq.POSIXt(Sys.time(), by = '15 mins', length.out = 19 )
        )
    x[, id := 1:.N]

    cc = cbind(x, cluster = dbscan::dbscan(x[, .(x,y)], eps = 1, minPts = 3)$cluster )
    ggplot(cc, aes(x,y,color = factor(cluster))) + geom_point( ) + geom_text_repel(aes(label = id))

    #
    eps = 1
    minPts = 3




    k =  dbscan::frNN(x[, .(x,y)], eps = eps,  sort = FALSE)

    ids = sapply(k$dist, length); ids = rep(1:nrow(x), times = ids)
    z = data.table(id = ids, ngb = unlist(k$id), dist = unlist(k$dist) )


    # max n by id
    z[, n :=  .N + 1, by = id] # +1 because a pt is always in its own eps neighborhood

    # eps neighborhood
    z[, iscore := n >= minPts]

    # find valid border points (points in the neighborhood of a core point)
    z[ !(iscore), iscore :=  z[!(iscore), ngb] %in% z[(iscore), id] ]
    z = z[(iscore)][,iscore := NULL]

    # pairs should be unique
    z[, uid := paste0( sort(c(id, ngb)) , collapse = ''), by = 1:nrow(z) ] # unique id
    z = unique(z, by = 'uid')[, uid := NULL]

    # temporal contiguity
    z[, tc := abs(id-ngb)]

    # find clusters

    z[, id := as.character(id)] # so it will be  interpreted as symbolic vertex name by graph_from_edgelist
    z[, ngb := as.character(ngb)]

    g = graph_from_edgelist( z[, .(id, ngb)] %>% as.matrix, directed = FALSE)


    g = set_edge_attr(g, 'tc', value = z$tc)

    g = subgraph.edges(g, E(g)[tc==1])


    gr = components(g) %>% groups

    ids = sapply(gr, length); ids = rep(names(ids), times = ids)%>% as.integer
    o = data.table(id = unlist(gr), clust = ids)
    o[, id := as.numeric(id)]

    # add o to the original data
    x[, id := 1:.N]

    o = merge(x, o, by = 'id', sort = FALSE, all.x = TRUE)

    # run dbscan on each cluster to check if they still hold
    o[!is.na(clust), checkClust := dbscan::dbscan( cbind(x,y), eps = eps, minPts = minPts)$cluster != 0, by = clust]
    o[!(checkClust), clust := NA]

    # cleanup & re-order clust
    x[, id := NULL]
    o[, clust  := factor(clust) %>% as.integer]
    o[, .(id, clust)]







