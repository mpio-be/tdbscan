
#' Spatio-temporal clustering from tdbscan outpout manipulated by dt2Convexhull
#'
#' @param d         a data.frame including points which belong to clusters (tdbscan output), latitude, longitude and datetime
#' @param pid       ID unique for points that belong to one cluster
#' @param arrival   arrival datetime in polygon
#' @param departure departure time from polygon
#' @param geometry  convex hull of points of a unique cluster
#'
#' @return data.table including pid, s_clustID = ID unique for spatial overlapping clusters,
#'         st_clustID = ID unique for spatial temporal overlapping clusters
#' @export
#'
#' @importFrom data.table  data.table as.data.table setnames .N := .SD .I
#' @importFrom igraph      groups graph_from_edgelist  components
#' @importFrom sf          st_as_sf st_intersects
#'
#' @examples
#' require(data.table)
#' require(ggplot2)
#'
#' data(zbird)
#' z = tdbscan(zbird, eps = 12, minPts   = 5, maxLag = 5, borderPoints = TRUE )
#' z = z[, clustID := factor(clustID)]
#'
#' d = data.frame(zbird) %>% data.table
#' d = merge(z, d, by.x = 'id', by.y = 'sp.ID')
#'
#' d = rbindlist(list(copy(d[, tagID := 'bird1']), copy(d[, tagID := 'bird2'])), use.names = TRUE)
#' d[tagID == 'bird2', x := x + 5]
#' d[tagID == 'bird2', x := x + 5]
#' d[!is.na(clustID), ID := paste0(tagID, '_', clustID)]
#'
#' dp = dt2Convexhull(d, pid = 'ID', projection = '+proj=utm +zone=4 +datum=WGS84')
#'
#' s = stoscan(dp)
#' d = merge(d, s, by.x = 'ID', by.y = 'pid', all.x = TRUE)
#'
#' # plot of spatial overlapping clusters
#' ggplot(d, aes(x, y, color = as.character(s_clustID) ) ) +
#'   geom_path(aes(color = NULL), col = 'grey', size = .5) +
#'   geom_point( alpha = .5, size = 2)
#'
#' # plot of spatial and temporal overlapping clusters
#' ggplot(d, aes(x, y, color = as.character(st_clustID) ) ) +
#'   geom_path(aes(color = NULL), col = 'grey', size = .5) +
#'   geom_point( alpha = .5, size = 2)
#'
stoscan = function(d, pid = 'pid', arrival = 'arrival', departure = 'departure', geometry = 'geometry'){

  ID1=ID2=arrival1=arrival2=departure1=departure2=geom1=geom2=rowID=row.id=col.id=t_overlap=NULL
  `.` = function(...) NULL

  d = data.table(d)
  setnames(d, c(pid, arrival, departure, geometry), c('pid', 'arrival', 'departure', 'geometry'))

  # check which polygons overlap in space
  o = st_as_sf(d) %>% st_intersects %>% as.data.table

  # subset unique combinations
  o = o[row.id != col.id]
  o = o[o[, .I[1], by = list(pmin(row.id, col.id), pmax(row.id, col.id))]$V1]

  # merge with polygon ids
  d[, rowID := 1:nrow(d)]
  o = merge(o, d[, .(rowID, ID1 = pid, arrival1 = arrival, departure1 = departure)], by.x = 'row.id', by.y = 'rowID', all.x = TRUE)
  o = merge(o, d[, .(rowID, ID2 = pid, arrival2 = arrival, departure2 = departure)], by.x = 'col.id', by.y = 'rowID', all.x = TRUE)

  # check which polygons overlap in space
  o[, t_overlap := ifelse(c(difftime(min(departure1, departure2),
                                     max(arrival1, arrival2), units = 'mins') %>% as.numeric) > 0, TRUE, FALSE), by = 1:nrow(o)]

  # find spatial clusters
  g = graph_from_edgelist( o[, .(ID1, ID2)] %>% as.matrix, directed = FALSE)
  gr = components(g) %>% groups

  ids = sapply(gr, length); ids = rep(names(ids), times = ids) %>% as.integer
  os = data.table(pid = unlist(gr), s_clustID = ids)

  # find spatio-temporal clusters
  g = graph_from_edgelist( o[t_overlap == TRUE, .(ID1, ID2)] %>% as.matrix, directed = FALSE)
  gr = components(g) %>% groups

  ids = sapply(gr, length); ids = rep(names(ids), times = ids) %>% as.integer
  osp = data.table(pid = unlist(gr), st_clustID = ids)

  o = merge(os, osp, by = 'pid', all.x = TRUE)

  setnames(d, c('pid', 'arrival', 'departure', 'geometry'), c(pid, arrival, departure, geometry))
  o

}


