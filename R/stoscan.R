
#' stoscan
#'
#' Spatio-temporal clustering from tdbscan outpout
#'
#' @param DT           A data.table including columns for ID, lat, lon and datetime_
#' @param ID           Unique ID for each cluster of each individual
#' @param lat          Latitude
#' @param lon          Longitude
#' @param datetime_    Date and time
#' @param projection   Projection of the data (should be equal area projection)
#'
# @import trajectories
#' @importFrom data.table  data.table setnames .N := CJ setnames .SD .I
#' @importFrom igraph      groups graph_from_edgelist  components
#' @importFrom sf          st_area st_as_sf st_cast st_crs st_intersection st_set_crs st_union st_convex_hull st_geometry
#'
#' @return                 Data.table with column s_clustID (unique spatially overlapping clusters) and st_clustID (unique spatio-temporal overlapping clusters)
#' @export
#'
#' @examples
#' require(tdbscan)
#' require(data.table)
#' require(magrittr)
#' require(ggplot2)
#'
#' # z bird
#' data(zbird)
#' z = tdbscan(zbird, eps = 12, minPts   = 5, maxLag = 5, borderPoints = TRUE )
#' z = z[, clustID := factor(clustID)]
#'
#' o = data.frame(zbird) %>% data.table
#' o = merge(z, o, by.x = 'id', by.y = 'sp.ID')
#'
#' setnames(o, c('x', 'y'), c('lon', 'lat') )
#' o = rbindlist(list(copy(o[, tagID := 'bird1']), copy(o[, tagID := 'bird2'])), use.names = TRUE)
#' o[tagID == 'bird2', lon := lon + 5]
#' o[!is.na(clustID), ID := paste0(tagID, '_', clustID)]
#'
#' z = stoscan(o, ID = 'ID', lat = 'lat', lon = 'lon', datetime_ = 'time',
#'             projection= '+proj=utm +zone=4 +datum=WGS84')
#' o = merge(o, z, by = 'ID', all.x = TRUE)
#'
#' # plot by tag ID
#' ggplot(o, aes(lon, lat, color = as.character(tagID) ) ) +
#'   geom_path(aes(color = NULL), col = 'grey', size = .5) +
#'   geom_point( alpha = .5, size = 2)
#'
#' # plot by cluster ID of each individual
#' ggplot(o, aes(lon, lat, color = as.character(ID) ) ) +
#'   geom_path(aes(color = NULL), col = 'grey', size = .5) +
#'   geom_point( alpha = .5, size = 2)
#'
#' # plot of spatial overlapping clusters
#' ggplot(o, aes(lon, lat, color = as.character(sp_clustID) ) ) +
#'   geom_path(aes(color = NULL), col = 'grey', size = .5) +
#'   geom_point( alpha = .5, size = 2)
#'
#' # plot of spatial and temporal overlapping clusters
#' ggplot(o, aes(lon, lat, color = as.character(s_clustID) ) ) +
#'   geom_path(aes(color = NULL), col = 'grey', size = .5) +
#'   geom_point( alpha = .5, size = 2)


stoscan = function(DT, ID, lat = 'lat', lon = 'lon', datetime_, projection){

  arrival=arrival1=arrival2=departure=departure1=departure2=geom1=geom2=geometry=NULL
  id1=id2=s_overlap=s_overlap_per=t_overlap=t_overlap_per=tenure1=tenure2=NULL
  `.` = function(...) NULL

  if(missing(projection)) {
    projection= '+proj=utm +zone=4 +datum=WGS84'
    warning( paste('\nAssuming', projection) )
  }

  setnames(DT, c(ID, lat, lon, datetime_), c('ID', 'lat', 'lon', 'datetime_'))

  # file with all convex hull polygons
  d = DT[!is.na(ID),  .(

    arrival = min(datetime_, na.rm = TRUE),
    departure = max(datetime_, na.rm = TRUE),
    tenure = difftime(max(datetime_, na.rm = TRUE), min(datetime_, na.rm = TRUE), units = 'mins') %>%
      as.numeric,

    geometry =
      st_as_sf(.SD, coords = c('lon', 'lat')) %>%
      st_union %>%
      st_convex_hull %>%
      st_geometry %>%
      st_cast('POLYGON') %>%
      st_set_crs(st_crs(projection)) #%>%
     #st_buffer(., dist = 3)  # potential to include buffer

  )  ,  by = .(ID) ]

  setnames(DT, c('ID', 'lat', 'lon', 'datetime_'), c(ID, lat, lon, datetime_))

  # create table with all combinations
  id = d$ID %>% unique
  x = CJ(id1 = id, id2 = id)

  # subset unique combinations
  x = x[x[, .I[1], by = list(pmin(id1, id2), pmax(id1, id2))]$V1]
  x = x[id1 != id2]

  # merge info to id
  x = merge(x, d[, .(id, geom1 = geometry, arrival1 = arrival, departure1 = departure, tenure1 = tenure)], by.x = 'id1', by.y = 'id')
  x = merge(x, d[, .(id, geom2 = geometry, arrival2 = arrival, departure2 = departure, tenure2 = tenure)], by.x = 'id2', by.y = 'id')

  # calculate spatial overlap
  x[, s_overlap := st_intersection(geom1, geom2) %>% st_area(), by = 1:nrow(x)]
  x[, s_overlap_per := st_intersection(geom1, geom2) %>% st_area() * 100 /sqrt( st_area(geom1) * st_area(geom2) ), by = 1:nrow(x)]

  # calculate temporal overlap
  x[, t_overlap := difftime(min(departure1, departure2), max(arrival1, arrival2), units = 'mins') %>% as.numeric, by = 1:nrow(x)]
  x[t_overlap <= 0, t_overlap := NA]
  x[, t_overlap_per := if(!is.na(t_overlap)) t_overlap * 100 / sqrt(tenure1 * tenure2), by = 1:nrow(x)]

  # find spatial clusters
  xs = x[!is.na(s_overlap)]

  g = graph_from_edgelist( xs[, .(id1, id2)] %>% as.matrix, directed = FALSE)
  gr = components(g) %>% groups

  ids = sapply(gr, length); ids = rep(names(ids), times = ids)%>% as.integer
  os = data.table(ID = unlist(gr), s_clustID = ids)

  # find spatio-temporal clusters
  xt = x[!is.na(s_overlap) & !is.na(t_overlap)]

  g = graph_from_edgelist( xt[, .(id1, id2)] %>% as.matrix, directed = FALSE)
  gr = components(g) %>% groups

  ids = sapply(gr, length); ids = rep(names(ids), times = ids)%>% as.integer
  osp = data.table(ID = unlist(gr), sp_clustID = ids)

  o = merge(os, osp, by = 'ID')
  o

}


