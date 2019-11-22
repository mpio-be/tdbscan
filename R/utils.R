
#' data.frame to Track
#'
#' @param d 	       a data.frame or an object inheriting from it
#' @param y 	       column name (character vector). The latitude.
#' @param x 	       column name (character vector). The longitude.
#' @param dt	       column name (character vector).  datetime
#' @param projection character vector. if missing will default to  `"+proj=longlat +ellps=WGS84"` with a warning.
#'
#' @return  A  \link[trajectories]{Track}
#' @export
#' @importFrom data.table     setorderv
#' @importFrom sp             SpatialPoints  CRS
#' @importFrom spacetime      STIDF
#' @importFrom trajectories   Track
#' @examples
#' require(data.table)
#' data(pesa56511)
#' x = data.frame(pesa56511) %>% data.table
#' o = dt2Track(x, projection = sp::proj4string(pesa56511) )
#' a = tdbscan(pesa56511, eps =6600, minPts   = 8, maxLag = 6, borderPoints = TRUE )
#' b = tdbscan(o, eps =6600, minPts   = 8, maxLag = 6, borderPoints = TRUE )
#' identical(a, b)
#'
dt2Track <- function(d, y = 'latitude', x = 'longitude', dt = 'time', projection ) {
	d = data.table(d)
	setorderv(d, dt, 1)

	if(missing(projection)) {
		projection= "+proj=longlat +ellps=WGS84"
		warning( paste('\nAssuming', projection) )
	}

	d[, c(x, y), with = FALSE] %>%
	SpatialPoints(CRS(projection) ) %>%
	STIDF( d[[dt]] ,  d[, setdiff(names(d), c(y, x, dt, 'endTime', 'timeIndex') ), with = FALSE] ) %>%
	Track
}




#' data.frame to data.table with column for convex hull geometry by ID
#'
#' @param d          a data.frame including points which belong to clusters (tdbscan output), latitude, longitude and datetime
#' @param pid        ID unique for points that belong to one cluster
#' @param y 	       column name (character vector). The latitude.
#' @param x 	       column name (character vector). The longitude.
#' @param dt	       column name (character vector).  datetime
#' @param projection character vector. if missing will default to  `"+proj=utm +zone=4 +datum=WGS84"` with a warning.
#'
#' @return a data.table with geometry column (convex hull polygons) and connected arrival and departure datetime
#' @export
#'
#' @importFrom data.table  data.table setnames := rbindlist
#' @importFrom sf         st_as_sf st_union st_convex_hull st_geometry st_set_crs st_crs
#'
#' @examples
#' require(data.table)
#' data(zbird)
#' z = tdbscan(zbird, eps = 12, minPts   = 5, maxLag = 5, borderPoints = TRUE )
#' z = z[, clustID := factor(clustID)]
#'
#' o = data.frame(zbird) %>% data.table
#' o = merge(z, o, by.x = 'id', by.y = 'sp.ID')
#'
#' o = rbindlist(list(copy(o[, tagID := 'bird1']), copy(o[, tagID := 'bird2'])), use.names = TRUE)
#' o[tagID == 'bird2', x := x + 5]
#' o[tagID == 'bird2', x := x + 5]
#' o[!is.na(clustID), ID := paste0(tagID, '_', clustID)]
#'
#' d = dt2Convexhull(o, pid = 'ID', projection = '+proj=utm +zone=4 +datum=WGS84')
#'
dt2Convexhull = function(d, pid, y = 'y', x = 'x', dt = 'time', projection){

  arrival=departure=geometry=dup=NULL
  `.` = function(...) NULL

  if(missing(projection)) {
    projection = '+proj=utm +zone=4 +datum=WGS84'
    warning( paste('\nAssuming', projection) )
  }

  d = data.table(d)

  setnames(d, c(pid, y, x, dt), c('pid', 'y', 'x', 'dt'))

  # jitter overlapping points to still get a polygon if points overlap
  d[, dup := duplicated(d, by = c('pid', 'y', 'x'))]
  d[dup == TRUE, y := jitter(y, factor = 0.0000000001)]
  d[dup == TRUE, x := jitter(x, factor = 0.0000000001)]
  d[, dup := NULL]

  # file with all convex hull polygons for each cluster of points
  o = d[!is.na(pid),  .(

    arrival = min(dt, na.rm = TRUE),
    departure = max(dt, na.rm = TRUE),

    geometry =
      st_as_sf(.SD, coords = c('x', 'y')) %>%
      st_union %>%
      st_convex_hull %>%
      st_geometry %>%
      st_set_crs(st_crs(projection))

  )  ,  by = .(pid) ]

  setnames(d, c('pid', 'y', 'x', 'dt'), c(pid, y, x, dt))
  o

}



