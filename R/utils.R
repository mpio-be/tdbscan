

#' data.frame to Track
#'
#' @param d 	     a data.frame or an object inheriting from it
#' @param y 	     column name (character vector). The latitude. 
#' @param x 	     column name (character vector). The longitude. 
#' @param dt	     column name (character vector).  datetime 
#' @param projection character vector. if missing will default to  `"+proj=longlat +ellps=WGS84"` with a warning.
#'
#' @return  A  \link[trajectories]{Track}
#' @export
#' @importFrom data.table     setorderv
#' @importFrom sp             SpatialPoints  CRS
#' @importFrom spacetime      STIDF
#' @importFrom trajectories   Track
#' @examples
#' 
#' data(pesa56511)
#' x = data.frame(pesa56511) %>% data.table
#' o = dt2Track(x, projection = proj4string(pesa56511) )
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


