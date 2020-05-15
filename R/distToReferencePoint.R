#' @export manyDistToLoc
# this function measures the distance of a set of points from one reference point 
manyDistToLoc <- function(points, reference.point) {
	R <- 6371 
	p1rad <- points * pi/180
	p2rad <- reference.point * pi/180 
	d <- sin(p1rad[, 2])*sin(p2rad[2])+cos(p1rad[, 2])*cos(p2rad[2])*cos(abs(p1rad[, 1]-p2rad[1]))	
	d <- acos(d) 
	return(	R*d )
}
