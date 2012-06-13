includePhy <- function(plant, ...){
	phy <- setPhy(...)
	plant$phy <- phy
return(plant)
}