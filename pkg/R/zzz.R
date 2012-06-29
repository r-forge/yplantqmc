.onAttach <- function(lib, pkg) {
    
	ver <- read.dcf(file.path(lib,pkg,"DESCRIPTION"), "Version")
    msg <- sprintf("\n-- Plant modelling with Yplant - QuasiMC for R (version %s)\n\tRefer to the manual for detailed instructions.\n", 
		as.character(ver))
    packageStartupMessage(msg)
	
	if(.Platform$OS.type != "windows")
		packageStartupMessage("-- You are not using a Windows machine - YplantDay() and runYplant() will not work.")
	
	if(!checkInstallation() & .Platform$OS.type == "windows"){
		packageStartupMessage("!- To Install QuasiMC, type installQuasiMC().")
		packageStartupMessage("   You need an internet connection - two files will be copied to c:/QuasiMC")
	}
}

.onUnload <- function(libpath){
    library.dynam.unload("YplantQMC", libpath)
	#unloadNamespace("YplantQMC")
}