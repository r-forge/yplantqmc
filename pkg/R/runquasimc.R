	
	runquasimc <- function(cfgfile, inputfile=NA, outputfile=NA, debug=FALSE, intern=TRUE){
	
		if(debug)
			cmd <- paste("c:\\QuasiMC\\QuasiMC.exe -e c:\\QuasiMC\\enviro.e",cfgfile, "-debug <", inputfile, ">", outputfile)
		else
			cmd <- paste("c:\\QuasiMC\\QuasiMC.exe -e c:\\QuasiMC\\enviro.e", cfgfile, "-no_xserver <", inputfile, ">", outputfile)
			
		if(intern)
			shell(cmd, intern=TRUE)
		else
			shell(cmd, intern=FALSE)
		
	}