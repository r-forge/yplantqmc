
installQuasiMC <- function(proxy=FALSE){

	if(.Platform$OS.type != "windows"){
		stop("QuasiMC is (currently) available for Windows only.")
	} else {
		
		if(!file.exists("c:/QuasiMC")){
			dir.create("c:/QuasiMC")
			message("Created the directory : c:/QuasiMC")
		}

		if(proxy)utils::setInternet2(TRUE)
		
		if(file.exists("c:/quasimc/QuasiMC.zip")){
			message("Updating existing QuasiMC installation.")
			unlink("c:/quasimc/QuasiMC.zip")
		}
		download.file("http://www.remkoduursma.com/quasimc/QuasiMC.zip",
			"c:/quasimc/QuasiMC.zip")
		unzip("c:/quasimc/QuasiMC.zip", exdir="C:/quasimc")
		message("'QuasiMC.exe' and 'enviro.e' have been placed in the folder 'c:/QuasiMC'.\n  !- Please do not rename or move!\n")
		
	}
}
	
	