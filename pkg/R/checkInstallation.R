checkInstallation <- function(){

  if(.Platform$OS.type != "windows")
    return(FALSE)
  
	chk <- file.exists("c:/QuasiMC/enviro.e") & file.exists("c:/QuasiMC/QuasiMC.exe")

return(chk)
}