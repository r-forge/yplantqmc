checkInstallation <- function(){

	chk <- file.exists("c:/QuasiMC/enviro.e") & file.exists("c:/QuasiMC/QuasiMC.exe")

return(chk)
}