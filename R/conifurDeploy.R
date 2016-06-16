conifurDeploy<-function(message="no message"){
	library(conifur)
	res <- system(paste0("git -C /lustre/maize/home/dlv04c/software/r/conifur/ add /lustre/maize/home/dlv04c/software/r/conifur/ &&\
	git -C /lustre/maize/home/dlv04c/software/r/conifur/ commit -a -m '",message,"' &&\
	git -C /lustre/maize/home/dlv04c/software/r/conifur/ push"))
	library(devtools)
	detach("package:conifur",unload=T)
	install_github("dvera/conifur")
	library(conifur)
}
