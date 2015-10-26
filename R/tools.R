cmdRun <- function( cmdString , threads=1 , intern=FALSE , tsv=FALSE , lines=FALSE , quiet=FALSE , considerJobs=getOption("considerJobs",TRUE) ){

  numStrings <- length(cmdString)
  numJobs <- max(unlist(lapply(strsplit(cmdString,split="|",fixed=T),length)))

  if(considerJobs){
    threads <- floor(threads/numJobs)
  }

  if( numStrings < threads ){ threads <- numStrings }

  if(threads<2){
    res<-lapply(1:length(cmdString), function(i){
      if(!quiet){print(cmdString[i])}

      if(intern){
        system(cmdString[i], intern=intern)
      } else if(tsv){
        cmdString.call <- pipe(cmdString[i],open="r")
        result <- read.delim(cmdString.call, header=FALSE, stringsAsFactors=FALSE)
        close(cmdString.call)
        return(result)
      } else if(lines){
        cmdString.call <- pipe(cmdString[i],open="r")
        result <- readLines(cmdString.call)
        close(cmdString.call)
        return(result)
      } else{
        system(cmdString[i])
      }

    })
  } else{
    res<-mclapply(1:length(cmdString), function(i){
      if( i <= threads ){ Sys.sleep(i/100) }
      if(!quiet){print(cmdString[i])}


      if(intern){
        system(cmdString[i], intern=intern)
      } else if(tsv){
        cmdString.call <- pipe(cmdString[i],open="r")
        result <- read.delim(cmdString.call, header=FALSE, stringsAsFactors=FALSE)
        close(cmdString.call)
        return(result)
      } else if(lines){
        cmdString.call <- pipe(cmdString[i],open="r")
        result <- readLines(cmdString.call)
        close(cmdString.call)
        return(result)
      } else{
        system(cmdString[i])
      }


    }, mc.cores=threads, mc.preschedule=FALSE)
  }

  return(res)

}


'%ni%' <- Negate('%in%')

lsl<-function(){system("ls -l")}
lsltr<-function(){system("ls -ltr")}
lslSr<-function(){system("ls -lSr")}
lslt<-function(){system("ls -lt")}
lslS<-function(){system("ls -lS")}


cranex <- function(what) {
  url <- paste("https://github.com/search?l=r&q=%22", as.character(substitute(what)), "%22+user%3Acran+language%3AR&ref=searchresults&type=Code&utf8=%E2%9C%93", sep="", collapse="")
  print(url)
}

resizeWindow <- function(){
	if(.Platform$GUI=="X11"){
		.adjustWidth <- function(...){
			options(width=Sys.getenv("COLUMNS"))
			TRUE
		}
		.adjustWidthCallBack <- addTaskCallback(.adjustWidth)
	}
}



randomStrings<-function(n=1,len=12){
	unlist(lapply(1:n, function(x) return(paste(sample(c(rep(0:9,each=5),LETTERS,letters),len,replace=TRUE),collapse='')) ))
}


shead <- function(filenames, n=10){
	cmdString <- paste("head -n",n,filenames)
  res <- cmdRun(cmdString,lines=TRUE, quiet=TRUE)
	dump <- lapply(res,cat,sep="\n")
}
stail <- function(filenames, n=10){
	cmdString <- paste("tail -n",n,filenames)
  res <- cmdRun(cmdString,lines=TRUE, quiet=TRUE)
	dump <- lapply(res,cat,sep="\n")
}
sless <- function(filename){
	system(paste("less",filename))
}
scats <- function(filenames, n=10){
	cmdString <- paste("cat",filenames)
  res <- cmdRun(cmdString,lines=TRUE, quiet=TRUE)
	dump <- lapply(res,cat,sep="\n")
}



filelines <- function( filenames, threads=getOption("threads",1L) , quiet=FALSE){
	cmdString <- paste("wc -l",filenames,"| awk '{print $1}'")
	res <- as.numeric( unlist( cmdRun( cmdString, threads, intern=TRUE, quiet=quiet ) ) )
	return(res)
}


gunzip <- function( gzfiles, threads=getOption("threads",1L) ){
	numfiles<-length(gzfiles)
	outnames<-removeext(gzfiles)
	cmdString <- paste("gunzip",gzfiles)
	res <- cmdRun( cmdString , threads )
	return(outnames)
}



# file finding
files2 <- function( cmd ){
	files.call <- 	pipe(paste("ls -d",cmd))
	files <- readLines(files.call)
	return(files)
}


files <- function( x,...){
	parts<-(unlist(strsplit(x,"/")))
	#cat("parts",parts,"\n")
	if(length(parts)>1){
		pth<-paste(parts[1:length(parts)-1],collapse="/")
	}
	else{
		pth="."
	}
	nm<-parts[length(parts)]
	if(length(parts)>1){
		list.files(pattern=glob2rx(nm),path=pth,full.names=TRUE, ... )
	}
	else{
		list.files(pattern=glob2rx(nm), ... )
	}
}


findfiles <- function( path="." , string="" ){
	files.call <- pipe(paste("find ",path," -name '",string,"'",sep=""))
	files <- readLines( files.call )
	return(files)
}


# filename extraction / manipulation
getfullpaths <- function( paths ){ # can be replaced with normalizePath
	cmdString <- paste("readlink -f",paths)
	res <- cmdRun(cmdString, intern=TRUE)
	return(res)
}


get.prefix <- function( names,separator){
	for(i in 1:length(names)){
		names[i]<-unlist(lapply(strsplit(names[i],separator),"[",1))
	}
}


get.suffix <- function( names,separator){
	for(i in 1:length(names)){
		stringvec<-unlist(strsplit(names[i],separator))
		names[i]<-stringvec[length(stringvec)]
	}
	names
}


removeheader <- function( filename ){
	ext<-file_ext(filename)
	shortname<-basename(removeext(filename))
	outname<-paste(shortname,"_rh.",ext,sep="")
	status <- system(paste("tail -n +2",filename,">",outname))
	if (status==0){system(paste("mv",outname,filename))}
	outname<-basename(filename)
	return(outname)
}


remove.prefix <- function( names, prefix){
	for(i in 1:length(names)){
		names[i]<-unlist(strsplit(names[i],prefix))[2]
	}
	names
}


remove.suffix <- function( names,suffix){
	for(i in 1:length(names)){
		names[i]<-unlist(strsplit(names[i],suffix))[1]
	}
	names
}


tsvRead <- function( tsv, col_names=F , threads=getOption("threads",1L) , progress=FALSE , ... ){
  if( length(tsv) < threads ){ threads <- length(tsv) }
  tsvs <- mclapply(tsv, read_tsv , col_names=col_names , progress=progress , ... , mc.cores=threads , mc.preschedule=FALSE )
  if( length(tsvs)==1 ){ tsvs <- tsvs[[1]] }
  return(tsvs)
}


tsvWrite <- function( tsv, file, col_names=FALSE, row_names=FALSE,  ... ){
	if(row_names){
    write.table( tsv, file, sep="\t", quote=FALSE, col.names=colnames, row.names=rownames, ... )
  } else{
    write_tsv(tsv, path=file, col_names=col_names )
  }
}


read.tsv <- function( tsv, ... ){
	read.table(tsv,stringsAsFactors=FALSE,sep="\t", ... )
}


removeext <- function( filenames ){
	filenames<-as.character(filenames)
	for(i in 1:length(filenames)){
		namevector<-unlist(strsplit(filenames[i],"\\."))
		filenames[i]<-paste(namevector[1:(length(namevector)-1)],collapse=".")
	}
	filenames
}


write.tsv <- function( tsv, colnames=FALSE, rownames=FALSE, ... ){
	write.table(tsv,sep="\t",quote=FALSE,col.names=colnames,row.names=rownames, ... )
}


# file naming
uniquefilename <- function( filename ){
	library(tools)
	if(grepl("\\.",basename(filename))==TRUE){
		ext<-paste(".",file_ext(filename),sep="")
	} else{ ext="" }
	suffixes<-c("",paste("_",1:100,sep=""))
	filenames<-paste(removeext(filename),suffixes,ext,sep="")
	filename<-filenames[which(file.exists(filenames)==FALSE)[1]]
	return(filename)
}


renamefiles <- function( filelist, pattern="", replacement=""){
	if(pattern==""){stop("YOU MUST SPECIFY 'pattern'\n")}
	filenames<-basename(filelist)
	dirnames<-dirname(filelist)
	outnames<-paste0(dirnames,"/",gsub(pattern,replacement,filenames))
	nametab<-data.frame("before"=filenames,"after"=outnames,stringsAsFactors=FALSE)
	print(nametab)
	for(i in 1:length(filelist)){
		cat("renaming ",filenames[i]," to ",outnames[i],"\n",sep="")
		system(paste("mv",filelist[i],outnames[i]))
	}
}

renamefiles2 <- function( filelist, pattern="", replacement=""){
	if(pattern==""){stop("YOU MUST SPECIFY 'pattern'\n")}
	cmdString <- paste0 ("rename '",pattern,"' '",replacement,"' ",paste(filelist,collapse=" "))
	print(cmdString)
	system(cmdString)
}

# rotation functions
rotcol  <- function( vec){ vec[,rot(ncol(vec))] }
rotlist <- function( l ){lapply(c(2:length(l),1),function(x) l[[x]] ) }
rot     <- function( x){(1:x %% x) +1}
rotrow  <- function( vec){ vec[rot(nrow(vec)),] }
rotvec  <- function( vec){ vec[rot(length(vec))] }
