#' Retreive scores in bedGraph files
#'
#' \code{bedRead} returns a list of vectors containing the scores for intervals in a set of bedGraph files.
#'
#' @param bgFiles A character vector of paths to bedgraph files.
#' @param threads A positive integer specifying how many bams to process simultaneously.

bedRead <- function( bgFiles , output="lines" , threads=getOption("threads", 1L), sample=NULL, chrom=NULL, first=NULL ){

  numbeds <- length(bgFiles)

  if( !is.null(sample) & !is.null(chrom) ){stop("must set sample or chrom or neither, but not both")}

  if(!is.null(sample)){
    stopifnot(is.numeric(sample), length(sample)==1)
    preset <- paste("shuf -n",sample,bgFiles)
  } else{
    preset <- paste("cat",bgFiles)
  }

  postset=""
  if( !is.null(chrom)){
    stopifnot(length(chrom)==1)
    postset <- paste0("| grep -P '^",chrom,"\\t'",postset)
  } else if(!is.null(first) & is.numeric(first)){
    postset <- paste("| head -n",first,postset)
  } else{
    
  }

  if(output=="scores"){
    postset <- paste(postset,"| awk '{print $4}'")
  } else if(output=="sizes"){
    postset <- paste(postset,"| awk '{print $3-$2}'")
  }

  cmdString <- paste(preset,postset)

  res <- cmdRun( cmdString, threads, lines=TRUE)
  res <- lapply( res, as.numeric )
  names(res) <- basename(bgFiles)

  return(res)

}
