#' @title Download all registered neuron data from the Fish Brai Atlas project
#'
#' @description Download all neurons from the fishatlas.neuro.mpg.de.
#' These will be stored as a 53.7 MB at the location of this package, at inst/exdata or a user
#' specified location. These neuron data can be retreived from fishatlas.neuro.mpg.de in their original locations,
#' after registration to the template brain, or after having been mirrored to one hemisphere, either left or right.
#' We can therefore either read neurons for one hemisphere, or the other, or 'mirror' neurons using the saved data, rather than a saved
#' spatial transform. All neurons from Kunst et al. 2019.
#' @param side the side of the brain for which you want to read neurons. Neurons have been mirrored by Kunst et al. to have their
#' somata all on one side of the brain, or the other. The raw data, 'Original' can have somata at any location.
#' @param save.path if you do not want to save at \code{package_location/inst/exdata} then specify a location
#' @param fishatlas_ids the name of fishatlas neurons to read or mirror, e.g. \code{T_161005_HuCxBG_erk_15_1}
#' @param fishatlas_neurons a \code{neuronlist} of neurons read using this package
#' @param fishatlas_url the web location of the fish brain atlas project
#' @return a \code{data.frame]
#' @seealso \code{\link{fishatlas_cell_types}}, \code{\link{fishatlas_read_brain}}
#' @examples
#' \dontrun{
#' ## First we need to download all of the neurons
#' ### We should only ever have to do this once!
#' fishatlas_download_neurons()
#'
#' ## Let's get all that sweet neuron data!
#' zfishn = fishatlas_read_saved_neurons(side = "Original)
#' plot3d(zfishn,soma = TRUE, lwd = 2)
#'
#' ## Hmm, but it would be better to have them all on the same side
#' clear3d()
#' zfishr = fishatlas_read_saved_neurons(side = "Right)
#' plot3d(zfishr,soma = TRUE, lwd = 2, col = "red")
#'
#' ## Great! How does that compare with neurons all on the left?
#' ### Let's just look at the first 10 mirored.
#' zfishl = fishatlas_mirror_saved_neurons(fishatlas_neurons[1:10], side = "Left")
#' plot3d(zfishl,soma = TRUE, lwd = 2, col = "cyan")
#'
#' }
#' @references Kunst, Michael, Eva Laurell, Nouwar Mokayes, Anna Kramer, Fumi Kubo, António M. Fernandes, Dominique Förster, Marco Dal Maschio, and Herwig Baier. 2019. “A Cellular-Resolution Atlas of the Larval Zebrafish Brain.” Neuron, May. https://doi.org/10.1016/j.neuron.2019.04.034.
#' @export
#' @rdname fishatlas_read_saved_neurons
fishatlas_download_neurons <- function(fishatlas_url = "https://fishatlas.neuro.mpg.de",
                              save.path = NULL){
  if(is.null(save.path)){
    save.path = path.package("fishatlas", quiet = FALSE)
  }
  package.path.data = paste(save.path,"inst/exdata",sep="/")
  if(!dir.exists(package.path.data)){
    dir.create(package.path.data)
  }
  # download neurons
  urls=file.path(paste0(fishatlas_url,"/neurons/download/download_all_neurons_aligned"))
  message("Checking for presence of data ...")
  for (url in urls){
    localfile= paste(package.path.data,"/","MPIN-Atlas__Kunst_et_al__neurons_all.zip",sep="")
    if(file.exists(localfile)){
      message("exists here: ", localfile)
      next
    }
    message("Downloading data ...")
    t=try(utils::download.file(url, localfile, mode='wb'))
    if(inherits(t,'try-error')) {
      message("unable to download ", url)
      next
    }
  }
}

#' @export
#' @rdname fishatlas_read_saved_neurons
fishatlas_read_saved_neurons <- function(side = c("Right","Left", "Original"),
                                         fishatlas_ids = NULL,
                                         save.path = NULL,
                                         fishatlas_url = "https://fishatlas.neuro.mpg.de"){
  side = match.arg(side)
  if(fishatlas_find_neurons()==""){
    fishatlas_download_neurons(fishatlas_url=fishatlas_url,save.path=save.path)
  }
  location = fishatlas_find_neurons(save.path=save.path)
  files = utils::unzip(location, list = TRUE)$Name
  files = files[grepl(side, files)]
  files = files[grepl(".swc", files)]
  nams = gsub(".swc","",basename(files))
  if(!is.null(fishatlas_ids)){
    files = files[nams%in%fishatlas_ids]
    nams = nams[nams%in%fishatlas_ids]
  }
  neurons = nat::read.neurons(location, neuronnames = files)
  names(neurons) = neurons[,"name"] = nams
  neurons[,"side"] = side
  neurons
}

#' @export
#' @rdname fishatlas_read_saved_neurons
fishatlas_mirror_saved_neurons <- function(fishatlas_neurons,
                                     side = c("Right","Left"),
                                     save.path = NULL,
                                     fishatlas_url = "https://fishatlas.neuro.mpg.de"){
  side = match.arg(side)
  nams = names(fishatlas_neurons)
  fishatlas_read_saved_neurons(side=side,
                               save.path=save.path,
                               fishatlas_url=fishatlas_url,
                               fishatlas_ids=nams)
}



# hidden
fishatlas_find_neurons <- function(save.path = NULL){
  if(is.null(save.path)){
    system.file("inst/exdata/MPIN-Atlas__Kunst_et_al__neurons_all.zip", package = 'fishatlas')
  }else{
    paste0(save.path,"/","MPIN-Atlas__Kunst_et_al__neurons_all.zip")
  }
}


