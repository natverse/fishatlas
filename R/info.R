#' @title Get information about data on fishatlas.neuro.mpg.de
#'
#' @description Get information about data hosted by fishatlas.neuro.mpg.de, including contributing publications,
#' the figures for these publications that show data hosted by the site and the cell types that can be retreived from the site.
#' This information can be used with \code{fishatlas_read_neurons} to read specific neurons into R.
#' @param fishatlas_url the web location of the fish brain atlas project.
#' @param study the study you can to find information on. See all available using \code{fishatlas_studies()}
#' @param figure figure you want to find information on. See all available by giving \code{fishatlas_figures()} a study.
#' @return a \code{data.frame] or \code{vector]
#' @seealso \code{\link{fishatlas_download_neurons}}, \code{\link{fishatlas_read_brain}}, \code{\link{fishatlas_read_saved_neurons}}
#' @examples
#' \dontrun{
#' ## What studies are available?
#' fishatlas_studies
#'
#' ## Huh, so what figures do we have in Kunst et al. 2019?
#' fishatlas_figures(study = "Kunst et al")
#'
#' ## Six figures. What neuron information did the last of them contain?
#' fishatlas_figure_clusters(study = "Kunst et al", figure = "Figure 6")
#'
#' ## What annotated cell types are there?
#' fishatlas_cell_types()
#'
#' }
#' @references Kunst, Michael, Eva Laurell, Nouwar Mokayes, Anna Kramer, Fumi Kubo, António M. Fernandes, Dominique Förster, Marco Dal Maschio, and Herwig Baier. 2019. “A Cellular-Resolution Atlas of the Larval Zebrafish Brain.” Neuron, May. https://doi.org/10.1016/j.neuron.2019.04.034.
#' @export
#' @rdname fishatlas_info
fishatlas_figure_clusters <- function(fishatlas_url = "https://fishatlas.neuro.mpg.de",
                              study = "Kunst et al",
                              figure = "Figure 3"){
  ct_info = fishatlas_fetch(path = "/neurons/get_pre_defined_neurons_groups",
                            body = NULL,
                            parse.json = TRUE,
                            simplifyVector=FALSE,
                            include_headers = FALSE,
                            fishatlas_url = "https://fishatlas.neuro.mpg.de")
  studies =  names(ct_info$neurons_groups)
  study = match.arg(study, studies)
  study_info = ct_info$neurons_groups[[study]]
  figures = names(study_info$figures)
  figure = match.arg(figure, figures)
  do.call(rbind,study_info$figures[[figure]]$groups)
}

#' @export
#' @rdname fishatlas_info
fishatlas_figures <- function(fishatlas_url = "https://fishatlas.neuro.mpg.de",
                                 study = "Kunst et al"){
  ct_info = fishatlas_fetch(path = "/neurons/get_pre_defined_neurons_groups",
                               body = NULL,
                               parse.json = TRUE,
                               simplifyVector=FALSE,
                               include_headers = FALSE,
                               fishatlas_url = "https://fishatlas.neuro.mpg.de")
  studies =  names(ct_info$neurons_groups)
  study = match.arg(study, studies)
  study_info = ct_info$neurons_groups[[study]]
  names(study_info$figures)
}

#' @export
#' @rdname fishatlas_info
fishatlas_studies <- function(fishatlas_url = "https://fishatlas.neuro.mpg.de"){
  ct_info = fishatlas_fetch(path = "/neurons/get_pre_defined_neurons_groups",
                            body = NULL,
                            parse.json = TRUE,
                            simplifyVector=FALSE,
                            include_headers = FALSE,
                            fishatlas_url = "https://fishatlas.neuro.mpg.de")
  names(ct_info[[1]])
}

#' @export
#' @rdname fishatlas_info
fishatlas_cell_types <- function(fishatlas_url = "https://fishatlas.neuro.mpg.de",
                                 study = "Kunst et al"){
  ct_info = fishatlas_fetch(path = "/neurons/get_neurons_cell_types",
                            body = NULL,
                            parse.json = TRUE,
                            simplifyVector=FALSE,
                            include_headers = FALSE,
                            fishatlas_url = "https://fishatlas.neuro.mpg.de")
  unlist(ct_info$cell_types)
}




