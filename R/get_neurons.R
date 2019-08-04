



fishatlas_read_neurons <- function(cell.type = "Mitral Cells",
                                   fishatlas_url = "https://fishatlas.neuro.mpg.de"){
  Payload = '{cell_type: "Mitral Cells"}'
  class(Payload) = "json"
  post_data=list()
  post_data["cell_type"]= cell.type
  ct_info = fishatlas_fetch(path = "neurons/get_neurons_of_cell_type",
                            body = Payload,
                            parse.json = TRUE,
                            simplifyVector=FALSE,
                            include_headers = FALSE,
                            fishatlas_url = "https://fishatlas.neuro.mpg.de")
  names(ct_info[[1]])
}



