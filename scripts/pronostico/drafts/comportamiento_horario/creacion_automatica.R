
#' #clean a process original bases to create month staging table
#' #' @param original_path : path field where original  base_list[[i]] places
#' #' @param staging_path : path field where staging  base_list[[i]] places
#' #' @return : staging table


original_path <- "Y:/V2.0/data/staging"
staging_path<- "Y:/V2.0/data/comportamiento_horarios"




compare_maker <-
  function(original_path,
           staging_path,
           month_to_create = NULL) {
    
setwd("Y:/V2.0/scripts/pronostico/drafts/comportamiento_horario")
source("extraer_numeros.R") 
    
    '%!in%' <- function(x,y)!('%in%'(x,y))
    #Compara la data en origina contra staging para halla posibles tablas faltantes
    
    ####original####
    files_original <- list.files(original_path)
    position_original <-
      as.vector(sapply(files_original, extraer_numeros))
    files_original <-
      data.frame(files = files_original , position = position_original)
    
    
    ####staging####
    files_staging <- list.files(staging_path)
    position_staging <-
      sapply(str_extract_all(files_staging, "[0-9]+"), "[[", 1) %>% as.numeric
    files_staging <-
      data.frame(files = files_staging , position = position_staging)
    
    ####compare####
   compare <-
        files_original$position[(which(files_original$position %!in% files_staging$position))]
   
    if (length(compare) == 0) {
      stop("Files Complete")
    }
    
    compare <- as.list(compare)
    
    #Evaluar deacuedo al origen del archivo.
    
 
    if (original_path == "Y:/V2.0/data/staging") {
      source("comportamiento_horario.R")
            staging <- "Y:/V2.0/data/staging"
      
      
            
      for (i in compare) {
        print(paste0("Creando staging mes ausente ", i))
        comportamiento_horario(staging, i)
      }
      
      print("Archivos completos")
 
      
    }
    
    print("xd")
  }
