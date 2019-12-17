
###############################
###Función para extraer números
################################


extraer_numeros <-  function(a) {
  b <- strsplit(a, "")
  c <- c()
  for (i in 1:length(b[[1]])) {
    numeros <- c(as.character(0:9))
    if (b[[1]][i] %in% numeros) {
      c <- c(c, b[[1]][i])
    }
  }
  c <-  as.numeric(paste(c, collapse = ""))
  return(c)
}

