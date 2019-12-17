
#' Imprimir string en la consola usando lenguaje C
#'
#' @param ... el texto a imprimir
pprint <- function(...){cat(sprintf(...), "\n")}


#' Extrae un numero de caracteres de un string, iniciando por la derecha
#'
#' @param x el objeto que contiene el texto
#' @param n el numero de caracteres que se deben extraer, partiendo de la derecha
#'
#' @return el texto considerando "n" numero de caracteres del texto "x"
#'
#' @examples
#' ultimas_dos <- substr_right(x = "hola", n = 2)
#' 
substr_right <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}


#' Dado un mes y cuantos meses se quieren adelante, generar el mes
#' 
#' Meses adelante YYYY_MM o YYYYMM o YYYY-MM
#' Esta funcion genera en formato "YYYY_MM" o "YYYYMM" o "YYYY-MM" los meses adelante que se quiera a partir de determinado mes
#'
#' @param mes Mes inicial. Debe estar en formato "YYYY_MM" o "YYYYMM" o "YYYY-MM"
#' @param n El numero de meses que se quiere tener adelante
#' @param sep separador entre ano y mes. Por default esta "_", pero tambien puede ser "" o "-"
#'
#' @return Mes en formato "YYYY_MM" o "YYYYMM" o "YYYY-MM" que corresponde a \code{n} meses despues de \code{Mes}
#'
#' @examples
#' Mes_adelante_n(Mes = "2018_12", n = 5)
#' 
mes_adelante_n <- function(mes, n, sep = "_"){
  
  if (sep == "") { mes <- paste0(substr(mes, 1, 4), "_", as.character(substr(mes, 5, 6)))}
  
  if(n <= 0) { stop("Esta funcion no es valida para n <= 0. El resultado sera el mismo mes para n = 0. Si quiere meses atras, utilice la funcion mes_atras_n")}
  
  mes_adelante <- function(mes, sep){
    if (substr(mes, 6, 7) == "12"){
      if (sep == "-"){
        mes_adelante <- paste0(as.numeric(substr(mes, 1, 4)) + 1, sep, "01")
      } else {
        mes_adelante <- paste0(as.numeric(substr(mes, 1, 4)) + 1, "_", "01")
      }
    } else if (substr(mes, 6, 7) == "11" | substr(mes, 6, 7) == "10") {
      if (sep == "-"){
        mes_adelante <- paste0(substr(mes, 1, 4), sep, as.numeric(substr(mes, 6, 7)) + 1)
      } else {
        mes_adelante <- paste0(substr(mes, 1, 4), "_", as.numeric(substr(mes, 6, 7)) + 1)
      }
    } else if (substr(mes, 6, 7) == "09") {
      if (sep == "-"){
        mes_adelante <- paste0(substr(mes, 1, 4), sep, "10")
      } else {
        mes_adelante <- paste0(substr(mes, 1, 4), "_", "10")
      }
    } else {
      if (sep == "-"){
        mes_adelante <- paste0(substr(mes, 1, 4), sep, 0, as.numeric(substr(mes, 6, 7)) + 1)
      } else {
        mes_adelante <- paste0(substr(mes, 1, 4), "_", 0, as.numeric(substr(mes, 6, 7)) + 1)
      }
    }
    return(mes_adelante)
  }
  
  A <- mes
  for (i in 1:n){
    A <- mes_adelante(mes = A, sep = sep)
  }
  
  if (sep == "") { A <- paste0(substr(A, 1, 4), as.character(substr(A, 6, 7)))}
  
  return(A)
}


#' Dado un mes y cuantos meses se quieren atras, generar el mes
#' 
#' meses atras YYYY_MM o YYYYMM o YYYY-MM
#' Esta funcion genera en formato "YYYY_MM" o "YYYYMM" o "YYYY-MM" los meses atras que se quiera a partir de determinado mes
#'
#' @param mes mes inicial. Debe estar en formato "YYYY_MM" o "YYYYMM" o "YYYY-MM"
#' @param n El numero de meses que se quiere tener atras
#' @param sep separador entre ano y mes. Por default esta "_", pero tambien puede ser "" o "-
#'
#' @return mes en formato "YYYY_MM" o "YYYYMM" O "YYYY-MM" que corresponde a \code{n} meses antes de \code{mes}
#'
#' @examples
#' mes_atras_n(mes = "2018_12", n = 5)
#' 
mes_atras_n <- function(mes, n, sep = "_"){
  
  if(n <= 0) { stop("Esta funcion no es valida para n <= 0. El resultado sera el mismo mes para n = 0. Si quiere meses adelante, utilice la funcion mes_adelante_n")}
  
  if (sep == "") { mes <- paste0(substr(mes, 1, 4), "_", as.character(substr(mes, 5, 6)))}
  
  congruencia <- (-1*n)%%12
  if(congruencia == 0){ congruencia <- 12}
  if (sep == "-"){
    mes_auxiliar <- mes_adelante_n(mes, congruencia, sep = sep)
  } else {
    mes_auxiliar <- mes_adelante_n(mes, congruencia)
  }
  anos_atras <- floor(n/12) + 1
  anos_resultado <- as.numeric(substr(mes_auxiliar, 1, 4)) - anos_atras
  
  if (sep == "-"){
    resultado <- paste0(anos_resultado, sep, substr(mes_auxiliar, 6, 7))
  } else {
    resultado <- paste0(anos_resultado, "_", substr(mes_auxiliar, 6, 7))
  }
  
  if (sep == "") { resultado <- paste0(substr(resultado, 1, 4), as.character(substr(resultado, 6, 7)))}
  
  return(resultado)
}


#' Funcion que hace lo contrario a %in%. Dice si el valor esta faltando o no en el vector de busqueda.
#'
#' @param x vector con valores de cualquier tipo que van a buscarse en \code{y}
#' @param y vector con valores de cualquier tipo donde se buscan los valores de \code{x}
#'
#' @return vector logico donde para cada valor de \code{x} se dice si esta faltando o no en \code{y}. OJO: FALTANDO PORQUE ES LA NEGACION DE %in%
#'
#' @examples
#' c(1,3,11) %!in% 1:10
#' 
'%!in%' <- function(x,y){
  !('%in%'(x,y))
}


#' Extrae todos los numeros de un string
#'
#' @param a string del que se quieren extraer los numeros
#'
#' @return un numeric con los numeros contenidos en a, pegados
#'
#' @examples
#' extraer_numeros("as222asda444")
#' 
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


#' Identifica cuales archivos no estan en la ruta final pero si en la inicial, por lo cual debe ejecutar para esos meses
#'
#' @param inicial_path ruta de archivos de insumos
#' @param final_path ruta de archivos de resultados
#'
#' @return vector con los meses que faltan en final_path
#'
#' @examples
#' comparador("data/canales", "data/staging")
#' 
comparador <- function(inicial_path, final_path) {
  
  #inicial
  files_inicial <- list.files(inicial_path)
  position_inicial <-
    as.vector(sapply(files_inicial, extraer_numeros))
  files_inicial <-
    data.frame(files = files_inicial , position = position_inicial)
  
  #final
  files_final <- list.files(final_path)
  position_final <-
    as.vector(sapply(files_final, extraer_numeros))
  files_final <-
    data.frame(files = files_final, position = position_final)
  
  #compare
  compare <-
    files_inicial$position[(which(files_inicial$position %!in% files_final$position))]
  
  compare <- as.character(compare)
  
  return(compare)
}



#' Extrae la moda de un vector
#'
#' @param v Vector que puede tener numeric o characters.
#'
#' @return Devuelve la moda del vector.
#'
#' @examples
#' mode(c(1,1,2,3))
#' 
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#' Define si el mes debe ser considerado en la informacion historica de cada llave - para el pronostico
#'
#' @param tabla usa la tabla de horarios, especificamente la variable indicadora, para establecer
#' desde que mes empieza cada llave. Poniendo 1s y 0s
#'
#' @return
#' @export
#'
#' @examples
dummy_considerar_fechas <- function(tabla){
  
  min_unos  <- tabla[indicadora == 1, .(min_unos = min(year_month)), by = llave]
  max_ceros <- tabla[indicadora == 0, .(max_ceros = max(year_month)), by = llave]
  
  dt <- merge(min_unos, max_ceros, by = "llave", all = T)
  
  dt[is.na(max_ceros), fecha_inicial := min_unos]
  dt[min_unos > max_ceros, fecha_inicial := min_unos]
  dt[min_unos < max_ceros, fecha_inicial := sapply(max_ceros, mes_adelante_n, n = 1, sep = "-")]
  
  dt <- dt[, .(llave, fecha_inicial)]
  
  tabla <- merge(tabla, dt, by = "llave", all.x = T, all.y = F)
  tabla[, considerar := as.numeric(year_month >= fecha_inicial)]
  tabla[, fecha_inicial := NULL]
  
  return(tabla)
  
}

#' De un string, eliminar los caracteres raros que luego se exportan mal a Excel
#'
#' @param string string del cual se van a eliminar los caracteres 
#'
#' @return el mismo string sin los caracteres extranos
#'
#' @examples
#' direccion_bien <- quitar_raros("Carrera 24 N° 26-01")
#' 
quitar_raros <- function(string) {
  
  aceptados <- c(".", " ", "", ",", "-", "_", "(", ")", "#", "$", "%", "/", "=", 
                 "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
                 "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
                 "1", "2", "3", "4", "5", "6", "7", "8", "9", "0")
  
  caracteres <- unlist(strsplit(string, ""))
  caracteres <- caracteres[caracteres %in% aceptados]
  
  return(paste(caracteres, collapse = ""))
}
