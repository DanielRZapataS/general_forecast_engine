
#' Ejecuta la seccion del historico del modelo de demanda transaccional
#' Primera parte: utiliza la data de canales para generar las tablas de staging
#' Segunda parte: genera los resultados del analisis historico para el mes de corte
#' Tercera parte: genera las series temporales: numero de tx por llave y fecha
#' Cuarta parte: genera los insumos del pron. por llaves y por llaves y horas (numero de cajeros y mix tx)
#' Quinta parte: genera la metadata para el pronostico, mostrando que meses considerar para el pronostico para cada llave
#' Adicionalmente, exporta los horarios con los que han trabajado historicamente las oficinas
#'  
#' @return Se exportan en las respectivas carpetas, todos los objetos mencionados anteriormente
#'
#' @examples
#' historico()
#' 
historico <- function() {
  
  #Crear las tablas de staging
  staging()
  #Archivos de tasa de ocupacion, carpeta por mes
  resultados_mes_historico()
  #Agrupar los resultados del historico - ultimos 12 meses
  agrupar_results_hist()
  #Generar las series temporales necesarias para el pronostico
  series_temporales()
  #Generar los insumos del pronostico
  insumos_pronostico()
  #Generar la metadata para el pronostico
  metadata()

}
