

### Arreglo de formato de horas
#Para entender, ver el objeto diccionario_arreglo_horas

arreglo_formato_horas <- function(){
  
  diccionario_arreglo_horas <- 
    data.table(hora_tx = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20", "20-21", "21-22", "22-23", "23-24"), 
               hora_tx2 = c("00 a 01", "01 a 02", "02 a 03", "03 a 04", "04 a 05", "05 a 06", "06 a 07", "07 a 08", "08 a 09", "09 a 10", "10 a 11", "11 a 12", "12 a 13", "13 a 14", "14 a 15", "15 a 16", "16 a 17", "17 a 18", "18 a 19", "19 a 20", "20 a 21", "21 a 22", "22 a 23", "23 a 24"))
  
  #historico
  horas_hist <- fread(os.path.join(ruta_tablas_bi, "t_ocup_horas.csv"))
  variables <- names(horas_hist)
  horas_hist <- merge(horas_hist, diccionario_arreglo_horas, by = "hora_tx", all.x = T, all.y = F)
  horas_hist[, hora_tx := NULL]
  setnames(horas_hist, "hora_tx2", "hora_tx")
  horas_hist[, hora_tx := factor(hora_tx, levels = diccionario_arreglo_horas$hora_tx2)]
  horas_hist <- horas_hist[, mget(variables)]
  horas_hist  <- horas_hist[order(mes, llave, hora_tx)]
  fwrite(horas_hist, os.path.join(ruta_tablas_bi, "t_ocup_horas.csv"))
  
  #pronostico
  horas_pron <- fread(os.path.join(ruta_tablas_bi, "pronostico_hora.csv"))
  variables <- names(horas_pron)
  horas_pron <- merge(horas_pron, diccionario_arreglo_horas, by = "hora_tx", all.x = T, all.y = F)
  horas_pron[, hora_tx := NULL]
  setnames(horas_pron, "hora_tx2", "hora_tx")
  horas_pron[, hora_tx := factor(hora_tx, levels = diccionario_arreglo_horas$hora_tx2)]
  horas_pron <- horas_pron[, mget(variables)]
  horas_pron <- horas_pron[order(llave, mes, hora_tx)]
  fwrite(horas_pron, os.path.join(ruta_tablas_bi, "pronostico_hora.csv"))
}





