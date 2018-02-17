rm(list = ls())
library(httr)
library(rvest)

#Campos que solicita la pagina 
#"http://infosiap.siap.gob.mx:8080/agricola_siap_gobmx/ResumenDelegacion.do"
lista_global <- list(
					 id = NA,
					 anio = as.character(2004:2017),
					 nivel = as.character(1),
					 delegacion = as.character(1:32),
					 mes = as.character(1:12),
					 moda = as.character(3),
					 ciclo = as.character(2),
					 descarga = T
)

#Número de renglones
(n_registros <- prod(lengths(lista_global)))

#Creamos todas las combinaciones entre los elementos de la lista
tabla_parametros_formulario <- expand.grid(lista_global, stringsAsFactors = F,  KEEP.OUT.ATTRS= F)
#Creamos un id para futuras referencias
tabla_parametros_formulario$id <- as.character(1:n_registros)

#Procesamos sólo las primeras 10 filas
tabla_parametros_formulario <- tabla_parametros_formulario[1:10,]

for(i_renglon in 1:nrow(tabla_parametros_formulario)){	
	#Creamos una bandera que nos indique la correcta descarga.
	se_descargo <- F
	parametros_formulario <- tabla_parametros_formulario[i_renglon, ]
	#hacemos una petición POST a la página http://infosiap.siap.gob.mx:8080/agricola_siap_gobmx/ResumenDelegacion.do
	res <- POST("http://infosiap.siap.gob.mx:8080/agricola_siap_gobmx/ResumenDelegacion.do", body = parametros_formulario, encode = "form", verbose())
	#200 Respuesta estándar para peticiones correctas.
	if(status_code(res) == 200){

		pagina_html <- read_html(res)
		datos_tabla_html <- pagina_html %>%
							html_nodes(xpath = '//*[@id="0"]/table[2]')
		if(length(datos_tabla_html) > 0){
			#Retorna una lista con un data.frame.
			tabla_datos <- html_table(datos_tabla_html, trim = T, fill = T)[[1]]

			#Creamos una tabla que usaremos para crear el csv de esta consulta, que contiene
			#los datos que nos interesa
			tabla_final <- as.data.frame(matrix(, 10 + nrow(tabla_datos) + 1, 6))

			#Título
			tabla_final[1,1] <- "Avance de siembras y cosechas"
			subtitulos <- html_nodes(pagina_html,css = '.textoTablaSubTitulo2')
			#Transformamos a cada nodo html a un formato de texto plano.
			subtitulos <- sapply(subtitulos, html_text,  trim = T)
			#Si se consulta un año t, en los metadatos de la consulta se muestra el año (t-1)
			anio_salida_consulta <- as.numeric(parametros_formulario$anio) - 1
			#subtitulos en la posición [1] tiene un texto como el siguiente:
			#"PRIMAVERA-VERANO    2005RIEGO+TEMPORAL", por eso separamos el string por el año (t-1)
			subtitulo <- trimws(strsplit(subtitulos[1], anio_salida_consulta)[[1]])
			tabla_final[3,1] <- subtitulo[1]
			tabla_final[4,1] <- anio_salida_consulta			
			tabla_final[5,1] <- subtitulo[2]
			tabla_final[10,1] <- subtitulos[2]
			tabla_final[10,2] <- subtitulos[2]

			pie_tabla <- html_text(html_nodes(pagina_html,'.textoTablaPie'))
			tabla_final[11:(nrow(tabla_final) - 1),] <- tabla_datos
			tabla_final[nrow(tabla_final), 1] <- pie_tabla

			tabla_final <- apply(tabla_final, c(1,2), gsub, pattern = "\n", replacement = "")

			write.table(tabla_final, file = paste0("outputs/",parametros_formulario$id, ".csv"), sep = ",", row.names = F, col.names = F
				, na = "")
			assign(paste0("tabla_",parametros_formulario$id), tabla_final)
			se_descargo = T
			#Reemplazamos los índices con el nombre del campo seleccionado en el formulario
			tabla_parametros_formulario[i_renglon, 1:(ncol(tabla_parametros_formulario)-1)] = c(
				i_renglon,#id
				parametros_formulario$anio, #anio
				"estatal", #nivel sólo consultamos estatal
				tabla_final[10,2], #delegacion
				parametros_formulario$mes, #mes
				subtitulo[2], #moda
				subtitulo[1]#ciclo
			)
		}

	}
	tabla_parametros_formulario$descarga[i_renglon] = se_descargo
}

#La tabla_parametros_formulario nos servirá para consultar que archivos se descargarn y sus 
#respectivos criterios de búsqueda
save(list = c("tabla_parametros_formulario",
			paste0("tabla_", tabla_parametros_formulario$id[tabla_parametros_formulario$descarga])),
	file = "outputs/descarga.RData")

