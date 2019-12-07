library(pdftools)
library(stringr)
library(tm)
library(tidytext)

rm(list=ls())

file_vector <- list.files(path = "data/pdfs/")
pdf_list <- file_vector[grepl(".pdf",file_vector)]

extrae_num_contrato<-function(texto){
  numero_contrato<-texto[grepl(pattern = 'contrato|Contrato',
                               texto) & grepl(pattern = 'numero|Numero|num',
                                              texto)]
  numero_contrato<-as.vector(str_split(numero_contrato, pattern = ' ', simplify = T))
  numero_contrato<-numero_contrato[grepl(x = numero_contrato, pattern = '[:0-9:]')]
  numero_contrato<-numero_contrato[grepl(x = numero_contrato, pattern = '[:A-Z:]')]
  
  numero_contrato<-numero_contrato[nchar(numero_contrato)>=20]
  numero_contrato<-gsub(':|,|;|\\.', '', numero_contrato)
  numero_contrato<-unique(numero_contrato)
  return(numero_contrato)
}
extrae_texto_pdfs<-function(pdf){
  texto<-pdf_text(paste0("data/pdfs/", pdf))
  #texto<-do.call(c, strsplit(texto, split = "\\b\\.\\b"))
  texto<-gsub('\n', ' ', texto)
  texto<-gsub("'",'', iconv(texto,from="UTF-8",to="ASCII//TRANSLIT"))
  return(texto)
}
extrae_monto_corrupto<-function(texto){
  texto<-do.call(c, strsplit(texto, split = "\\b\\.\\b"))
  monto<-texto[unique(c(#c(which(grepl('monto de ', texto)),
  which(grepl("se presume un probable", texto))))]
  #which(grepl("se presume un probable", texto))+1))]
  monto<-monto[grepl( '[:0-9:]', monto)]
  monto<-sub(".*un monto de *", "", monto)
  #monto<-as.vector(str_split(monto, ' ', simplify = T))
  monto<-monto[grepl( '[:0-9:]', monto)]
  monto<-monto[!grepl( '/100', monto)]
  monto<-sum(as.numeric(gsub(',', '', monto)))
  return(monto)
}
extrae_rfc<-function(texto){
  tmp<-as.vector(str_split(string = texto, pattern = ' ', simplify = T))
  tmp<-tmp[grepl('[:0-9:]', tmp)]
  tmp<-gsub('[[:punct:]]', '', tmp)
  tmp<-tmp[nchar(tmp) %in% c(12, 13)]
  tmp<-tmp[grepl('[:0-9:]', tmp)]
  rfc<-ifelse(length(tmp)==0, NA, tmp)
  return(rfc)
}
genera_matriz_palabras<-function(texto){
  texto<-gsub('[:0-9:]', '', texto)
  texto<-texto[texto!='']
  
  texto<-removeWords(texto, iconv(stopwords("spanish"),  from="UTF-8",to="ASCII//TRANSLIT"))
  texto<-paste0(gsub("\\s+", " ", str_trim(texto)), collapse = ' ')
  
  
  texto_df<-as.data.frame(texto, stringsAsFactors = F)
  trigrams<-unnest_tokens(tbl = texto_df,token ="ngrams", input =texto, output=grams,n=3)
  bygrams<-unnest_tokens(tbl = texto_df,token ="ngrams", input =texto, output=grams,n=2)
  words<-unnest_tokens(tbl = texto_df,token ="words", input =texto, output=grams)
  
  
  trigrams<-as.data.frame(table(trigrams$grams))
  bygrams<-as.data.frame(table(bygrams$grams))
  words<-as.data.frame(table(words$grams))
  
  mat_i<-rbind.data.frame(trigrams, bygrams, words)
  
  mat_i$id_pdf<-pdf
  
  #trigrams<-trigrams[trigrams>1]
  #bygrams<-bygrams[bygrams>1]
  #words<-words[words>1]
  
  mat_i<-mat_i[mat_i$Freq>2,]
  return(mat_i)
}


mat<-list()
contratos<-data.frame(id_pdf=NULL,
                      rfc=NULL,
                      monto_corrupto=NULL,
                      stringsAsFactors = F)
for( pdf in pdf_list){
  print(pdf)
  texto<-extrae_texto_pdfs(pdf)
  #num_contrato<-extrae_num_contrato(texto)
  rfc<-extrae_rfc(texto)
  
  
  
  texto<-tolower(texto)
  mat_i<-genera_matriz_palabras(texto)
  mat_i<-as.data.frame(mat_i)
  
  
  
  mat<-rbind.data.frame(mat, mat_i)
  
  monto<-extrae_monto_corrupto(texto)

  texto<-gsub('[[:punct:]]', '',texto)
  texto<-gsub('  |   |    ', ' ',texto  )
  
  
  contrato_i<-data.frame(id_pdf=pdf,
                         rfc=rfc,
                         monto_corrupto=monto,
                         stringsAsFactors = F)
  contratos<-rbind.data.frame(contratos, contrato_i)
}

write.csv(mat, 'data/conteos_grams.csv', row.names = F)



write.csv(contratos, 'data/prueba_contratos.csv', row.names = F)


contratos_tot<-aggregate(data=contratos, monto_corrupto~rfc, sum)
texto
tail(contratos_tot[order(contratos_tot$monto_corrupto),], 10)








tm::stopwords("spanish")

