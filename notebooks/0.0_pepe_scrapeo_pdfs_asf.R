rm(list=ls())

library(data.table)
Sys.setenv(CC="clang",
           CXX="clang++",
             PKG_CFLAGS="-g -O2",
           PKG_CXXFLAGS="-g -O2 -stdlib=libc++")
setwd("~/Documents/GIT/asf/")
data<-fread("/Users/pepe_opi/Downloads/6397bb987f644df6890beb30f711343d.csv")


head(data)


table(data[data$`Estado de Trámite`=='','Texto Acción'])


names(data)<-gsub('__', '_', gsub(x = make.names(tolower(names(data))),pattern = '\\.',replacement =  '_'))
data<-data[data$estado_de_trámite=='Con seguimiento concluido',]
table(data$año_cuenta_pública)



links_unicos<-unique(paste0("http://www.asf.gob.mx/Trans/Informes/IR",
                            data$año_cuenta_pública,
                            "a/Documentos/Auditorias/",
                            data$año_cuenta_pública,'_',
       stringr::str_pad(data$número,
                        width = 4,side = 'left',
                        pad = '0'), '_a.pdf'))




data$url_pdf<-paste0("http://www.asf.gob.mx/Trans/Informes/IR",
       data$año_cuenta_pública,
       "a/Documentos/Auditorias/",
       data$año_cuenta_pública,'_',
       stringr::str_pad(data$número,
                        width = 4,side = 'left',
                        pad = '0'), '_a.pdf')


log_descargas<-data[, c('clave_acción', 'url_pdf')]

log_descargas$status<-'pendiente'

patron<-paste0(unique(paste0("http://www.asf.gob.mx/Trans/Informes/IR",
       data$año_cuenta_pública,"a/Documentos/Auditorias/"
       )), collapse = '|')

for( i in 1:length(links_unicos)){
  print(i)
  nombre_archivo<-gsub(patron, '', links_unicos[i])
  if (file.exists(paste0('./data/pdfs/',
             nombre_archivo))){
    outcome<-'descargado'
    next
  } else{
  
  outcome<-tryCatch(download.file(links_unicos[i],
                         destfile = paste0('./data/pdfs/',
                                           nombre_archivo)),
                    #
                    error=function(e) return('error'),
                    
                    silent=F )
  }
  if (outcome=='error'){
    log_descargas$status[log_descargas[,'url_pdf']==links_unicos[i]]<-'error'
  } else{
    log_descargas$status[log_descargas[,'url_pdf']==links_unicos[i]]<-'descargado'
  }
  
  write.csv(log_descargas, './data/pdfs/log_descargas.csv', row.names = F)
}



table(log_descargas$status)