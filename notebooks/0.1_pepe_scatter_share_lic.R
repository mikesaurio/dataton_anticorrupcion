rm(list=ls())
data<-fread('/Users/pepe_opi/Downloads/contrataciones_ganadores.csv')

head(data)

head(data$award_supplier_name)
head(data$award_amount)

data$metodo<-ifelse(data$tender_procurement_method=='open', 'Licitaciones', 'Adjudicacion')

data_res<-do.call(cbind.data.frame,
                  aggregate(data=data,
                            award_amount~award_supplier_name+metodo,
                            function(x) c(sum(x), length(x))))
data_res<-data_res[data_res$award_amount.1>=1000000,]

library(ggplot2)

head(data_res)
data_res_porcs<-dcast(data_res, award_supplier_name~metodo, value.var='award_amount.2', fill=0)
data_res_montos<-dcast(data_res, award_supplier_name~metodo, value.var='award_amount.1', fill=0)

data_res_montos$totales<-data_res_montos$Adjudicacion+data_res_montos$Licitaciones

data_res_porcs$Adjudicacion_share<-data_res_porcs$Adjudicacion/(data_res_porcs$Adjudicacion+data_res_porcs$Licitaciones)
data_res_porcs$Licitaciones_share<-data_res_porcs$Licitaciones/(data_res_porcs$Adjudicacion+data_res_porcs$Licitaciones)

data_tot<-merge(data_res_montos[, c('award_supplier_name', 'totales')],
                data_res_porcs, by='award_supplier_name')

plot<-ggplot(data=data_tot, aes(x=Adjudicacion_share, y=Licitaciones_share, size=totales, color=totales,
                                texto=award_supplier_name, alpha=totales))+
  geom_point()+
  theme_classic()
plot<-plotly::ggplotly(plot, tooltip=c("texto"))
htmlwidgets::saveWidget(plot,file =  paste0('~/Documents/adjudicacion.html'))

  

head(data_res)