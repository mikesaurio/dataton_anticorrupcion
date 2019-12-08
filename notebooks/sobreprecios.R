rm(list=ls())
library(data.table)
library(ggplot2)

data<-fread('dataton_anticorrupcion/data/prueba_vis.csv')
table(data$raras)
library(ggrepel)
library(ggbeeswarm)

data<-aggregate(data=data, ocid~raras+award_supplier_name, length)

data<-dcast(data, award_supplier_name~raras, fill=0)

data$total<-data$normal+data$sobreprecio+data$subprecio

data$normal<-data$normal/data$total
data$sobreprecio<-data$sobreprecio/data$total
data$subprecio<-data$subprecio/data$total
summary(data$total)
data<-data[data$total>10,]

data<-data[data$sobreprecio>0.5,]

data$award_supplier_name<-factor(data$award_supplier_name, levels = data$award_supplier_name[order(data$sobreprecio)])
plot<-ggplot(data=data, aes(x=award_supplier_name, y=round(sobreprecio*100, 1)))+
  geom_bar(stat='identity')+
  theme_bw()+
  xlab('')+ylab('% de contratos ganados con sobreprecio')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
system('mkdir ~/Documents/GIT/dataton_anticorrupcion/figures/')
ggsave(plot = plot,filename = 'Documents/GIT/dataton_anticorrupcion/figures/top_sobreprecio.png', device = 'png')

head(data[order(data$sobreprecio,decreasing = T),])




data$monto_miles<-paste0('mDP: ',
       as.character(round(data$contract_item_pesos/1000,2) ))
#system('pwd')
#system('mkdir dataton_anticorrupcion/data/graficas_prueba')

head(data)
data[,'award_supplier_name']


data[, c(award_supplier_name, '']

data$rarr







head(data$)


for( key_i in unique(data$key)){
  plot<-ggplot(data=data[data$key==key_i,], aes(x=key,
                              y=contract_item_pesos_log,
                              texto_1 =binomios,
                              texto_2 =monto_miles))+
                 geom_quasirandom(aes(color=raras))+theme_void()
  plot<-plotly::ggplotly(plot, tooltip=c("texto_1", 'texto_2'))
  htmlwidgets::saveWidget(plot,file =  paste0('~/dataton_anticorrupcion/data/graficas_prueba/', key_i, '.html'))
}


