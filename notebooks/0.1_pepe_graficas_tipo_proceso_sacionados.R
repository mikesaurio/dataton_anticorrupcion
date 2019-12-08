rm(list=ls())
library(data.table)
data<-fread("/Users/pepe_opi/Downloads/contrataciones_ganadores_pepe.csv")


head(data)

data$tender_submission_method<-gsub("\\[|\\]|'",'', data$tender_submission_method)


data_res<-aggregate(data=data,award_amount~award_supplier_name+tender_submission_method, sum)

#data_res<-dcast(data_res, award_amount~tender_submission_method, fill=0)


library(ggplot2)

data_res<-data_res[data_res$award_amount>1000000,]


plot<-ggplot(data=data_res,
             aes(x=tender_submission_method,
                 y=log(award_amount),
                 texto=award_supplier_name, size=award_amount))+
  geom_point()
  


plot<-plotly::ggplotly(plot, tooltip=c("texto"))  
htmlwidgets::saveWidget(plot,file =  paste0('~/Documents/type_plot.html'))


joselo<-fread("/Users/pepe_opi/Downloads/sancionados.csv")

joselo$award_supplier_name<-factor(joselo$award_supplier_name, levels=joselo$award_supplier_name[order(joselo$porcentaje_sancionado, decreasing = T)])
plot<-ggplot(data=joselo, aes(x=award_supplier_name, y=porcentaje_sancionado))+theme_classic()+
         geom_bar(stat = 'identity')+theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave(plot = plot,filename = 'Documents/GIT/dataton_anticorrupcion/figures/top_porcentajes.png', device = 'png')


joselo<-fread("/Users/pepe_opi/Downloads/dep_sancion.csv")

names(joselo)<-c('v1', 'nombre', 'monto_multa')
joselo$nombre<-factor(joselo$nombre, levels=joselo$nombre[order(joselo$monto_multa, decreasing = T)])

plot<-ggplot(data=joselo, aes(x=nombre, y=monto_multa))+theme_classic()+
  geom_bar(stat = 'identity')+theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave(plot = plot,filename = 'Documents/GIT/dataton_anticorrupcion/figures/top_monto_sancion', device = 'png')

