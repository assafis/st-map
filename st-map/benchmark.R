#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('psd_functions_shiny.R')

if(!exists("perimeter.be.gadash.1")){ 
  perimeter.be.gadash.1 <- sf::st_read(paste0(dataDir,"perimeter_gadash_1.shp"))
  perimeter.df.be.gadash.1<<-as(perimeter.be.gadash.1, 'Spatial')
}

if(!exists("perimeter.be.hohova.n")){ 
  perimeter.be.hohova.n <- sf::st_read(paste0(dataDir,"perimeter_hohova_n.shp"))
  perimeter.df.be.hohova.n<<-as(perimeter.be.hohova.n, 'Spatial')
}

if(!exists("perimeter.sasa")){ 
  perimeter.sasa <- sf::st_read(paste0(dataDir,"perimeter_sasa.shp"))
  perimeter.df.sasa<<-as(perimeter.sasa, 'Spatial')
}

benchmark.params <- list(
  #dataset=c("beeri_gadash_9_24","ny_3_25_I","ny_3_25_II","beeri_hohova_n_12_24","sasa_4_23"),
  dataset=c("sasa_4_23"),
  sid=c(123,456,567),  #c(123,234,345,456,567,678,789,890,901,012), 
  depth=c("A","B"),
  sample=c(1,2,3),
  cs.limit=c(2,3,4,5,6,7,8),
  response=c("D"),  # "texture",
  var.factor=c(10,100,200), #c(10,20,50,100,200,300,600),
  ntrees=c(20,50),
  ncv=c(10),
  v.set=c(1,2,3) #c(1,2,3,4)
)

samples.actual <- data.frame(
  dataset=c("beeri_gadash_9_24","beeri_gadash_9_24","ny_3_25_I","ny_3_25_II","beeri_hohova_n_12_24","sasa_4_23","sasa_4_23","sasa_4_23","sasa_4_23","sasa_4_23"),
  depth=c("A","B","B","B","A","A","B","A","B","A"),
  sample=c(1,1,1,1,1,1,1,2,2,3)
)

# for multiple samples (13,23,12)


benchmark.df<-data.frame(matrix(nrow=0,ncol=15))
names(benchmark.df)<-c("data.set","depth","sample","cs.limit","features","response","sid","n.txt.class","var.factor","ntrees","ncv","acc.tr","kappa.tr","acc.v","validation.set")
features=c("ECaV_1m","ECaV_05m","ECaH_1m","ECaH_05m")

i<-1
tt=0
start.epoch<-Sys.time()

for(ds in benchmark.params$dataset){
  for(dp in benchmark.params$depth){
    for(smp in benchmark.params$sample){
      for(csl in benchmark.params$cs.limit){
        for(rsp in benchmark.params$response){
          for(vf in benchmark.params$var.factor){
            for(nt in benchmark.params$ntrees){
              for(nc in benchmark.params$ncv){
                for(sid in benchmark.params$sid){
                  for(vs in benchmark.params$v.set){
                    
                    samp.valid=F
                    for(s in 1:nrow(samples.actual)){
                      if(ds %in% samples.actual[s,"dataset"] & dp %in% samples.actual[s,"depth"] & smp %in% samples.actual[s,"sample"]){samp.valid=T}
                    }
                    
                    if(dp=="B" & vs==3){samp.valid=F} # depth B has no sample 3 (QCvar)
                    
                    if(samp.valid==T){ # Only valid depths and samples
                      tt=tt+1
                      if(rsp=="D"){D2T=T} else {D2T=F}
                      
                      start_time <- Sys.time()
                      df.bm<-gModels(toPlot="benchmark",data.set=ds,depth=dp,sample=smp,cs.limit=csl,
                                     miv=features,mdv=rsp,var.factor=vf,ntree=nt,ncv=nc,sid=sid,v.set=vs,D2text=D2T)
                      end_time <- Sys.time()
                      time.diff <- end_time - start_time
                      
                      if(!is.null(df.bm)){
                        df.bm$runtime<-round(as.numeric(time.diff),3)
                        benchmark.df<-rbind(benchmark.df,df.bm)
                        print(paste0("- Done: #",i," (",df.bm$runtime," sec) -"))
                      } else {
                        print(paste0("- No data: #",i," (",df.bm$runtime," sec) -"))
                      }
                    }
                    
                    i<-i+1
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}



end.epoch<-Sys.time()
epoch.time<-end.epoch-start.epoch

print(paste("- - - Benchmark overall runtime:"))
print(epoch.time)

saveRDS(benchmark.df,paste0(dataDir,"benchmark/benchmark",format(as.POSIXct(Sys.time()), "%Y%m%d_%H%M%S"),".RData"))

###


benchmark.df<-readRDS(paste0(dataDir,"benchmark/benchmark_sasa_D_texture.RData"))

benchmark.df<-benchmark.df[benchmark.df$sample!=benchmark.df$validation.set,]

# bm.df<-benchmark.df %>% group_by(cs.limit,response) %>%
#       dplyr::summarise(acc.v = max(acc.v,na.rm=T), .groups = 'drop_last')

benchmark.df<-benchmark.df[benchmark.df$sample=="QC",]
benchmark.df<-benchmark.df[benchmark.df$validation.set!="QC",]


# benchmark.df <- benchmark.df[benchmark.df$data.set=="beeri_gadash_9_24"&benchmark.df$sample==1 |
#                benchmark.df$data.set=="ny_3_25_I"&benchmark.df$sample==1 &benchmark.df$depth=="B" |
#                benchmark.df$data.set=="ny_3_25_II"&benchmark.df$sample==1 &benchmark.df$depth=="B" |
#                benchmark.df$data.set=="beeri_hohova_n_12_24"&benchmark.df$sample==1 &benchmark.df$depth=="A" |
#                benchmark.df$data.set=="sasa_4_23",]

benchmark.df$uid<-as.factor(paste0(substring(benchmark.df$data.set,1,3),str_sub(benchmark.df$data.set,-5,-1),"_",benchmark.df$depth,"_",benchmark.df$sample))
#unique(benchmark.df$uid)

#benchmark.df["acc.tr","acc.v"]<-as.numeric(benchmark.df["acc.tr","acc.v"])
# benchmark.df["acc.tr","acc.v"]<-lapply(benchmark.df["acc.tr","acc.v"], function(x) {as.numeric(as.character(x))})
# benchmark.df %>% group_by(ntrees) %>%
#      dplyr::summarise(acc.tr = mean(acc.tr,na.rm=T),acc.v = mean(acc.v,na.rm=T), .groups = 'drop_last')



#psd.df$id<-as.factor(psd.df$id)

theme.box<-theme(axis.text=element_text(size=12),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
                 panel.grid.minor = element_blank(),panel.grid.major.y = element_line(linewidth=0.1),panel.border = element_blank(),
                 axis.line=element_line(linewidth=0.1),axis.ticks.x = element_blank()) 
pal <- c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6","#7023ed")[c(1,4,7)]


# benchmark.df %>%
#   dplyr::select(c(data.set,depth,sample,acc.v))%>%
#   dplyr::mutate(across(!acc.v, as.factor))%>%
#   #gather(key="data.set", value="acc.v") %>%
#   pivot_longer(c(data.set, depth, sample), names_to = "key", values_to = "value")%>%


benchmark.df <- benchmark.df %>%
  dplyr::mutate(across(!c(acc.v,acc.tr,runtime), as.factor))

#sapply(benchmark.df,class)
i.var<-"sample"
#m.var<-"acc.v"
ggplot(benchmark.df, aes(x=uid,y=acc.v),fill="bisque1") + theme_bw() + theme.box +
  #geom_violin(aes(x=as.factor(data.set),y=acc.v),fill="grey80",adjust = 0.7,draw_quantiles = c(0.5),scale="count") +
  geom_boxplot(width=0.38,  outlier.size = 0.2,aes(fill=factor(depth)),alpha=0.5) +
  #geom_point(size=0.2) +
  #stat_summary(fun=mean, geom="point", size=0.2, color="red") +
  theme(axis.text.x = element_text(angle=0, vjust=0.5, size=12),legend.position = "left",
        legend.title = element_text(size=13), legend.text = element_text(size=12),
        axis.text = element_text(size=12),axis.title = element_text(size=13,vjust=1), plot.title = element_text(size=15),
        panel.grid.major = element_line(linewidth=0.2,colour = "grey20"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey40"),
        panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
  labs(title=paste0("Validation accuracy by ",i.var),subtitle = "",x=i.var)+
  scale_fill_manual(name="Depth",values = pal)

#+ #,caption = "N=",N) +
unique(data.frame(a=benchmark.df$data.set,b=benchmark.df$depth))
#scale_y_continuous(data.set#scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0))




##
###
#### correlation benchmark
datasets=c("sasa_4_23")
depths=c("A","B")
samples=c(1,2,3)
items.x=c("clay","silt","sand","D")
items.y=c("ECaV.15","ECaV.075","ECaH.1","ECaH.05")
cs.limits=c(2:8)
  
corr.df<-data.frame(matrix(nrow=0,ncol=8))
names(corr.df)<-c("ECaV.15","ECaV.075","ECaH.1","ECaH.05","sample","depth","cs.limit")

    for(ds in datasets){
      for(dp in depths){
        for(smp in samples){
          if(!(smp==3&dp=="B")){
            for(cs in cs.limits){
              psd.df<-loadPSDCor("mean",ds,dp,smp)
              psd.df<-psd.df[1:(nrow(psd.df)-3),]
              if(cs!=2){
                psd.df$clay<-psd.df[[paste0("clay",cs)]]
                psd.df$silt<-psd.df[[paste0("silt",cs)]]
              }
              
              new.cor<-as.data.frame(cor(psd.df[items.x], psd.df[items.y]))
              new.cor$var.x<-rownames(new.cor)
              new.cor$sample<-smp
              new.cor$depth<-dp
              new.cor$cs.limit<-cs
              corr.df<-rbind(corr.df,new.cor)
            }
          }
        }
      }
    }
  

saveRDS(corr.df,paste0(dataDir,"benchmark/corr_benchmark.RData"))





####
##


cor(psd.df[[items.x[1]]],psd.df[[items.y[1]]])
  ggcorrplot(p.mat)
  # Plot
  ggcorrplot(corr, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = labs, 
             method=mtd, 
             tl.cex=axsize,
             #colors = c("tomato2", "white", "springgreen3"), 
             outline.col = "white",
             colors = c("#6D9EC1", "white", "#E46726"),
             title="PSD Correlation matrix", 
             ggtheme=theme_bw) + labs(subtitle=paste0("n=",N)) +
    theme(plot.title = element_text(size = 13),plot.subtitle = element_text(size = 11)) 
