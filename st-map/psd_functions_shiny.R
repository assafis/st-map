library(ggcorrplot)
library(ggplot2)
library(gstat)
library(dplyr)
library(plyr)
library(sf)
library(RColorBrewer)
library(e1071)
library(Hmisc)
library(tidyverse)
library(caTools)
library(randomForest)
library(datasets)
library(caret)
library(stringr)
library(fpc)
library(png)
library(entropy)
library(Ternary)
library(plotly)
library (raster)   
###



options(scipen=999)
#par(mfrow=c(1,1),bty = 'n',mar=c(5.1, 4.1, 4.1, 2.1))
pdf(NULL)

### GLOBALS
`%notin%` <<- Negate(`%in%`) # add not in operator

UTM36N<<-"+init=epsg:32636"
dataDir <<- "data/"

grd.ny<<-readRDS(paste0(dataDir,"ny_100d_1x1_grid.RData"))
grd10.ny<<-readRDS(paste0(dataDir,"ny_100d_10x10_grid.RData"))

grd.be.gadash<<-readRDS(paste0(dataDir,"beeri_gadash1_1x1_grid.RData"))
grd10.be.gadash<<-readRDS(paste0(dataDir,"beeri_gadash1_10x10_grid.RData"))

grd.be.hohova.n<<-readRDS(paste0(dataDir,"beeri_hn_1x1_grid.RData"))
grd10.be.hohova.n<<-readRDS(paste0(dataDir,"beeri_hn_10x10_grid.RData"))

grd.sasa<<-readRDS(paste0(dataDir,"sasa_65d_1x1_grid.RData"))
grd10.sasa<<-readRDS(paste0(dataDir,"sasa_65d_10x10_grid.RData"))

perimeter.ny<<-sf::st_read(paste0(dataDir,"perimeter_100_act_21.shp"))
perimeter.df.ny<<-as(perimeter.ny, 'Spatial')


perim.data <<- data.frame(dataset=c("beeri_gadash_9_24","ny_3_25_I","ny_3_25_II","beeri_hohova_n_12_24","sasa_4_23"),
                          p.file=c("perimeter.be.gadash.1","perimeter.ny","perimeter.ny","perimeter.be.hohova.n","perimeter.sasa"),
                          p.df.file=c("perimeter.df.be.gadash.1","perimeter.df.ny","perimeter.df.ny","perimeter.df.be.hohova.n","perimeter.df.sasa"),
                          grd.file=c("grd.be.gadash","grd.ny","grd.ny","grd.be.hohova.n","grd.sasa"),
                          grd.10.file=c("grd10.be.gadash","grd10.ny","grd10.ny","grd10.be.hohova.n","grd10.sasa"),
                          plot=c("Beeri Gadash","Neve Yaar","Neve Yaar","Beeri Hohova North","Sasa Hula")
)
####################################

#upData()   ### update the dataset from CSV


# Correlation
gCorrs <- function(xitem="clay",yitem="sand",cs.limit=2,data.set="beeri_gadash_9_24",depth="A",sample=1){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  
  psd.df<-loadPSDCor(data.type,data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  
  xlabel<-paste0(xitem)
  ylabel<-paste0(yitem)
  
  N=nrow(psd.df)
  
  cr=signif(cor(psd.df[[xitem]],psd.df[[yitem]]),2)
  #summary(lm(psd.df[[yitem]]~psd.df[[xitem]]))$r.squared # R2
  
  palcor<-rev(brewer.pal(9,"Set1")[c(3,5,7)])
  
  ggplot(psd.df, aes(.data[[xitem]],.data[[yitem]])) + 
    geom_smooth(method="lm", se=F, col="burlywood2",lwd=1.3) +
    geom_point(col="blue",size=1) + # aes_string(col=factor(nyvals[[mz.col]]))
    #geom_text(x = 2, y = 300, label = eq(psd.df$xitem,psd.df$yitem), parse = TRUE)
    theme_bw() +
    theme(plot.title = element_text(size = 13,face = "bold"),plot.subtitle = element_text(size = 11), plot.caption=element_text(size=10),axis.title=element_text(size=12)) +
    labs(
      subtitle=paste0("Pearson's R=",cr,"     n=",N,"      equation: ",eq(psd.df[[xitem]],psd.df[[yitem]])     ), 
      x=xlabel,
      y=ylabel,
      title=paste0("Correlation: ",xitem," ~ ",yitem))+
    #caption=paste0("n=",N)) +
    scale_colour_manual(values = palcor, name="sample")
}



gCgram <- function(pout="plot",items=c("sand","silt","clay","D"),labs=2,mtd="square",axsize=12, data.type="mean",cs.limit=2,data.set="ny_3_25_II",depth="all",sample=1){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  psd.df<-loadPSDCor(data.type,data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  N=nrow(psd.df)
  
  # Correlation matrix
  corr <- round(cor(psd.df[items]), 2)
  labs=as.numeric(labs)
  axsize=as.numeric(axsize)
  
  p.mat <- round(cor_pmat(psd.df[items]),3)
  
  if(pout=="p"){return(p.mat)} else {
  #ggcorrplot(psd.df[items])
  # Plot
  ggcorrplot(corr, hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = labs, 
             method=mtd, 
             #p.mat=p.mat,
             tl.cex=axsize,
             #colors = c("tomato2", "white", "springgreen3"), 
             outline.col = "white",
             colors = c("#6D9EC1", "white", "#E46726"),
             title="PSD Correlation matrix", 
             ggtheme=theme_bw) + labs(subtitle=paste0("n=",N)) +
    theme(plot.title = element_text(size = 13),plot.subtitle = element_text(size = 11)) 
  }
}






epCor <- function(eca.layer="ECaH.1",cs.limit=2,data.set="ny_3_25_II",depth="all",sample=1){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  
  N=nrow(psd.df)
  
  xlabel<-paste0(eca.layer)
  ylabel<-"%"
  
  cr.clay=signif(cor(psd.df[[eca.layer]],psd.df["clay"]),2)
  cr.silt=signif(cor(psd.df[[eca.layer]],psd.df["silt"]),2)
  cr.sand=signif(cor(psd.df[[eca.layer]],psd.df["sand"]),2)
  palcor<-rev(brewer.pal(9,"Set1")[c(3,5,7)])
  
  ggplot(psd.df) +
    geom_point(col="blue",size=1, aes(.data[[eca.layer]],clay)) + 
    geom_smooth(method="lm", se=F, col="royalblue2",lwd=1.1, aes(.data[[eca.layer]],clay)) +
    geom_point(col="orange",size=1, aes(.data[[eca.layer]],silt)) + 
    geom_smooth(method="lm", se=F, col="burlywood2",lwd=1.1, aes(.data[[eca.layer]],silt)) +
    geom_point(col="red",size=1, aes(.data[[eca.layer]],sand)) +
    geom_smooth(method="lm", se=F, col="tomato2",lwd=1.1, aes(.data[[eca.layer]],sand)) +
    geom_text(x = min(psd.df[[eca.layer]]), y = 52, label = paste0("R"), parse = TRUE,color="black",hjust=0,size=4,fontface="plain") +
    geom_text(x = min(psd.df[[eca.layer]]), y = 48, label = paste0("Clay: ",cr.clay), parse = TRUE,color="blue",hjust=0,size=4,fontface="plain") +
    geom_text(x = min(psd.df[[eca.layer]]), y = 44, label = paste0("Silt: ",cr.silt), parse = TRUE,color="orange",hjust=0,size=4,fontface="plain") +
    geom_text(x = min(psd.df[[eca.layer]]), y = 40, label = paste0("Sand: ",cr.sand), parse = TRUE,color="red",hjust=0,size=4,fontface="plain") +
    theme_bw() +
    theme(plot.title = element_text(size = 13,face = "bold"),plot.subtitle = element_text(size = 11), plot.caption=element_text(size=10),axis.title=element_text(size=12)) +
    labs(
      subtitle=paste0("Pearson's R"), 
      x=xlabel,
      y=ylabel,
      title=paste0("Correlation PSD ~ ",eca.layer),
      caption=paste0("n=",N)) +
    #scale_x_continuous(expand = c(0, 0),limits = c(0,(max(psd.df[[eca.layer]]*1.2))))+ 
    scale_y_continuous(expand = c(0, 0),limits = c(0,100))
}



eq <- function(x,y) {
  m <- lm(y ~ x)
  return(paste0("Y=", format(coef(m)[2], digits = 2),"*X+",format(coef(m)[1], digits = 2)))
}




HistPointPlot <- function(data.type="mean",I=101,cs.limit=2,data.set="ny_3_25_II",depth="all",sample=1){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  
  psd.df<-loadPSDCor(data.type,data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  sizes <- readRDS(paste0(dataDir,"ny_psd_sizes.RData"))
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  N=nrow(psd.df)
  n.points=nrow(psd.df)
  I<-as.numeric(I)  
  depth.label<-""
  if(depth=="A"){ depth.label<-"0-20 cm" }
  if(depth=="B"){ depth.label<-"20-40 cm" }
  
  for(p in 1:n.points){
    v = data.frame(prob=as.numeric(psd.df[p,-c(1:(which(names(psd.df)=="q.0.010")-1))]),
                   p.size=sizes[,"p.size"],
                   r.size=sizes[,"r.size"])
    
    if(I=="3"){ # proportions of sand, silt, clay
      df.condensed = sizes %>% 
        dplyr::mutate(binz=ifelse(v$p.size<50,ifelse(v$p.size<2,1,2),3))
      v.condensed <- v %>%
        dplyr::mutate(binz=ifelse(v$p.size<50,ifelse(v$p.size<2,1,2),3)) %>%
        group_by(binz) %>%
        dplyr::summarise(prob = sum(prob), .groups = 'drop_last')  # multiple summary columns 
    } else { # percentiles
      df.condensed = sizes %>% 
        dplyr::mutate(binz=ntile(sizes$p.size,as.numeric(I)))
      v.condensed <- v %>% 
        dplyr::mutate(binz=ceiling(I*seq(1,ncol(psd.df[-c(1:(which(names(psd.df)=="q.0.010")-1))]))/ncol(psd.df[-c(1:(which(names(psd.df)=="q.0.010")-1))]))) %>%
        group_by(binz) %>%
        dplyr::summarise(prob = sum(prob), .groups = 'drop_last')  # multiple summary columns
    }
    
    df.condensed = df.condensed %>%
      group_by(binz) %>%
      dplyr::summarise(r.size = sum(r.size), p.size = max(p.size), .groups = 'drop_last')  # multiple summary columns
    names(v.condensed)[2] <- paste0("p",p)
    df.condensed <- cbind(df.condensed,v.condensed[,2])
    
    
    
    theme.cor <- theme(legend.position = "left",
                       legend.title = element_text(size=12,lineheight = 0.4), legend.text = element_text(size=14),
                       legend.spacing.y = unit(0.1, 'cm'),
                       axis.text = element_text(size=12),axis.title = element_text(size=12,vjust=1), 
                       axis.text.x = element_text(angle = 60,hjust = 1),
                       plot.title = element_text(size=13),
                       panel.background = element_rect(fill = NA),
                       panel.grid.major = element_line(linewidth=0.1,colour = "grey30"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey50"),
                       panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())
    # Plot
    
    ### add bin width
    for(qt in 1:I){
      if(qt==1){
        bin.width=df.condensed[qt,"p.size"]
      } else {
        bin.width<-c(bin.width,df.condensed[qt,"p.size"] - df.condensed[qt-1,"p.size"] )
      }
    }
    
    # Calculate the future positions on the x axis of each bar (left border, central position, right border)
    df.condensed$right <- df.condensed$p.size #cumsum(df.condensed$p.size)
    df.condensed$left <- df.condensed$right - df.condensed$r.size 
    df.condensed[df.condensed$left==0 & !is.na(df.condensed$left),"left"]<-0.00001
    
    df.condensed[,4]<-df.condensed[,4]/100
    
    
    pl <-   # Plot
      ggplot(df.condensed[c(1:I),],aes_string("p.size",paste0("p",p))) + 
      geom_rect(aes_string(xmin = "left", xmax = "right", ymin="0", ymax = paste0("p",p)),color="grey50",fill="royalblue3",alpha=0.7,lwd=0.1) +
      theme.cor +
      labs(title=paste0(names(df.condensed[4])," | depth: ",depth.label," | ",I," bins"), y="",x="particle size [µm]") +
      scale_x_continuous(trans='log10', breaks=c(0.1,1,10,100,1000),expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0),limits = c(0,max(df.condensed[4])*1.2))
    assign(paste0("p",p),pl)
  }
  
  params <- as.list(parse(text=paste0("p",1:n.points)))
  params$cols<-ceiling(sqrt(n.points))
  #par(mfrow=c(1,1))
  do.call(multiplot,params)
}







# Triangle
psdTriangle <- function(to.plot="tri",color.by="texture",I=100,cs.limit=2,data.set="sasa_4_23",depth="A",sample=1){
  #library(ggtern)
  #if(!is.null(ggtern)){}
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  clay.col="clay"
  silt.col="silt"
  sand.col="sand"
  if(cs.limit!=2){
    clay.col=paste0("clay",cs.limit)
    silt.col=paste0("silt",cs.limit)
  }
  
  if(color.by=="sample"){ 
    psd.df$sample<-ifelse(psd.df$id<35,ifelse(psd.df$id<18,1,2),3)
  }
  if(color.by=="texture"){ 
    psd.df$texture<-getSoilTexture(data.frame(clay=psd.df[[clay.col]],silt=psd.df[[silt.col]],sand=psd.df[[sand.col]])) 
    palt <- suppressWarnings(brewer.pal(length(unique(psd.df$texture)),"Spectral"))
  } else {
    palcl<-brewer.pal(9,"Blues")
  }
  
  depth.label<-depth
  
  N=nrow(psd.df)
  if(to.plot=="hist")  {
    psd.df$texture<-getSoilTexture(data.frame(clay=psd.df[[clay.col]],silt=psd.df[[silt.col]],sand=psd.df[[sand.col]])) 
    
    p <- plot_ly(psd.df, x = as.formula(paste0('~', "texture")), type = 'histogram',marker=list(color="#162CD5"))%>%
      layout(title = paste(""),plot_bgcolor='#fff', xaxis = list( zerolinecolor = '#777f', zerolinewidth = 1, gridcolor = 'cccf'), 
             yaxis = list( zerolinecolor = '#777f', zerolinewidth = 1, gridcolor = 'cccf',showticklabels=F,title=""),
             bargap = 0.4,margin = list(l = 10,r = 10,b = 10,t = 40,pad = 0))
    # %>%
    #   add_markers(mode = "markers",text = ~texture,hovertemplate = '%{text}<extra></extra>',marker=list(fill="#160CA5",color="#160CA5",size=1),name = "") 
    return(p)  

  }
  if(to.plot=="tri")  {
    
    TernaryPlot(point = "up", atip = "", btip = "", ctip = "",lab.cex = 1.3,
                alab = "clay", blab = "silt", clab = "sand")

    data.points<-data.frame(clay=psd.df[[clay.col]],silt=psd.df[[silt.col]],sand=psd.df[[sand.col]],psd.df[[color.by]])
    if(color.by=="texture"){
      pal.v=as.numeric(as.factor(psd.df$texture))
    } else { 
      if(color.by=="sample"){
        pal.v=as.numeric(as.factor(psd.df$sample))
      } else { 
      cols = brewer.pal(5, "Spectral")
      palt = colorRampPalette(cols)
      data.points$order = findInterval(data.points[[4]], sort(data.points[[4]]))
      pal.v=palt(nrow(data.points))[data.points$order]
      }
    }
    

    AddToTernary(graphics::points, data.points[1:3], pch = 21, cex = 1, col = "gray20",bg=alpha(pal.v,0.9))
    
    title(paste("Soil texture triangle\n",data.set), cex.main = 1.2)
    
  } else { # correlation plot
    
    positions <- data.frame(
      x = c(0, 100, 100),
      y = c(100, 0, 100)
    )
    
    p <- 
      ggplot(psd.df,aes_string(sand.col,clay.col)) + 
      geom_abline(intercept = 100, slope = -1, color="grey30", size=0.2)+
      geom_point(aes_string(fill=color.by),col="grey50",size=3,shape=21) + 
      geom_polygon(data=positions, aes(x = x, y = y),fill = "white")+
      coord_equal() + 
      theme(legend.position="left",plot.title = element_text(hjust = 0,size=12),plot.subtitle = element_text(size=12,hjust = 0),
            plot.caption = element_text(size=12),axis.text=element_text(size=14),panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(linewidth=0.15,colour = "grey30"),panel.grid.minor = element_line(linewidth=0.15,colour = "grey30"),
            panel.border = element_rect(linewidth = 0.2, fill = NA)) + 
      labs( y="clay %", x="sand %", title="Sand vs Clay content",subtitle = paste0("N=",N)) + 
      scale_x_continuous(expand = c(0, 0),limits = c(0,100))+ scale_y_continuous(expand = c(0, 0),limits = c(0,100))
    if(color.by=="texture"){
      p <- p + scale_fill_manual(values = palt,name=color.by)
    } else {
      p <- p + scale_fill_gradientn(colours = palcl,name=color.by)
    }
    
    return(p) 
  }
  
}




#Get soil texture classification
getSoilTexture <- function(css.df){

  css.df[]<-lapply(css.df[], function(x) {as.numeric(x)})

  ## add column texture
  css.df<-mutate(css.df,
                 texture = case_when(
                   clay>=40 & silt<40 & sand<45 ~ "clay",
                   clay>=40 & clay<60 & silt>=40 & silt<60 & sand<20 ~ "silty-clay",
                   clay>=28 & clay<40 & silt>=40 & silt<73 & sand<20 ~ "silty-clay-loam",
                   clay<28 & silt>=50 & sand<50 ~ "silt-loam",
                   clay<13 & silt>=80 & sand<20 ~ "silt",
                   clay>=28 & clay<40 & sand>=20 & sand<45 ~ "clay-loam",
                   clay>=35 & clay<55 & sand>=45 & sand<65 ~ "sandy-clay",
                   clay>=20 & clay<35 & silt<28 & sand>=45 & sand<80 ~ "sandy-clay-loam",
                   clay>=8 & clay<28 & silt>=28 & silt<50 & sand<53 & sand>=22 ~ "loam",
                   clay<15 & silt<30 & sand>=70 & sand<(0.5*clay+85) ~ "loamy-sand",
                   clay<20 & silt<50 & sand>=43 & sand<clay+70 ~ "sandy-loam",
                   clay<10 & silt<15 & sand>=85 ~ "sand",
                   TRUE ~ "Unknown combination"
                 )
  )
  
  return(css.df$texture)
}


# Distribution
psdDistribution <- function(data.type="mean",cs.limit=2,data.set="sasa_4_23",depth="B",sample="all"){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  
  psd.df<-loadPSDCor(data.type,data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  
  
  cs.limit<-as.numeric(cs.limit)
  sizes <- readRDS(paste0(dataDir,"ny_psd_sizes.RData"))
  t.sizes <- as.data.frame(t(sizes)[2:3,])
  
  t.samples <- psd.df[1:(nrow(psd.df)-3),-c(1:(which(names(psd.df)=="q.0.010")-1))]
  names(t.sizes)<-names(t.samples)
  t.samples <- rbind(t.samples,t.sizes)
  p.name<-c(1:(nrow(psd.df)-3),"p.size","r.size")
  
  t.samples<-cbind(p.name,t.samples)
  tt.samples=as.data.frame(t(t.samples))
  names(tt.samples)<-tt.samples[1,]
  
  t.samples.long <- pivot_longer(tt.samples[-c(1),], cols=1:(ncol(tt.samples)-2))#  
  names(t.samples.long)<-c("p.size","r.size","name","value")
  t.samples.long[c(1,2,4)]<-apply(t.samples.long[c(1,2,4)],2,as.numeric)
  t.samples.long$name<-as.character(t.samples.long$name)
  t.samples.long <- t.samples.long[!is.na(t.samples.long$p.size),]
  
  t.samples.long<-t.samples.long[order(as.numeric(t.samples.long$name)),]
  t.samples.long$name <- factor(t.samples.long$name, levels = order(as.numeric(t.samples.long$name)))
  #t.samples.long$name<-as.factor(t.samples.long$name)
  
  # ### plot by samples
  # t.samples.long$sample<-"QC"
  # t.samples.long[as.numeric(t.samples.long$name)>17,"sample"]<-"Grid"
  # t.samples.long$sample<-as.factor(t.samples.long$sample)
  # ###
  
  palc <- colorRampPalette(rev(brewer.pal(9,"Paired")))((nrow(t.samples)-2))
  #palc<-brewer.pal(9,"Set1")[c(5,2)]
  val.max<-max(t.samples.long$value)
  ggplot(t.samples.long) + theme_bw()+ geom_line(aes(x=p.size,y=value,col=name,group=name)) +
    annotate("rect", xmin = 0, xmax = 2, ymin = 0, ymax = val.max*1.2, alpha = .03,fill="blue3") +
    annotate("rect", xmin = 2, xmax = 50, ymin = 0, ymax = val.max*1.2, alpha = .03,fill="red3") +
    annotate("rect", xmin = 50, xmax = 2000, ymin = 0, ymax = val.max*1.2, alpha = .03,fill="yellow3") +
    geom_line(aes(x=p.size,y=value,group=name,col=name)) +
    geom_vline(xintercept = cs.limit,col="grey30",alpha=0.5)+      
    geom_vline(xintercept = 50,col="grey30",alpha=0.5)+      
    geom_vline(xintercept = 2000,col="grey30",alpha=0.5)+    
    #geom_vline(xintercept = 2000,col="grey30",alpha=0.5)+      
    annotate("text", x = 0.1, y = val.max*0.95, label = "Clay") + 
    annotate("text", x = 10, y = val.max*0.95, label = "Silt") + 
    annotate("text", x = 400, y = val.max*0.95, label = "Sand") + 
    #geom_density(aes_string(x="p.size",group="name",fill="name"),position = "stack",alpha=0.5) +
    #geom_histogram(aes_string(x="value"),position = "stack",stat="bin",alpha=0.7) +
    coord_cartesian(clip = "off",ylim = c(0, val.max*1.2))+
    labs(title="Particle size distribution", subtitle=paste0(data.set),x="particle size [µm]",y="density") + 
    scale_colour_manual(name="Point",values=c(palc)) +  #,breaks=paste0("p",t.samples.long$name)
    scale_x_continuous(trans='log10',breaks=c(0.1,1,2,3,4,5,6,10,50,100,1000,2000),expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
}




loadECaStack <- function(data.set="sasa_4_23",variance=F){
  
  eca.dir <- switch (data.set,
                     "ny_3_25_I" = "ECa 100d 4-2022/",
                     "ny_3_25_II" = "ECa 100d 4-2022/",
                     "beeri_hohova_n_12_24" = "ECa Beeri Hohova_N 2024/",
                     "beeri_gadash_9_24" = "ECa Beeri 2024/",
                     "sasa_4_23" = "ECa Sasa 2023/",
                     stop("Invalid input")
  )
  
  if(eca.dir=="Invalid input"){
    return(NULL)
  }
  
  var.names<-c("ecv15","ecv075","ech1","ech05")
  i<-1
  eca.layers<-character(0)
  
  if(variance==T){
    eca.dir<-paste0(eca.dir,"variance/")
    file.names<-c("ECaV_1m_variance.tif","ECaV_05m_variance.tif","ECaH_1m_variance.tif","ECaV_05m_variance.tif")
  } else {
    file.names<-c("ECaV_1m.tif","ECaV_05m.tif","ECaH_1m.tif","ECaH_05m.tif")
  }
  
  for(f in file.names){
    if(file.exists(paste0(dataDir,eca.dir,f))){
      assign(var.names[i],raster(paste0(dataDir,eca.dir,f)))
      eca.layers<-c(eca.layers,var.names[i])
    }
    i<-i+1
  }
  
  eca.stack<-stack()
  for(el in eca.layers){
    if(exists(el)){
      eca.stack <- addLayer(eca.stack, eval(parse(text = el)))
    }
  }
  
  return(eca.stack)
}


ECaPlots <- function(sid="All",smooth=T,showdata="labels",data.set="ny_3_25_II"){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  
  perimeter.file<-perim.data[perim.data$dataset==data.set,"p.file"]
  perimeter <-  eval(parse(text=perimeter.file))
  
  eca.stack<-loadECaStack(data.set=data.set)

  zmin=floor(min(cellStats(eca.stack,min)))
  zmax=ceiling(max(cellStats(eca.stack,max)))
  pal.hr <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(40)
  plot(eca.stack,col=pal.hr,ylab="Northing",xlab="Easting",bty="n",addfun=dress.map(perimeter),axes=FALSE,cex.main=1.1,zlim=c(zmin,zmax))
  
}




dress.map <- function(perimeter) {
  narrow<-readPNG(paste0("www/na.png"), TRUE)
  grid(lty = 1,col="grey20",lw=0.2)
  rect(xleft=par("usr")[1]-1, ybottom=par("usr")[3]-1, 
       xright=par("usr")[2]+1,ytop=par("usr")[4]+1, 
       lwd=0.4, border="black", xpd=TRUE)
  plot(perimeter,add=T,lwd=1)
}





# Kriging  
psdKrige <- function(kitem="D",toPlot="var",resol=10,data.type="mean",cs.limit=2,data.set="ny_3_25_I",depth="all",sample=1){
  #kitem="clay"
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  
  psd.df<-loadPSDCor(data.type,data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  psd.df<-psd.df[,c("x","y",kitem)]
  
  N=nrow(psd.df)
  
  depth.label<-""
  if(depth=="A"){ depth.label<-"0-20 cm" }
  if(depth=="B"){ depth.label<-"20-40 cm" }
  
  sizes <- readRDS(paste0(dataDir,"ny_psd_sizes.RData"))
  
  # Histogram
  if(toPlot=="hist"){
    palh <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(100)
    hp <- hist(psd.df[[kitem]], breaks=seq(min(psd.df[[kitem]]), max(psd.df[[kitem]]), length.out = 101), plot=FALSE)
    plot(hp, col="steelblue3", main=paste0("points histogram: ",kitem),sub = paste0("\nN=",N),xlab=paste0(kitem) )
  } else {
    
    sp.df<-psd.df
    coordinates(sp.df) = ~x + y
    proj4string(sp.df) <- CRS(UTM36N)
    #sp.df <- sp.df[-zerodist(sp.df)[,1],] 
    
    #if(zerodist(sp.df))
    formla <- as.formula(paste(kitem, "1", sep=" ~ "))
    v.item = variogram(formla, sp.df)
    fit.sp<-fit.variogram(v.item, vgm(c("Exp", "Mat", "Sph")))
    if (any(fit.sp$range < 0.0)) {  # avoid negative values - fit Spehere model
      fit.sp<-fit.variogram(v.item, vgm(c("Sph")))
    }
    
    range=round(fit.sp$range[2],2)
    if(toPlot=="var"){ # variogram
      plot(v.item, fit.sp,xlab="Distance (m)",main=paste("variogram - ",kitem),sub=paste0("Range: ",range,"m,  N=",N))
      
    } else {
      
      if(resol==10){
        grid.file<-perim.data[perim.data$dataset==data.set,"grd.10.file"]
        grdk<-eval(parse(text=grid.file))
      } else { 
        grid.file<-perim.data[perim.data$dataset==data.set,"grd.file"]
        grdk<-eval(parse(text=grid.file))
      }
      
      kriged = krige(formla, sp.df, grdk, model = fit.sp)
      r.kriged <- raster(kriged)
      r.smooth <- focal(r.kriged, w=matrix(1,3,3), fun=median,na.rm=TRUE)
      palc <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(20)
      
      perimeter.file<-perim.data[perim.data$dataset==data.set,"p.file"]
      perimeter.df.file<-perim.data[perim.data$dataset==data.set,"p.df.file"]
      perimeter<-eval(parse(text=perimeter.file))
      perimeter.df<-eval(parse(text=perimeter.df.file))
      
      #extent(perimeter)
      #extent(r.kriged)
      cr.k <- raster::crop(r.smooth, extent(perimeter))
      fr.k <- rasterize(perimeter.df, cr.k)   
      lr.k <- raster::mask(x=cr.k, mask=fr.k)
      #par(mfrow=c(1,1),bty = 'n',mar=c(5.1, 4.1, 4.1, 2.1))
      plot(lr.k,col=palc,main=kitem,sub=paste0("n=",N),legend.args=list(text="%", side=3, font=1, line=0.5, cex=0.8),bty="l")
      #points(sample.points,pch=19,cex=0.3)
      grid(lty = 1,col="grey20",lw=0.1)
      
      writeRaster(lr.k, filename=paste0(dataDir,"krigResults/",data.set,"_",kitem,"_kriged"), format="GTiff",overwrite=TRUE)  
    }
  }
}


# benchmak
# gModels(mdv,miv,toPlot="benchmark",ntree=5, ncv=2,var.factor=100,smooth.w=0,D2text=F,mdv.smooth.w=0,cs.limit=2,data.set="sasa_4_23",depth="A",sample=1,points.model=T,sid=1234,v.set=0)

gModels <- function(mdv,miv,toPlot="model",ntree=20,ncv=10,var.factor=100,smooth.w=0,D2text=F,mdv.smooth.w=0,cs.limit=2,data.set="ny_3_25_I",depth="A",sample=1,points.model=F,sid=456,v.set=2){
  #miv=c("ECaH_05m","ECaH_1m","ECaV_05m","ECaV_1m")
  ## best regression
  #data.set="sasa_4_23";depth="A";cs.limit=3;mdv="D";sid=567;var.factor=100;ntree=50;ncv=10;D2text=T
  ## best classification
  #data.set="sasa_4_23";depth="B";cs.limit=8;mdv="texture";sid=456;var.factor=10;ntree=50;ncv=10;D2text=F
  ncv<-as.numeric(ncv);smooth.w<-as.numeric(smooth.w);ntree<-as.numeric(ntree);mdv.smooth.w<-as.numeric(mdv.smooth.w);orig.mdv<-mdv;
  if(D2text==T & mdv!="D"){return()}
  
  if(ntree==1){
    text(x = 0.5, y = 0.5, paste("Select number of trees...\n"), cex = 1.6, col = "black")
    return();
  }
  
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(nrow(psd.df)<1){return(NULL)}
  if(is.null(psd.df)){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  
  if(any(is.na(psd.df[[mdv]]))){
    psd.df<-psd.df[!is.na(psd.df[[mdv]]),]
  }
  
  psd.df[-c(1)] <- lapply(psd.df[-c(1)], function(x) {as.numeric(as.character(x)) })
  N=nrow(psd.df)
  
  if(mdv=="texture"){
    dataset<-getTextureCol(psd.df[c("x","y","clay","silt","sand")],var.factor,mdv.smooth.w,data.set=data.set)
    dataset<-dataset[c("x","y","texture")]
    #count(dataset$texture)
    dataset$texture<-as.factor(dataset$texture)
  } else {
    df.mdv.var<-krigeDF(psd.df[c("x","y","clay")],"clay",variance=T,data.set=data.set) # variance by clay
    df.mdv<-krigeDF(psd.df[c("x","y",mdv)],v.name=mdv,variance=F,smooth.w=mdv.smooth.w,idw.mode=T,data.set=data.set)
    #mdv.r <- rasterFromXYZ(as.data.frame(df.mdv)[, c("x", "y", mdv)],crs=UTM36N)
    dataset<-merge(df.mdv,df.mdv.var,by=c('x','y'))
    
    # reduce by variance
    dataset<-dataset[order(dataset[[paste0("clay.var")]]),]   
    dataset<-dataset[1:round(nrow(dataset)/(as.numeric(var.factor))),] # reduce by variance
    dataset.xy<-dataset
    dataset<-dataset[-c(4)]
  }
  ndp <- nrow(dataset)
  
  # independent variables
  eca <- loadECaStack(data.set,F)
  eca <- dropLayer(eca,which(names(eca) %notin% miv))
  eca.df <- data.frame(rasterToPoints(eca))
  
  dataset <- merge(dataset,eca.df,by=c("x","y"))
  
  # Splitting the dataset into the Training set and Test set
  set.seed(sid) #1234
  split = sample.split(dataset[[mdv]], SplitRatio = 3/4)
  training_set = subset(dataset[-c(1:2)], split == TRUE)
  test_set = subset(dataset[-c(1:2)], split == FALSE)
  
  # Feature Scaling
  # training_set
  # training_set = terra::scale(training_set)
  # test_set = terra::scale(test_set)
  
  #set.seed(1234)
  # regressor = randomForest(x = training_set[,-1],
  #                          y = training_set[,3],
  #                          ntree = as.numeric(ntree))
  # 
  # #y_pred = predict(regressor, dataset[,-c(1:3)])
  
  # if at least 2 classes
  if(length(unique(training_set[[mdv]]))>1){
    # Define the control
    trControl <- trainControl(method = "cv",number = ncv, search = "grid")
    #tuneGrid <- expand.grid(.mtry = c(1:3))
    
    forml <- as.formula(paste0(mdv," ~ ."))
    f.metric <- ifelse(mdv=="texture","Accuracy","RMSE")
    # Run the model
    tryCatch(
      expr = {
        rf_default <- caret::train(forml,
                                   data = training_set,
                                   method = "rf",
                                   metric = f.metric,
                                   ntree = ntree,
                                   trControl = trControl)
      },
      error = function(e){ 
        text(x = 0.5, y = 0.5, paste("Training set size is too small - decrease variance limit factor\n",e), cex = 1.6, col = "black")
        return();
      }
    )
    # Print the results
    #print(rf_default)
    
    prediction <- predict(rf_default, eca.df[-c(1:2)])
    
    # (a) Prediction error, RMSE
    # RMSE(prediction, dataset$D)
    # (b) R-square
    # R2(prediction, dataset$D)
    
  } else { # Single class
    prediction<-unique(training_set[[mdv]])
  }
  
  eca.df$y_pred <- prediction
  
  p.title<-paste("Prediction:", mdv)
  p.var<-"y_pred"
  
  if(mdv=="D" & D2text==T){ # transform: D --> texture class 
    eq.D.clay <- getEQ(x="D",y="clay",cs.limit=cs.limit,data.set=data.set,depth=depth)
    eq.D.sand <- getEQ(x="D",y="sand",cs.limit=cs.limit,data.set=data.set,depth=depth)
    eca.df$clay <- eq.D.clay$slope*eca.df$y_pred + eq.D.clay$intercept
    eca.df$sand <- eq.D.sand$slope*eca.df$y_pred + eq.D.sand$intercept
    eca.df$silt<-100-eca.df$sand-eca.df$clay
    eca.df$texture.c<-getSoilTexture(data.frame(clay=eca.df$clay,silt=eca.df$silt,sand=eca.df$sand)) # get texture class
    eca.df$y_pred<-as.numeric(as.factor(eca.df$texture.c)) #eca.df$texture<-
    
    txt.names<-character(length(unique(eca.df$y_pred)))
    for(txt in 1:length(unique(eca.df$y_pred))){
      txt.names[unique(data.frame(eca.df$texture,eca.df$y_pred))[txt,2]]<-as.character(unique(data.frame(eca.df$texture,eca.df$y_pred))[txt,1])
    }
    
    mdv<-"texture"
    p.title<-"Predicted soil texture (by D)"
    p.var<-"texture"
  }
  
  #pal <- c("#ea5545", "#b33dc6", "#27aeef", "#ef9b20", "#87bc45",  ,"#7023ed", "#ede17b", "#adef32" )
  #pal <- c("#4053d3","#ddb310","#b51d14","#00a25d","#00beff","#8033cd","#fb49f0","#cacaca","#87ac45")
  pal<-c("#6948a3","#00763C","#c88A3B","#00A6CC","#DDCA7E","#D26577","#7ECBEB","#8D2655","#2EA998","#979742","#4053d3","#629d25","#6e6929")
  
  # Training / Test Error - classification
  if(mdv=="texture" | D2text==T){
    #if(length(unique(eca.df$y_pred))>1){
    if(D2text==T | length(unique(training_set[[mdv]]))<2){
      accuracy<-"";kappa<-""
    } else { 
      accuracy<-paste0("Training accuracy = ",round(rf_default$results$Accuracy,2));kappa<-paste0(", Kappa=",round(rf_default$results$Kappa,2))
    }
    psd.df$texture<-getSoilTexture(data.frame(clay=psd.df$clay,silt=psd.df$silt,sand=psd.df$sand)) # get texture class
    if(D2text==F){eca.df$texture<-eca.df$y_pred}else{eca.df$texture<-eca.df$texture.c}
    
    # Validtion
    if(v.set==0){ # validate with own points
      validation.set<-merge(psd.df,eca.df,by=c("x","y"),all.x=T)[c("id","x","y","texture.x","texture.y","y_pred")]
    } else {
      if(data.set!="sasa_4_23"){
        validation.set<-merge(psd.df,eca.df,by=c("x","y"),all.x=T)[c("id","x","y","texture.x","texture.y","y_pred")]
      } else {
        ind.set<-loadPSDCor("mean",data.set,depth,sample="all")
        ind.set<-ind.set[1:(nrow(ind.set)-3),]
        samples<-c(1,2,3)
        sample1<-1:17
        sample2<-18:34
        sample3<-35:51
        ids<-numeric()
        if(v.set==4){ # 2 others
          for(vsmp in samples[samples!=sample]){
            ids<-c(ids,eval(parse(text=paste0("sample",vsmp))))
          }
          ind.set<-ind.set[ind.set$id%in%ids,]
        } else { 
          ids<-eval(parse(text=paste0("sample",v.set)))
          ind.set<-ind.set[ind.set$id%in%ids,]
        }
      }
      
      if(cs.limit!=2){
        ind.set$clay<-ind.set[[paste0("clay",cs.limit)]]
        ind.set$silt<-ind.set[[paste0("silt",cs.limit)]]
      }
      
      ind.set$texture<-getSoilTexture(data.frame(clay=ind.set$clay,silt=ind.set$silt,sand=ind.set$sand)) # get texture class
      validation.set<-merge(ind.set,eca.df,by=c("x","y"),all.x=T)[c("id","x","y","texture.x","texture.y","y_pred")]
    }
    validation.set$accurate<-as.numeric(validation.set$texture.x==validation.set$texture.y)
    v.acc<-paste0("\nValidation accuracy: ",sum(validation.set$accurate),"/",nrow(validation.set)," = ",round(sum(validation.set$accurate)/nrow(validation.set),2))
    #} else {
    if(length(unique(eca.df$y_pred))==1 & D2text==F){
      accuracy="Accuracy = 100% (one class)"
      kappa=""
    }
    #v.acc<-paste0("\nValidation accuracy: ",nrow(psd.df),"/",nrow(psd.df)," = 1")
    #}
    
    if(toPlot=="benchmark"){
      sample.names=c("QC","Grid","QCvar","2 others")
      df.bm <- data.frame(
        data.set=data.set,
        depth=depth,
        sample=sample, # sample.names[sample],
        cs.limit=cs.limit,
        features=paste(names(eca),collapse=","),
        response=orig.mdv,
        sid=sid,
        n.txt.class=length(unique(eca.df$texture)),
        var.factor=var.factor,
        ntrees=ntree,
        ncv=ncv,
        acc.tr=ifelse(length(unique(eca.df$y_pred))==1 | D2text==T,NA,round(rf_default$results$Accuracy,2)),
        kappa.tr=ifelse(length(unique(eca.df$y_pred))==1 | D2text==T,NA,round(rf_default$results$Kappa,2)),
        acc.v=round(sum(validation.set$accurate)/nrow(validation.set),2),
        validation.set= v.set #sample.names[v.set]
      )
      return(df.bm)
    }
    eca.df$texture_pred<-eca.df$y_pred
    eca.df$y_pred<-as.factor(eca.df$y_pred)
    #if(D2text==T){subt=""}else{
    subt<-paste0(accuracy, kappa, v.acc)
    #}
    if(mdv=="texture"&!D2text){
      #pal<-c("#4053d3","#ddb310","#b51d14","#00a25d","#00beff","#8033cd","#aa5533","#cacaca","#87ac45")
      #palc <- pal[c(6,4,3,7,2,1,5,8)] 
      #pal <- c("#4053d3","#ddb310","#b51d14","#00a25d","#00beff","#8033cd","#fb49f0","#cacaca","#87ac45")
      palc <- pal
    } else {
      palc <- pal[c(8,5,6,7,9,1,2,3)]
    }
     #[1:(length(unique(psd.df$texture))+1)]
    
    p.var<-"texture"
  } else {
    min.rmse<-which.min(rf_default$results$RMSE)
    subt<-paste0("RMSE=",round(rf_default$results$RMSE[min.rmse],2), ", R2=",round(min(rf_default$results$Rsquared[min.rmse],2)))
    palc <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(20)
  }
  #t.levels<-unique(eca.df$y_pred)
  
  # Smooth raster  
  if(smooth.w>0){
    if(mdv=="texture"){
      eca.df$texture<-as.numeric(as.factor(eca.df$y_pred))
      t.names<-character(length(unique(eca.df$y_pred)))
      for(txt in 1:length(unique(eca.df$y_pred))){
        t.names[unique(data.frame(eca.df$texture,eca.df$y_pred))[txt,1]]<-as.character(unique(data.frame(eca.df$texture,eca.df$y_pred))[txt,2])
      }
      #unique(data.frame(eca.df$texture,eca.df$y_pred))
    }
    
    perimeter.file<-perim.data[perim.data$dataset==data.set,"p.file"]
    perimeter.df.file<-perim.data[perim.data$dataset==data.set,"p.df.file"]
    perimeter<-eval(parse(text=perimeter.file))
    perimeter.df<-eval(parse(text=perimeter.df.file))
    
    dataset.r <- rasterFromXYZ(as.data.frame(eca.df)[, c("x", "y", p.var)],crs=UTM36N)
    r.smooth <- focal(dataset.r, w=matrix(1,smooth.w,smooth.w), fun=median,na.rm=TRUE) ### Median filter - smooth
    cr <- raster::crop(r.smooth, extent(perimeter))
    fr <- rasterize(perimeter.df, cr)
    r.smooth.c <- raster::mask(x=cr, mask=fr)
    eca.df<-as.data.frame(rasterToPoints(r.smooth.c))
    names(eca.df)[3]<-"y_pred"
    eca.df$y_pred<-round(eca.df$y_pred)
    eca.df[] <- lapply(eca.df[], function(x) {as.numeric(as.character(x)) })
    if(mdv=="texture"){
      eca.df$texture<-t.names[eca.df$y_pred]
      #unique(data.frame(eca.df$texture,eca.df$y_pred))
    }
  }
  
  if(mdv=="texture"){p.var<-"texture"}else{p.var<-"y_pred"}
  
  if(D2text==T){
    eca.df$texture<-txt.names[eca.df$y_pred]
  } else {
    #if(mdv=="texture"){eca.df$y_pred<-t.levels[eca.df$y_pred]}
  }
  
  p.theme <- theme(legend.position="right",plot.title = element_text(hjust = 0.5,size=12),plot.subtitle =  element_text(hjust = 0.5,size=10),panel.background = element_rect(fill = NA),panel.grid.major = element_line(linewidth=0.1,colour = "grey30"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey30"),panel.border = element_rect(linewidth=0.2, fill = NA),panel.ontop = TRUE)
  
  #if(mdv=="texture"&smooth.w>0){eca.df$y_pred<-t.levels[eca.df$y_pred]}
  p<-ggplot(eca.df,aes(x,y)) + coord_equal() + theme_bw() + p.theme + 
    geom_tile(aes(fill=.data[[p.var]]),size=1) +
    labs( y="Northing", x="Easting", title=paste("Prediction:", mdv),subtitle=subt, caption = paste0("n=",ndp)) 
  if(points.model==T & mdv=="texture"){p<-p+geom_point(data = validation.set,aes(x,y,fill=texture.x),shape=21,col="white")}
  if(mdv=="texture"){ p<-p+scale_fill_manual(name=paste0(mdv),values = palc) } else { p<-p+scale_fill_gradientn(name=paste0(mdv),colours = palc) }
  #p
  
  #ggplot()+geom_sf(data=perimeter,col="skyblue3")+ theme_bw() +# p.theme + 
  #  geom_point(data = validation.set,aes(x,y,fill=texture.x),size=2,shape=21,col="white") +scale_fill_manual(name=paste0(mdv),values = palc)
  
    if(D2text==T|mdv=="texture"){
    eca.df$texture.n<-as.numeric(as.factor(eca.df[[3]])) 
    dataset.r <- rasterFromXYZ(as.data.frame(eca.df)[, c("x", "y", "texture.n")],crs=UTM36N)
    writeRaster(dataset.r, filename=paste0(dataDir,"krigResults/",data.set,"_predicted_",mdv), format="GTiff",overwrite=TRUE)  
    #print(paste0(dataDir,"krigResults/",data.set,"_predicted_",mdv))
  }
  #p# ,"_",format(Sys.time(), "%Y%m%d_%H%M%S")
  
  return(p)
}



gModelsPoints <- function(mdv,toPlot="points",cs.limit=2,data.set="ny_3_25_II",depth="all",sample=1){
  #mdv="texture"  
  #mdv="D"
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  
  if(any(is.na(psd.df[[mdv]]))){
    psd.df<-psd.df[!is.na(psd.df[[mdv]]),]
  }
  
  if(mdv=="texture"){
    psd.df$texture<-getSoilTexture(data.frame(clay=psd.df$clay,silt=psd.df$silt,sand=psd.df$sand)) # get texture class
  } else {
    psd.df[-c(1)] <- lapply(psd.df[-c(1)], function(x) {as.numeric(as.character(x)) })
  }
  
  psd.df<-psd.df[c("x","y",mdv)]
  
  N=nrow(psd.df)
  
  palc <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(20)
  
  if(toPlot=="hist"){
    if(mdv=="texture"){
      return(barplot(prop.table(table(psd.df[3])),sub=paste0("N=",N)))
    } else {
      h <- hist(psd.df[[mdv]], breaks=20, plot=FALSE)
      return( plot(h, col="midnightblue", main=paste0("histogram ",mdv),xlab=mdv,sub = paste0("n=",N)) )
    }
  }
  
  sp.df<-as.data.frame(cbind(psd.df$x,psd.df$y,psd.df[[mdv]]))
  sp.df[c(1:2)] <- lapply(sp.df[c(1:2)], function(x) {as.numeric(as.character(x)) })
  names(sp.df)<-c("x","y",mdv)
  if(mdv=="texture"){ sp.df$texture<-as.numeric(as.factor(sp.df$texture)) }
  coordinates(sp.df) = ~x + y
  proj4string(sp.df) <- CRS(UTM36N)
  
  perimeter.file<-perim.data[perim.data$dataset==data.set,"p.file"]
  perimeter.df.file<-perim.data[perim.data$dataset==data.set,"p.df.file"]
  perimeter<-eval(parse(text=perimeter.file))
  perimeter.df<-eval(parse(text=perimeter.df.file))
  
  p.coords <- as.data.frame(perimeter.df@polygons[[1]]@Polygons[[1]]@coords)
  names(p.coords)<-c("x","y")
  
  p <- ggplot(psd.df,aes(x,y)) + 
    geom_polygon(data=p.coords, aes(x,y),col="grey30",fill="#dddddd")+
    geom_point(aes_string(fill=mdv),shape=21,col="grey30",size=3) +
    
    #geom_tile(aes_string(fill=mdv),alpha = 0.9) + 
    coord_equal() + theme(legend.position="right",plot.title = element_text(hjust = 0,size=14),plot.subtitle = element_text(size=12,hjust = 0),plot.caption = element_text(size=12),axis.text=element_text(size=10),panel.background = element_rect(fill = NA),panel.grid.major = element_line(linewidth=0.1,colour = "grey30"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey30"),panel.border = element_rect(linewidth=0.2, fill = NA)) + labs( y="Northing", x="Easting", title=paste0(mdv), subtitle=paste0(), caption=paste0()) 
  
  if(mdv!="texture"){
    p <- p + scale_fill_gradientn(name=paste0(mdv),colours = palc)
  }
  return(p)
  
}


gModelsKrig <- function(mdv,toPlot="dataset",var.factor=20,mdv.smooth.w=0,cs.limit=2,data.set="ny_3_25_I",depth="all",sample=1){
  #mdv="texture"
  #mdv="D"
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  
  perimeter.file<-perim.data[perim.data$dataset==data.set,"p.file"]
  perimeter<-eval(parse(text=perimeter.file))
  
  if(mdv=="texture"){
    if(toPlot!="dataset"){ mdv="clay"}       # text(x = 0.5, y = 0.5, paste("Discrete variable - variogram not available.\n"), cex = 1.6, col = "black")# return();
    dataset <- dataset.full <- getTextureCol(psd.df[c("x","y","clay","silt","sand")],var.factor,mdv.smooth.w,data.set=data.set)
  } else {
    if(any(is.na(psd.df[[mdv]]))){
      psd.df<-psd.df[!is.na(psd.df[[mdv]]),]
    }
  }
  
  if(toPlot=="variance"){
    dataset <- krigeDF(psd.df[c("x","y",mdv)],mdv,variance = T,data.set=data.set)
    
    palc <- colorRampPalette(rev(brewer.pal(9,"Blues")))(20)
    dvname=paste0(mdv,".var")
    dataset.sp<-dataset[c("x","y",dvname)]
    dataset.r<-rasterFromXYZ(as.data.frame(dataset.sp)[, c("x", "y", dvname)],crs=UTM36N)
    
    N=nrow(dataset)
    
    par(mfrow=c(1,1),mar=c(5,4,4,2) + 0.1)
    p<-plot(dataset.r,col=palc,main=paste(mdv,"variance"),sub=paste0("n=",N))#,legend.args=list(text="a", side=3, font=1, line=0.5, cex=0.8),bty="l")
    plot(perimeter,add=T,lwd=1)
    return(p)
    #grid(lty = 1,col="grey20",lw=0.1)
  } else {
    
    if(toPlot=="var"){  # variogram
      psd.df[-c(1:2)] <- lapply(psd.df[-c(1:2)], function(x) {as.numeric(as.character(x)) })
      psd.sp<-psd.df[c("x","y",mdv)]
      N=nrow(psd.df)
      coordinates(psd.sp) = ~x + y
      proj4string(psd.sp) <- CRS(UTM36N)
      
      formla <- as.formula(paste(mdv, "1", sep=" ~ "))
      v.item = variogram(formla, psd.sp)
      fit.sp<-fit.variogram(v.item, vgm(c("Exp", "Mat", "Sph")))
      if (any(fit.sp$range < 0.0)) {  # avoid negative values - fit Spehere model
        fit.sp<-fit.variogram(v.item, vgm(c("Sph")))
      }
      range=round(fit.sp$range[2],2)
      p<-plot(v.item, fit.sp,xlab="Distance (m)",main=paste("variogram - ",mdv),sub=paste0("Range: ",range," m,  n=",N))
      return(p)
    } else { # Kriging
      if(mdv!="texture"){
        dataset <- dataset.full <- krigeDF(psd.df[c("x","y",mdv)],v.name=mdv,smooth.w=mdv.smooth.w,variance=F,idw.mode=F,data.set=data.set)
        dataset.var<-krigeDF(psd.df[c("x","y",mdv)],mdv,variance=T,data.set=data.set)
        dataset<-merge(dataset,dataset.var,by=c('x','y'))
        
        if(toPlot=="dataset"){  # MDV plot
          dataset<-dataset[order(dataset[[paste0(mdv,".var")]]),]   
          dataset<-dataset[1:round(nrow(dataset)/(as.numeric(var.factor))),] # reduce by variance
        }
      }
    }
  }
  
  if(toPlot!="var" & toPlot!="variance"){  
    N=nrow(dataset)
    palc <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(20)
    show.legend=T
    # print(names(dataset))
    # print(mdv)
    dataset.sp<-dataset[c("x","y",mdv)]
    psd.coords<-dataset.full[c("x","y")]
    psd.all <- merge(psd.coords,dataset.sp,by=c("x","y"),all.x=T)
    if(mdv=="texture"){
      psd.all$texture<-as.numeric(as.factor(psd.all$texture))
      palc <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(length(unique(psd.all$texture)))
      show.legend=F
    }
    
    dataset.r<-rasterFromXYZ(as.data.frame(psd.all)[, c("x", "y", mdv)],crs=UTM36N)
    dataset.r<-as.factor(dataset.r)
    #extent(dataset.r)<-extent(perimeter)

    par(mfrow=c(1,1),mar=c(5,4,4,2) + 0.1)
    plot(dataset.r,legend=show.legend,col=palc,main=mdv,sub=paste0("n=",N))#,legend.args=list(text="a", side=3, font=1, line=0.5, cex=0.8),bty="l")
    if(mdv=="texture"){legend(x='bottomright', legend = unique(dataset.r$texture), fill = palc,cex=0.9)}
    #plot(perimeter,add=T,lwd=1)
    plot(st_geometry(perimeter)[1],add=TRUE)
    
    grid(lty = 1,col="grey20",lw=0.1)
  }
}



gModelsIV <- function(miv,data.set="ny_3_25_II"){
  #miv=c("ECaV_1m","ECaV_05m","ECaH_1m","ECaH_05m")  
  #data.set="beeri_gadash_9_24"
  eca <- loadECaStack(data.set,F)
  eca <- dropLayer(eca,which(names(eca) %notin% miv))
  palc <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(20)
  N=sum(!is.na(getValues(eca)))
  plot(eca,col=palc,main=paste(names(eca)),sub=paste0("n=",N),legend.args=list(text='mS/m', side=3, font=1, line=0.5, cex=0.8),bty="l")
}



gModelsIVareduce <- function(miv,mdv="sand",var.factor=100,data.set="beeri_gadash_9_24",depth="A",sample=1,cs.limit=2){
  
  #print(mdv);print(var.factor)
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  
  if(mdv=="texture"){
    dataset <- getTextureCol(psd.df[c("x","y","clay","silt","sand")],var.factor,data.set=data.set)
    dataset<-dataset[c("x","y","var.mean")]
  } else {
    dataset <- krigeDF(psd.df[c("x","y",mdv)],mdv,variance=T,data.set=data.set)  
  }
  
  eca <- loadECaStack(data.set,F)
  eca <- dropLayer(eca,which(names(eca) %notin% paste0(miv)))
  
  eca.df<-rasterToPoints(eca)
  eca.sp<-merge(eca.df,dataset,by=c("x","y"))
  
  # reduce by variance
  if(mdv=="texture"){
    iv.red<-eca.sp[,c(1:(ncol(eca.sp)-1))]
  } else {
    q.val<-quantile(eca.sp[[ncol(eca.sp)]],probs=1/(as.numeric(var.factor)))
    iv.red<-eca.sp[eca.sp[[ncol(eca.sp)]]<q.val,c(1:(ncol(eca.sp)-1)),]
  }
  
  iv<-rasterFromXYZ(iv.red,crs=UTM36N)  
  extent(iv)<-extent(eca.sp)
  
  palc <- colorRampPalette(rev(brewer.pal(11,"Spectral")))(20)
  
  N=sum(!is.na(getValues(iv)))
  mdv.label<-ifelse(is.na(mdv),"",paste("by",mdv))
  plot(iv,col=palc,main=paste(names(iv),mdv.label),sub=paste0("n=",N),legend.args=list(text='mS/m', side=3, font=1, line=0.5, cex=0.8),bty="l")
  #plot(perimeter,add=T,lwd=1)
  grid(lty = 1,col="grey20",lw=0.1)
  
  par(mfrow=c(1,1),mar=c(5,4,4,2) + 0.1)
}







gModelsIVvar <- function(miv,var.level=50,data.set="ny_3_25_II"){
  #miv=c("ECaV_1m","ECaV_05m","ECaH_1m","ECaH_05m")
  #print(miv)
  par(mfrow=c(ceiling(length(miv)/2),2),mar=c(2,2,2,2),oma=c(1,1,1,1))
  palc <- colorRampPalette(brewer.pal(9,"Blues"))(10)
  eca.var.stack<-raster()
  for (i in 1:length(miv)) {
    eca.var.stack <- stack(eca.var.stack,raster(paste0(dataDir,"shiny/",miv[i],"_variance.tif")))
  }
  
  # plot(eca.var.stack)
  if(var.level!="all"){
    max.var <- cellStats(eca.var.stack,'max')
    for (i in 1:length(miv)) {
      eca.var.stack[[i]][ntile(eca.var.stack[[i]][,],20)>=(as.numeric(var.level)/5)]<-NA
    }
  }
  plot(eca.var.stack)
  
  par(mfrow=c(1,1),mar=c(5,4,4,2) + 0.1)
}




######################################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}






###





########################################

# update the dataset
#  upData <- function(){
#    csv.path <- paste0("/home/ai/Documents/Data/MasterSizer/GSI PSD Raw.csv")
# #   # csv.path <- paste0("/home/ai/Documents/Data/erosion/PSD raw - Gadash Beeri 9-24.csv")
#    raw.df <- data.frame(read.csv(csv.path,header = T,sep = ',',quote = '"',row.names = NULL, stringsAsFactors = F,fileEncoding='UTF-16'))
# 
#   csv1.path <- "/home/ai/Documents/Data/sasa/Sasa 4-23b raw.csv"
#   csv2.path <- "/home/ai/Documents/Data/sasa/Sasa 4-23 2nd raw.csv"
#   raw.df.1 <- data.frame(read.csv(csv1.path,header = T,sep = ',',quote = '"',row.names = NULL, stringsAsFactors = F,fileEncoding='UTF-16'))
#   raw.df.2 <- data.frame(read.csv(csv2.path,header = T,sep = ',',quote = '"',row.names = NULL, stringsAsFactors = F,fileEncoding='UTF-16'))
#   names(raw.df.2)<-names(raw.df.1)
#   raw.df <- rbind(raw.df.1,raw.df.2)
# 
#   firstCol <- which(names(raw.df)=="X0.01")    #"Fit.to.Sample.Data")
#   #lastCol <- which(names(raw.df)=="Modified.Sample.Data")
# 
#   #psd <- raw.df[,c(3,4,firstCol:ncol(raw.df))]
#   psd <- raw.df[,c(6,7,firstCol:ncol(raw.df))] # sasa
#   names(psd) <- c("point","horizon",paste0("p.",round(as.numeric(substring(names(psd)[-c(1,2)], 2, 7)),3)))
#   
#   psd <- raw.df[,c(3,firstCol:ncol(raw.df))] # sasa
#   names(psd) <- c("point",paste0("p.",round(as.numeric(substring(names(psd)[-c(1)], 2, 7)),3)))
# 
#   #saveRDS(psd,paste0(dataDir,"ny_psd_raw.RData"))
#   psd.samples <- psd %>%
#     group_by(point) %>%
#     summarise_all(list(mean), .groups = 'drop_last')
# 
#   saveRDS(psd.samples,paste0("/home/ai/Documents/Data/MasterSizer/GSI_psd_points.RData"))
# 
# }
# 



# get particle size indices: H, d, D
getIndex <- function(points.psd,meta.cols,sizes){
  #points.psd<-psd.df
  S.cols = rowSums(points.psd[-meta.cols])
  p.cols = points.psd[-meta.cols]/rep(S.cols,nrow(points.psd[-meta.cols]))
  
  # Shannon H
  p.log <- p.log.p <- data.frame(matrix(nrow = nrow(p.cols), ncol = ncol(p.cols)))
  
  for(i in 1:ncol(p.cols)){
    for(j in 1:nrow(p.cols)){
      p.log[j,i]=ifelse(round(p.cols[j,i],5)==0,0,log(p.cols[j,i]));
    }
  }
  
  for(i in 1:ncol(p.cols)){
    for(j in 1:nrow(p.cols)){
      p.log.p[j,i]<-p.log[j,i]*p.cols[j,i]
    }
  }
  
  H.samples = data.frame(points.psd[meta.cols],H=apply(p.log.p,1,function(x){ signif(-sum(x),3) }))
  
  
  # d - Martin et al.
  p.log.r<-vector()
  for(j in 1:ncol(p.cols)){
    p.log.r[j]=ifelse(round(sizes[j,"r.size"],5)==0,0,log(sizes[j,"r.size"]));
  }
  
  d.samples <- data.frame(matrix(nrow = nrow(p.cols),ncol = 1))
  for(i in 1:nrow(p.cols)){
    d.samples[i,1] <- signif(sum(p.log.p[i,])/sum(p.log.r),3)
  }
  
  D.samples <- data.frame(matrix(nrow = nrow(p.cols),ncol = 1))
  for(i in 1:nrow(p.cols)){
    D.samples[i,1] <- signif(H.samples[i,"H"] / (H.samples[i,"H"] + KL.plugin(p.cols[i,],sizes[,"r.size"])),3)
  }
  
  indices <- cbind(H.samples,d=d.samples[,1],D=D.samples[,1])
  return(indices)
}






loadPSDCor <- function(data.type="mean",data.set="sasa_4_23",depth="all",sample="all"){
  #print(data.set)
  source.file <- switch (data.set,
                         "ny_3_25_I" = "ny_psd_points.RData",
                         "ny_3_25_II" = "ny_psd_points_repeat.RData",
                         "beeri_hohova_n_12_24" = "beeri_hohovaN24_points_data.RData",
                         "beeri_gadash_9_24" = "beeri_gadash_9-24_psd_points.RData",
                         "sasa_4_23"="sasa_gadash_23_psd_points.RData",
                         stop("Invalid input")
  )
  psd.data<-readRDS(paste0(dataDir,source.file))
  psd.df<-psd.data

  if(data.set=="sasa_4_23"){
    sample1<-1:17
    sample2<-18:34
    sample3<-35:51
    if(sample!="all"){
      #print(sample)
      #if(as.numeric(sample)<10){  
      psd.df<-psd.df[is.na(psd.df$id) | psd.df$id%in%eval(parse(text=paste0("sample",sample))),]
      # } else {
      #   s1<-as.numeric(substr(sample,1,1))
      #   s2<-as.numeric(substr(sample,2,2))
      #   psd.df<-psd.df[is.na(psd.df$id) | psd.df$id%in%eval(parse(text=paste0("sample",s1))) | psd.df$id%in%eval(parse(text=paste0("sample",s2))),]
      # }
    }
  } 
  
  if(length(psd.df$horizon)>0){
    dep.filter<-integer(0)
    if(depth=="A"){dep.filter<-which(psd.df$horizon==1|psd.df$id=="id"|psd.df$id=="r.size"|psd.df$id=="p.size")}
    if(depth=="B"){dep.filter<-which(psd.df$horizon==2|psd.df$id=="id"|psd.df$id=="r.size"|psd.df$id=="p.size")}
    dep.filter<-c(dep.filter,which(is.na(psd.df$id)))
    if(depth!="all"){ psd.df<-psd.df[dep.filter,]}
  }
  
  psd.df<-psd.df[suppressWarnings(order(as.numeric(psd.df$id))),]
  if(!any(!is.na(psd.df$id))){psd.df<-data.frame()}
  
  return(psd.df)
}



# Points PSD
pBox <- function(vi="sand",data.type="mean",cs.limit=2,data.set="ny_3_25_II",depth="all",sample=1){
  psd.df<-loadPSDCor(data.type,data.set)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  N=nrow(psd.df)
  depth.label<-depth
  
  theme.box<-theme(axis.text=element_text(size=12),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
                   panel.grid.minor = element_blank(),panel.grid.major.y = element_line(linewidth=0.1),panel.border = element_blank(),
                   axis.line=element_line(linewidth=0.1),axis.ticks.x = element_blank()) 
  psd.df$id<-as.factor(psd.df$id)
  p <-
    ggplot(psd.df, aes_string(x="id",y=vi),fill="bisque1") + theme_bw() + theme.box +
    #  geom_violin(fill="grey80",adjust = 0.7,draw_quantiles = c(0.5),scale="count") +
    geom_boxplot(width=0.38,  outlier.size = 0.15,) +
    geom_point(size=0.15) +
    stat_summary(fun=mean, geom="point", size=0.2, color="red") +
    theme(axis.text.x = element_text(angle=0, vjust=0.5, size=12),legend.position = "left",
          legend.title = element_text(size=13), legend.text = element_text(size=12),
          axis.text = element_text(size=12),axis.title = element_text(size=13,vjust=1), plot.title = element_text(size=15),
          panel.grid.major = element_line(linewidth=0.2,colour = "grey20"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey40"),
          panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
    labs(title=paste0(vi," by points"),subtitle = paste0("Depth: ",depth.label))+ #,caption = "N=",N) +
    scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0))
  
  ###
  print(p)
}




pFractions <- function(xitem="ECaH.1",fr="2.421",toPlot="cor",data.set="beeri_hohova_n_12_24",depth="all",sample=1){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  #print(paste(xitem,"-",fr))
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df) | nrow(psd.df)<1 | is.null(psd.df[[xitem]])){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  
  N=nrow(psd.df)
  depth.label<-"" # "0-20 cm"
  
  iv <- psd.df[[xitem]]
  # #fraction.cols<-23:123
  # eca.cols<-19:22
  fraction.cols <- which(names(psd.df)=="q.0.010"):ncol(psd.df)
  eca.cols <- which(substr(names(psd.df),1,3)=="ECa")
  
  psd.df[fraction.cols] <- as.data.frame(apply(psd.df[fraction.cols], 2, as.numeric))
  
  if(toPlot=="cor"){
    dv <- psd.df[[paste0("q.",fr)]]
    
    vars <- data.frame(iv,dv)
    
    xlabel<-names(vars)[1]<-xitem
    ylabel<-names(vars)[2]<-fr
    
    cr=signif(cor(iv,dv),2)
    palcor<-rev(brewer.pal(9,"Set1")[c(3,5,7)])
    
    ggplot(vars, aes(.data[[xlabel]],.data[[ylabel]])) + 
      geom_smooth(method="lm", se=F,col="burlywood2",lwd=1.3) +
      geom_point(col="#cc3377",size=2) +
      theme_bw() +
      theme(plot.title = element_text(size = 13,face = "bold"),plot.subtitle = element_text(size = 11), plot.caption=element_text(size=10),axis.title=element_text(size=12)) +
      labs(
        subtitle=paste0("Pearson's R=",cr,"     n=",N), 
        x=xlabel,
        y=ylabel,
        title=paste0("Correlation: ",xlabel," ~ ",ylabel))+
      scale_colour_manual(values = palcor, name="sample")  
    
  } else {
    df.cgram <- cbind(psd.df[eca.cols],psd.df[fraction.cols])
    df.cgram <- as.data.frame(apply(df.cgram, 2, as.numeric))
    #df.cgram<-df.cgram[colSums(df.cgram)>0]
    
    # Correlation matrix
    #    corr <- round(cor(df.cgram), 2)
    
    eca.p.cors<-data.frame()
    for(i in 1:4){
      for(j in 1:101){
        j.ind<-fraction.cols[j]
        if(sd(as.numeric(psd.df[[j.ind]]))>0){
          ep.cor<-round(cor(psd.df[,eca.cols[i]],psd.df[,j.ind]), 2)
        } else {
          ep.cor<-0
        }
        eca.p.cors[i,j]<-ep.cor
        names(eca.p.cors)[j]<-names(psd.df[j.ind])
      }
      rownames(eca.p.cors)[i]<-names(psd.df[eca.cols[i]])
    }
    
    eca.p.cors<-as.data.frame(eca.p.cors)
    #eca.p.cors<-data.frame(name=rownames(eca.p.cors),eca.p.cors)
    #ggplot(eca.p.cors,aes(name)
    
    eca.p.mat<-as.matrix(eca.p.cors)
    
    # aa<-pivot_longer(eca.p.cors,cols=1:ncol(eca.p.cors))
    # ggplot(aa,aes(name,value))+
    #   geom_point(aes(col=name),size=2) + # aes_string(col=factor(nyvals[[mz.col]]))
    #   theme_bw() +
    #   theme(plot.title = element_text(size = 13,face = "bold"),plot.subtitle = element_text(size = 11), plot.caption=element_text(size=10),axis.title=element_text(size=12)) +
    #   labs(
    #     subtitle=paste0("Pearson's R=",cr,"     n=",N), 
    #     x=xlabel,
    #     y=ylabel,
    #     title=paste0("Correlation: ",xitem," ~ ",yitem))+
    #   #caption=paste0("n=",N)) +
    #   scale_colour_manual(values = palcor, name="sample")
    
    p.mat <- cor_pmat(eca.p.cors)
    
    # Plot
    ggcorrplot(eca.p.cors, hc.order = TRUE, 
               type = "lower", 
               lab = F, 
               lab_size = 10, 
               method="square", 
               tl.cex=8,
               #colors = c("tomato2", "white", "springgreen3"), 
               outline.col = "white",
               colors = c("#6D9EC1", "white", "#E46726"),
               title="PSD Correlation matrix", 
               ggtheme=theme_bw) + labs(subtitle=paste0("n=",N)) +
      theme(plot.title = element_text(size = 13),plot.subtitle = element_text(size = 11))   
  }
}




pFractionsSgram <- function(pout="table",data.set="ny_3_25_II",depth="all",sample=1){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  #print(paste(xitem,"-",fr))
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){
    if(pout=="table"){return(data.frame(message="No data..."))} 
    if(pout=="tableTop"){return("No data...")}
    else {text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  }
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  
  N=nrow(psd.df)
  depth.label<-depth
  
  eca.cols <- which(lapply((str_locate_all(names(psd.df),"ECa")),length)>0)
  fraction.cols <- which(names(psd.df)=="q.0.010"):ncol(psd.df) #22:122
  
  c.mat <- data.frame(matrix(ncol = length(fraction.cols), nrow = length(eca.cols)))
  names(c.mat)<-substring(names(psd.df[fraction.cols]),3)
  row.names(c.mat)<-names(psd.df[eca.cols])
  for(i in 1:length(eca.cols)){
    for(j in 1:length(fraction.cols)){
      if(sd(as.numeric(psd.df[[fraction.cols[j]]]))>0){
        c.mat[i,j]=signif(cor(as.numeric(psd.df[[eca.cols[i]]]),as.numeric(psd.df[[fraction.cols[j]]])),2)
      } else {
        c.mat[i,j]=0
      }
    }
  }
  
  
  if(pout=="table"){ # full correlation table
    return(c.mat) 
  } else {
    if(pout=="tableTop"){ # top correlations with each ECa layer
      top.text <- paste("Top ECa ~ PSD correlations:\n")
      for(i in 1:4){
        top.text<-paste0(top.text,row.names(c.mat)[i]," ~ I(", names(which.max(c.mat[i,])),"): ",c.mat[i,which.max(c.mat[i,])],"\n")
        top.text<-paste0(top.text,"\n",row.names(c.mat)[i]," ~ I(", names(which.min(c.mat[i,])),"): ",c.mat[i,which.min(c.mat[i,])],"\n")
      }
      top.text<-paste0(top.text,"N=",N)
      return(top.text)
    } else {
      
      theme.cor <- theme(legend.position = "left",
                         legend.title = element_text(size=12,lineheight = 0.4), legend.text = element_text(size=14),
                         legend.spacing.y = unit(0.1, 'cm'),
                         axis.text = element_text(size=12),axis.title = element_text(size=12,vjust=1), 
                         axis.text.x = element_text(angle = 60,hjust = 1),
                         plot.title = element_text(size=13),
                         panel.background = element_rect(fill = NA),
                         panel.grid.major = element_line(linewidth=0.1,colour = "grey30"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey50"),
                         panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())
      
      palc <- colorRampPalette(rev(brewer.pal(9,"RdYlBu")))(20)
      
      c.df <- c.mat %>% 
        as.data.frame() %>%
        rownames_to_column("ECa") %>%
        pivot_longer(-c(ECa), names_to = "particle.size", values_to = "corr") #%>%
      #arrange(as.numeric(particle.size)) 
      c.df$particle.size<-as.factor(as.character(c.df$particle.size))
      c.df$particle.size <- factor(c.df$particle.size, levels=levels(c.df$particle.size)[order(as.numeric(as.character(levels(c.df$particle.size))))])
      
      p <- ggplot(c.df,aes(x=particle.size, y=ECa, fill=corr)) + 
        labs(y="ECa\n",x="\nparticle size [µm]",subtitle=paste("Depth:",depth.label,"| N=",N)) +
        geom_tile() + 
        theme.cor + 
        theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 9, hjust = 1),
              axis.text.y = element_text(angle = 0, vjust = 1, size = 9, hjust = 1),plot.subtitle = element_text(size=11),
              axis.title = element_text(size=10),
              legend.title = element_text(size=9),
              legend.text = element_text(size=9))+
        coord_fixed()+ 
        scale_y_discrete()+
        scale_fill_gradient2(low = "blue3", high = "red3", mid = "white",
                             midpoint = 0, limit = c(-1,1), space = "Lab",
                             name="Pearson's R\n\n")
      print(p)
    }
  }
}



getPSData <- function(data.set="ny_3_25_II",depth="all",sample=1){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){return(data.frame("message"="No data for this selection...",row.names=""));}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  row.names(psd.df)<-NULL
  psd.df[]<-lapply(psd.df[], function(x) {as.numeric(x)})
  psd.df[names(psd.df)%notin%c("id","horizon")]<-apply(psd.df[names(psd.df)%notin%c("id","horizon")],2,function(x){round(x,2)})
  
  #write.csv(psd.df,paste0(dataDir,"Beeri_Gadash_PSD_9-24.csv"), row.names = FALSE,fileEncoding = "UTF-8")  

  #write.csv(psd.data,paste0(dataDir,"sasa_points_data.csv"), row.names = FALSE,fileEncoding = "UTF-8")    
  
  # psd.df<-datatable(psd.df) %>% formatStyle(
  #   'clay',
  #   backgroundColor = styleInterval(1, c('bisque2','blue1'))
  # )
  
  return(psd.df)
}




pMeasures <- function(pout="plot",var="D",sortby="point",cs.limit=2,data.set="ny_3_25_II",depth="all",sample=1){
  if(any(grepl("package:ggtern", search()))) detach("package:ggtern")
  #print(paste(xitem,"-",fr))
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  
  psd.var <- psd.df[c("id",var)] %>%
    group_by(id) %>%
    summarise_at(c(var),list(mean=mean))
  
  psd.var <- psd.var[order(as.numeric(as.character(psd.var$id))), ]  # sort
  if(sortby=="value"){ psd.var <- psd.var[order(psd.var$mean), ]}
  psd.var$id <- factor(psd.var$id, levels = psd.var$id)  # to retain the order in plot.
  
  if(pout=="plot"){
  N=nrow(psd.var)
  depth.label<-depth
  
  pal.m <- c("#ea5545", "#f46a9b", "#ef9b20", "#edbf33", "#ede15b", "#bdcf32", "#87bc45", "#27aeef", "#b33dc6","#7023ed")[c(1,4,7)]
  
  p <- 
    ggplot(psd.var, aes(x=id, y=mean)) + 
    geom_bar(stat="identity", width=.5,fill="#ef6b20") + scale_fill_manual(values=pal.m) +
    labs(title=paste0(var," - depth: ",depth.label," | sorted by ",sortby,"\n")) +
    #subtitle=paste("source: ",basename(f.path))) +  
    theme(axis.text.x = element_text(angle=0, vjust=0.5, size=12),legend.position = "left",
          legend.title = element_text(size=12), legend.text = element_text(size=12),
          axis.text = element_text(size=14),axis.title = element_text(size=14,vjust=1), plot.title = element_text(size=14),
          panel.grid.major = element_line(linewidth=0.2,colour = "grey20"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey40"),
          panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
    ylab("vaule\n") + xlab("\npoint") 
  
  return(p)
  } else {
  dtable<-data.frame(horizon=depth,variable=var,
                     mean=round(mean(psd.var$mean),2),sd=round(sd(psd.var$mean),2),
                     min=round(min(psd.var$mean),2),max=round(max(psd.var$mean),2),cv=round(sd(psd.var$mean)/mean(psd.var$mean)*100,2))
  return(dtable)
  }
}



#df=psd.df[c("x","y","clay")]
krigeDF <- function(df,v.name="clay",variance=F,smooth.w=0,idw.mode=F,data.set="ny_3_25_II"){ 
  #print(data.set)
  sp.df<-as.data.frame(cbind(x=df$x,y=df$y,var=df[[v.name]]))
  coordinates(sp.df) = ~x + y
  proj4string(sp.df) <- CRS(UTM36N)
  
  if(length(zerodist(sp.df)>0)){
    sp.df <- sp.df[-zerodist(sp.df)[,1],] 
  }
  
  #hist(sp.df$var)
  
  v.item = variogram(var ~ 1, sp.df)
  fit.sp<-fit.variogram(v.item, vgm(c("Exp", "Mat", "Sph")))
  if (any(fit.sp$range < 0.0)) {  fit.sp<-fit.variogram(v.item, vgm(c("Sph")))}
  
  grid.file<-perim.data[perim.data$dataset==data.set,"grd.file"]
  perimeter.file<-perim.data[perim.data$dataset==data.set,"p.file"]
  perimeter.df.file<-perim.data[perim.data$dataset==data.set,"p.df.file"]
  grd<-eval(parse(text=grid.file))
  perimeter<-eval(parse(text=perimeter.file))
  perimeter.df<-eval(parse(text=perimeter.df.file))
  
  if(idw.mode==T){
    kriged = idw(var ~ 1, sp.df, grd)
  } else {
    kriged = krige(var ~ 1, sp.df, grd, model = fit.sp)
  }
  
  if(variance==F){
    r.kriged <- raster(kriged[1])
  } else {
    r.kriged <- raster(kriged[2])
    v.name<-paste0(v.name,".var")
  }
  
  smooth.w<-as.numeric(smooth.w)
  if(smooth.w>0){
    r.smooth <- focal(r.kriged, w=matrix(1,smooth.w,smooth.w), fun=mean,na.rm=TRUE)
  } else {
    r.smooth <- r.kriged
  }
  
  cr.ny <- raster::crop(r.smooth, extent(perimeter))
  fr.ny <- rasterize(perimeter.df, cr.ny)   
  lr.ny <- raster::mask(x=cr.ny, mask=fr.ny)
  #  plot(lr.ny)
  
  dataset <- data.frame(rasterToPoints(lr.ny))
  names(dataset)[3]<-v.name
  
  return(dataset)
}




textKrige <- function(){ 
  
  clay.r <- raster(paste0(dataDir,"Clay_kriged beeri gadash.tif"))
  sand.r <- raster(paste0(dataDir,"Sand_kriged beeri gadash.tif"))
  psd.stack <- stack(clay.r,sand.r)
  
  df.clay <- data.frame(rasterToPoints(clay.r))
  df.sand <- data.frame(rasterToPoints(sand.r))
  df.psd <- merge(df.clay,df.sand,by=c("x","y"))
  names(df.psd)<-c("x","y","clay","sand")
  df.psd$silt <- (100-df.psd$clay-df.psd$sand)
  psd<-data.frame(clay=df.psd$clay,silt=df.psd$silt,sand=df.psd$sand)
  texture.col<-getSoilTexture(psd)
  
  df.psd$texture<-texture.col
  
  sp.text<-df.psd[c(1,2,6)]
  coordinates(sp.text) <- ~ x + y
  gridded(sp.text) <- TRUE
  r.text <- raster(sp.text)
  
  palc<-rev(brewer.pal(9,"Spectral")[c(1,3,7,9)])
  r.range <- c(minValue(r.text), maxValue(r.text))
  par(mar=c(2,4,2,2))
  p<-plot(r.text,col=palc,main="Soil texture by kriging",legend.args=list(text="Texture\n", side=3, font=1, line=0.5, cex=1),bty="l",
          axis.args=list(at=seq(r.range[1], r.range[2], 1),cex.axis=0.9,
                         labels=c("Loam","Loamy-sand","Sandy-loam","Silt-loam")
          ))
  writeRaster(r.text, filename=paste0(dataDir,"krigResults/Beeri_texture_kriged"), format="GTiff",overwrite=TRUE) 
  return(p)
}



getTextureCol <- function(css,var.factor=10,mdv.smooth.w=0,data.set="ny_3_25_II"){ 
  var.factor<-as.numeric(var.factor)
  #css <- psd.df[c("x","y","clay","silt","sand")]
  # get 3 layers of interpolation
  df.clay<-krigeDF(css,"clay",smooth.w=mdv.smooth.w,idw.mode=T,data.set=data.set)
  df.silt<-krigeDF(css,"silt",smooth.w=mdv.smooth.w,idw.mode=T,data.set=data.set)
  df.sand<-krigeDF(css,"sand",smooth.w=mdv.smooth.w,idw.mode=T,data.set=data.set)
  df.clay.var<-krigeDF(css,"clay",variance=T,data.set=data.set)
  df.silt.var<-krigeDF(css,"silt",variance=T,data.set=data.set)
  df.sand.var<-krigeDF(css,"sand",variance=T,data.set=data.set)
  
  dataset<-merge(df.clay,df.silt,by=c('x','y'))
  dataset<-merge(dataset,df.sand,by=c('x','y'))
  dataset<-merge(dataset,df.clay.var,by=c('x','y'))
  dataset<-merge(dataset,df.silt.var,by=c('x','y'))
  dataset<-merge(dataset,df.sand.var,by=c('x','y'))
  
  # Normalize distribution
  dataset$d.sum<-rowSums(dataset[,c("clay","silt","sand")]) 
  dataset[,c("clay.n","silt.n","sand.n")]<-lapply(dataset[,c("clay","silt","sand")], function(x){100*x/dataset$d.sum})
  
  # reduce by variance
  dataset[,c("clay.var.n","silt.var.n","sand.var.n")]<-lapply(dataset[,c("clay.var","silt.var","sand.var")], function(x){round(x/max(x),3)})
  dataset$var.mean<-rowMeans(dataset[,c("clay.var.n","silt.var.n","sand.var.n")])
  
  dataset<-dataset[order(dataset$var.mean),]  
  dataset<-dataset[1:round(nrow(dataset)/var.factor),]
  dataset$texture<-getSoilTexture(data.frame(clay=dataset$clay.n,silt=dataset$silt.n,sand=dataset$sand.n)) # get texture class
  #hist(dataset$var.mean)
  
  return(dataset)
}



getEQ <- function(x,y,cs.limit=2,data.set="ny_3_25_II",depth="all",sample=1){  
  #x="clay";y="D"
  psd.df<-loadPSDCor("mean",data.set,depth,sample)
  if(is.null(psd.df)|nrow(psd.df)<1){text(x = 0.5, y = 0.5, paste("No overlapping data...\n"), cex = 1.6, col = "black");return();}
  psd.df<-psd.df[1:(nrow(psd.df)-3),]
  if(cs.limit!=2){
    psd.df$clay<-psd.df[[paste0("clay",cs.limit)]]
    psd.df$silt<-psd.df[[paste0("silt",cs.limit)]]
  }
  
  df<-data.frame(psd.df[[x]],psd.df[[y]])
  #df[]<-lapply(df[], function(x) {as.numeric(x)})
  names(df)<-c("x","y")
  N=nrow(df)
  model <- lm(y ~ x, data = df)
  #paste('y =',  signif(coef(model)[[2]],3), '* x', '+', signif(coef(model)[[1]],3))
  coefs<-data.frame(intercept=model$coefficients[1],slope=model$coefficients[2])
  
  return(coefs)
}





# Points PSD
gBenchmark <- function(bm.file="benchmark_sasa_D_texture.RData",xitem="response",yitem="acc.v",output="plot",sample="all",depth="all",response="all"){
  # bm.file="benchmark20250610_014412.RData"
   # depth="all"
    #xitem="uid"
  benchmark.df<-loadBenchmark(bm.file,sample,depth)
  # benchmark.df[benchmark.df$validation.set==2,"validation.set"]<-"Grid"
  # saveRDS(benchmark.df,paste0(dataDir,"benchmark/",bm.file))
  
  if(response!="all"){
    benchmark.df<-benchmark.df[benchmark.df$response==response,]
  }
  if(is.null(benchmark.df)|is.null(benchmark.df[[xitem]])|nrow(benchmark.df)<1){text(x = 0.5, y = 0.5, paste("No data...\n"), cex = 1.6, col = "black");return();}
  #psd.df<-psd.df[1:(nrow(psd.df)-3),]
  
  if(xitem!="validation.set" & any(benchmark.df$data.set=="sasa_4_23")){
    if(yitem=="acc.v"){benchmark.df<-benchmark.df[benchmark.df$sample!=benchmark.df$validation.set,]} # validation set: independent points
    if(yitem=="acc.v.own"){benchmark.df<-benchmark.df[benchmark.df$sample==benchmark.df$validation.set,]; yitem="acc.v"} # validation set: own points
  }
  
  if(nrow(benchmark.df)<1){text(x = 0.5, y = 0.5, paste("No indepedendent validation dataset...\n"), cex = 1.6, col = "black");return();}
  
  # set uid
  #benchmark.df$uid<-paste0(benchmark.df$response,"/",benchmark.df$depth)
  
  benchmark.df[[yitem]]<-as.numeric(benchmark.df[[yitem]])
  benchmark.df[[xitem]]<-as.factor(benchmark.df[[xitem]])
  if(output=="table"){
    bm.df <- benchmark.df %>%
      group_by(.data[[xitem]]) %>%
      dplyr::summarise(mean = round(mean(.data[[yitem]],na.rm=T),2),sd = round(sd(.data[[yitem]],na.rm=T),2),median = round(median(.data[[yitem]],na.rm=T),2),max = round(max(.data[[yitem]],na.rm=T),2), .groups = 'drop_last')  # multiple summary columns 
    return(bm.df)  
  }
  
  if(output=="table.top"){
    benchmark.df<-benchmark.df[!is.nan(benchmark.df[[yitem]]),]
    benchmark.df<-benchmark.df[rev(order(as.numeric(benchmark.df[[yitem]]))),]
    benchmark.df<-benchmark.df[1:20,]
    return(benchmark.df)  
  }
  
  N=nrow(benchmark.df)
  #depth.label<-depth
  y.label<-"Validation accuracy"
  if(yitem=="acc.tr"){y.label<-"Training accuracy"}
  if(yitem=="runtime"){y.label<-"Runtime (sec.)"}
  
  if(xitem=="uid"){ benchmark.df[[xitem]]<- paste0(substring(benchmark.df[[xitem]],13),"/",substring(benchmark.df[[xitem]],11,11)) } # Sample name
  benchmark.df<-benchmark.df[order(benchmark.df$depth,benchmark.df$sample),]
  #benchmark.df[[xitem]]<-str_to_sentence(benchmark.df[[xitem]])
  benchmark.df[[xitem]]<-as.factor(benchmark.df[[xitem]])
  benchmark.df[[yitem]]<-as.numeric(benchmark.df[[yitem]])
  
  min.limit <- min(benchmark.df[[yitem]])-(min(benchmark.df[[yitem]])*0.2)
  max.limit <- ceil(max(benchmark.df[[yitem]]))
  
  theme.box<-theme(axis.text=element_text(size=12),#panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
                   panel.grid.major = element_line(linewidth=0.2,colour = "grey30"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey50"),
                   panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
                   panel.border = element_blank(),
                   axis.title = element_text(size=14,vjust=0.5), plot.title = element_text(size=15),plot.subtitle = element_text(size=10),
                   legend.title = element_text(size=13), legend.text = element_text(size=12),legend.position = "left",
                   axis.line=element_line(linewidth=0.1),axis.ticks.x = element_blank(),
                   axis.text.y = element_text(size=12,face="bold"),
                   axis.text.x = element_text(size=13,face="bold")) #angle = 45,hjust = 1))
  
  p <-
    ggplot(benchmark.df, aes(x=.data[[xitem]],y=.data[[yitem]]),fill="bisque1") + theme_bw() + theme.box +
    #  geom_violin(fill="grey80",adjust = 0.7,draw_quantiles = c(0.5),scale="count") +
    geom_boxplot(width=0.24,  outlier.size = 0.3,) +
    geom_point(size=0.3) +
    stat_summary(fun=mean, geom="point", size=1, color="red") +
    #panel.grid.major = element_line(linewidth=0.2,colour = "grey20"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey40"),
    #panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank()) +
    labs(title=paste0(y.label," by ",xitem),subtitle = paste0("N=",N))+ scale_x_discrete() 
  if(yitem=="runtime"){
    p <- p + scale_y_continuous(limits = c(min.limit,max.limit)) #expand = c(0,0))
  } else {
    p <- p + scale_y_continuous(limits = c(0,1),expand = c(0,0))
  }
  
  p
  
  ###
  print(p)
}


loadBenchmark <- function(bm.file,sample,depth){
  bm.df<-readRDS(paste0(dataDir,"benchmark/",bm.file))
  
  if(sample!="all" & !any(bm.df$data.set!="sasa_4_23")){ bm.df<-bm.df[bm.df$sample==sample,] }
  if(depth!="all"){ bm.df<-bm.df[bm.df$depth==depth,] }
  if(nrow(bm.df)<1){return(data.frame(matrix(nrow = 0,ncol = 0)))}
  bm.df$uid<-as.factor(paste0(bm.df$data.set,"_",bm.df$depth,"_",bm.df$sample))
  for(i in 1:length(unique(bm.df$data.set))){
    bm.df[which(bm.df$data.set==perim.data[i,"dataset"]),"plot"]<-perim.data[i,"plot"]
  }
  
  if(i==1 & bm.df[1,"data.set"]=="sasa_4_23"){bm.df$plot="Sasa Hula"}
  return(bm.df)
}




# Points PSD
gCorrBenchmark <- function(display.c="cs.llmit",xitem="clay",yitem="ECaH.1",output="plot",sample="all",depth="all"){
  
  benchmark.df<-loadCorrBenchmark("corr_benchmark.RData",sample,depth)
    
  if(yitem=="all"){
    benchmark.df[1:4]<-lapply(benchmark.df[1:4], function(x) {as.numeric(x)})
    benchmark.df$ECa<-rowMeans(benchmark.df[,1:4])
    yitem="ECa"
  } else {
    benchmark.df[[yitem]]<-as.numeric(benchmark.df[[yitem]])
  }
  benchmark.df<-benchmark.df[benchmark.df$var.x==xitem,]
  benchmark.df[[display.c]] <- as.factor(benchmark.df[[display.c]])

  if(output=="table"){
    bm.df <- benchmark.df %>%
      group_by(.data[[display.c]]) %>%
      dplyr::summarise(mean = round(mean(.data[[yitem]],na.rm=T),2),median = round(median(.data[[yitem]],na.rm=T),2),max = round(max(.data[[yitem]],na.rm=T),2), .groups = 'drop_last')  # multiple summary columns 
    return(bm.df)  
  }
  
  if(output=="table.top"){
    benchmark.df<-benchmark.df[!is.nan(benchmark.df[[yitem]]),]
    benchmark.df<-benchmark.df[rev(order(as.numeric(benchmark.df[[yitem]]))),]
    benchmark.df<-benchmark.df[1:20,]
    return(benchmark.df)  
  }
  
  N=nrow(benchmark.df)
  #depth.label<-depth
  y.label<-paste(yitem,"~",xitem)
  #if(yitem=="acc.tr"){y.label<-"Training accuracy"}
  #if(yitem=="runtime"){y.label<-"Runtime (sec.)"}
  
  theme.box<-theme(axis.text=element_text(size=12),#panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
                   panel.grid.major = element_line(linewidth=0.2,colour = "grey30"),panel.grid.minor = element_line(linewidth=0.1,colour = "grey50"),
                   panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),
                   axis.title = element_text(size=13,vjust=1), plot.title = element_text(size=15),plot.subtitle = element_text(size=10),
                   legend.title = element_text(size=13), legend.text = element_text(size=12),legend.position = "left",
                   axis.line=element_line(linewidth=0.1),axis.ticks.x = element_blank(),axis.text.x = element_text(angle = 45,hjust = 1))
  

  benchmark.df[[yitem]]<-as.numeric(benchmark.df[[yitem]])
  
  min.limit <- min(benchmark.df[[yitem]])-(min(benchmark.df[[yitem]])*0.2)
  max.limit <- ceil(max(benchmark.df[[yitem]]))
  
  p <-
    ggplot(benchmark.df, aes(x=.data[[display.c]],y=.data[[yitem]]),fill="bisque1") + theme_bw() + theme.box +
    #  geom_violin(fill="grey80",adjust = 0.7,draw_quantiles = c(0.5),scale="count") +
    geom_boxplot(width=0.38,  outlier.size = 0.3,) +
    geom_point(size=0.3) +
    stat_summary(fun=mean, geom="point", size=1, color="red") +
    labs(title=paste0(y.label," by ",display.c),subtitle = paste0("N=",N),x=display.c)+ scale_x_discrete() #,caption = "N=",N) +
    p <- p + scale_y_continuous(limits = c(-1,1)) #expand = c(0,0))

  ###
  print(p)
}


loadCorrBenchmark <- function(bm.file="corr_benchmark.RData",sample,depth){
  bm.df<-readRDS(paste0(dataDir,"benchmark/",bm.file))
  samples<-c("QC","Grid","QCvar")
  bm.df$sample<-samples[bm.df$sample]
  if(sample!="all"){ bm.df<-bm.df[bm.df$sample==sample,] }
  if(depth!="all"){ bm.df<-bm.df[bm.df$depth==depth,] }
  if(nrow(bm.df)<1){return(data.frame(matrix(nrow = 0,ncol = 0)))}
  
  bm.df$uid<-paste0(bm.df$sample,"_",bm.df$depth,"_",bm.df$cs.llmit)
  bm.df$sample.depth<-paste0(bm.df$sample,"_",bm.df$depth)
  bm.df$sample<-paste0(bm.df$sample)
  bm.df$plot="Sasa Hula"
  return(bm.df)
}




