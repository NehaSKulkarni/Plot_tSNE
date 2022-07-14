options(repos = list(CRAN="http://cran.rstudio.com/"))
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")}
BiocManager::install(c("ggplot2","gridExtra"),update = F)
list.of.packages <- c(c("Hmisc","Tmisc","tidyverse"))
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)}
library(ggplot2)
library(gridExtra)

args <- commandArgs(trailingOnly = T)
Genes <- unlist(strsplit(args,","))


#plot markers on a tsne/pca - create with make_pca or make_tsne
plot_markers <- function(df,markers,return_grid=T){
  colnames(df)[1:2] <- c("tSNE1","tSNE2")
  plot_data_column = function (data, column){
      ggplot(df)+geom_point(aes_string("tSNE1","tSNE2",colour= column))
  }
  myplots <- lapply(markers, plot_data_column, data = pca_df)
  
  # if T return a grid instead of list
  if(return_grid){
    n <- length(myplots)
    nCol <- floor(sqrt(n))
    grid<-do.call("grid.arrange", c(myplots, ncol=2))
    return(grid)
  }
  
  #return list 
  return(myplots)
}

Embryo <- read.csv("TSNE_Embryo.csv",row.names = 1)
Gata3 <- read.csv("TSNE_Blastoid_GATA3.csv",row.names = 1)
Nipsc1 <- read.csv("TSNE_Blastoid_NIPSC1.csv",row.names = 1)
Corrected <- read.csv("TSNE_Blastoid_Embryo_batch_corrected.csv",row.names = 1)
colnames(Corrected)[which(colnames(Corrected) == "condition")] <- "Mural_Polar"

pdf("Embryo_tSNE.pdf",width = 15,height = 4*length(Genes)+2)
plot_markers(df = Embryo,c("lineage","day","Mural_Polar",Genes))
dev.off()

pdf("Gata3_tSNE.pdf",width = 15,height = 4*length(Genes)+2)
plot_markers(df = Gata3,c("lineage","day","Mural_Polar",Genes))
dev.off()

pdf("Nipsc1_tSNE.pdf",width = 15,height = 4*length(Genes)+2)
plot_markers(df = Nipsc1,c("lineage","day","Mural_Polar",Genes))
dev.off()

pdf("Embryo+Blastoid_tSNE.pdf",width = 15,height = 4*length(Genes)+2)
plot_markers(df = Corrected,c("lineage","day","Mural_Polar",Genes))
dev.off()

