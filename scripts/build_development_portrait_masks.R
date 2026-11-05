#!/usr/bin/env Rscript
# User-approved background removal: change alpha only; keep original JPEG RGB.
suppressPackageStartupMessages(library(magick))
paths<-list.files("WallyApps/shared/assets",pattern="^portrait-.*[.]jpg$",full.names=TRUE)
for(path in paths) {
  photo<-image_read(path)
  info<-image_info(photo)
  corners<-c("1x1",paste0(info$width-2,"x1"),paste0("1x",info$height-2),paste0(info$width-2,"x",info$height-2))
  for(point in corners) photo<-image_fill(photo,"none",point=point,fuzz=12)
  channel<-image_data(photo,channels="a")
  alpha<-t(matrix(as.integer(channel[1,,]),nrow=dim(channel)[2]))/255
  # A small alpha feather and bottom fade avoid a rectangular shirt edge.
  mask<-image_read(as.raster(alpha))
  mask<-image_morphology(mask,"Erode",kernel="Disk:1")
  mask<-image_blur(mask,radius=0,sigma=.55)
  channel<-image_data(mask,channels="gray")
  alpha<-t(matrix(as.integer(channel[1,,]),nrow=dim(channel)[2]))/255
  fade<-max(2L,round(nrow(alpha)*.07))
  index<-(nrow(alpha)-fade+1L):nrow(alpha)
  alpha[index,]<-alpha[index,]*seq(1,0,length.out=fade)
  png::writePNG(alpha,sub("[.]jpg$","-mask.png",path))
}
cat("Saved",length(paths),"background masks; original portrait RGB files unchanged.\n")
