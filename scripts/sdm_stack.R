
library(sf)
library(terra)


lf<-list.files("outputs/polygons", full = TRUE, pattern = "1950-2024") |> grep("", x = _, value = TRUE)
l <- lapply(lf, st_read)


x <- do.call("rbind", l)

r <- rast()

png("stacked.png", width = 16, height = 10, res = 400, units = "in")
par(mfrow = n2mfrow(length(lf), asp = 3 / 2))
r<-lapply(lf,function(i){
  r<-rast(i)
  titre<-basename(i)
  titre<-gsub(".tif","",titre)
  titre<-sapply(strsplit(titre,"_"),function(i){paste(c(sub("^([a-z])", "\\U\\1", paste(i[2:3], collapse = " "), perl = TRUE), i[1], i[4:7]), collapse="\n")})
  titre <- gsub("(^[[:alpha:]])", "\\U\\1", titre, perl=TRUE)
  a <- add_range(i, add = FALSE)
  if(!is.null(a)){
    r <- crop(r, st_buffer(a, 250000))
  }
  plot(r, mar = c(0, 0.5, 0.5, 0), legend = TRUE, axes = FALSE, plg = list(size =c(0.4, 1.1)))
  add_range(i)
  text(par("usr")[1], par("usr")[4], label = titre, adj = c(0, 1), cex = 1, xpd = TRUE)
})
par(mfrow=c(1,1))
dev.off()
#system("xdg-open sdms.png")