
library(gstat)


ik <- s[sample(1:nrow(s),100000),]
ik <- st_jitter(ik)
ik <- cbind(st_coordinates(ik),as.data.frame(ik))
ik$ik <- as.logical(ik$pres)
#ik <- cbind(st_coordinates(s2),as.data.frame(s2))
#ik$ik <- (ik$pres/ik$counts) >= 0.15
names(ik)[1:2] <- c("x","y")
ik <- ik[,c("x", "y", "ik")]
ik <- na.omit(ik)



v <- variogram(ik ~ 1, data = ik, locations = ~ x + y, width = 50000)
plot(v, cex = (v$np/max(v$np)) * 10)
# Intial parameter set by eye esitmation
m<-vgm(0.001,"Exp",250000,0.0001)

# least square fit
fv<-fit.variogram(v, m)

plot(v, pl=T, 
         model=fv,
         col="black", 
         cex=0.9, 
         lwd=0.5,
         lty=1,
         pch=19,
         cut=1000000,
         main="Indicator Variogram\n As > 10 ppb",
         xlab="Distance (m)",
         ylab="Semivariance")


k <- krige(ik ~ 1, nmax=50,
              loc = st_as_sf(ik, coords = c("x", "y"), crs = st_crs(s2)),        # Data frame
              newdata = s2,     # Prediction location
              model = m)   # fitted varigram model  

gs <- setValues(g, k$var1.pred)
gs <- mask(gs, vect(qc))
plot(gs)
plot(st_geometry(qc), lwd = 0.75, add = TRUE)
plot(st_geometry(s[s$pres == 1, ]), cex = 0.1, add = TRUE)
plot(st_geometry(lakes), add = TRUE, col = "white", lwd = 0.5)




