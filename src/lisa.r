
##### Make a function for multi-faceted maps with shared legend.
##### Originally the function was written by Shaun Jackman: http://rpubs.com/sjackman/grid_arrange_shared_legend
##### Later Baptiste Auguié rewrote and enhanced it: https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

#### Make an indicator map of Georgian regions with labels

#### Calculate global and local Moran's I for each wave of elections

###### Step 1: Make an empty table to store the results
etable<-as.data.frame(setNames(replicate(3,numeric(0), simplify = F), letters[1:3]))

###### Step 2: Do calculations for each election year

### 2016 Parliamentary elections
IDs<-row.names(m2016)
coords<-coordinates(m2016)
m8w <- poly2nb(m2016)
m8wg <- graph2nb(gabrielneigh(coords), row.names = IDs)
m2016$IDs<-row.names(m2016)
wghts<-nb2listw(m8wg,zero.policy=TRUE)

morans_i8 <- moran.mc(m2016@data$UNM/m2016@data$Total_vote, 
                      listw = wghts, zero.policy = TRUE, nsim = 666)
					  

dtable<-as.data.frame(cbind("Parliamentary 2016", morans_i8$statistic, morans_i8$p.value))
etable<-rbind(etable, dtable)
cor8 <- sp.correlogram(neighbours = m8w, zero.policy = TRUE, 
                       var = m2016@data$UNM/m2016@data$Total_vote, 
                       order = 10, method = "I", style = "C", randomisation = TRUE)

lags16 <- as.data.frame(cor8$res)
lags16$lag <- as.numeric(row.names(lags16))
lags16$dataset <- "Parliamentary 2016"

localm <- localmoran(m2016@data$UNM/m2016@data$Total_vote, listw = wghts, zero.policy = TRUE)
m2016$UNM <- scale(m2016@data$UNM/m2016@data$Total_vote)
m2016$lag <- lag.listw(wghts, m2016@data$UNM/m2016@data$Total_vote)

m2016$quad_sig <- NA
  
  # high-high quadrant
m2016[(m2016@data$UNM/m2016@data$Total_vote >= 0 & 
          m2016$lag >= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5]))
       , "quad_sig"] <- "high-high"
  # low-low quadrant
  m2016[(m2016@data$UNM/m2016@data$Total_vote <= 0 & 
          m2016$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-low"
  # high-low quadrant
  m2016[(m2016@data$UNM/m2016@data$Total_vote >= 0 & 
          m2016$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "high-low"
  # low-high quadrant
  m2016@data[(m2016@data$UNM/m2016@data$Total_vote <= 0 
             & m2016$lag >= 0) & 
              (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-high"
  # non-significant observations
  m2016@data[(localm[, 5] > 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "not signif."  
  
  m2016$quad_sig <- as.factor(m2016$quad_sig)
  m2016@data$id <- rownames(m2016@data)

df <- fortify(m2016, region="id")
df <- left_join(df, m2016@data)
  
m5 <- ggplot()+
		geom_polygon(data=df, aes(long, lat, group = group, fill = quad_sig))+
		geom_polygon(color = NA)  +
    coord_equal() + 
		geom_polygon(data=fgeorgia, aes(long, lat, group=group), alpha=1/10)+
		scale_fill_manual(name="Clusters",
		                  values=c("high-high"="#d7191c",
		                          "high-low"="#fdae61",
		                          "low-low"="#2c7bb6",
		                          "not signif."= "#bababa"),
		                  label=c("High-high", "High-low", "Low-low", "Not significant"))+
		labs(title = "2016 Parliamentary Elections")+
		theme_void()+
		theme(
				plot.title = element_text(colour = "Black", size=20, family = "Times New Roman"),
				legend.position = "bottom",
				legend.title=element_text(family="Times New Roman", face="bold", size=14),
				legend.text=element_text(family="Times New Roman", face="bold", size=14)
		  )
		  
print(m5)

### 2013 Presidential elections
IDs<-row.names(m2013)
coords<-coordinates(m2013)
m8w <- poly2nb(m2013)
m8wg <- graph2nb(gabrielneigh(coords), row.names = IDs)
m2013$IDs<-row.names(m2013)
wghts<-nb2listw(m8wg,zero.policy=TRUE)


morans_i8 <- moran.mc(m2013@data$Bakradze/m2013@data$Valid_ballots, 
                      listw = wghts, zero.policy = TRUE, nsim = 666)


dtable<-as.data.frame(cbind("Presidential 2013", morans_i8$statistic, morans_i8$p.value))
etable<-rbind(etable, dtable)


cor8 <- sp.correlogram(neighbours = m8w, zero.policy = TRUE, 
                       var = m2013@data$Bakradze/m2013@data$Valid_ballots, 
                       order = 10, method = "I", style = "C", randomisation = TRUE)

lags13 <- as.data.frame(cor8$res)
lags13$lag <- as.numeric(row.names(lags13))
lags13$dataset <- "Presidential 2013"

localm <- localmoran(m2013@data$Bakradze/m2013@data$Valid_ballots, listw = wghts, zero.policy = TRUE)
m2013$Bakradze <- scale(m2013@data$Bakradze/m2013@data$Valid_ballots)
m2013$lag <- lag.listw(wghts, m2013@data$Bakradze/m2013@data$Valid_ballots)

  m2013$quad_sig <- NA
  
  # high-high quadrant
  m2013[(m2013@data$Bakradze/m2013@data$Valid_ballots >= 0 & 
          m2013$lag >= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5]))
       , "quad_sig"] <- "high-high"
  # low-low quadrant
  m2013[(m2013@data$Bakradze/m2013@data$Valid_ballots <= 0 & 
          m2013$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-low"
  # high-low quadrant
  m2013[(m2013@data$Bakradze/m2013@data$Valid_ballots >= 0 & 
          m2013$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "high-low"
  # low-high quadrant
  m2013@data[(m2013@data$Bakradze/m2013@data$Valid_ballots <= 0 
             & m2013$lag >= 0) & 
              (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-high"
  # non-significant observations
  m2013@data[(localm[, 5] > 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "not signif."  
  
  m2013$quad_sig <- as.factor(m2013$quad_sig)
  m2013@data$id <- rownames(m2013@data)

df <- fortify(m2013, region="id")
df <- left_join(df, m2013@data)
  
m4 <- ggplot()+
		geom_polygon(data=df, aes(long, lat, group = group, fill = quad_sig))+
		geom_polygon(color = NA)  +
    coord_equal() + 
		geom_polygon(data=fgeorgia, aes(long, lat, group=group), alpha=1/10)+
		scale_fill_manual(name="Clusters",
		                  values=c("high-high"="#d7191c",
		                          "high-low"="#fdae61",
		                          "low-low"="#2c7bb6",
		                          "not signif."= "#bababa"),
		                  label=c("High-high", "High-low", "Low-low", "Not significant"))+
		labs(title = "2013 Presidential Elections")+
		theme_void()+
		theme(
				plot.title = element_text(colour = "Black", size=20, family = "Times New Roman"),
				legend.position = "bottom",
				legend.title=element_text(family="Times New Roman", face="bold", size=14),
				legend.text=element_text(family="Times New Roman", face="bold", size=14)
		  )
		  
print(m4)

### 2012 Parliamentary elections
IDs<-row.names(m2012)
coords<-coordinates(m2012)
m8w <- poly2nb(m2012)
m8wg <- graph2nb(gabrielneigh(coords), row.names = IDs)
m2012$IDs<-row.names(m2012)
wghts<-nb2listw(m8wg,zero.policy=TRUE)

morans_i8 <- moran.mc(m2012@data$UNM/m2012@data$Total_vote, 
                      listw = wghts, zero.policy = TRUE, nsim = 666)

dtable<-as.data.frame(cbind("Parliamentary 2012", morans_i8$statistic, morans_i8$p.value))
etable<-rbind(etable, dtable)

cor8 <- sp.correlogram(neighbours = m8w, zero.policy = TRUE, 
                       var = m2012@data$UNM/m2012@data$Total_vote, 
                       order = 10, method = "I", style = "C", randomisation = TRUE)

lags12 <- as.data.frame(cor8$res)
lags12$lag <- as.numeric(row.names(lags16))
lags12$dataset <- "Parliamentary 2012"

localm <- localmoran(m2012@data$UNM/m2012@data$Total_vote, listw = wghts, zero.policy = TRUE)
m2012$UNM <- scale(m2012@data$UNM/m2012@data$Total_vote)
m2012$lag <- lag.listw(wghts, m2012@data$UNM/m2012@data$Total_vote)

  m2012$quad_sig <- NA
  
  # high-high quadrant
  m2012[(m2012@data$UNM/m2012@data$Total_vote >= 0 & 
          m2012$lag >= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5]))
       , "quad_sig"] <- "high-high"
  # low-low quadrant
  m2012[(m2012@data$UNM/m2012@data$Total_vote <= 0 & 
          m2012$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-low"
  # high-low quadrant
  m2012[(m2012@data$UNM/m2012@data$Total_vote >= 0 & 
          m2012$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "high-low"
  # low-high quadrant
  m2012@data[(m2012@data$UNM/m2012@data$Total_vote <= 0 
             & m2012$lag >= 0) & 
              (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-high"
  # non-significant observations
  m2012@data[(localm[, 5] > 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "not signif."  
  
  m2012$quad_sig <- as.factor(m2012$quad_sig)
  m2012@data$id <- rownames(m2012@data)

df <- fortify(m2012, region="id")
df <- left_join(df, m2012@data)
  
m3 <- ggplot()+
		geom_polygon(data=df, aes(long, lat, group = group, fill = quad_sig))+
		geom_polygon(color = NA)  +
    coord_equal() + 
		geom_polygon(data=fgeorgia, aes(long, lat, group=group), alpha=1/10)+
		scale_fill_manual(name="Clusters",
		                  values=c("high-high"="#d7191c",
		                          "low-low"="#2c7bb6",
		                          "not signif."= "#bababa"),
		                  label=c("High-high", "Low-low", "Not significant"))+
		labs(title = "2012 Parliamentary Elections")+
		theme_void()+
		theme(
				plot.title = element_text(colour = "Black", size=20, family = "Times New Roman"),
				legend.position = "bottom",
				legend.title=element_text(family="Times New Roman", face="bold", size=14),
				legend.text=element_text(family="Times New Roman", face="bold", size=14)
		  )
		  
print(m3)

### 2008 Parliamentary elections
IDs<-row.names(m2008)
coords<-coordinates(m2008)
m8w <- poly2nb(m2008)
m8wg <- graph2nb(gabrielneigh(coords), row.names = IDs)

m2008$IDs<-row.names(m2008)
wghts<-nb2listw(m8wg,zero.policy=TRUE)

morans_i8 <- moran.mc(m2008@data$UNM/m2008@data$Total_voters, 
                      listw = wghts, zero.policy = TRUE, nsim = 666)

dtable<-as.data.frame(cbind("Parliamentary 2008", morans_i8$statistic, morans_i8$p.value))
etable<-rbind(etable, dtable)

cor8 <- sp.correlogram(neighbours = m8w, zero.policy = TRUE, 
                       var = m2008@data$UNM/m2008@data$Total_voters, 
                       order = 10, method = "I", style = "C",
                       randomisation = TRUE)

lags08 <- as.data.frame(cor8$res)
lags08$lag <- as.numeric(row.names(lags08))
lags08$dataset <- "Parliamentary 2008"

localm <- localmoran(m2008@data$UNM/m2008@data$Total_vote, listw = wghts, zero.policy = TRUE)

m2008$UNM <- scale(m2008@data$UNM/m2008@data$Total_vote)
m2008$lag <- lag.listw(wghts, m2008@data$UNM/m2008@data$Total_vote)

  m2008$quad_sig <- NA
  
  # high-high quadrant
  m2008[(m2008@data$UNM/m2008@data$Total_vote >= 0 & 
          m2008$lag >= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5]))
       , "quad_sig"] <- "high-high"
  # low-low quadrant
  m2008[(m2008@data$UNM/m2008@data$Total_vote <= 0 & 
          m2008$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-low"
  # high-low quadrant
  m2008[(m2008@data$UNM/m2008@data$Total_vote >= 0 & 
          m2008$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "high-low"
  # low-high quadrant
  m2008@data[(m2008@data$UNM/m2008@data$Total_vote <= 0 
             & m2008$lag >= 0) & 
              (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-high"
  # non-significant observations
  m2008@data[(localm[, 5] > 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "not signif."  
  
  m2008$quad_sig <- as.factor(m2008$quad_sig)
  m2008@data$id <- rownames(m2008@data)

df <- fortify(m2008, region="id")
df <- left_join(df, m2008@data)


m2 <- ggplot()+
		geom_polygon(data=df, aes(long, lat, group = group, fill = quad_sig))+
		geom_polygon(color = NA)  +
    coord_equal() + 
		geom_polygon(data=fgeorgia, aes(long, lat, group=group), alpha=1/10)+
		scale_fill_manual(name="Clusters",
		                  values=c("high-high"="#d7191c",
		                           "low-low"="#2c7bb6",
		                          "not signif."= "#bababa"),
		                  label=c("High-high", "Low-low",  "Not significant"))+
		labs(title = "2008 Parliamentary Elections")+
		theme_void()+
		theme(
				plot.title = element_text(colour = "Black", size=20, family = "Times New Roman"),
				legend.position = "bottom",
				legend.title=element_text(family="Times New Roman", face="bold", size=14),
				legend.text=element_text(family="Times New Roman", face="bold", size=14)
		  )
		  
print(m2)

### 2008 Presidential elections
m2008p@data$UNMP <- (m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout)
m2008p <- m2008p[!is.na(m2008p@data$UNMP), ]

IDs<-row.names(m2008p)
coords<-coordinates(m2008p)
m8w <- poly2nb(m2008p)
m8wg <- graph2nb(gabrielneigh(coords), row.names = IDs)

m2008p$IDs<-row.names(m2008p)
wghts<-nb2listw(m8wg,zero.policy=TRUE)



morans_i8 <- moran.mc(m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout, 
                      listw = wghts, zero.policy = TRUE, nsim = 666, na.action = na.exclude)

dtable<-as.data.frame(cbind("Presidential 2008", morans_i8$statistic, morans_i8$p.value))
etable<-rbind(etable, dtable)

cor8 <- sp.correlogram(neighbours = m8w, zero.policy = TRUE, 
                       var = m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout, 
                       order = 10, method = "I", style = "C", randomisation = TRUE)

lags08p <- as.data.frame(cor8$res)
lags08p$lag <- as.numeric(row.names(lags08p))
lags08p$dataset <- "Presidential 2008"

localm <- localmoran(m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout, listw = wghts, zero.policy = TRUE)
m2008p$Mikheil_Saakashvili <- scale(m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout)
m2008p$lag <- lag.listw(wghts, m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout)

  m2008p$quad_sig <- NA
  
  # high-high quadrant
  m2008p[(m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout >= 0 & 
          m2008p$lag >= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5]))
       , "quad_sig"] <- "high-high"
  # low-low quadrant
  m2008p[(m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout <= 0 & 
          m2008p$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-low"
  # high-low quadrant
  m2008p[(m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout >= 0 & 
          m2008p$lag <= 0) & 
         (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "high-low"
  # low-high quadrant
  m2008p@data[(m2008p@data$Mikheil_Saakashvili/m2008p@data$Turnout <= 0 
             & m2008p$lag >= 0) & 
              (localm[, 5] <= 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "low-high"
  # non-significant observations
  m2008p@data[(localm[, 5] > 0.05 & !is.na(localm[, 5])), "quad_sig"] <- "not signif."  
  
  m2008p$quad_sig <- as.factor(m2008p$quad_sig)
  m2008p@data$id <- rownames(m2008p@data)

df <- fortify(m2008p, region="id")
df <- left_join(df, m2008p@data)
  
m1 <- ggplot()+
		geom_polygon(data=df, aes(long, lat, group = group, fill = quad_sig))+
		geom_polygon(color = NA)  +
    coord_equal() + 
		geom_polygon(data=fgeorgia, aes(long, lat, group=group), alpha=1/10)+
		scale_fill_manual(name="Clusters",
		                  values=c("high-high"="#d7191c",
		                          "low-low"="#2c7bb6",
		                          "not signif."= "#bababa"),
		                  label=c("High-high", "Low-low", "Not significant"))+
		labs(title = "2008 Presidential Elections")+
		theme_void()+
		theme(
				plot.title = element_text(colour = "Black", size=24, family = "Times New Roman"),
				legend.position = "bottom",
				legend.title=element_text(family="Times New Roman", face="bold", size=18),
				legend.text=element_text(family="Times New Roman", face="bold", size=18)
		  )
print(m1)


###### Step 4: Make spatial correlogram for the UNM & its presidential candidate vote share

lags <- rbind(lags08p, lags08, lags12, lags13, lags16)

corlg <- ggplot(lags, aes(x=lag, y=V1, group=dataset))+
  geom_point(aes(colour=dataset, shape=dataset), size=4)+
  geom_path(aes(colour=dataset), size=1)+
  scale_color_manual(name="Elections",
					labels = c("Parliamentary 2008", "Parliamentary 2012", "Parliamentary 2016", 
						"Presidential 2008", "Presidential 2013"),
                     values=c("#377eb8", "#e41a1c", "#4daf4a", "#984ea3", "#ff7f00"))+
  scale_shape_manual(name="Elections",
					labels = c("Parliamentary 2008", "Parliamentary 2012", "Parliamentary 2016", 
						"Presidential 2008", "Presidential 2013"),
					values = c(15, 16, 17, 18, 8))+
  # guides(shape=guide_legend(title="Elections"))+
  geom_errorbar(aes(ymin=V1-2*sqrt(V3), ymax=V1+2*sqrt(V3),
                    colour=dataset),
                width = 0.15, size=1)+
  labs(
		# title="Spatial Correlogram of UNM and Its Presidential Candidate Votes, 2008-2016",
       x="Spatial Lag",
       y="Moran's I")+
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.8))+
  scale_x_continuous(
                    #limits = c(1, 10),
                     breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))+
  theme_mplot+
  theme(
	legend.text=element_text(size=20, family = "Times New Roman"),
	legend.title=element_text(size=22, family = "Times New Roman"),
	legend.position = c(0.85, 0.5),
	legend.background = element_rect(fill = "white", size = 1, linetype = "solid")

  )

print(corlg)

ggsave("print/figure1.tiff", corlg, width = 13, height = 10, dpi=300, compression = "lzw")

###### Refer to the qgis_map folder for figure2


###### Step 5: Combine LISA maps for each elections and save it in graphic format as figure3

combi<-grid_arrange_shared_legend(m1, m2, m3, m4, m5,  ncol=2, nrow=3)

print(combi)

ggsave("print/figure3.tiff", combi, width = 13, height = 10, dpi=300, compression = "lzw")

###### Make a table for global Moran's I coefficients

stargazer(etable, summary = FALSE, rownames = FALSE,
          type="html", digits = 1,
          digits.extra = 1,
          out="print/table2.htm")

.
