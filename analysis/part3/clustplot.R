require(ggplot2); require(hexbin); require(gridExtra); require(reshape)

df <- read.table("age.F2.batch_intersect_mdscoords.txt", header=TRUE, comment ="", sep = "\t")
#df <- read.table("plots/mds.coords.aa.D.txt", header=TRUE, comment ="", sep = "\t")
dm <- read.table("metadata.txt", header=TRUE, comment ="", sep = "\t")
df <- merge(df,dm,by.x="lbl",by.y="label")

# compute distance, fit model
df$r <- sqrt((df$x-mean(df$x))^2+(df$y-mean(df$y))^2)
mdl <- lm(r ~ age, df)
eq <- substitute(italic(distance) == a + b%.% italic(age)*","~~italic(r)^2~"="~r2,
     list(a = format(coef(mdl)[1], digits = 2),
          b = format(coef(mdl)[2], digits = 2),
         r2 = format(summary(mdl)$r.squared, digits = 3)))
eq.lbl<-as.character(as.expression(eq))

pal <- colorRampPalette(c("#feb24c", "#31a354", "#2b8cbe"))

g1 <- ggplot() + geom_smooth(data = df, aes(x=age, y=r)) + geom_point(data = df, aes(x=age, y=r, color = age)) + scale_color_gradientn(colours=pal(nrow(df)))+
       geom_line(data = df, aes(x=age, y=r), stat="smooth", method = 'lm', formula = y~x, se=FALSE, linetype ="dashed", size = 1.0, color="black") +
       theme_bw() +
	   scale_x_continuous(expand=c(0,0)) + scale_y_continuous(name = "distance", expand=c(0,0), limits=c(0,max(df$r)), oob=scales::rescale_none) +
	   geom_text(aes(x = max(df$age), y = min(df$r), label = eq.lbl), hjust=1.1, vjust=-1.2, parse = TRUE) +
	   theme(panel.grid.minor = element_blank())#, panel.grid.major = element_blank())

g2 <- ggplot(df,aes(x=x,y=y, z=age)) + 
   stat_summary_hex(bins=5,color="black") + 
   scale_fill_gradientn(colours=pal(nrow(df))) + 
   theme_bw() + ylab("") + xlab("") +#scale_x_continuous(name="", limits=c(-2,2)) + scale_y_continuous(name="", limits=c(-1.5,1.5)) + 
   theme(panel.background = element_rect(fill = "transparent"),
   legend.position="none", 
   plot.margin = unit(c(0,0,0,0), "cm"),
   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border=element_rect(size=2))
  
pdf("clust-dist.pdf")
 
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.4, height = 0.4, x = 0.3, y = 0.75) #plot area for the inset map
print(g1,vp=v1) 
print(g2,vp=v2)
  
dev.off()

df <- read.table("age.annot.trdb.summary.txt", header=TRUE, comment ="", sep = "\t")
df$sample_freq_in_matches <- as.vector(sapply(as.character(df$sample_freq_in_matches),as.numeric))#log10(as.vector(sapply(as.character(df$sample_freq_in_matches),as.numeric)))
df$average_clone_size <- log10(df$sample_freq_in_matches/df$sample_diversity_in_matches)

g1 <- ggplot(df, aes(age, average_clone_size)) + geom_point(aes(color = age)) + scale_color_gradientn(colours=pal(nrow(df)))+
      geom_smooth() + theme_bw() + ylab("Mean clonotype size, log10") + theme(legend.position="none") 

g2 <- ggplot(df, aes(age, sample_diversity_in_matches)) + geom_point(aes(color = age)) + scale_color_gradientn(colours=pal(nrow(df))) +
      geom_smooth() + theme_bw() + scale_y_continuous(name = "Number of clonotypes", limits = c(0,130), expand=c(0,0)) + theme(legend.position="none")

df <- read.table("age.diversity.txt", header=TRUE, comment ="", sep = "\t")
df$conv <- df$clones_nt/df$clones_aa-1

g3 <- ggplot(df, aes(age, conv)) + geom_point(aes(color = age)) + scale_color_gradientn(colours=pal(nrow(df))) +
      geom_smooth() + theme_bw() + ylab("Convergence") + theme(legend.position="none")

ge <- ggplot() + geom_point(aes(1,1), colour="white")+
     theme(                              
       plot.background  = element_blank(), 
       panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(), 
       panel.border     = element_blank(), 
       panel.background = element_blank(),
       axis.title.x     = element_blank(),
       axis.title.y     = element_blank(),
       axis.text.x      = element_blank(),
       axis.text.y      = element_blank(),
       axis.ticks       = element_blank()
     )
     
pdf("clust-details.pdf", useDingbats=FALSE)

grid.arrange(g3, g2, g1, ge, ge, ge, ncol =3, nrow=2)
      
dev.off()

#df$af <- cut(df$age, c(0, 1, 25, 50, 75, max(df$age)), include.lowest=TRUE)

#df.m <- melt(df, id.vars = "age", 
#             measure.vars = c("sample_freq_in_matches", "average_clone_size", "match_size", "sample_diversity_in_matches", "db_diversity_in_matches"))

#ggplot(subset(df.m, variable %in% c("average_clone_size", "sample_diversity_in_matches")), aes(age, value)) + 
#      geom_smooth() +
#      theme_bw() + ylab("") + scale_x_continuous(expand=c(0,0)) +
#      facet_grid(variable ~ ., scales = "free_y")

#df <- read.table("age.diversity.txt", header=TRUE, comment ="", sep = "\t")
#df$conv <- df$clones_nt/df$clones_aa-1
#ggplot(melt(tmp, id.vars = "age"), aes(age,value)) + geom_smooth() + facet_grid(variable ~ ., scales = "free_y")
#
#
#pairwise.t.test(df$average_clone_size, df$af, p.adjust.method = "holm")
