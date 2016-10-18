require(ggplot2); require(reshape); require(lsr)

df <- read.table("age.txt", header=TRUE)
# convert age to factor
df$af <- cut(df$age, c(0, 1, 25, 50, 75, max(df$age)), include.lowest=TRUE)

df.m <- melt(df)

con <- file("age-pval-summary.txt")
sink(con, append=TRUE)
sink(con, append=TRUE, type="output")

out <- data.frame(fig=character(), var1=character(), var2=character(), F=numeric(), p=numeric(), etaSq=numeric(), stringsAsFactors=FALSE)
getPF <- function(expr) {
   a<-aov(expr,df)
   c(summary(a)[[1]][["F value"]][[1]], summary(a)[[1]][["Pr(>F)"]][[1]], etaSquared(a)[1])
}
getPtable <- function(fig, v, f, ...) {
	cat("\n", fig, ": ", v, " vs ", f, "\n")
	x <- df[, v]
	y <- df[, f]
	n <- is.na(x) | is.na(y)
	x <- x[!n]
    y <- y[!n]
	pairwise.t.test(x, y, p.adjust.method = "holm", ...)[[3]]
}


# CB

pdf("fig1-cb.pdf")
ggplot() + geom_boxplot(data = subset(df.m, variable %in% c("mean.insert.sz", "mean.ndn.sz", "oof.fraction")), 
           aes(af, value)) + 
           facet_grid(variable ~ ., scales = "free") + 
           theme_bw() + xlab("") + ylab("")
invisible(dev.off())

out[1,]<-c("fig1", "mean.insert.sz", "af", getPF(mean.insert.sz ~ af))
out[2,]<-c("fig1", "mean.ndn.sz", "af", getPF(mean.ndn.sz ~ af))
out[3,]<-c("fig1", "oof.fraction", "af", getPF(oof.fraction ~ af))

getPtable("fig1", "mean.insert.sz", "af")
getPtable("fig1", "mean.ndn.sz", "af")
getPtable("fig1", "oof.fraction", "af")

# FACS

pdf("fig2-naive-in-subsets.pdf")
ggplot() + geom_boxplot(data = subset(df.m, variable %in% c("naive.of.cd3")),#, "naive.of.cd4", "naive.of.cd8")), 
           aes(af, value))+#, fill = variable)) + scale_fill_grey(start = .4, end = .9) + 
           theme_bw() + xlab("") + ylab("") + theme(legend.position = c(1, 1), legend.justification = c(1, 1))
invisible(dev.off())

out[4,]<-c("fig2", "naive.of.cd3", "af", getPF(naive.of.cd3 ~ af))
#out[4,]<-c("fig2", "naive.of.cd4", "af", getPF(naive.of.cd4 ~ af))
#out[5,]<-c("fig2", "naive.of.cd8", "af", getPF(naive.of.cd8 ~ af))

getPtable("fig2", "naive.of.cd3", "af")
#getPtable("fig2", "naive.of.cd4", "af")
#getPtable("fig2", "naive.of.cd8", "af")

pdf("fig3-naive-of-cd3.pdf")
ggplot() + geom_boxplot(data = subset(df.m, variable %in% c("naive.cd4.of.cd3", "naive.cd8.of.cd3")), 
           aes(af, value, fill = variable)) + scale_fill_grey(start = .4, end = .9) + 
           theme_bw() + xlab("") + ylab("") + theme(legend.position = c(1, 1), legend.justification = c(1, 1))
invisible(dev.off())

out[5,]<-c("fig3", "naive.cd4.of.cd3", "af", getPF(naive.cd4.of.cd3 ~ af))
out[6,]<-c("fig3", "naive.cd8.of.cd3", "af", getPF(naive.cd8.of.cd3 ~ af))

getPtable("fig3", "naive.cd4.of.cd3", "af")
getPtable("fig3", "naive.cd8.of.cd3", "af")

pdf("fig4-cd48-ratios.pdf")
ggplot() + geom_boxplot(data = subset(df.m, variable %in% c("cd4.to.cd8", "cd4.naive.to.cd8.naive", "naive.cd4.cd8.odds")), 
           aes(af, value)) + 
           facet_grid(variable ~ ., scales = "free") + 
           theme_bw() + xlab("") + ylab("")
invisible(dev.off())

out[7,] <-c("fig4", "cd4.to.cd8", "af", getPF(cd4.to.cd8 ~ af))
out[8,] <-c("fig4", "cd4.naive.to.cd8.naive", "af", getPF(cd4.naive.to.cd8.naive ~ af))
out[9,]<-c("fig4", "naive.cd4.cd8.odds", "af", getPF(naive.cd4.cd8.odds ~ af))

getPtable("fig4", "cd4.to.cd8", "af")
getPtable("fig4", "cd4.naive.to.cd8.naive", "af")
getPtable("fig4", "naive.cd4.cd8.odds", "af")

pdf("fig5-div300k.pdf")
ggplot() + geom_boxplot(data = subset(df.m, variable %in% c("clones.per.0.3mln")), 
           aes(af, value)) + 
           theme_bw() + xlab("") + ylab("")
invisible(dev.off())

out[10,] <-c("fig5", "clones.per.0.3mln", "af", getPF(clones.per.0.3mln ~ af))

getPtable("fig5", "clones.per.0.3mln", "af")

pdf("fig6-abs-counts.pdf")
df.m$faceta <- rep("Naive", nrow(df.m))
df.m$faceta[df.m$variable=="abs.cd3"] <- "Absolute CD3"
df.m2 <- na.omit(df.m[,c("af","variable", "faceta", "value")])
ggplot() + geom_boxplot(data = subset(df.m2, variable %in% c("abs.naive.cd4", "abs.naive.cd8")), 
           aes(af, value, fill = variable)) +
           scale_fill_grey(start = .4, end = .9) +
           geom_boxplot(data = subset(df.m2, variable %in% c("abs.cd3")), 
           aes(af, value)) +
           facet_grid(faceta ~ ., scales = "free") + 
           theme_bw() + xlab("") + ylab("") + theme(legend.position = c(1, 1), legend.justification = c(1, 1))
invisible(dev.off())

out[11,] <-c("fig6", "abs.naive.cd4", "af", getPF(abs.naive.cd4 ~ af))
out[12,] <-c("fig6", "abs.naive.cd8", "af", getPF(abs.naive.cd8 ~ af))

getPtable("fig6", "abs.naive.cd4", "af", pool.sd = TRUE)
getPtable("fig6", "abs.naive.cd8", "af", pool.sd = TRUE)

# Sex

pdf("fig7-sex.pdf")
ggplot() + geom_boxplot(data = subset(df.m, variable %in% c("clones.per.0.3mln") & sex %in% c("M","F")), 
           aes(af, value, fill = sex)) + scale_fill_grey(start = .4, end = .9, na.value="white") +
           theme_bw() + xlab("") + ylab("") + theme(legend.position = c(1, 1), legend.justification = c(1, 1))
invisible(dev.off())

out[13,] <-c("fig7", "clones.per.0.3mln", "sex", getPF(clones.per.0.3mln ~ sex))

cat("\nfig7: clones.per.0.3mln vs sex in age\n")
ll <- levels(df$af)
res <- data.frame(af=ll, p=numeric(length(ll)))
for (i in 1:length(ll)) {
    df.s <- subset(df.m, af == ll[i] & variable == "clones.per.0.3mln")
	res[i,2] <- t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
}
res
res$p <- p.adjust(res$p)
cat("\nfig7: clones.per.0.3mln vs sex in age ADJUSTED\n")
res

pdf("fig7-sex-details-1.pdf")
df.m1 <- subset(df.m, variable %in% c("naive.of.cd3", "naive.cd4.of.cd3", "naive.cd8.of.cd3") & af=="(1,25]")
ggplot() + geom_boxplot(data = df.m1, aes(sex, value, fill = sex)) + 
           facet_grid(. ~ variable, space = "free") + scale_fill_grey(start = .4, end = .9) + 
                      ylim(c(0,80)) +
           theme_bw() + xlab("") + ylab("")+ guides(fill = FALSE)
invisible(dev.off())

pdf("fig7-sex-details-2.pdf")
df.m2 <- subset(df.m, variable %in% c("naive.of.cd3", "naive.cd4.of.cd3", "naive.cd8.of.cd3") & af=="(25,50]")
ggplot() + geom_boxplot(data = df.m2, aes(sex, value, fill = sex)) + 
           facet_grid(. ~ variable, space = "free") + scale_fill_grey(start = .4, end = .9) + 
           ylim(c(0,80)) +
           theme_bw() + xlab("") + ylab("")+ guides(fill = FALSE)
invisible(dev.off())

pdf("fig7-sex-details-3.pdf")
df.m3 <- subset(df.m, variable %in% c("naive.of.cd3", "naive.cd4.of.cd3", "naive.cd8.of.cd3") & af=="(50,75]")
ggplot() + geom_boxplot(data = df.m3, aes(sex, value, fill = sex)) + 
           facet_grid(. ~ variable, space = "free") + scale_fill_grey(start = .4, end = .9) + 
                      ylim(c(0,80)) +
           theme_bw() + xlab("") + ylab("")+ guides(fill = FALSE)
invisible(dev.off())

pdf("fig7-sex-details-4.pdf")
df.m4 <- subset(df.m, variable %in% c("naive.of.cd3", "naive.cd4.of.cd3", "naive.cd8.of.cd3") & af=="(75,103]")
ggplot() + geom_boxplot(data = df.m4, aes(sex, value, fill = sex)) + 
           facet_grid(. ~ variable, space = "free") + scale_fill_grey(start = .4, end = .9) + 
                      ylim(c(0,80)) +
           theme_bw() + xlab("") + ylab("")+ guides(fill = FALSE)
invisible(dev.off())

cat("\nfig7-details: naive.of.cd3 vs sex\n")
cat("\nfGroup1\n")
df.s<-subset(df.m1, variable == "naive.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfGroup2\n")
df.s<-subset(df.m2, variable == "naive.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfGroup3\n")
df.s<-subset(df.m3, variable == "naive.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfGroup4\n")
df.s<-subset(df.m4, variable == "naive.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfig7-details: naive.cd4.of.cd3 vs sex\n")
df.s<-subset(df.m1, variable == "naive.cd4.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfGroup2\n")
df.s<-subset(df.m2, variable == "naive.cd4.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfGroup3\n")
df.s<-subset(df.m3, variable == "naive.cd4.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfGroup4\n")
df.s<-subset(df.m4, variable == "naive.cd4.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfig7-details: naive.cd8.of.cd3 vs sex\n")
df.s<-subset(df.m1, variable == "naive.cd8.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfGroup2\n")
df.s<-subset(df.m2, variable == "naive.cd8.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfGroup3\n")
df.s<-subset(df.m3, variable == "naive.cd8.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]
cat("\nfGroup4\n")
df.s<-subset(df.m4, variable == "naive.cd8.of.cd3")
t.test(subset(df.s, sex == "M")$value, subset(df.s, sex == "F")$value)[[3]]

sink()

write.table(out, "age-aov-summary.txt", quote = FALSE, sep = "\t", row.names = FALSE)

# anova ~ sex
# summary(aov(clones.per.0.3mln~age+sex,df)), etaSquared(a)[2]