require(plyr); require(ggplot2)
df <- read.table("age.intersect.batch.aa.txt", header=T, comment="", sep = "")

df.s <- data.frame(sample=as.factor(c(df$X.1_sample_id, df$X2_sample_id)),
                   div = c(df$div1, df$div2),
                   freq = c(df$freq12, df$freq21))

df.1 <- ddply(df.s, .(sample), summarize, div = mean(div), freq = mean(freq))