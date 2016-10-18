r <- sqrt(0.541)
n <- 73
z <- 0.5 * log((1+r)/(1-r))
zse <- 1/sqrt(n-3)
min(pnorm(z, sd=zse), pnorm(z, lower.tail=F, sd=zse))*2


df <- read.table("age.annot.trdb.summary.txt", header=TRUE, comment ="", sep = "\t")
df$sample_freq_in_matches <- as.vector(sapply(as.character(df$sample_freq_in_matches),as.numeric))
df$average_clone_size <- log10(df$sample_freq_in_matches/df$sample_diversity_in_matches)
df$af <- cut(df$age, c(0, 1, 25, 50, 75, max(df$age)), include.lowest=TRUE)
pairwise.t.test(df$average_clone_size, df$af)

10^(mean(subset(df, af == "(50,75]")$average_clone_size)-mean(subset(df, af == "[0,1]")$average_clone_size))
10^(mean(subset(df, af == "(75,103]")$average_clone_size)-mean(subset(df, af == "[0,1]")$average_clone_size))

