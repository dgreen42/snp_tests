library(qqman)
library(maps)
library(mapdata)

data <- read.csv('ORCDRC_all_SNPs_for_percentiles.csv')
ph2_data <- read.csv("pH2_all_SNPs_for_percentiles.csv")
latd_snps <- read.csv("MtLATD SNP data.csv")
fset <- read.csv("Medicago_fset_climatic-country.csv")
fset <- fset[2:nrow(fset),]
sig_snps <- read.csv("significant_snps.csv")

plot(data$p ~ data$Chr)

sig_data <- data[data$p > 0.99,]

boxplot(sig_data$p ~ sig_data$Chr)

manhattan(data, chr = "Chr", bp = "Pos", p = "p", snp = "X", highlight = latd_snps$X.2.POS)



manhattan(ph2_data, chr = "Chr", bp = "Pos", p = "p", snp = "X", highlight = sig_snps$number)

# parse latd snp df
latd_only_snps <- latd_snps[,3:ncol(latd_snps)]

parse_snps <- function(snp_df = data.frame(NULL)) {
    
}

# fset

boxplot(prec_1 ~ HM_NUMBER, fset[fset$prec_1 > 100, ])
boxplot(prec_1 ~ Country, fset[fset$prec_1 > 100, ])


map_HM <- map("world", xlim = c(-20, 60), ylim = c(20, 50))
map.axes()
points(fset$LON, fset$LAT, pch = 16)
