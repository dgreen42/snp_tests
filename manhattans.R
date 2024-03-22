library(qqman)

data <- read.csv("ORCDRC_all_SNPs_for_percentiles.csv")
ph2_data <- read.csv("pH2_all_SNPs_for_percentiles.csv")

plot(data$p ~ data$Chr)

sig_data <- data[data$p > 0.99, ]

boxplot(sig_data$p ~ sig_data$Chr)

jpeg("ORCDRC SNPS with LATD SNPS highlighted.jpeg")
manhattan(data,
  chr = "Chr",
  bp = "Pos",
  p = "p",
  snp = "X",
  highlight = latd_snps$X.2.POS
)
title("ORCDRC SNPS with LATD SNPS highlighted")
dev.off()

jpeg("ph2 SNPS with LATD SNPS highlighted.jpeg")
manhattan(ph2_data,
  chr = "Chr",
  bp = "Pos",
  p = "p",
  snp = "X",
  highlight = sig_snps$number
)
title("ph2 SNPS with LATD SNPS highlighted")
dev.off()

jpeg("ph2 SNPS with LATD SNPS highlighted.jpeg")
manhattan(ph2_data,
  chr = "Chr",
  bp = "Pos",
  p = "p",
  snp = "X",
  highlight = latd_snps$X.2.POS
)
title("ph2 SNPS with LATD SNPS highlighted")
dev.off()
