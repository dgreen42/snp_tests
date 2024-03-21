library(maps)
library(mapdata)
library(randomForest)

data <- read.csv("ORCDRC_all_SNPs_for_percentiles.csv")
ph2_data <- read.csv("pH2_all_SNPs_for_percentiles.csv")
latd_snps <- read.csv("MtLATD SNP data.csv")
fset <- read.csv("Medicago_fset_climatic-country.csv")
sig_snps <- read.csv("significant_snps.csv")

plot(data$p ~ data$Chr)

sig_data <- data[data$p > 0.99, ]

boxplot(sig_data$p ~ sig_data$Chr)

manhattan(data,
  chr = "Chr",
  bp = "Pos",
  p = "p",
  snp = "X",
  highlight = latd_snps$X.2.POS
)



manhattan(ph2_data,
  chr = "Chr",
  bp = "Pos",
  p = "p",
  snp = "X",
  highlight = sig_snps$number
)


# parse latd snp df
latd_only_snps <- latd_snps[, 3:ncol(latd_snps)]
write.csv(latd_only_snps, file = "latd_snp_pairs.csv", row.names = FALSE)
snps <- read.csv("snp_TF.csv")
total_snps <- NULL
for (i in 1:ncol(snps)) {
  total_snps[i] <- nrow(snps) - sum(snps[, i])
}


total_snps_df <- data.frame(HM = colnames(snps), total = total_snps)
non_zero <- total_snps_df[total_snps_df$total > 0, ]
write.table(non_zero, "non_zero.txt")

non_zero_HM <- c(
  "HM006",
  "HM007",
  "HM011",
  "HM038",
  "HM098",
  "HM112",
  "HM126",
  "HM151",
  "HM156",
  "HM163",
  "HM165",
  "HM170",
  "HM178",
  "HM197",
  "HM208",
  "HM218",
  "HM224",
  "HM308",
  "HM311"
)

non_zero$sname <- non_zero_HM

barplot(total ~ sname, non_zero, las = 2, xlab = NULL, ann = FALSE)
title("Number of LATD SNPS in each HM", ylab = "Number of SNPs")

significantHM <- data.frame(
  HM = non_zero$sname,
  totalsnps = non_zero$total
)

fset2 <- fset[, 6:ncol(fset)]
fsetsubset <- matrix(, nrow = nrow(significantHM), ncol = ncol(fset2))
fsetsubset[1, ] <- fset2[1, ]

counter <- 0
for (HM in non_zero_HM) {
  for (test in 1:nrow(fset2)) {
    if (HM == fset$HM_NUMBER[test]) {
      counter <- counter + 1
      print(counter)
      fsetsubset[counter, ] <- fset2[test, ]
    }
  }
}
rm(counter)

snps$X.2.POS <- latd_snps$X.2.POS
snps$X...1.CHROM <- latd_snps$X...1.CHROM
snpscols <- snps[, 3:ncol(snps)]


shaperecurs <- function(i = 1, n = NULL, base = NULL, shape = NULL) {
  if (i != base) {
    shape <- c(shape, rep(i, n))
    i <- i + 1
    return
    shaperecurs(
      i = i,
      n = n,
      base = base,
      shape = shape
    )
  } else {
    return(shape)
  }
}

shape <- NULL
numshape <- shaperecurs(
  i = 1,
  n = n,
  base = 22,
  shape = shape
)

pchframe <- matrix(NA, ncol = ncol(snpscols), nrow = nrow(snpscols))
for (i in 1:ncol(snpscols)) {
  for (j in 1:nrow(snpscols)) {
    if (snpscols[j, i] == 0) {
      pchframe[j, i] <- shape[i]
    } else {
      pchframe[j, i] <- 1
    }
  }
}

plot(NULL,
  xlim = c(min(snps[, 2]), max(snps[, 2])),
  ylim = c(0, 1)
)

for (i in 1:ncol(snps) - 2) {
  test <- sum(snps[, i + 2] == 0)
  points(snps[, 2], snps[, i + 2], pch = pchframe)
}



# fset

boxplot(prec_1 ~ HM_NUMBER, fset[fset$prec_1 > 100, ])
boxplot(prec_1 ~ Country, fset[fset$prec_1 > 100, ])


map_HM <- map("world",
  xlim = c(-20, 60),
  ylim = c(20, 50)
)

map.axes()
points(fset$LON, fset$LAT, pch = 16)

random - forest <- randomForest(HM_NUMBER ~ ., data = fset)
