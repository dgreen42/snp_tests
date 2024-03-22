library(maps)
library(mapdata)
library(randomForest)

latd_snps <- read.csv("MtLATD SNP data.csv")
fset <- read.csv("Medicago_fset_climatic-country.csv")
sig_snps <- read.csv("significant_snps.csv")

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

snps <- read.csv("snp_TF.csv")

snps$X.2.POS <- latd_snps$X.2.POS
snps$X...1.CHROM <- latd_snps$X...1.CHROM
snpscols <- snps[, 3:ncol(snps)]


shaperecurs <- function(i = 1, n = NULL, base = NULL, shape = NULL) {
  if (i != base + 1) {
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

snum <- shaperecurs(
  i = 2,
  n = nrow(snpscols),
  base = ncol(snpscols),
  shape = shape <- NULL
)

snumat <- matrix(snum, ncol = ncol(snpscols), nrow = nrow(snpscols))

pchframe <- matrix(NA, ncol = ncol(snpscols), nrow = nrow(snpscols))
for (i in 1:ncol(snpscols)) {
  for (j in 1:nrow(snpscols)) {
    if (snpscols[j, i] == 0) {
      print(i + j)
      pchframe[j, i] <- snum[i + j]
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
  if (test > 0) {
    points(snps[, 2], snps[, i + 2], pch = pchframe[, i])
  } else {
    points(snps[, 2], snps[, i + 2], col = "red")
  }
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
