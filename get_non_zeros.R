latd_snps <- read.csv("MtLATD SNP data.csv")
# parse latd snp df
latd_only_snps <- latd_snps[, 3:ncol(latd_snps)]
write.csv(latd_only_snps, file = "latd_snp_pairs.csv", row.names = FALSE)

system2(
  "parse_snp_pairs",
  args = "latd_snp_pairs.csv",
)

snps <- read.csv("snp_TF.csv")
total_snps <- NULL
for (i in 1:ncol(snps)) {
  total_snps[i] <- nrow(snps) - sum(snps[, i])
}


total_snps_df <- data.frame(HM = colnames(snps), total = total_snps)
non_zero <- total_snps_df[total_snps_df$total > 0, ]
write.table(non_zero, "non_zero.txt")
# at this point the non_zero dataframe is written to a text file and I
# copy and pasted in the HM names so they do not have the extension
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
