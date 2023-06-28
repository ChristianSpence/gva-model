# build sic 5 digit to IOAT group lookup

sic <- readRDS("~/Data/Classifications/SIC/sic.RDS")

temp.df <- sic |>
  dplyr::select(`Full Code`) |>
  dplyr::filter(!is.na(`Full Code`))

bres_ioat_lookup <- readr::read_csv("data/bres_ioat_lookup.csv")

ioat.vector <- vector("character", nrow(temp.df))

for (i in 1:nrow(temp.df)) {
  sic <- substr(temp.df$`Full Code`[i], 1 ,4)
  if (sic %in% bres_ioat_lookup$bres_industry) {
    ioat.vector[i] <- bres_ioat_lookup$iaot_industry_code[bres_ioat_lookup$bres_industry == sic]
  } else {
    sic <- substr(temp.df$`Full Code`[i], 1 ,3)
    if (sic %in% bres_ioat_lookup$bres_industry) {
      ioat.vector[i] <- bres_ioat_lookup$iaot_industry_code[bres_ioat_lookup$bres_industry == sic]
    } else {
      sic <- substr(temp.df$`Full Code`[i], 1 ,2)
      if (sic %in% bres_ioat_lookup$bres_industry) {
        ioat.vector[i] <- bres_ioat_lookup$iaot_industry_code[bres_ioat_lookup$bres_industry == sic]
      } else {
        ioat.vector[i] <- NA
      }
    }
  }
}

temp.df$IOAT <- ioat.vector

saveRDS(temp.df, 'data/SIC_IOAT_lookup.rds')
