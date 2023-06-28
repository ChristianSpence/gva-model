# replicate Scottish government IO model
# using this rather than UK ONS as SG also imputes type II Leontif multipliers

# import Scottish Industry x Industry

io.industry <- readxl::read_excel("data-raw/Supply_Use_I-O_1998-2019/IxI_wide.xlsx") |>
  # filter for 2019 values only
  dplyr::filter(year == "2019") # |>
  # remove unnecessary columns
  # dplyr::select(-c(table, pubdate, year, ioc098_short_description))

io.matrix <- io.industry |>
  # remove rows outside the core 98x98 matrix
  dplyr::filter(!ioc098_sic %in% c("TDU", "RUKImp", "RoWImp", "TIU",
                                   "TlSPrds", "TlSPrdn",
                                   "CoE", "GOS", "GVA", "TOut")) |>
  # and the columns ...
  dplyr::select(IOC098_001:IOC098_098) |>
  # and convert to a matrix
  as.matrix()

# verify we have column and row names in the matrix
colnames(io.matrix)
rownames(io.matrix) <- colnames(io.matrix)

# replicate the 12-sector model

# create an empty 12 x 12 matrix
io.matrix.12 <- matrix(nrow = 12, ncol = 12)

# bring in the SIC table to generate the 89 to 12 lookup map
sic07 <- readr::read_csv("https://raw.githubusercontent.com/ChristianSpence/SIC2007/main/SIC2007.csv")
# create a new column for the 12 lookups
sic07 <- sic07 |>
  dplyr::mutate(`12-sectors` =
                  dplyr::case_when(SECTION %in% LETTERS[1:6] ~ SECTION,
                                   SECTION %in% c("G", "I") ~ "G, I",
                                   SECTION %in% c("H", "J") ~ "H, J",
                                   SECTION %in% c("K", "L") ~ "K, L",
                                   SECTION %in% c("M", "N") ~ "M, N",
                                   SECTION %in% c("O", "P", "Q") ~ "O-Q",
                                   SECTION %in% c("R", "S", "T") ~ "R-T")) |>
  dplyr::filter(!is.na(`12-sectors`))

# verify we now have twelve sectors
length(unique(sic07$`12-sectors`)) == 12
# > TRUE

lookup <- sic07 |>
  dplyr::select(Division, `12-sectors`) |>
  dplyr::distinct()

industry_lookup <- io.industry |>
  dplyr::select(ioc098_sic) |>
  dplyr::mutate(division = substr(ioc098_sic, 1, 2)) |>
  dplyr::left_join(lookup, by = c("division" = "Division")) |>
  dplyr::filter(!is.na(`12-sectors`)) |>
  dplyr::mutate(ioc_098 = colnames(io.matrix))

matrix.as.df <- as.data.frame(io.matrix)
matrix.as.df$rownames <- rownames(io.matrix)
matrix.as.df <- matrix.as.df |>
  tidyr::pivot_longer(dplyr::starts_with("IOC098"), names_to = "colnames") |>
  dplyr::left_join(industry_lookup, by = c("rownames" = "ioc_098")) |>
  dplyr::left_join(industry_lookup, by = c("colnames" = "ioc_098")) |>
  dplyr::select(rownames = `12-sectors.x`,
                colnames = `12-sectors.y`,
                value)

io.matrix.12 <- matrix.as.df |>
  dplyr::group_by(rownames, colnames) |>
  dplyr::summarise(value = sum(value)) |>
  tidyr::pivot_wider(names_from = colnames) |>
  dplyr::ungroup() |>
  dplyr::select(-rownames) |>
  as.matrix()
rownames(io.matrix.12) <- colnames(io.matrix.12)

# Leontif type I (L) is (I-A)^-1 where I is the identity matrix and A is the io.matrix calculated for direct requirements, i.e. each row value in each column divided by the column total (which is not the sum of the core IxI vector, but instead total output at basic prices)

# Start by extracting the total output at basic prices

TOut <- io.industry |>
  dplyr::filter(ioc098_sic == "TOut") |>
  # remove unnecessary columns
  dplyr::select(IOC098_001:IOC098_098) |>
  # pivot longer for the join
  tidyr::pivot_longer(dplyr::starts_with("IOC098"), names_to = "IOC098") |>
  # bring in the 12 sector model codes
  dplyr::left_join(industry_lookup, by = c("IOC098" = "ioc_098")) |>
  # summarise by the 12-sector codes
  dplyr::group_by(`12-sectors`) |>
  dplyr::summarise(value = sum(value))

TOut.vector <- TOut$value

A <- matrix(rep(0, 12*12), nrow = 12, ncol = 12)
for (i in 1:12) {
  col <- io.matrix.12[ , i]
  if (sum(col) == 0) {
    col <- rep(0, 12)
  } else {
    # TODO not sum(col) but instead Total output at basic prices
    col <- col / TOut.vector[i]
  }
  A[, i] <- col
}

# create an identity matrix dim 12
I <- diag(12)

# and subtract A from it and then invert
# This is the Leontif type I matrix
L <- solve(I - A)

# Leontif type II ---------------------------------------------------------

# To create a type ii matrix, we start with the A matrix from above, but we then also endogenise the household sector, i.e. we create a new row and column n the matrix that represents the household sector as if it were an industry of its own. These represent:
 # (row) compensation of employees
 # (col) household expenditure

A

# In the Scottish example, we use total HH income from all sources (in £m, as the main matrix)
# This can be obtained from the sum of the primary resources total and secondary resources total from ONS Regional GDHI, Blue Book 2021 consistent. (Table 6, Components of total GDHI at current basic prices)
HHI <- 156241

# To calculate this, we need to extract two other vectors from the io.industry object: Final use (consumers) and Compensation of Employees

# Final use (consumers) (HOUSEHOLD)

FUc <- io.industry |>
  dplyr::select(ioc098_sic, HOUSEHOLD) |>
  dplyr::filter(!is.na(HOUSEHOLD)) |>
  dplyr::inner_join(industry_lookup, by = c("ioc098_sic")) |>
  dplyr::group_by(`12-sectors`) |>
  dplyr::summarise(HOUSEHOLD = sum(HOUSEHOLD))

# Compensation of employees

CoE <- io.industry |>
  dplyr::filter(ioc098_sic == "CoE") |>
  dplyr::select(dplyr::starts_with("IOC098", ignore.case = FALSE)) |>
  tidyr::pivot_longer(dplyr::starts_with("IOC098"), names_to = "ioc_098") |>
  dplyr::inner_join(industry_lookup, by = c("ioc_098")) |>
  dplyr::group_by(`12-sectors`) |>
  dplyr::summarise(CoE = sum(value))

# We can now place these two vectors into a second A matrix (A2)

A2 <- A
A2 <- rbind(A2, CoE$CoE / TOut.vector)
A2 <- cbind(A2, c(FUc$HOUSEHOLD, NA) / HHI)
# R13 C13 currently NA; should be zero
A2[13,13] <- 0

# Construct identity matrix
I2 <- diag(13)

# And solve by calculating inverse of the difference of identity and A matrix
L2 <- solve(I2 - A2)

# Multipliers and effects -------------------------------------------------

# Methodology paper does not provide estimates for these; they are only available in the full 98-sector format. To verify correct calculation, we'll now need to calculate L1 and L2 (the two inverse Leontif matrices (type I and type II)) for the full 98-sector dataset.

# io.matrix is our 98-sector matrix

# first create our direct requirements matrix, A (A98)
# This is the cells of io.matrix divided by the Tout

TOut98 <- io.industry |>
  dplyr::filter(ioc098_sic == "TOut") |>
  # remove unnecessary columns
  dplyr::select(IOC098_001:IOC098_098) |>
  tidyr::pivot_longer(dplyr::everything())

A98 <- matrix(rep(0, 98 * 98), nrow = 98, ncol = 98)

for (i in 1:98) {
  col <- io.matrix[, i]
  if (TOut98$value[i] == 0) {
    col <- rep(0, 98)
  } else {
    col <- col / TOut98$value[i]
  }
  A98[,i] <- col
}

I98 <- diag(98)

L1.98 <- solve((I98 - A98))

A98_2 <- A98

consumers_expenditure <- io.industry |>
  dplyr::select(HOUSEHOLD) |>
  dplyr::filter(!is.na(HOUSEHOLD))

consumers_expenditure <- consumers_expenditure$HOUSEHOLD[1:98]

compensation_employees <- io.industry |>
  dplyr::filter(ioc098_sic == "CoE") |>
  dplyr::select(dplyr::starts_with("IOC098", ignore.case = FALSE)) |>
  tidyr::pivot_longer(dplyr::everything())

compensation_employees <- compensation_employees$value

total_output <- io.industry |>
  dplyr::filter(ioc098_sic %in% c("TOut", "CoE", "GVA")) |>
  dplyr::select(ioc098_sic,
                dplyr::starts_with("IOC098", ignore.case = FALSE)) |>
  tidyr::pivot_longer(-1) |>
  tidyr::pivot_wider(names_from = ioc098_sic)

tout <- total_output$TOut
tout_minus_coe <- total_output$TOut - total_output$CoE
gva <- total_output$GVA

A98_2 <- rbind(A98_2, compensation_employees / tout)
A98_2 <- cbind(A98_2, c(consumers_expenditure, 0) / HHI)
A98_2[99, 19] <- 0 # correct for NaN error (/0)

# create an identity matrix
I99 <- diag(99)

# subtract the A matrix from the identity matrix and invert (solve() with only one parameter inverts a matrix, i.e. this does (I-A)^-1)
L2.98 <- solve(I99 - A98_2)

# Output multiplier
# Sum of all outputs from each industry required to produce one additional unit of output.
# Type 1 = Column sums from Leontif 1
# Type 2 = column sums from Leontif 2 of industry rows only (i.e exclude CoE, row 99)

OMult1 <- colSums(L1.98)
OMult2 <- colSums(L2.98[1:98, 1:98])

# Income multiplier
# Increase in income from employment that results from a change of £1 of income from employment in each industry

# v = IfE (income from employment) / total output
v_i <- compensation_employees / total_output$TOut
v_i[19] <- 0

IMult1 <- colSums(L1.98 * v_i) / v_i
IMult2 <- colSums(L2.98[1:98, 1:98] * v_i) / v_i

# Income effects
IEff1 <- colSums(L1.98 * v_i)
IEff2 <- colSums(L2.98[1:98, 1:98] * v_i)

# GVA multiplier
g_i <- gva / tout
g_i[19] <- 0

Gmult1 <- colSums(L1.98 * g_i) / g_i
Gmult2 <- colSums(L2.98[1:98, 1:98] * g_i) / g_i

# GVA effects
GEff1 <- colSums(L1.98 * g_i)
GEff2 <- colSums(L2.98[1:98, 1:98] * g_i)

# Employment multiplier
# w_i is FTE per £ of total output
# we need both BRES and WFJ data to calculate this

# import BRES
ioc_lookup <- readxl::read_excel("data-raw/Supply_Use_I-O_1998-2019/_SIC(2007)_to_IOC098.xlsx", col_types = "text") |>
  dplyr::mutate(SIC = ifelse(nchar(SIC) == 4, paste0("0", SIC), SIC))

bres <- readr::read_csv("data-raw/bres2019scotland.csv") |>
  dplyr::select(INDUSTRY_CODE, EMPLOYMENT_STATUS_NAME, OBS_VALUE) |>
  tidyr::pivot_wider(names_from = EMPLOYMENT_STATUS_NAME, values_from = OBS_VALUE) |>
  dplyr::mutate(Other = Employment - (`Full-time employees` + `Part-time employees`)) |>
  dplyr::mutate(FTE = `Full-time employees` + (0.5 * `Part-time employees`) + Other) |>
  dplyr::left_join(ioc_lookup, by = c("INDUSTRY_CODE" = "SIC")) |>
  dplyr::group_by(IOC098_long) |>
  dplyr::summarise(FTE = sum(FTE, na.rm = TRUE)) |>
  dplyr::bind_rows(data.frame(IOC098_long = "IOC098_071", FTE = 0)) |>
  dplyr::arrange(IOC098_long)
bres <- bres[1:98,]


# TODO FIX FTE ISSUE

w_i <- bres$FTE / tout
w_i[19] <- 0

# Employment multipliers

Emult1 <- colSums(L1.98 * w_i) / w_i
Emult2 <- colSums(L2.98[1:98, 1:98] * w_i) / w_i

# Employment effects
Eeff1 <- colSums(L1.98 * w_i)
Eeff2 <- colSums(L2.98[1:98, 1:98] * w_i)

