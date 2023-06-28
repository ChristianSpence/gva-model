# UK I-O model

# import core IOAT file from ONS
# https://www.ons.gov.uk/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesindustrybyindustry

ioat.path <- "data-raw/nasu1719in.xlsx"
ioat <- readxl::read_excel(ioat.path, sheet = "IOT", skip = 3, col_names = FALSE)

# We'll reduce the col and row names to codes, so we build a human lookup

ioat.row.lookup <- ioat[4:117, 1:2] |>
  setNames(c("code", "desc"))
# fill in the rows with missing codes
ioat.row.lookup[107:109, 1] <- c("Imp", "TlSPrds", "TIUPurch")

ioat.col.lookup <- data.frame(code = ioat[1, 3:119] |> as.vector() |> unlist(),
                              desc = ioat[2, 3:119] |> as.vector() |> unlist()
)

# build a matrix from ioat removing the rows now not needed and
# naming the columns and rows and correcting data types

ioat.matrix <- ioat[-(1:3), -(1:2)] |>
  dplyr::mutate(dplyr::across(.fns = as.numeric)) |>
  as.matrix()
rownames(ioat.matrix) <- ioat.row.lookup$code
colnames(ioat.matrix) <- ioat.col.lookup$code

# de-construct this matrix into its three component matrices, B, F and W

B.matrix <- ioat.matrix[ , 1:105]
F.matrix <- ioat.matrix[1:105, 106:117]
W.matrix <- ioat.matrix[106:114, 1:105]

# Now we construct our A matrix
# First, build an empty matrix of appropriate dims and names
A <- matrix(rep(0, nrow(B.matrix) * ncol(B.matrix)),
                nrow = nrow(B.matrix), ncol = ncol(B.matrix),
            dimnames = list(rownames(B.matrix),
                            colnames(B.matrix)
                            )
            )
# and then populate it
for (i in 1:ncol(B.matrix)) {
  col <- B.matrix[ ,i]
  col <- col / W.matrix["P1", i]
  A[ ,i] <- col
}

# Construct an identity matrix
I <- diag(ncol(A))

# Build the Leontif inverse (removing the non-industry rows)
L <- solve(I - A[1:105,])


### Effects and multipliers (type 1)

# Output
Omult <- colSums(L)


# Income
# ratio of compensation of employees to total output
v <- W.matrix["D1", ] / W.matrix["P1", ]

Imult <- colSums(L * v) / v
Ieff <- colSums(L * v)

# GVA
# ratio of GVA to total output
g <- W.matrix["GVA", ] / W.matrix["P1", ]

Gmult <- colSums(L * g) / g
Geff <- colSums(L * g)

### Leontif type 2

# First step is to endogenise the household sector and add it to the core 'B' matrix as an additional row and column.
# To be able to calculate this, we need household income from all sources (in millions)
primary_resources_total <- 1658578
secondary_resources_total <- 421799
HHI <- primary_resources_total + secondary_resources_total

household_consumption_expenditure <- F.matrix[ ,"P3 S14"]
compensation_of_employees <- B.matrix["D1", ]

A2 <- A[1:105, 1:105]
A2 <- rbind(A2, compensation_of_employees / W.matrix["P1", ])
A2 <- cbind(A2, c(household_consumption_expenditure, 0) / HHI)

# construct and identity matrix (1 row and col larger than previous as we have the household row and column as well)

I2 <- diag(ncol(A2))

L2 <- solve(I2 - A2)

### Effects and multipliers (type 2)

# Output
Omult2 <- colSums(L2)


# Income

Imult2 <- colSums(L2[1:105, 1:105] * v) / v
Ieff2 <- colSums(L2[1:105, 1:105] * v)

# GVA

Gmult2 <- colSums(L2[1:105, 1:105] * g) / g
Geff2 <- colSums(L2[1:105, 1:105] * g)

