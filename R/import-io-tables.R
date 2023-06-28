# import I-O data and build matrix

io_tables_path <- "data-raw/publicationtablesbb22.xlsx"

# identify worksheets for 2019
io_tables_sheets <- readxl::excel_sheets(io_tables_path)
io_tables_sheets_2019 <- io_tables_sheets[grepl("2019", io_tables_sheets)]

temp <- readxl::read_excel(io_tables_path, sheet = io_tables_sheets_2019[2],
                           skip = 3, na = "-")
temp.matrix <- as.matrix(temp[-1, ])
rownames(temp.matrix) <- temp.matrix[ , 1]
temp.matrix <- temp.matrix[ , -(1:2)]


colnames(temp.matrix)[ncol(temp.matrix)] <- "Total intermediate demand"
# build row names from across the first two columns of temp
rownames <- temp[[1]][-1]
rownames[106:111] <- temp[[2]][107:112]
rownames(temp.matrix) <- rownames

# convert dimensions to numeric and tidy up
io.matrix <- apply(temp.matrix, c(1, 2), as.numeric)

# write final object out to data
saveRDS(io.matrix, "data/io.matrix.rds")
#####################



# TODO need to remove totals, and separate the three nested matrices in the core one.

# From there we'll build out to the type I multiplier tables but need to work out whether we can do GVA and employment separately or just combined
# After that it's we'll try out the type II throuhg endogenous HH accounts
