.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



.testsUrl <- paste0(
    "https://r.acidgenomics.com/testdata/", tolower(.pkgName), "/",
    "v", .pkgVersion$major, ".", .pkgVersion$minor # nolint
)
