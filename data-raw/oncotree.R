## OncoTree tumor type metadata.
## Updated 2023-08-24.
##
## See also:
## - OncoTree API tumor type info:
##   http://oncotree.mskcc.org/api/tumorTypes
## - OncoTree API tumor hierarchy info:
##   http://oncotree.mskcc.org/api/tumorTypes/tree
## - DepMap 23q2 "Model.csv" file:
##   https://figshare.com/ndownloader/files/40448834
## - DepMap uses these column names for OncoTree:
##   - "OncotreeCode"
##   - "OncotreeSubtype"
##   - "OncotreePrimaryDisease"
##   - "OncotreeLineage"
## - This script has code that parses the tumor type info:
##   https://github.com/vanallenlab/moalmanac/blob/main/moalmanac/datasources/
##     oncotree/get_oncotree.py
## - This notebook has code that parses the OncoTree hierarchy:
##   https://github.com/PedroSebe/signature-inference/blob/main/workflow/
##     notebooks/tidy_oncotree.ipynb
## - cBioPortal types of cancer
##   http://www.cbioportal.org/webservice.do?cmd=getTypesOfCancer
## - This script has code that parses the cBioPortal types of cancer:
##   https://github.com/ashleyrchen/cancer-web-tool/blob/master/cBioPortal.ipynb

## nolint start
suppressPackageStartupMessages({
    library(AcidPlyr)
    library(pipette)
    library(syntactic)
})
## nolint end

json <- getJSON("http://oncotree.mskcc.org/api/tumorTypes")
df <- rbindToDataFrame(json)
colnames(df) <- camelCase(colnames(df))
df <- df[order(df[["code"]]), ]
rownames(df) <- df[["code"]]
saveRDS(df, "oncotree.rds")
export(df, "oncotree.csv")
