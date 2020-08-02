#' Cellosaurus metadata table
#'
#' @export
#' @note Updated 2020-08-02.
#'
#' @param x `character`.
#'   Cellosaurus identifiers (e.g. `CVCL_*`.)
#'
#' @examples
#' cellosaurusTable("CVCL_1045")
cellosaurusTable <- function(
    x,
    BPPARAM = BiocParallel::bpparam()
) {
    assert(
        isCharacter(x),
        allAreMatchingFixed(x, "CVCL_")
    )
    list <- bplapply(
        X = x,
        FUN = function(id) {
            url <- paste0("https://web.expasy.org/cellosaurus/", id, ".txt")
            response <- GET(url)
            content <- content(response, as = "text")
            lines <- strsplit(content, split = "\n")[[1L]]
            ## Disease: NCIt
            ncit <- .str_subset_and_match(
                string = lines,
                pattern = "^DI   NCIt; (C[0-9]+); (.+)$"
            )
            if (identical(nrow(ncit), 1L)) {
                ncitDiseaseID <- ncit[1L, 2L]
                ncitDiseaseName <- ncit[1L, 3L]
            } else {
                ncitDiseaseID <- NULL
                ncitDiseaseName <- NULL
            }

            tibble(
                cellosaurus_id = id,
                ncitDiseaseID = ncitDiseaseID,
                ncitDiseaseName = ncitDiseaseName
            )


            ## Useful stuff:
            ## Disease 	Bladder carcinoma (NCIt: C4912)
            ## Species of origin 	Homo sapiens (Human) (NCBI Taxonomy: 9606)
            ## Sex of cell 	Male
            ## Age at sampling 	68Y
            ## Category 	Cancer cell line
            ##
            ## Cell line collections 	AddexBio; C0002001/8
            ##ATCC; HTB-9
            ##BCRC; 60061
            ##BCRJ; 0026
            ##CLS; 300105/p667_5637
            ##DSMZ; ACC-35
            ##ICLC; HTL01017
            ##KCLB; 30009
            ##NCBI_Iran; C450
            ##RCB; RCB1191
            ##RCB; RCB3676
            ##TKG; TKG 0605
            ##Cell line databases/resources 	CLDB; cl1
            ##CLDB; cl105
            ##CLDB; cl7118
            ##CCLE; 5637_URINARY_TRACT
            ##CCRID; 3111C0001CCC000242
            ##CCRID; 3131C0001000700001
            ##Cell_Model_Passport; SIDM00807
            ##Cosmic-CLP; 687452
            ##DepMap; ACH-000905
            ##IGRhCellID; 5637
            ##LINCS_HMS; 50001
            ##LINCS_LDP; LCL-1702
            ##SKY/M-FISH/CGH; 17
            ##
            ##Ontologies 	BTO; BTO:0003137
            ##CLO; CLO_0001421
            ##CLO; CLO_0050845
            ##EFO; EFO_0002096
            ##MCCL; MCC:0000018
            ##
            ## Wikidata; Q54603866
        },
        BPPARAM = BPPARAM
    )


}
