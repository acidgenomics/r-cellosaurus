## Regarding C locale collation during `R CMD check`:
## https://github.com/r-lib/devtools/issues/2121#issuecomment-534173065

test_that("Cellosaurus", {
    ## nolint start
    object <- withr::with_collate(
        new = "C",
        code = {
            Cellosaurus()
        }
    )
    expect_s4_class(object, "Cellosaurus")
    df <- as.data.frame(object)
    x <- as.list(df["CVCL_0001", , drop = FALSE])
    y <- list(
        "accession" = "CVCL_0001",
        "ageAtSampling" = "30Y",
        "atccId" = NA_character_,
        "category" = "Cancer cell line",
        "cellLineName" = "HEL",
        "comments" = list(list(
            "Derived from site" = "In situ; Peripheral blood; UBERON=UBERON_0000178",
            "Donor information" = "Originally the patient was suffering from Hodgkin lymphoma",
            "Doubling time" = "~24 hours (PubMed=6177045); 17.8 +- 1.8 hours (Note=In serum-containing medium), 23.3 +- 0.8 hours (Note=In serum-free medium) (PubMed=7538619); ~36 hours (DSMZ=ACC-11)",
            "Genome ancestry" = "African=2.4%; Native American=1.79%; East Asian, North=3.9%; East Asian, South=0%; South Asian=5.12%; European, North=13.33%; European, South=73.45% (PubMed=30894373)",
            "HLA typing" = c(
                "A*03:01,32:01; B*35:08,35:08; C*04:01,04:01; DQA1*05:01,05:01; DRB1*03:38,13:03 (PubMed=26589293)",
                "A*03:01:01,32:01:01; B*35:08:01,35:01:01; C*04:01:01,04:01:01; DPA1*01:03:01,01:03:01; DPB1*04:01:01,02:01:02 (DSMZCellDive=ACC-11)"
            ),
            "Microsatellite instability" = "Stable (MSS) (PubMed=10739008; PubMed=11226526; Sanger)",
            "Omics" = c(
                "Deep antibody staining analysis",
                "Deep exome analysis",
                "Deep quantitative proteome analysis",
                "DNA methylation analysis",
                "SNP array analysis",
                "Transcriptome analysis by microarray",
                "Transcriptome analysis by RNAseq"
            ),
            "Part of" = c(
                "Cancer Dependency Map project (DepMap) (includes Cancer Cell Line Encyclopedia - CCLE)",
                "COSMIC cell lines project",
                "LL-100 blood cancer cell line panel"
            ),
            "Population" = "Caucasian",
            "Sequence variation" = c(
                "Mutation; HGNC; 6192; JAK2; Simple; p.Val617Phe (c.1849G>T); ClinVar=VCV000014662; Zygosity=Homozygous (PubMed=16408098)",
                "Mutation; HGNC; 11998; TP53; Simple; p.Met133Lys (c.398T>A); ClinVar=VCV002430159; Zygosity=Homozygous (Cosmic-CLP; DepMap)"
            )
        )),
        "crossReferences" = list(list(
            "ArrayExpress" = c(
                "E-MTAB-783",
                "E-MTAB-2706",
                "E-MTAB-2770",
                "E-MTAB-3610",
                "E-MTAB-7721",
                "E-MTAB-7722"
            ),
            "BTO" = "BTO:0000565",
            "BioSample" = c(
                "SAMN03471056",
                "SAMN03472269",
                "SAMN10987697"
            ),
            "CCRID" = c("3101HUMTCHu71", "4201HUM-CCTCC00071"),
            "CCTCC" = "GDC0071",
            "CLDB" = c("cl0", "cl1585"),
            "CLO" = c("CLO_0003679", "CLO_0023565"),
            "CLS" = "305022",
            "Cell_Model_Passport" = "SIDM00594",
            "ChEMBL-Cells" = "CHEMBL3308090",
            "ChEMBL-Targets" = "CHEMBL613888",
            "Coriell" = "GM06141",
            "Cosmic" = c(
                "787432", "798663", "850392", "907053", "919128", "924040",
                "947363", "975251", "976722", "994176", "996314", "1012051",
                "1013868", "1037665", "1070698", "1078733", "1122162",
                "1127268", "1139508", "1139638", "1140092", "1151879",
                "1168467", "1170892", "1171333", "1176583", "1191710",
                "1192825", "1601067", "1604866", "1633512", "1943609",
                "2089667", "2131538", "2306207", "2649243"
            ),
            "Cosmic-CLP" = "907053",
            "DSMZ" = "ACC-11",
            "DSMZCellDive" = "ACC-11",
            "DepMap" = "ACH-000004",
            "EFO" = "EFO_0006579",
            "EGA" = c(
                "EGAS00001000610", "EGAS00001000978", "EGAS00001002554"
            ),
            "GDSC" = "907053",
            "GEO" = c(
                "GSM482506", "GSM743410", "GSM887077", "GSM888147",
                "GSM1028243", "GSM1446763", "GSM1669874"
            ),
            "IARC_TP53" = "21368",
            "JCRB" = "JCRB0062",
            "KCB" = "KCB 200808YJ",
            "LINCS_LDP" = "LCL-1053",
            "LiGeA" = "CCLE_453",
            "MCCL" = "MCC:0000187",
            "PRIDE" = "PXD030304",
            "PharmacoDB" = "HEL_534_2019",
            "Progenetix" = "CVCL_0001",
            "PubChem_Cell_line" = "CVCL_0001",
            "Wikidata" = "Q54882501"
        )),
        "date" = list(c(
            "Created: 04-04-12",
            "Last updated: 29-06-23",
            "Version: 42"
        )),
        "depmapId" = "ACH-000004",
        "diseases" = list(list(
            "NCIt" = "C7152; Erythroleukemia",
            "ORDO" = "Orphanet_318; Acute erythroid leukemia"
        )),
        "hierarchy" = list(character()),
        "isCancer" = TRUE,
        "isContaminated" = FALSE,
        "isProblematic" = FALSE,
        "misspellings" = list(character()),
        "msiStatus" = list("Stable (MSS) (PubMed=10739008; PubMed=11226526; Sanger)"),
        "ncbiTaxonomyId" = list(9606L),
        "ncitDiseaseId" = list("C7152"),
        "ncitDiseaseName" = list("Erythroleukemia"),
        "oncotreeCode" = "AML",
        "oncotreeLevel" = 3L,
        "oncotreeMainType" = "Leukemia",
        "oncotreeName" = "Acute Myeloid Leukemia",
        "oncotreeParent" = "MNM",
        "oncotreeTissue" = "Myeloid",
        "organism" = list("Homo sapiens"),
        "originateFromSameIndividual" = list(character()),
        "population" = list("Caucasian"),
        "referencesIdentifiers" = list(list(
            "CelloPub" = "CLPUB00447",
            "DOI" = "10.1007/978-1-4757-1647-4_13",
            "PubMed" = c(
                "2985879", "3159941", "3874327", "6177045", "7538619",
                "8558913", "9290701", "9510473", "9738977", "10739008",
                "11021758", "11226526", "14504097", "16408098", "20164919",
                "22460905", "23955599", "25485619", "26589293", "27277069",
                "27397505", "29533902", "30285677", "30629668", "30894373",
                "31068700", "31160637", "31978347", "35839778"
            )
        )),
        "samplingSite" = list(paste(
            "In situ",
            "Peripheral blood",
            "UBERON=UBERON_0000178",
            sep = "; "
        )),
        "sangerModelId" = "SIDM00594",
        "secondaryAccession" = NA_character_,
        "sexOfCell" = "Male",
        "strProfileData" = list(list(
            "Amelogenin" = c(
                "X (CCRID; Cosmic-CLP JCRB)",
                "X,Y (COG; DSMZ; PubMed=25877200)"
            ),
            "CSF1PO" = c(
                "10 (Cosmic-CLP; COG; DSMZ; JCRB; PubMed=25877200)",
                "10,11 (CCRID)"
            ),
            "D13S317" = "9,11",
            "D16S539" = "11",
            "D18S51" = c(
                "12,16 (CCRID; DSMZ; PubMed=25877200)",
                "16 (COG)"
            ),
            "D19S433" = "11,13",
            "D21S11" = c(
                "29,30.2 (DSMZ; PubMed=25877200)",
                "29,30.2,31.2 (CCRID; COG)"
            ),
            "D2S1338" = "18,19",
            "D3S1358" = "15",
            "D5S818" = "11",
            "D7S820" = "7",
            "D8S1179" = "13,15",
            "FGA" = c(
                "21,22,23 (CCRID)",
                "22,23 (COG; DSMZ; PubMed=25877200)"
            ),
            "Penta D" = "11,13",
            "Penta E" = "13,18",
            "Source(s)" = "CCRID; Cosmic-CLP; COG; DSMZ; JCRB; PubMed=25877200",
            "TH01" = "7",
            "TPOX" = "11",
            "vWA" = "14,17"
        )),
        "synonyms" = list(c(
            "Hel",
            "GM06141",
            "GM06141B",
            "Human ErythroLeukemia"
        )),
        "webPages" = list(c(
            "https://en.wikipedia.org/wiki/HEL_cell_line",
            "https://www.proteinatlas.org/learn/cellines",
            "https://www.thermofisher.com/ch/en/home/technical-resources/cell-lines/h/cell-lines-detail-346.html"
        ))
    )
    expect_identical(x, y)
    expect_identical(
        object = df["CVCL_0011", "ncitDiseaseId"][[1L]],
        expected = c("C9140", "C80334")
    )
    expect_identical(
        object = df["CVCL_0011", "ncitDiseaseName"][[1L]],
        expected = c(
            "Childhood B acute lymphoblastic leukemia",
            "B lymphoblastic leukemia/lymphoma with t(12;21)(p13.2;q22.1) ETV6-RUNX1"
        )
    )
    ## Check discontinued identifier handling.
    ## CVCL_0455 has 2 DepMap identifiers:
    ## - ACH-000474 - Discontinued
    ## - ACH-001075
    expect_identical(
        object = df["CVCL_0455", "depmapId"],
        expected = "ACH-001075"
    )
    ## nolint end
})
