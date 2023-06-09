here::i_am("data-raw/sdtm/prepareRoxygen.R")
library(here)

options(useFancyQuotes = FALSE)

# Generate basic roxygen documentation
# Thanks to: https://stackoverflow.com/questions/51092509/whats-the-best-way-to-automatically-generate-roxygen2-documentation-for-a-data
paths<-list.files(
    here::here("data"),
    pattern = "sdtm_",
    ignore.case = TRUE,
    full.names = FALSE,
    recursive = TRUE
)

metaCols <- read.csv(here('data-raw','sdtm','meta_var.csv'))
metaDomains <- read.csv(here('data-raw','sdtm','meta_study.csv'))

roxyheaders <- unlist(
    sapply(
        paths,
        function(path){
            d <- substr(path,1,nchar(path)-4)
            load(file=here("data",path))
            df <- get(d)
            metaCol <- metaCols |> filter(paste0("sdtm_",tolower(Form))==tolower(d))
            metaDomain <- metaDomains |> filter(paste0("sdtm_",tolower(Dataset))==tolower(d))

            columns <- paste(
                "#' \\item{\\code{",
                metaCol$Variable,
                "}}{",
                metaCol$Type,
                "-",
                metaCol$Label,"}"
            )
            roxy <- c(
                paste("#' @title", d),
                paste("#' @description", metaDomain$Description,"data frame -",metaDomain$Structure),
                paste("#' @format a data frame with",dim(df)[1], "rows and", dim(df)[2],"columns. See \\url{https://github.com/SafetyGraphics/safetyData/blob/main/data-raw/sdtm/README.md} for more details and full data specification. Columns:"),
                paste("#' \\describe{"),
                columns,
                paste("#'}"),
                dQuote(d),
                "\n"
            )
            return(roxy)
        },
        simplify=FALSE
    ),
    use.names=FALSE
)

cat(roxyheaders, file=here::here("R/sdtm.R"), sep="\n")
