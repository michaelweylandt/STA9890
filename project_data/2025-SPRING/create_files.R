## Prepare Data for Prediction Competition
## HCAD Appraisals
## Goal: Predict 2019(?) assessment values per parcel
##
## Covariates provided
## 1) Unique Parcel ID
## 2) 2015, 2016, 2017, 2018 assessment values
## 3) Land Area
## 4) Living Area
## 5) Neighborhood Code
## 6) Neighborhood Group Code
## 7) Standardized info?
## 8) Account numbers for some neighbors(??)

set.seed(123456789)
SALT <- "SECRET CODE GOES HERE"

library(MASS)
library(here)
library(glue)
library(tidyverse)
library(digest)
conflicted::conflicts_prefer(dplyr::select)

md5_salt <- function(x){
    Digest <- Vectorize(digest, "object")
    Digest(paste0(SALT, as.character(x)), algo="sha1")
}

options(timeout = max(300, getOption("timeout")))


TARGET_YEAR <- 2019
PRIOR_YEARS <- c(2015, 2016, 2017, 2018)

YEARS <- c(PRIOR_YEARS, TARGET_YEAR)

# Download necessary data files
for(year in YEARS){
    year_dir <- here("project_data", "2025-SPRING", "HCAD_raw", year)

    if(!dir.exists(year_dir)) dir.create(year_dir, showWarnings=FALSE, recursive=TRUE)

    real_property_file <- file.path(year_dir, "REALPROPERTY.zip")
    if(!file.exists(real_property_file)){
        real_property_url <- glue("https://download.hcad.org/data/CAMA/{year}/Real_acct_owner.zip")
        download.file(real_property_url, destfile=real_property_file)
    }

    building_info_file <- file.path(year_dir, "BUILDINGINFO.zip")
    if(!file.exists(building_info_file)){
        building_info_url <- glue("https://download.hcad.org/data/CAMA/{year}/Real_building_land.zip")
        download.file(building_info_url, destfile=building_info_file)
    }
}

LOAD_APPRAISAL_INFO <- function(yy){
    fname <- unzip(here("project_data",
                        "2025-SPRING",
                        "HCAD_raw",
                        yy,
                        "REALPROPERTY.zip"),
                   "real_acct.txt")

    INFO <- readr::read_delim(fname) |>
        filter(state_class=="A1") |>
        select(acct,
               yr,
               site_addr_1,
               site_addr_2,
               site_addr_3,
               school_dist,
               map_facet,
               key_map,
               Neighborhood_Code,
               Neighborhood_Grp,
               Market_Area_1,
               yr_impr,
               bld_ar,
               land_ar,
               bld_val,
               land_val,
               assessed_val,
               protested) |>
        mutate(acct = str_trim(acct),
               map_facet = str_trim(map_facet),
               school_dist = as.integer(str_trim(school_dist)),
               Neighborhood_Code = as.character(Neighborhood_Code),
               Neighborhood_Grp = as.character(Neighborhood_Grp),
               protested = if_else(protested == "Y", TRUE, FALSE))

    if(yy == TARGET_YEAR){
        has_bld_area <- "bld_area" %in% colnames(INFO)
        if(!has_bld_area) INFO <- INFO |> rename(bld_area=bld_ar)
        has_land_area <- "land_area" %in% colnames(INFO)
        if(!has_land_area) INFO <- INFO |> rename(land_area=land_ar)
        INFO |> rename("assessed_{yy}":=assessed_val,
                       "protested_{yy}":=protested,
                       "building_value_{yy}":=bld_val,
                       "land_value_{yy}":=land_val,
                       "building_area_{yy}":=bld_area,
                       "land_area_{yy}":=land_area
        )
    } else {
        has_bld_area <- "bld_area" %in% colnames(INFO)
        if(!has_bld_area) INFO <- INFO |> rename(bld_area=bld_ar)
        has_land_area <- "land_area" %in% colnames(INFO)
        if(!has_land_area) INFO <- INFO |> rename(land_area=land_ar)
        INFO |>
            select(acct,
                   assessed_val,
                   protested,
                   bld_val,
                   land_val,
                   bld_area,
                   land_area)|>
            rename("assessed_{yy}":=assessed_val,
                   "protested_{yy}":=protested,
                   "building_value_{yy}":=bld_val,
                   "land_value_{yy}":=land_val,
                   "building_area_{yy}":=bld_area,
                   "land_area_{yy}":=land_area)
    }
}

LOAD_DETAILS <- function(yy, salt_acct=TRUE){
    ZIP <- here("project_data",
                "2025-SPRING",
                "HCAD_raw",
                yy,
                "BUILDINGINFO.zip")

    # Exterior information (size of built area mainly)
    fname <- unzip(ZIP, "exterior.txt")
    EXTERIOR <- read_delim(fname) |>
        semi_join(ALL_Y, join_by(acct)) |>
        select(-bld_num, -sar_cd) |>
        pivot_wider(id_cols=acct,
                    names_from=sar_dscr,
                    values_from=area,
                    values_fn=sum,
                    values_fill = 0) |>
        mutate(floor_area_primary=`BASE AREA PRI`,
               floor_area_upper=`BASE AREA UPR`,
               floor_area_lower=`BASE AREA LWR` + `BASEMENT` + `PART BASEMENT` + `UNFIN BASEMENT LWR`,
               garage_area=`MAS/BRK GARAGE PRI` + `FRAME GARAGE PRI` + `FRAME GARAGE LWR` + `CARPORT PRI` + `MAS/BRK GARAGE LWR`,
               porch_area=`OPEN FRAME PORCH PRI`+`OPEN MAS PORCH PRI` + `ENCL FRAME PORCH PRI` + `OPEN FRAME PORCH UPR` + `OPEN MAS PORCH UPR` + `OPEN FRAME PORCH LWR` + `ENCL MAS PORCH PRI`,
               patio_area=`MAS/CONC PATIO PRI`+`STONE/TILE PATIO PRI`+`MAS/CONC PATIO LWR`+`STONE/TILE PATIO UPPER` + `STONE/TILE PATIO LWR`,
               deck_area=`WOOD DECK PRI` + `WOOD DECK UPR` + `WOOD DECK LWR`,
               mobile_home_area=`MOBILE HOME 12-14 Width` + `MOBILE HOME 15-19 Width`+ `MOBILE HOME 20-29 Width`+ `MOBILE HOME 8-11 Width`+`MOBILE HOME > 30 Width`
               ) |>
        select(acct,
               floor_area_primary,
               floor_area_upper,
               floor_area_lower,
               garage_area,
               porch_area,
               deck_area,
               mobile_home_area)


    ## Rough room counts
    fname <- unzip(ZIP, "fixtures.txt")
    FIXTURES <- read_delim(fname) |>
        semi_join(ALL_Y, join_by(acct)) |>
        select(-type, -bld_num) |>
        pivot_wider(id_cols=acct,
                    names_from=type_dscr,
                    values_from=units,
                    values_fill=0,
                    values_fn=sum) |>
        mutate(floors=`Story Height Index`,
            half_bath=`Room:  Half Bath`,
            full_bath=`Room:  Full Bath`,
            total_rooms=`Room:  Total`,
            bedrooms=`Room:  Bedroom`,
            fireplaces=`Fireplace: Metal Prefab` + `Fireplace: Masonry Firebrick`+`Fireplace: Direct Vent`,
            elevator=`Elevator Stops` + `Elev:  Elect / Pass` > 0) |>
        select(acct,
               floors,
               half_bath,
               full_bath,
               total_rooms,
               bedrooms,
               fireplaces,
               elevator)

    ## Building history (not sure what this is for)
    fname <- unzip(ZIP, "building_res.txt")
    HISTORY <- read_delim(fname) |>
        mutate(acct=str_trim(acct)) |>
        filter(bld_num==1) |>
        semi_join(ALL_Y, join_by(acct)) |>
        mutate(acct=acct,
               quality=str_trim(qa_cd),
               quality_description=dscr,
               year_built=date_erected,
               year_remodeled = as.integer(str_trim(yr_remodel))) |>
        select(acct,
               quality,
               quality_description,
               year_built,
               year_remodeled)

    ## Various structural elements
    fname <- unzip(ZIP, "structural_elem1.txt")
    STRUCTURE <- read_delim(fname) |>
        mutate(acct=str_trim(acct),
               type=str_trim(type)) |>
        semi_join(ALL_Y, join_by(acct)) |>
        filter(type %in% c("CDU", "FND", "GRD", "HAC", "PCR", "XWR"))

    STRUCTURE_XWR <- STRUCTURE |>
        filter(type == "XWR") |>
        mutate(exterior_walls = case_when(
            str_detect(category_dscr, "Frame") ~ "Concrete Block",
            str_detect(category_dscr, "Veneer") ~ "Brick Veneer",
            str_detect(category_dscr, "Masonry") ~ "Brick Masonry",
            str_detect(category_dscr, "Stucco") ~ "Stucco",
            str_detect(category_dscr, "Stone") ~ "Stone",
            str_detect(category_dscr, "Vinyl") ~ "Vinyl",
            TRUE ~ "Other")) |>
        filter(bld_num == 1) |>
        select(acct, exterior_walls) |>
        unique() |>
        group_by(acct) |>
        summarize(exterior_walls = paste(sort(exterior_walls), collapse=" and "))

    STRUCTURE_CDU <- STRUCTURE |>
        filter(type == "CDU", bld_num==1) |>
        select(acct, category_dscr) |>
        rename(building_condition=category_dscr)

    STRUCTURE_FND <- STRUCTURE |>
        filter(type == "FND",
               bld_num==1) |>
        mutate(foundation_type=case_when(
            str_detect(category_dscr, "Crawl") ~ "Crawl Space",
            str_detect(category_dscr, "Basement") ~ "Basement",
            str_detect(category_dscr, "Piers") ~ "Pier and Beam",
            str_detect(category_dscr, "Slab") ~ "Slab",
        )) |>
        group_by(acct) |>
        summarize(foundation_type=paste(sort(foundation_type), collapse=" and "))

    STRUCTURE_GRD <- STRUCTURE |>
        filter(type=="GRD", bld_num==1) |>
        select(acct=acct, grade=category_dscr)

    STRUCTURE_HAC <- STRUCTURE |>
        filter(type=="HAC", bld_num==1) |>
        select(acct, category_dscr, code) |>
        pivot_wider(id_cols=acct,
                    names_from=category_dscr,
                    values_from=code,
                    values_fill=0,
                    values_fn=sum) |>
        mutate(
            has_cooling = `Central Heat/AC` + `A/C Only` > 0,
            has_heat=`Central Heat/AC` + `Central Heat` > 0) |>
        select(acct,
               has_cooling,
               has_heat)

    STRUCTURE_PCR <-
        STRUCTURE |>
        filter(type=="PCR",
               bld_num==1) |>
        select(acct=acct,
               physical_condition=category_dscr)

    ALL_STRUCTURE <- STRUCTURE_CDU |>
        full_join(STRUCTURE_FND, join_by(acct)) |>
        full_join(STRUCTURE_GRD, join_by(acct)) |>
        full_join(STRUCTURE_HAC, join_by(acct)) |>
        full_join(STRUCTURE_PCR, join_by(acct)) |>
        full_join(STRUCTURE_XWR, join_by(acct)) |>
        mutate(year=yy)

    FINAL <- full_join(EXTERIOR, FIXTURES,
              join_by(acct)) |>
        full_join(HISTORY, join_by(acct)) |>
        full_join(ALL_STRUCTURE, join_by(acct))

    if(salt_acct){
        FINAL |> mutate(acct=md5_salt(acct))
    } else {
        FINAL
    }
}

if(!exists("ALL_APPRAISALS")){
    CACHE <- here("project_data", "2025-SPRING", "cache", "ALL_APPRAISALS.rds")
    if(file.exists(CACHE)){
        ALL_APPRAISALS <- readRDS(CACHE)
    } else {
        ALL_APPRAISALS <- map(YEARS, LOAD_APPRAISAL_INFO, .progress = TRUE)
        saveRDS(ALL_APPRAISALS, CACHE)
    }
}


if(!exists("ALL_Y")){
    CACHE <- here("project_data", "2025-SPRING", "cache", "ALL_Y.rds")
    if(file.exists(CACHE)){
        ALL_Y <- readRDS(CACHE)
    } else {
        ALL_Y <- reduce(ALL_APPRAISALS,
                        function(x, y) full_join(x, y, join_by(acct))) |>
            filter(!is.na(assessed_2019)) |>
            select(-yr, -site_addr_1, -site_addr_2, -site_addr_3) |>
            mutate(map_facet = str_trim(map_facet)) |>
            mutate(map_facet = ifelse(nzchar(map_facet), map_facet, NA)) |>
            # Don't hide account info until later
            mutate(#acct=md5_salt(acct),
                map_facet=md5_salt(map_facet),
                key_map=md5_salt(key_map),
                Neighborhood_Code=md5_salt(Neighborhood_Code),
                Neighborhood_Grp=md5_salt(Neighborhood_Grp),
                Market_Area_1=md5_salt(Market_Area_1),
                year_improved=as.integer(yr_impr)) |>
            select(acct,
                   contains("area"),
                   contains("value"),
                   assessed_2015,
                   protested_2015,
                   assessed_2016,
                   protested_2016,
                   assessed_2017,
                   protested_2017,
                   assessed_2018,
                   protested_2018,
                   assessed_2019,
                   protested_2019,
                   school_dist,
                   map_facet,
                   key_map,
                   Neighborhood_Code,
                   Neighborhood_Grp,
                   Market_Area_1) |>
            rename(neighborhood=Neighborhood_Grp,
                   subneighborhood=Neighborhood_Code,
                   region=Market_Area_1,
                   zone=map_facet) |>
            select(-key_map) |>
            mutate(TARGET=assessed_2019)
        saveRDS(ALL_Y, CACHE)
    }
}

if(!exists("ALL_DETAILS")){
    CACHE <- here("project_data", "2025-SPRING", "cache", "ALL_DETAILS.rds")
    if(file.exists(CACHE)){
        ALL_DATA <- readRDS(CACHE)
    } else {
        ALL_DETAILS <- map(YEARS, LOAD_DETAILS, .progress = TRUE)
        saveRDS(ALL_DETAILS, CACHE)
    }
}

for(ix in seq_along(ALL_DETAILS)){
    DETAILS <- ALL_DETAILS[[ix]]
    y <- mean(DETAILS$year, na.rm=TRUE)
    fname <- here("project_data",
                  "2025-SPRING",
                  "students",
                  glue("building_details_{y}.csv"))
    readr::write_csv(DETAILS, fname)
}

stop()

## Create 60/20/20 split for training/public leader/final test
ALL_Y_FOR_DISTRIB <- ALL_Y |> mutate(acct=md5_salt(acct))

N <- NROW(ALL_Y)

N_TRAIN <- floor(0.6 * N)
TRAIN_IX <- sample(N, N_TRAIN)

Y_TRAIN <- ALL_Y_FOR_DISTRIB[TRAIN_IX,]

TEST_IX <- setdiff(1:N, TRAIN_IX)

Y_TEST <- ALL_Y_FOR_DISTRIB[TEST_IX,]

Y_TEST_FOR_STUDENTS <- Y_TEST |>
    select(-TARGET,
           -assessed_2019,
           -building_value_2019,
           -land_value_2019)

readr::write_csv(Y_TRAIN,
                 here("project_data",
                 "2025-SPRING",
                 "students",
                 "assessment_history_train.csv"))

readr::write_csv(Y_TEST_FOR_STUDENTS,
                 here("project_data",
                      "2025-SPRING",
                      "students",
                      "assessment_history_test.csv"))

Y_TEST_FOR_KAGGLE <- Y_TEST |> select(acct, TARGET) |> rename(ACCOUNT=acct)

readr::write_csv(Y_TEST_FOR_KAGGLE,
                 here("project_data",
                      "2025-SPRING",
                      "kaggle",
                      "solution.csv"))

AVG_APPRAISAL <- Y_TRAIN |> pull(TARGET) |> mean()

Y_TEST_EXAMPLE_SOLUTION <- Y_TEST |> select(ACCOUNT=acct) |> mutate(TARGET=AVG_APPRAISAL)

readr::write_csv(Y_TEST_EXAMPLE_SOLUTION,
                 here("project_data",
                      "2025-SPRING",
                      "kaggle",
                      "example.csv"))
