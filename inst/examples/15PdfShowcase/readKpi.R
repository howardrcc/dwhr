addAlias <- function(kpi, kpiKey, level, level1Key, sort, kpiId = NULL,level2Groups,url,gs) {

    from <- kpi[kpi$kpiCode == kpiKey,]
    parent <- kpi[kpi$kpiCode == level1Key,]
    
    return(rbind(
        kpi,
        data.table(
            kpiId = isNull(kpiId,from$kpiId),
            level1Label = parent$kpiLabel,
            level1Code = level1Key,
            level2Label = from$kpiLabel,
            level2Code = from$kpiCode,
            level3Label = from$kpiLabel,
            level3Code = from$kpiCode,
            level4Label = from$kpiLabel,
            level4Code = from$kpiCode,
            level5Label = from$kpiLabel,
            level5Code = from$kpiCode,
            kpiNaam = from$kpiLabel,
            format = from$format,
            sorting = from$sorting,
            level1Sort = parent$level1Sort,
            level2Sort = sort,
            level3Sort = NA,
            level4Sort = NA,
            level5Sort = NA,
            level1Tooltip = parent$level1Tooltip,
            level2Tooltip = from$level2Tooltip,
            level3Tooltip = NA,
            level4Tooltip = NA,
            level5Tooltip = NA,
            level2Groups = level2Groups,
            url = url,
            normNaam = from$normNaam,
            level = level,
            kpiCode = from$kpiCode,
            kpiLabel = from$kpiLabel,
            kpiParent = parent$kpiLabel,
            isAlias = TRUE,
            gs = gs)))
    
    
}



readKpi <- function() {

    kpi <- read.csv(  
        file = paste0(getwd(),"/data/ds_d_kpi.txt"),
        header = FALSE,
        encoding = 'UTF-8',
        sep = ";",
        col.names = 
            c("kpiId",
              "level1Label",
              "level1Code",
              "level2Label",
              "level2Code" ,
              "level3Label",
              "level3Code",
              "level4Label",
              "level4Code",
              "level5Label",
              "level5Code",
              "kpiNaam",
              "format",
              "sorting",
              "level1Sort",
              "level2Sort",
              "level3Sort",
              "level4Sort",
              "level5Sort",
              "level1Tooltip",
              "level2Tooltip",
              "level3Tooltip",
              "level4Tooltip",
              "level5Tooltip",
              "level2Groups",
              "url",
              "normNaam",
              "level",
              "gs",
              "isAlias"), 
        stringsAsFactors = FALSE)
    
    # uniekmakers van de labels (spatie ervoor) onder productieplan (krijgen anders dubbeltellingen) 
    kpi$level5Label[kpi$level2Label == 'Sieges by Region'] <- paste0(' ',kpi$level5Label[kpi$level2Label == 'Sieges by Region'])
    kpi$level5Code[kpi$level2Label == 'Sieges by Region'] <- paste0(kpi$level5Code[kpi$level2Label == 'Sieges by Region'],'|x')
    kpi$level4Label[kpi$level2Label == 'Sieges by Region'] <- paste0(' ',kpi$level4Label[kpi$level2Label == 'Sieges by Region'])
    kpi$level4Code[kpi$level2Label == 'Sieges by Region'] <- paste0(kpi$level4Code[kpi$level2Label == 'Sieges by Region'],'|x')
    kpi$level3Label[kpi$level2Label == 'Sieges by Region'] <- paste0(' ',kpi$level3Label[kpi$level2Label == 'Sieges by Region'])
    kpi$level3Code[kpi$level2Label == 'Sieges by Region'] <- paste0(kpi$level3Code[kpi$level2Label == 'Sieges by Region'],'|x')

    kpi$level1Tooltip <- as.character(kpi$level1Tooltip)
    kpi$level2Tooltip <- as.character(kpi$level2Tooltip)
    kpi$level3Tooltip <- as.character(kpi$level3Tooltip)
    kpi$level4Tooltip <- as.character(kpi$level4Tooltip)
    kpi$level5Tooltip <- as.character(kpi$level5Tooltip)

    kpi$isAlias <- as.logical(kpi$isAlias)
    kpi$gs <- as.logical(kpi$gs)

    kpi$kpiCode <- kpi$level5Code
    kpi$kpiLabel <- kpi$level5Label
    kpi$kpiParent <- ifelse(kpi$level == 2,kpi$level1Label,
                            ifelse(kpi$level == 3, kpi$level2Label,
                                   ifelse(kpi$level == 4, kpi$level3Label,
                                          ifelse(kpi$level == 5, kpi$level4Label,'Alle kpi\'s'))))
    
    kpi <- addAlias(kpi, kpiKey = 'XXXXX3', level = 2, level1Key = '20004|kpi', sort = 1, kpiId = '54|kpi', level2Groups = 'Iron Bank Ledger', url = 'https://example.com/54|kpi',gs = 1)
    kpi <- addAlias(kpi, kpiKey = 'XXXXX4', level = 2, level1Key = '20004|kpi', sort = 1, kpiId = '55|kpi', level2Groups = 'Iron Bank Ledger', url = 'https://example.com/55|kpi',gs = 1)

    kpi$level2Groups[kpi$level2Groups == 'Sieges by Burden' & kpi$level1Label == 'Realm Overview'] <- paste0(
        '<a href="javascript:void(0);" onclick="rowGroupEvent(\'s1Kpi\',\'Productieplan - bedrijfsdrukte\');"',
        'data-toggle="tooltip" title="Navigeer naar alle indicatoren van Productieplan - bedrijfsdrukte" data-placement="right">Productieplan - bedrijfsdrukte</a>')
    kpi$level2Groups[kpi$level2Groups == 'Iron Bank Ledger' & kpi$level1Label == 'Realm Overview'] <- paste0(
        '<a href="javascript:void(0);" onclick="rowGroupEvent(\'s1Kpi\',\'Financien\');"',
        'data-toggle="tooltip" title="Navigeer naar alle indicatoren van Financien" data-placement="right">Financien</a>')
    kpi$level2Groups[kpi$level2Groups == 'Bannerman Levies' & kpi$level1Label == 'Realm Overview'] <- paste0(
        '<a href="javascript:void(0);" onclick="rowGroupEvent(\'s1Kpi\',\'Personele ontwikkeling\');"',
        'data-toggle="tooltip" title="Navigeer naar alle indicatoren van Personele ontwikkeling" data-placement="right">Personele ontwikkeling</a>')
    
    kpi <- data.frame(kpi)
    
    kpi
}
