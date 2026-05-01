library(data.table)
library(dwhr)
library(stlplus)
library(shiny)

source('func.R', local = TRUE)

bestuurder <- read.csv(  
    file = paste0(getwd(),"/data/ds_bestuurder.txt"),
    header = FALSE,
    encoding = 'UTF-8',
    strip.white = TRUE,
    sep = ";",
    col.names = c("rvbLid","afdeling"), 
    colClasses = c('character','character'),
    stringsAsFactors = FALSE)

gs <- data.frame(
    gsCode = c('E','O','D'),
    level1Code = c('E','O','D'),
    level1Label = c('Crown', 'Bannermen', 'Onbekend'),
    stringsAsFactors = FALSE)

kpl <- read.csv(  
    file = paste0(getwd(),"/data/ds_d_kostenplaats.txt"),
    header = FALSE,
    encoding = 'UTF-8',
    sep = ";",
    col.names = c("kostenplaatsId","level1Label","level2Label","level2Code","level3Label","level3Code"), 
    colClasses = c('integer','character','character','character','character','character'),
    stringsAsFactors = FALSE)

per <- read.csv(  
    file = paste0(getwd(),"/data/ds_d_periodemaand.txt"),
    header = FALSE,
    encoding = 'UTF-8',
    sep = ";",
    col.names = 
        c("periodemaandId",
          "level1Label",
          "level3Label",
          "isLaatstGesloten"), 
    colClasses = c('integer','character','character','character'),
    stringsAsFactors = FALSE)

per$level2Label <- paste0(per$level1Label,'-Q',(((per$periodemaandId %% 100) - 1) %/% 4) + 1)

per$level1Code <- per$level1Label  
per$level2Code <- per$level2Label  
per$level3Code <- as.character(per$periodemaandId)

per$jaarCode <- per$level1Code
per$jaarLabel <- per$level1Label

per$tertiaalCode <- per$level2Code
per$tertiaalLabel <- per$level2Label

per$maandCode <- per$level3Code
per$maandLabel <- per$level3Label

opn <- as.data.table(read.csv(
    file = paste0(getwd(),"/data/ds_f_kpi_opname.txt"),
    header = FALSE,
    encoding = 'UTF-8',
    sep = ";",
    col.names = 
        c("kpiId",
          'periodemaandId',
          'kostenplaatsId',
          'opnameNr'),
    colClasses = c('character','integer','integer','character'),
    stringsAsFactors = FALSE))

pat <- as.data.table(read.csv(
    file = paste0(getwd(),"/data/ds_f_kpi_patient.txt"),
    header = FALSE,
    encoding = 'UTF-8',
    sep = ";",
    col.names =
        c("kpiId",
          'periodemaandId',
          'kostenplaatsId',
          'patCount',
          'inDeMaand'),
    colClasses = c('character','integer','integer','integer','integer'),
    stringsAsFactors = FALSE))

facts <- as.data.table(read.csv( 
    file = paste0(getwd(),"/data/kpiRvb.txt"),
    header = FALSE,
    encoding = 'UTF-8',
    sep = ";",
    col.names = 
        c('kpiId',
          'periodemaandId',
          'kostenplaatsId',
          'tellerReal',
          'noemerReal',
          'tellerRealVj',
          'noemerRealVj',
          'tellerNorm',
          'noemerNorm',
          'tellerProg',
          'noemerProg',
          'inDeMaand',
          'gsCode'),
    colClasses = c('character','integer','integer','numeric','numeric','numeric','numeric','numeric','numeric','numeric','numeric','integer','character'),
    stringsAsFactors = FALSE))



inMaand <- data.frame(
    inDeMaand = c(0,1),
    level1Label = c('Up to the Moon', 'Within the Moon'), 
    stringsAsFactors = FALSE)

mnd4 <- data.frame(
  mnd4 = c(0,1),
  level1Label = c('notMnd4', 'mnd4'), 
  stringsAsFactors = FALSE)

nunq <- data.frame(
  nunq = c(0,1),
  level1Label = c('unq', 'nunq'), 
  stringsAsFactors = FALSE)

laatstGesloten <- per$maandLabel[per$isLaatstGesloten == 'J']
laatstGeslotenId <- per$periodemaandId[per$isLaatstGesloten == 'J']

laatstGeslotenIdT <- per$tertiaalCode[per$isLaatstGesloten == 'J']

if ((laatstGeslotenId %% 4) != 0) {
    laatstGeslotenIdT <- max(per$tertiaalCode[per$tertiaalCode < laatstGeslotenIdT])    
}

laatstGeslotenT <- max(per$tertiaalLabel[per$tertiaalCode == laatstGeslotenIdT])

#
# toevoegen jaar kolommen 
#

ditJaar <- laatstGeslotenId %/% 100

facts[, c('jaar','vorigJaar') := list(
    periodemaandId %/% 100, 
    (periodemaandId %/% 100) - 1)]


zz <- rbind(
    facts[facts$periodemaandId == (100*ditJaar + 12) & facts$inDeMaand == 0,
          ][, c('tellerProgHjT','noemerProgHjT','tellerNormHjT', 'noemerNormHjT') := list(
              tellerProg,
              noemerProg,
              tellerNorm,
              noemerNorm)],
    facts[facts$periodemaandId == (100*(ditJaar - 1) + 12) & facts$inDeMaand == 0,
          ][, c('tellerProgHjT','noemerProgHjT','tellerNormHjT', 'noemerNormHjT') := list(
              tellerReal,
              noemerReal,
              tellerNorm,
              noemerNorm)],
    facts[facts$periodemaandId == (100*(ditJaar - 2) + 12) & facts$inDeMaand == 0,
          ][,c('tellerProgHjT','noemerProgHjT','tellerNormHjT', 'noemerNormHjT') := list(
              tellerReal,
              noemerReal,
              tellerNorm,
              noemerNorm)]
)

facts[zz, on = c(jaar = 'jaar', kpiId = 'kpiId', kostenplaatsId = 'kostenplaatsId', gsCode = 'gsCode'),
      c('tellerProgHjT','noemerProgHjT','tellerNormHjT', 'noemerNormHjT') := list(
          tellerProgHjT,
          noemerProgHjT,
          tellerNormHjT, 
          noemerNormHjT)]

facts[zz, on = c(vorigJaar = 'jaar', kpiId = 'kpiId', kostenplaatsId = 'kostenplaatsId', gsCode = 'gsCode'),
      c('tellerRealVjT','noemerRealVjT') := list(
          i.tellerProgHjT,
          i.noemerProgHjT)]

facts[is.na(tellerProgHjT),tellerProgHjT := 0]
facts[is.na(noemerProgHjT),noemerProgHjT := 0]
facts[is.na(tellerNormHjT),tellerNormHjT := 0]
facts[is.na(noemerNormHjT),noemerNormHjT := 0]
facts[is.na(tellerRealVjT),tellerRealVjT := 0]
facts[is.na(noemerRealVjT),noemerRealVjT := 0]

source('readKpi.R',local = TRUE)

kpi <- readKpi()

opbrengstIds <- kpi$kpiId[kpi$level2Code == 'XXXXX3']

zz <- facts[kpiId %in% opbrengstIds,
            ][, c('kpiId') := '54|kpi'
              ][, list(
                  tellerReal = sum(tellerReal), 
                  noemerReal = sum(noemerReal),
                  tellerNorm = sum(tellerNorm),
                  noemerNorm = sum(noemerNorm),
                  tellerProg = sum(tellerProg),
                  noemerProg = sum(noemerProg),
                  tellerRealVj = sum(tellerRealVj),
                  noemerRealVj = sum(noemerRealVj),
                  tellerProgHjT = sum(tellerProgHjT),
                  noemerProgHjT = sum(noemerProgHjT),
                  tellerNormHjT = sum(tellerNormHjT),
                  noemerNormHjT = sum(noemerNormHjT),
                  tellerRealVjT = sum(tellerRealVjT),
                  noemerRealVjT = sum(noemerRealVjT)), 
                by = c('kpiId','periodemaandId','kostenplaatsId','inDeMaand','jaar','vorigJaar','gsCode')]

facts <- rbind(facts,zz)

lastenIds <- kpi$kpiId[kpi$level2Code == 'XXXXX4']

zz <- facts[kpiId %in% lastenIds,
            ][, c('kpiId') := '55|kpi'
              ][, list(
                  tellerReal = sum(tellerReal), 
                  noemerReal = sum(noemerReal),
                  tellerNorm = sum(tellerNorm),
                  noemerNorm = sum(noemerNorm),
                  tellerProg = sum(tellerProg),
                  noemerProg = sum(noemerProg),
                  tellerRealVj = sum(tellerRealVj),
                  noemerRealVj = sum(noemerRealVj),
                  tellerProgHjT = sum(tellerProgHjT),
                  noemerProgHjT = sum(noemerProgHjT),
                  tellerNormHjT = sum(tellerNormHjT),
                  noemerNormHjT = sum(noemerNormHjT),
                  tellerRealVjT = sum(tellerRealVjT),
                  noemerRealVjT = sum(noemerRealVjT)), 
                by = c('kpiId','periodemaandId','kostenplaatsId','inDeMaand','jaar','vorigJaar','gsCode')]

facts <- rbind(facts,zz)

facts$mnd4 <- ifelse(facts$inDeMaand | !((facts$periodemaandId %% 100) %% 4),1,0)

facts[facts$periodemaandId > laatstGeslotenId, tellerReal := tellerProg]
facts[facts$periodemaandId > laatstGeslotenId, noemerReal := noemerProg]
facts$ids <- seq.int(nrow(facts))

pat <- initPat(pat)

begrooteKpi <- union(unique(facts$kpiId[!is.na(facts$tellerNorm) & facts$tellerNorm != 0]), 
                     kpi$kpiCode[kpi$level2Code %in% c('XXXXX3','XXXXX4','30002|kpi','30003|kpi','30004|kpi','40001|kpi','40002|kpi')])

kpi$url <- paste0('<a class="kpiInfoAnchor" href="',kpi$url,'" target="_blank"><img src="dwhRs/info-sign.png" height="16"></a>')
kpi$link <- ''
kpi$link[kpi$kpiCode %in% c('XX8000','XX8001')] <- paste0(
    '<span style="cursor: pointer;" onclick="window.open(\'\',\'Sq 003 Imd dashboard\')" data-toggle="tooltip" title="Details Imd Aanvrager" data-placement="right">',icon('new-window', lib = 'glyphicon'),'</span>')
kpi$link[kpi$kpiCode %in% c('XX7000','XX7001')] <- paste0(
    '<span style="cursor: pointer;" onclick="window.open(\'\',\'Sq 003 Imd dashboard\')" data-toggle="tooltip" title="Details Imd Uitvoerder" data-placement="right">',icon('new-window', lib = 'glyphicon'),'</span>')

opnameKpiId <- head(opn$kpiId,1)
patKpiId <- head(pat$kpiId,1)

if(length(opnameKpiId) == 0) 
    opnameKpiId <- -1

if(length(patKpiId) == 0) 
    patKpiId <- -1

facts$nunq <- ifelse(facts$kpiId == patKpiId,0,1)

kvGroups <- data.table(
    code = c('MS','KV1','KV2','KV3','KV4','KV5','KV6','KV7','KV8'),
    oms = c("Royal Council Summary",
            "Raven Dispatch Effectiveness",
            "Diplomatic Transfers",
            "Green Wave",
            "Septon Cleansing",
            "Maester Brews",
            "Hostage Verification",
            "Wounded by Battle",
            "Defections to the Wall"))      

maatrDropDown <- data.table(
    ovzCode = c('MS','20000|kpi','XXXXX3','XXXXX4','30001|kpi','30000|kpi','20002|kpi'),
    dropDownChoice = c('Counsel of the Small Council',
                       'Realm Balance Notes',
                       'Tribute Measures',
                       'Expenditure Measures', 
                       'Battle Plan Measures', 
                       'Other Battle Measures',
                       'Bannermen Measures'))

ovzMRItems <- data.table(
    ovzCode = c('20000|kpi','XXXXX3','XXXXX4','30001|kpi','30001|kpi','30001|kpi','30000|kpi','20002|kpi'),
    kpiParent = c('','Iron Bank Ledger','Iron Bank Ledger','Battle Conduct','Battle Conduct','Battle Conduct','Battle Conduct',''),
    kpiLabel = c('Iron Bank Ledger',
                 'Iron Throne Tribute (x 1000)',
                 'Crown Expenditures (x 1000)',
                 'Sieges by Burden',
                 'Sieges by Tactic',
                 'Sieges by Region',
                 'Other Battle Burdens',
                 'Bannerman Levies'),
    kpiLvl = c(2,3,3,3,3,3,3,2))

ovzMRHulp <- facts[kpi,on = 'kpiId', nomatch = 0, allow.cartesian = TRUE
                   ][kpl, on = 'kostenplaatsId', nomatch = 0
                     ][,list(cnt = length(ids)),by = c('periodemaandId','i.level2Code','level1Label','level2Label','level3Label','level1Code','level2Code','level3Code','level1Sort','level2Sort','level3Sort')]

names(ovzMRHulp)[1] <- 'perCode'
names(ovzMRHulp)[2] <- 'kostenplaats'

ovzMPHulp <- facts[kpi,on = 'kpiId', nomatch = 0, allow.cartesian = TRUE
                   ][kpl, on = 'kostenplaatsId', nomatch = 0
                     ][per, on = 'periodemaandId', nomatch = 0
                       ][level2Groups %in% kvGroups$oms][,list(cnt = length(kpiId)),by = c('tertiaalCode','i.level2Code','level2Code','level2Groups')]

names(ovzMPHulp)[1] <- 'perCode'
names(ovzMPHulp)[2] <- 'kostenplaats'
names(ovzMPHulp)[3] <- 'kpiCode'

conc3Dat <- 202001

initAfdChoices <- sort(unique(facts[kpiId == '56|kpi' & periodemaandId == laatstGeslotenId][kpl, on = .(kostenplaatsId), nomatch = 0]$level2Label))

facts <- facts[periodemaandId >= min(per$periodemaandId),]
facts <- facts[kostenplaatsId %in% kpl$kostenplaatsId,]

rDataFile <- paste0(getwd(),"/tmp/kpiRvb.RData")
save(list = c('inMaand','gs','kpi','kpl','per','facts',
              'begrooteKpi', 'bestuurder', 'ditJaar', 'kvGroups',
              'laatstGesloten','laatstGeslotenId','laatstGeslotenT','laatstGeslotenIdT','maatrDropDown',
              'opn','ovzMRItems','ovzMRHulp','ovzMPHulp',"mnd4","nunq",
              'pat','patKpiId', 'opnameKpiId','conc3Dat','initAfdChoices'), 
    file = rDataFile)
