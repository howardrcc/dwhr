commentCheck <-function(zz) {
    
    if (!'gsCode' %in% names(zz)) {
        zz$gsCode <- '0'
        zz$kpiCode[grep('^X',zz$kpiCode, invert = TRUE)] <- paste0(zz$kpiCode[grep('^X',zz$kpiCode, invert = TRUE)],'|kpi')
    }
    
    if ('periodemaandId' %in% names(zz)) {
        zz$periodemaandId <- as.character(zz$periodemaandId)
        names(zz)[names(zz) == 'periodemaandId'] <- 'perCode'
    }
    
    if (!'perType' %in% names(zz)) {
        zz$perType <- 'maand'
    }
    
    zz$kostenplaats <- sprintf('%06d',as.integer(zz$kostenplaats))
    
    zz <- zz[!(type == 'conc' & perCode >= conc3Dat & perType == 'maand')]
    
    zz
}

initComments <- function() {
    
    comments <- NULL
    
    for (file in Sys.glob(paste0(getwd(),'/tmp/*Comments'))) {
        
        zz <- readRDS(file)
        comments <- rbind(
            comments,
            commentCheck(zz)
        )    
    }
    
    if (is.null(comments)) {
        
        comments <- data.table(
            kpiCode = character(0),
            kostenplaats = character(0),
            perCode = character(0),
            perType = character(0),
            gsCode = character(0),
            volgnr = integer(0),
            type = character(0),
            status = character(0),
            txt = character(0),
            creationDate = Sys.time()[0],
            lastUpdateDate = Sys.time()[0],
            createdBy = character(0),
            updatedBy = character(0),
            subVolgnr = integer(0),
            endDate = integer(0),
            verzameld = logical(0)
        ) 
        
    } else {
        
        # duplicates verwijderen, we bewaren alleen het record met de laatste lastUpdateDate 
        
        comments <- comments[
            comments[, list(lastUpdateDate = max(lastUpdateDate)), 
                     by = c('kpiCode','kostenplaats','perCode','perType','gsCode','volgnr','subVolgnr','type')
                     ],
            on = c('kpiCode','kostenplaats','perCode','perType','gsCode','volgnr','subVolgnr','type','lastUpdateDate'), 
            nomatch = 0
            ]
        
        comments <- unique(comments)
    }
    comments
}

initScore <- function() {
    
    score <- NULL
    file <- paste0(getwd(),'/tmp/score')
    
    if (file.exists(file)) {
        score <- readRDS(file)
        
        if ('periodemaandId' %in% names(score)) {
            score$periodemaandId <- as.character(score$periodemaandId)
            names(score)[names(score) == 'periodemaandId'] <- 'perCode'
        }
        
        score$kostenplaats <- sprintf('%06d',as.integer(score$kostenplaats))
    }
    
    if (is.null(score)) {
        
        score <- data.table(
            kostenplaats = character(0),
            perCode = character(0),
            score = character(0),
            lastUpdateDate = Sys.time()[0],
            updatedBy = character(0)
        ) 
        
    } 
   
    score
}


updateComments <- function() {
    
    if (!exists('comments') || is.null(comments)) {
        dwhrStop('Geen comments gevonden')
    }
    
    cmnts <- comments[comments$verzameld == TRUE,]

    for (file in Sys.glob(paste0(getwd(),'/tmp/*Comments'))) {
        
        if (basename(file) != 'allComments') {
            
            cmnts <- rbind(
                cmnts,
                commentCheck(readRDS(file))
            )    
        }
    }
        
    # duplicates verwijderen, we bewaren alleen het record met de laatste lastUpdateDate 

    cmnts <- cmnts[
        cmnts[, list(lastUpdateDate = max(lastUpdateDate)), 
              by = c('kpiCode','kostenplaats','perCode','perType','gsCode','volgnr','subVolgnr','type')
              ],
        on = c('kpiCode','kostenplaats','perCode','perType','gsCode','volgnr','subVolgnr','type','lastUpdateDate'), 
        nomatch = 0
        ]
    
    cmnts <- unique(cmnts)

    cmnts
}


getComment <- function(star,dim,df,type,commentType) {
    
    ret <- rep('',nrow(df))
    
    if (type == 'body' && !checkMs(star,dim)) {
        
        if (dim == 'kpi') {
        
            perInfo <- getPerInfo(star)
            
            gsSel <- star$dims[['gs']]$selected$label
            gsLvl <- star$dims[['gs']]$selected$level
         
            if (isNull(gsLvl,0) == 0) {
                gsKey <- 0
            } else {
                gsKey <- gs$gsCode[gs$level1Label == gsSel]
            }
            
            kplSel <- star$dims[['kpl']]$selected$label
            kplLvl <- star$dims[['kpl']]$selected$level
            
            if (kplLvl == 0) {
                kplKey <- '000000'
            } else {
                kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
            }
            
            ids <- unique(comments[kostenplaats == kplKey & 
                                       perCode == perInfo$key & 
                                       gsCode == gsKey &
                                       txt != '' &
                                       volgnr == 0 &
                                       subVolgnr == 0 &
                                       type == commentType,
                                   .(kpiCode,txt)])
            
            ret <- merge(df, ids, by.x = 'memberKey', by.y = 'kpiCode', all.x = TRUE)
            ret <- ret[order(match(ret[,c('memberKey')],df[,c('memberKey')])),]$txt
        }
        
        if (dim == 'kpl') {

            perInfo <- getPerInfo(star)
            
            gsSel <- star$dims[['gs']]$selected$label
            gsLvl <- star$dims[['gs']]$selected$level
            
            if (isNull(gsLvl,0) == 0) {
                gsKey <- 0
            } else {
                gsKey <- gs$gsCode[gs$level1Label == gsSel]
            }
            
            kpiSel <- star$dims[['kpi']]$selected$label
            kpiLvl <- star$dims[['kpi']]$selected$level
            kpiParent <- star$dims[['kpi']]$selected$parent
            
            kpiKey <- unique(kpi$kpiCode[kpi$kpiLabel == kpiSel & kpi$level == kpiLvl & kpi$kpiParent == kpiParent])
            
            ids <- unique(comments[kpiCode == kpiKey & 
                                       perCode == perInfo$key & 
                                       gsCode == gsKey &
                                       txt != '' &
                                       volgnr == 0 &
                                       subVolgnr == 0 &
                                       type == commentType,
                                   .(kostenplaats,txt)])
            
            ret <- merge(df, ids, by.x = 'memberKey', by.y = 'kostenplaats', all.x = TRUE)
            ret <- ret[order(match(ret[,c('memberKey')],df[,c('memberKey')])),]$txt
    
        }
        
        if (dim %in% c('per','perAfd')) {

            kpiSel <- star$dims[['kpi']]$selected$label
            kpiLvl <- star$dims[['kpi']]$selected$level
            kpiParent <- star$dims[['kpi']]$selected$parent
            
            kpiKey <- unique(kpi$kpiCode[kpi$kpiLabel == kpiSel & kpi$level == kpiLvl  & kpi$kpiParent == kpiParent])
            
            kplSel <- star$dims[['kpl']]$selected$label
            kplLvl <- star$dims[['kpl']]$selected$level
            
            if (kplLvl == 0) {
                kplKey <- '000000'
            } else {
                kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
            }
            
            gsSel <- star$dims[['gs']]$selected$label
            gsLvl <- star$dims[['gs']]$selected$level
            
            if (isNull(gsLvl,0) == 0) {
                gsKey <- 0
            } else {
                gsKey <- gs$gsCode[gs$level1Label == gsSel]
            }
            
            ids <- unique(comments[kpiCode == kpiKey & 
                                       kostenplaats == kplKey & 
                                       gsCode == gsKey &
                                       txt != '' &
                                       volgnr == 0 &
                                       subVolgnr == 0 &
                                       type == commentType,
                                   .(perCode,txt)])
            
            ret <- merge(df, ids, by.x = 'memberKey', by.y = 'perCode', all.x = TRUE)
            ret <- ret[order(match(ret[,c('memberKey')],df[,c('memberKey')])),]$txt
            
        }
        
        if (dim == 'gs') {
            
            perInfo <- getPerInfo(star)
            
            kpiSel <- star$dims[['kpi']]$selected$label
            kpiLvl <- star$dims[['kpi']]$selected$level
            kpiParent <- star$dims[['kpi']]$selected$parent
            
            kpiKey <- unique(kpi$kpiCode[kpi$kpiLabel == kpiSel & kpi$level == kpiLvl  & kpi$kpiParent == kpiParent])
            
            kplSel <- star$dims[['kpl']]$selected$label
            kplLvl <- star$dims[['kpl']]$selected$level
            
            if (kplLvl == 0) {
                kplKey <- '000000'
            } else {
                kplKey <- kpl$level2Code[kpl$level2Label == kplSel]
            }
            
            ids <- unique(comments[kpiCode == kpiKey & 
                                       kostenplaats == kplKey & 
                                       perCode == perInfo$key & 
                                       txt != '' &
                                       volgnr == 0 &
                                       subVolgnr == 0 &
                                       type == commentType,
                                   .(gsCode,txt)])
            
            ret <- merge(df, ids, by.x = 'memberKey', by.y = 'gsCode', all.x = TRUE)
            ret <- ret[order(match(ret[,c('memberKey')],df[,c('memberKey')])),]$txt
            
        }
        
    }
    
    ret[is.na(ret)] <- ''
    ret
 
}


getOpmerkingTekst <- function() {
    getComment(star,dim,df,type,'opm')
}

getOpmerking <- function() { ifelse(df$opmTekst == '',0,1) }
getOpmerking2 <- function() { ifelse(df$opmTekst == '',NA,1) }

getKpiProps <- function(env, init = FALSE) {

    perInfo <- getPerInfo(env)
    
    if (init) {
        
        kpiSel <- env$session$userData$dashOpts$kpiSel$label
        kpiPar <- env$session$userData$dashOpts$kpiSel$parent
        kpiLvl <- env$session$userData$dashOpts$kpiSel$level

    } else {
        kpiSel <- env$dims[['kpi']]$selected$label
        kpiPar <- env$dims[['kpi']]$selected$parent
        kpiLvl <- env$dims[['kpi']]$selected$level
        
        if (kpiLvl == 0)
            return(list())
    }

    l <- list()

    k <- kpi[kpi$kpiLabel == kpiSel & kpi$level == kpiLvl & kpi$kpiParent == kpiPar,]

    l$format <- max(k$format)
    l$sort <- switch(max(k$sorting), LH = 'asc', HL = 'desc')
    normNaam <- max(k$normNaam)
    l$kpiId <- max(k$kpiId)
    l$kpiCode <- max(k$kpiCode)
    l$kpiLabel <- kpiSel
    l$kpiParent <- kpiPar
    l$kpiLvl <- kpiLvl
    l$isKV <- max(k$level1Label) == 'Houses Pact Compliance'
    
    l$heeftAbsoluut <- (l$format %in% c('perc','perc1','perc2') && max(k$level1Label) == 'Houses Pact Compliance')
    l$groenLabel <- 'Aantal Groen'  
    l$roodLabel <- 'Aantal Rood'
    l$roodFormat <- 'integer'  
    l$groenFormat <- 'integer'
    
    l$normNaam <- switch(normNaam, B = 'Begroting', N = 'Norm')
    l$isBegroot <- l$kpiCode %in% begrooteKpi
    
    l$verschilNaam <- 'Verschil'
    l$realNaam <- ifelse(perInfo$future,'Prognose','Realisatie')
    
    env$kpiProps <- l
    
    return(l)
    
}

initPat <- function(pat) {
    pat <- pat[order(pat$periodemaandId)]
    
    if (laatstGeslotenId %% 100 < 12) {
        
        jaar <- laatstGeslotenId %/% 100
        
        # toevoegen prognose facts voor unieke patienten 
        
        xx <- facts[periodemaandId == laatstGeslotenId & kpiId == '160|kpi']
        
        for (m in (laatstGeslotenId + 1):(100 * (laatstGeslotenId %/% 100) + 12)) {
            xx$periodemaandId <- m
            facts <<- rbind(facts,xx)
        }
        
        # bepalen cumulatieve prognose voor unieke patienten mbh seizoenspatroon
        
        for (k in unique(pat$kostenplaatsId[pat$inDeMaand == 0])) {
            
            if (length(pat$patCount[pat$kostenplaatsId == k & pat$inDeMaand == 0]) > 24 ) {
                
                stl <- stl(ts(pat$patCount[pat$kostenplaatsId == k & pat$inDeMaand == 0],frequency = 12),'periodic')
                zz <- seasonal(stl)[((laatstGeslotenId %% 100) + 1):12] + tail(trend(stl),1)
                
                pat <- rbind(
                    pat,
                    data.table(
                        kpiId = '160|kpi',
                        periodemaandId = 100 * (laatstGeslotenId %/% 100) + (((laatstGeslotenId %% 100) + 1):12),
                        kostenplaatsId = k,
                        patCount = zz,
                        inDeMaand = 0))
            } 
        }
        
        # bepalen In de periode prognose voor unieke patienten dmv gemiddelde
        
        for (k in unique(pat$kostenplaatsId[pat$inDeMaand == 1])) {
            
            x <- mean(pat$patCount[pat$kostenplaatsId == k & pat$inDeMaand == 1 & pat$periodemaandId %/% 100 == laatstGeslotenId %/% 100])
            pat <- rbind(
                pat,
                data.table(
                    kpiId = '160|kpi',
                    periodemaandId = 100 * (laatstGeslotenId %/% 100) + (((laatstGeslotenId %% 100) + 1):12),
                    kostenplaatsId = k,
                    patCount = x,
                    inDeMaand = 1))
        }
    
    }
    
    pat <- merge(pat,kpl,by.x = 'kostenplaatsId', by.y = 'kostenplaatsId', all.x = TRUE)

    pat
}

getOpnameCount <- function(sd,per) {
    
    x <- max(per)
    
    if (x > laatstGeslotenId) {
        
        cum <- length(unique(opn$opnameNr[opn$kostenplaatsId %in% sd$kostenplaatsId &
                                              opn$periodemaandId %in% (((x %/% 100) * 100 + 1) : x)])) 
        if (0 %in% sd$inDeMaand) {
            len <- (cum / (laatstGeslotenId %% 100)) * (x %% 100)
        } else {
            len <- sum(per > laatstGeslotenId) * (cum / (laatstGeslotenId %% 100)) + 
                length(unique(opn$opnameNr[opn$kostenplaatsId %in% sd$kostenplaatsId &
                                               opn$periodemaandId %in% per]))
        }
        
    } else {
        
        if (0 %in% sd$inDeMaand) {
            len <-length(unique(opn$opnameNr[opn$kostenplaatsId %in% sd$kostenplaatsId &
                                                 opn$periodemaandId %in% (((x %/% 100) * 100 + 1) : x)])) 
        } else {
            len <- length(unique(opn$opnameNr[opn$kostenplaatsId %in% sd$kostenplaatsId &
                                                  opn$periodemaandId %in% per]))
        }
        
    }
    
    as.integer(round(len,0))
}

opnCountVj <- function(env,dim,sd) {
    if (opnameKpiId %in% sd$kpiId) {
        per <- unique(sd$periodemaandId) - 100 
        getOpnameCount(sd,per)
    }
    else {
        0L
    }
}

opnCount <- function(env,dim,sd) {
    if (opnameKpiId %in% sd$kpiId) {
        per <- unique(sd$periodemaandId)
        getOpnameCount(sd,per)
    }
    else {
        0L
    }
}

waardeNoemer <- function(star,dim,ds,colName,type) {
    
    kpiId <- star$dims[['kpi']]$selectedIds[1]
    res <- ds[[colName]]
    
    if ((dim == 'kpi' && opnameKpiId %in% ds$max_kpiId) ||
        (kpiId == opnameKpiId)) {
        
        if (type == 'vj') {
            res <- ifelse(ds$opnCountVj_ids > 0,ds$opnCountVj_ids,res)
        }
        else
            res <- ifelse(ds$opnCount_ids > 0,ds$opnCount_ids,res)
            
    }
    
    res
}


waardeTeller <- function(star,dim,ds,colName,type) {

    kpiId <- star$dims[['kpi']]$selectedIds[1]
    res <- ds[[colName]]
    
    if (kpiId == patKpiId || (dim == 'kpi' && patKpiId %in% ds$max_kpiId)) {
        
        perSel <- star$dims[['perAfd']]$selectedIds[1]
        
        mnd <- star$dims[['mnd']]$selectedIds[1]
        
        if (type == 'vj') {
            perSel <- perSel -100
        }
        
        if (type == 'phjt') {
            perSel <- 100 * (perSel %/% 100) + 12 
        }
        
        if (dim %in% c('perInst') || star$dims[['kpl']]$selected$level == 0 ) {
            kpl <- 0
        } else {
            kpl <- star$dims[['kpl']]$selectedIds
        }
        
        if ( dim == 'kpi') {
            res[ds$max_kpiId == patKpiId] <- pat$patCount[pat$periodemaandId == perSel & pat$kostenplaatsId %in% kpl & pat$inDeMaand == mnd]
        }
        
        if ( dim %in% c('kpl','kpl2')) {
            if (length(res) == 1) {
                res <- pat$patCount[pat$periodemaandId == perSel & pat$kostenplaatsId == 0 & pat$inDeMaand == mnd]
            } else {
                
                x <- pat[periodemaandId == perSel & inDeMaand == mnd,]
                res <- x$patCount[sapply(ds$member,function(z) which(x$level2Label == z))]
            }
        }
        if (dim == 'per' && type == 'vj') {
            res <- pat$patCount[pat$periodemaandId %in% (per$periodemaandId - 100) & pat$kostenplaatsId %in% kpl & pat$inDeMaand == mnd]
        } else {
            if ( dim %in% c('per','perAfd','perInst')) {
                res <- pat$patCount[pat$periodemaandId %in% per$periodemaandId & pat$kostenplaatsId %in% kpl & pat$inDeMaand == mnd]
            }
        }
    }
    res
}

aantalRoodHulp <- function(df) {
    ifelse(df$sum_noemerReal == 0,0,df$sum_noemerReal - df$sum_tellerReal)
}

aantalRood <- function() {
    if (perState(star) == 'tertiaal')
        geslotenId = laatstGeslotenIdT
    else
        geslotenId = laatstGeslotenId
    
    res <- aantalRoodHulp(df)
    res[which(df$memberKey > geslotenId)] <- 0
    res
}

aantalRoodVj <- function() {
    ifelse(df$sum_noemerRealVj == 0,0,df$sum_noemerRealVj - df$sum_tellerRealVj)
}

aantalRoodProg <- function() {
    if (perState(star) == 'tertiaal')
        geslotenId = laatstGeslotenIdT
    else
        geslotenId = laatstGeslotenId
    
    res <- aantalRoodHulp(df)
    res[which(df$memberKey <= geslotenId)] <- 0
    res
}

aantalRoodRealProg <- function() {
    aantalRoodHulp(df)
}

aantalGroenHulp <- function(df) {
    df$sum_tellerReal
}

aantalGroen <- function() {
    if (perState(star) == 'tertiaal')
        geslotenId = laatstGeslotenIdT
    else
        geslotenId = laatstGeslotenId
    
    res <- aantalGroenHulp(df)
    res[which(df$memberKey > geslotenId)] <- 0
    res
}

aantalGroenVj <- function() {
    df$sum_tellerRealVj
}

aantalGroenProg <- function() {
    if (perState(star) == 'tertiaal')
        geslotenId = laatstGeslotenIdT
    else
        geslotenId = laatstGeslotenId
    
    res <- aantalGroenHulp(df)
    res[which(df$memberKey <= geslotenId)] <- 0
    res
}

aantalGroenRealProg <- function() {
    aantalGroenHulp(df)
}

realProgHulp <- function(star,dim,df) {
    tel <- waardeTeller(star,dim,df,'sum_tellerReal','realisatie')
    noem <- waardeNoemer(star,dim,df,'sum_noemerReal','realisatie')
    noem[is.na(noem)] <- 1
    noem[noem == 0] <- 1
    
    tel / noem
}

waardeRealProg <- function() {
    realProgHulp(star,dim,df)
}

waardeRealisatie <- function() {
    
    if (perState(star) == 'tertiaal')
        geslotenId = laatstGeslotenIdT
    else
        geslotenId = laatstGeslotenId
    
    res <- realProgHulp(star,dim,df)
    res[which(df$memberKey > geslotenId)] <- 0
    res
}

waardePrognose <- function() {
    if (perState(star) == 'tertiaal')
        geslotenId = laatstGeslotenIdT
    else
        geslotenId = laatstGeslotenId
    
    res <- realProgHulp(star,dim,df)
    res[which(df$memberKey <= geslotenId)] <- 0
    res
}

waardeRealVj <- function() {
    tel <- waardeTeller(star,dim,df,'sum_tellerRealVj','vj') 
    noem <- waardeNoemer(star,dim,df,'sum_noemerRealVj','vj')
    noem[is.na(noem)] <- 1
    noem[noem == 0] <- 1
    tel / noem
} 

waardeRealVj2 <- function() {
    tel <- waardeTeller(star,dim,df,'sum_tellerRealVj','vj') 
    noem <- waardeNoemer(star,dim,df,'sum_noemerRealVj','vj')
    noem[is.na(noem)] <- 1
    noem[noem == 0] <- 1
    res <- tel / noem
    
    if (perState(star) == 'tertiaal')
        geslotenId = laatstGeslotenIdT
    else
        geslotenId = laatstGeslotenId
    
    res[which(df$memberKey > geslotenId)] <- 0
    res
}

waardeProgHjT <- function() {
    tel <- waardeTeller(star,dim,df,'sum_tellerProgHjT','phjt') 
    noem <- waardeNoemer(star,dim,df,'sum_noemerProgHjT','phjt')
    noem[is.na(noem)] <- 1
    noem[noem == 0] <- 1
    tel / noem
}    

waardeBegroting <- function() {
    ds <- df
    ds[is.na(ds$sum_noemerNorm),c('sum_noemerNorm')] <- 1
    ds[ds$sum_noemerNorm == 0,c('sum_noemerNorm')] <- 1
    ds$sum_tellerNorm / ds$sum_noemerNorm
}

waardeNormHjT <- function() {
    ds <- df
    ds[is.na(ds$sum_noemerNormHjT),c('sum_noemerNormHjT')] <- 1
    ds[ds$sum_noemerNormHjT == 0,c('sum_noemerNormHjT')] <- 1
    ds$sum_tellerNormHjT / ds$sum_noemerNormHjT
}    

waardeVerschil <- function() {
    round(realProgHulp(star,dim,df) - df$waardeNorm,2)
}


waardeTrend <- function() {
    
    if (type == 'body' && star$id %in% c('s1') && star$dims[['mnd']]$selected$label == 'Within the Moon') {
        res <- c()  
            
        if (perState(star) == 'maand') {
            y <- df$waardeReal[df$memberKey <= laatstGeslotenId]
            len <- 24
        } else { 
            y <- df$waardeReal[df$memberKey <= laatstGeslotenIdT]
            len <- 6
        }
        
        x <- 1:length(y)
        
        if (length(which(!is.na(y))) > 1) {
            fn <- function(x,a,b) {(a*x) + b}
            co <- coef(lm(y~x))
            res <- fn(1:len,co[2],co[1])
        } else {
            res <- rep(NA,len)
        }
        
    } else {
        res <- rep(NA,nrow(df))
    }
    
    res
}

waardeVerschilRealProg <- function() {
    round(df$waardeReal - df$waardeNorm,2)
}

setPosNeg <- function() {
    if (type %in% c('footer','summary')) {
        return(rep(0,nrow(df)))
    }

    sapply(as.data.frame(t(df[,c('memberKey','waardeVerschil')]),stringsAsFactors=FALSE),function(x) {

        if (x[1] %in% begrooteKpi) {

            sort <- max(kpi$sorting[kpi$kpiCode == x[1]])
            
            if (sort == 'LH') {
                ret <- sign(as.numeric(x[2]))
            } else {
                ret <- -1 * sign(as.numeric(x[2]))
            }
            if (ret == 0) 
                ret <- 1
        } else {
            ret <- 0
        }
        ret
    }) 
}


getSpark <- function() {
    
    if (!sparkVis(star)) {
        return(as.character(rep(0,nrow(df))))
    }
    
    if (type != 'footer' && star$id %in% c('s1','q2','q5','s5')) {
        
        perSel = laatstGeslotenId
        perIds <- tail(per$periodemaandId[per$periodemaandId <= perSel],24)

        if (dim == 'kpl') {

            kpiIds <- star$dims[['kpi']]$selectedIds
            
            if (all(kpiIds %in% c(opnameKpiId,patKpiId)))
                return(rep(c(0),nrow(df)))
            
            gsIds <- isNull(star$dims[['gs']]$selectedIds,c('E','O','D'))
            kplIds <- df$mean_kostenplaatsId
            
            zz <- facts[kpiId %in% kpiIds &
                            kostenplaatsId %in% kplIds &
                            gsCode %in% gsIds &
                            inDeMaand == 1 &
                            periodemaandId %in% perIds ,][, list(
                                tellerReal = sum(tellerReal),
                                noemerReal = sum(noemerReal),
                                tellerNorm = sum(tellerNorm),
                                noemerNorm = sum(noemerNorm)),
                                by = c('kostenplaatsId','periodemaandId')]
            
            zz$waarde <- round(
                (zz$tellerReal / ifelse(zz$noemerReal == 0,1,zz$noemerReal)) - 
                (zz$tellerNorm / ifelse(zz$noemerNorm == 0,1,zz$noemerNorm)) ,2)
            
            zz <- zz[order(zz$kostenplaatsId,zz$periodemaandId),]
            xx <- zz[,list(waarde = paste0(waarde,collapse = ',')),by = c('kostenplaatsId')]
            return(xx$waarde[order(match(t(xx[,c('kostenplaatsId')]),df[,c('mean_kostenplaatsId')]))])
        }
        
        if (dim == 'gs') {
            
            kpiIds <- star$dims[['kpi']]$selectedIds
            kplIds <- star$dims[['kpl']]$selectedIds
            gsIds <- df$memberKey
            if (all(kpiIds %in% c(opnameKpiId,patKpiId)))
                return(rep(c(0),nrow(df)))
                    
            zz <- facts[kpiId %in% kpiIds &
                            kostenplaatsId %in% kplIds &
                            gsCode %in% gsIds &
                            inDeMaand == 1 &
                            periodemaandId %in% perIds ,][, list(
                                tellerReal = sum(tellerReal),
                                noemerReal = sum(noemerReal),
                                tellerNorm = sum(tellerNorm),
                                noemerNorm = sum(noemerNorm)),
                                by = c('gsCode','periodemaandId')]
            
            zz$waarde <- round(
                (zz$tellerReal / ifelse(zz$noemerReal == 0,1,zz$noemerReal)) - 
                    (zz$tellerNorm / ifelse(zz$noemerNorm == 0,1,zz$noemerNorm)) ,2)
            
            zz <- zz[order(zz$gsCode,zz$periodemaandId),]
            xx <- zz[,list(waarde = paste0(waarde,collapse = ',')),by = c('gsCode')]
            return(xx$waarde[order(match(t(xx[,c('gsCode')]),df[,c('memberKey')]))])
            
            
        }
        
        if (dim == 'kpi') {

            lvl <- star$dims[['kpi']]$level
            par <- star$dims[['kpi']]$parent
            
            if (par == 'Battle Conduct') 
                return(rep(0,nrow(df)))
                
            parLabel <- paste0('level',lvl - 1,'Label')
            code <- paste0('level',lvl,'Code')
            mks <- df$memberKey

            kplIds <- star$dims[['kpl']]$selectedIds
            gsIds <- isNull(star$dims[['gs']]$selectedIds,c('E','O','D'))
            kk <- as.data.table(kpi)[get(code) %in% mks & get(parLabel) == par]
            
            excludeKpi <- kpi$kpiCode[kpi$kpiId %in% c(opnameKpiId,patKpiId)]

            zz <- facts[kk, on = c('kpiId'),nomatch = 0][
                    kostenplaatsId %in% kplIds &
                    inDeMaand == 1 &
                    gsCode %in% gsIds &
                    periodemaandId %in% perIds ,][, list(
                        tellerReal = sum(tellerReal),
                        noemerReal = sum(noemerReal),
                        tellerNorm = sum(tellerNorm),
                        noemerNorm = sum(noemerNorm)),
                        by = c(code,'periodemaandId')]

            zz$waarde <- round(
                (zz$tellerReal / ifelse(zz$noemerReal == 0,1,zz$noemerReal)) - 
                    (zz$tellerNorm / ifelse(zz$noemerNorm == 0,1,zz$noemerNorm)) ,2)
            
            zz <- zz[order(zz[[code]],zz$periodemaandId),]
            xx <- zz[,list(waarde = paste0(waarde,collapse = ',')),by = code]

            xx[(get(code) %in% excludeKpi),]$waarde <- '0'
            
            return(xx$waarde[order(match(t(xx[,get(code)]),df[,c('memberKey')]))])

        }
        
        if (dim %in% 'kpl2') {
            xx <- star$dims[['kpl']]$membersFiltered[,c('mean_kostenplaatsId','waardeTrend')]
            xx$waarde <- sparkRelativeChange(xx$waardeTrend)
            return(xx$waarde[order(match(t(xx[,c('mean_kostenplaatsId')]),df[,c('mean_kostenplaatsId')]))])
        }
        
    } else {
        
        return(as.character(rep(0,nrow(df))))
    }
    
}

setLock <- function(env,key,user) {
 
    file <- paste0(getwd(),'/tmp/locks')  
    
    if (file.exists(file)) {
        locks <- readRDS(file)
        if (key %in% names(locks$comments)) {
            inUse <- locks$comments[[key]]
            if (inUse != user) {
                shinyjs::alert(paste0('record gelocked door: ',inUse,'!'))
                return(FALSE)
            }
        }
    } else {
        locks <- list(
            comments = list(dummy = 1))
    }

    locks$comments[[key]] <- user
    env$session$userData$lock <- key
    
    saveRDS(locks,file)
    return(TRUE)
    
}

releaseLock <- function(env,key) {
    
    file <- paste0(getwd(),'/tmp/locks')  
    
    if (file.exists(file)) {
        locks <- readRDS(file)
    } else {
        locks <- list(
            comments = list(dummy = 1))
    }
    
    locks$comments[[key]] <- NULL
    env$session$userData$lock <- NULL
    
    saveRDS(locks,file)
    
}

releaseLockUser <- function(env,usr) {
    
    file <- paste0(getwd(),'/tmp/locks')  
    
    if (file.exists(file)) {
        locks <- readRDS(file)
    } else {
        locks <- list(
            comments = list(dummy = 1))
    }
    
    for (x in names(locks$comments)) { 
        if (locks$comments[[x]] == usr) 
            locks$comments[[x]] <- NULL
    }
    
    env$session$userData$lock <- NULL
    
    saveRDS(locks,file)
    
}


checkMs <- function(env,dd = 'all') {
    
    single <- TRUE
    for (d in setdiff(c('per','kpl','gs'),dd)) {
        if (d %in% names(env$dims))
            single <- single & (nrow(env$dims[[d]]$selected) == 1)
    }
    
    !single
    
}


perState <- function(env) {
    
    if (all(is.null(env$dims[['perAfd']]$selected$label)) || all(env$dims[['perAfd']]$selected$level == 2))
        return('maand')
    
    return('tertiaal')
    
}

getPerInfo <- function(env) {
    
    if (perState(env) == 'maand') {
        if (all(is.null(env$dims[['perAfd']]$selected$label)))
            perSel <- laatstGesloten
        else
            perSel <- env$dims[['perAfd']]$selected$label

        return(list(
            key = max(per$maandCode[per$maandLabel %in% perSel]),
            sel = perSel,
            type = 'maand',
            realNaam = ifelse(any(per$periodemaandId[per$maandLabel %in% perSel] > laatstGeslotenId),'Prognose','Realisatie'),
            future = any(per$periodemaandId[per$maandLabel %in% perSel] > laatstGeslotenId)))    
    }
    
    if (perState(env) == 'tertiaal') {
        if (is.null(env))
            perSel <- laatstGeslotenT
        else
            perSel <- env$dims[['perAfd']]$selected$label
        return(list(
            key = max(per$tertiaalCode[per$tertiaalLabel %in% perSel]),
            sel = perSel,
            type = 'tertiaal',
            realNaam = ifelse(any(per$tertiaalCode[per$tertiaalLabel %in% perSel] > laatstGeslotenT),'Prognose','Realisatie'),
            future = any(per$tertiaalCode[per$tertiaalLabel %in% perSel] > laatstGeslotenT)))    
    }

    
}


isKV <- function(env) {
    getKpiProps(env)
    
    env$kpiProps$isKV
}

isNotKV <- function(env) {!isKV(env)}
 
