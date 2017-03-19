#'
getIncreasingFactor <- function(factorMerger) {
    stats <- calculateGroupStatistic(factorMerger, factorMerger$factor)
    colnames(stats)[2] <- "stat"
    stats <- stats %>% arrange(stat)
    return(factor(factorMerger$factor, levels = as.character(stats[, 1])))
}

setIncreasingOrder <- function(response, factor, family = "gaussian") {
    if (NROW(response) != NROW(factor)) {
        stop("Input data sizes do not match.")
    }

    if (NCOL(response) > 1) {
        tmpResponse <- isoMDS(dist(response), k = 1, trace = FALSE)$points
        return(setIncreasingOrder(tmpResponse, factor))
    }

    factor <- as.factor(as.character(factor))
    data <- data.frame(y = as.numeric(response), c = factor)
    newOrder <- aggregate(y ~ c, mean, data = data) %>%
        dplyr::arrange(y)
    factor(factor, levels = as.character(newOrder[, 1]))
}

#' Merge factor
#'
mergeLevels <- function(factor, groupA, groupB, groupAB = NULL) {
    if (is.null(groupAB)) {
        groupAB <- paste0(groupA, groupB)
    }
    whichLevels <- which(levels(factor) %in% c(groupA, groupB))
    levels(factor)[whichLevels] <- groupAB
    factor
}

calculateGroupStatistic <- function(factorMerger, factor) {
    UseMethod("calculateGroupStatistic", factorMerger)
}

calculateGroupStatistic.default <- function(factorMerger, factor) {
    if (NCOL(factorMerger$response) == 1) {
        return(calculateMeans(factorMerger$response, factor))
    }
    else {
        if (!"projectedResponse" %in% names(factorMerger)) {
            factorMerger$projectedResponse <- MASS::isoMDS(dist(factorMerger$response), k = 1, trace = FALSE)$points[, 1]
        }
        return(calculateMeans(factorMerger$projectedResponse, factor))
    }
}

calculateGroupStatistic.survivalFactorMerger <- function(factorMerger, factor) {
    model <- NULL
    return(NULL)
}

#' Calculate means by factor
#'
#' @rdname calculateMeans
#' @importFrom dplyr group_by summarize arrange
#'
calculateMeans <- function(response, factor) {
    if (is.null(response)) {
        return(NA)
    }
    df <- data.frame(response, level = factor)
    df <- aggregate(. ~ level, function(x) mean(x, na.rm = TRUE), data = df)

    if (NCOL(response) == 1) {
        colnames(df)[2] <- "mean"
        df <- df %>% arrange(mean)
    }
    return(df)
}

#' @importFrom reshape2 melt
#' @importFrom dplyr rename
calculateMeansAndRanks <- function(response, factor) {
    means <- apply(as.data.frame(response), 2, function(x) {
        aggregate(x ~ level, mean, data = data.frame(x = x, level = factor))
    })

    means <- lapply(means, function(x) {
        df <- x %>% arrange(x)
        df$rank <- ave(df$x, FUN = rank)
        df
    })

    return(melt(means, id.vars = c("level", "rank")) %>%
               subset(select = -variable) %>%
               rename(mean = value,
                      variable = L1))
}


#' Filter groups - ...
#'
filterGroups <- function(response, factor, groupA, groupB) {
    response <- as.matrix(response)
    xA <- response[factor == groupA, ]
    xB <- response[factor == groupB, ]
    return(list(xA, xB))
}

bindLevels <- function(groups, groupVec) {
    groupLabel <- paste(groupVec, sep = ":", collapse = ":")
    groups[groups %in% groupVec] <- groupLabel
    groups
}

getTree <- function(factorMerger) {
    steps <- length(factorMerger$mergingList)
    return(paste0(factorMerger$mergingList[[steps]]$groups, ";"))
}