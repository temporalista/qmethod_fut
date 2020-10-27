##Funciones para gráficos qplot personalizados

my.dotchart <- function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
                         pt.cex = cex, pch = 21, gpch = 21, bg = par("bg"), adj=0, 
                         color = par("fg"), gcolor = par("fg"), lcolor = "gray", 
                         xlim = range(x[is.finite(x)]), main = NULL, xlab = NULL, 
                         ylab = NULL, ...) 
{
  opar <- par("mai", "mar", "cex", "yaxs")
  on.exit(par(opar))
  par(cex = cex, yaxs = "i")
  if (!is.numeric(x)) 
    stop("'x' must be a numeric vector or matrix")
  n <- length(x)
  if (is.matrix(x)) {
    if (is.null(labels)) 
      labels <- rownames(x)
    if (is.null(labels)) 
      labels <- as.character(1L:nrow(x))
    labels <- rep_len(labels, n)
    if (is.null(groups)) 
      groups <- col(x, as.factor = TRUE)
    glabels <- levels(groups)
  }
  else {
    if (is.null(labels)) 
      labels <- names(x)
    glabels <- if (!is.null(groups)) 
      levels(groups)
    if (!is.vector(x)) {
      warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
      x <- as.numeric(x)
    }
  }
  
  plot.new()
  linch <- if (!is.null(labels)) 
    max(strwidth(labels, "inch"), na.rm = TRUE)
  else 0
  if (is.null(glabels)) {
    ginch <- 0
    goffset <- 0
  }
  else {
    ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
    goffset <- 0.4
  }
  if (!(is.null(labels) && is.null(glabels))) {
    nmai <- par("mai")
    nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
      0.1
    par(mai = nmai)
  }
  if (is.null(groups)) {
    o <- 1L:n
    y <- o
    ylim <- c(0, n + 1)
  }
  else {
    o <- sort.list(as.numeric(groups), decreasing = TRUE)
    x <- x[o]
    groups <- groups[o]
    color <- rep_len(color, length(groups))[o]
    lcolor <- rep_len(lcolor, length(groups))[o]
    offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
    y <- 1L:n + 2 * offset
    ylim <- range(0, y + 2)
  }
  plot.window(xlim = xlim, ylim = ylim, log = "")
  lheight <- par("csi")
  if (!is.null(labels)) {
    linch <- 2 # max(strwidth(labels, "inch"), na.rm = TRUE)
    loffset <- (linch + 0.1)/lheight
    labs <- labels[o]
    mtext(labs, side = 2, line = 1, at = y+0.25, adj = adj, #at allows to fine tuning the vertial position of text
          col = color, las = 2, cex = cex, ...)
  }
  abline(h = y, lty = "dotted", col = lcolor)
  points(x, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)
  if (!is.null(groups)) {
    gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
                         2) - 1)
    ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
    goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
    mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
          col = gcolor, las = 2, cex = cex, ...)
    if (!is.null(gdata)) {
      abline(h = gpos, lty = "dotted")
      points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
             cex = pt.cex/cex, ...)
    }
  }
  axis(1)
  box(lwd=0.2)
  title(main = main, xlab = xlab, ylab = ylab, ...)
  invisible()
}


my.qmb.plot <- function (qmbsum, 
                         type = c("zsc", "loa"), 
                         nfactors=3, 
                         cex = 0.7,
                         cex.leg = 0.8, 
                         errbar.col = rgb(0.4,0.4,0.4),
                         errbar.lwd = 0.5,
                         errbar.lty = "solid",
                         vertdist = 0.4,
                         limits = NULL,
                         leg.pos = "bottomleft",
                         sort = c("none", "difference","sd"), 
                         sbset = NULL,
                         bty = "n",
                         plot.std = FALSE,
                         plot.mark=FALSE,
                         ylabels="",
                         pch = c(21,22,24,23),
                         col=c("black","black","black"),
                         grid.col = "gray50", 
                         xlab="",
                         ylab="",
          ...) 
{
  
  if (type == "loa") {
    boloa <- qmbsum[[1]]
    db <- boloa[, c(grep("loa", names(boloa)),
                    grep("SE",names(boloa)), 
                    grep("std", names(boloa)),
                    grep("flag",names(boloa)))]
    item <- "Q-sort"
    values <- "Factor loading"
    if (is.null(limits)) 
      limits <- c(-1, 1)
    
    db$m1 <- (db$flag.freq1 >0.75)
    db$m2 <- (db$flag.freq2 >0.75)
    db$m3 <- (db$flag.freq3 >0.75)
    
  }
  
  if (type == "zsc") {
    boloa <- qmbsum[[2]]
    db <- boloa[, c(grep("zsc.bts", names(boloa)), 
                    grep("SE",names(boloa)), 
                    grep("std", names(boloa)),
                    grep("sta_id", names(boloa)),
                    grep("Declaracion", names(boloa))
                    )]
    
    item <- "Statement"
    values <- "z-score"
    if (is.null(limits)) {
      zscs <- grep("zsc.bts", names(db))
      SEs <- grep("SE", names(db))
      lms.down <- db[, zscs] - db[, SEs]
      lms.up <- db[, zscs] + db[, SEs]
      limits <- c(round(min(lms.down) + (0.05*min(lms.down)),1), 
                  round(max(lms.up) + (0.05*max(lms.up)),1))
      # limits <- c(floor(min(lms.down)), ceiling(max(lms.up)))
    }
    
    db$m1 <- (abs(db$f1.zsc.bts)>1 & db$f1.SE<0.4)
    db$m2 <- (abs(db$f2.zsc.bts)>1 & db$f2.SE<0.4)
    db$m3 <- (abs(db$f3.zsc.bts)>1 & db$f3.SE<0.4)
    
    #row names
    rownames(db) <- db$sta_id
    if (length(ylabels)==nrow(db)) rownames(db) <- ylabels

    # if (labdec==TRUE) rownames(db) <- labdecs
    # if (length(r.names) == nrow(db)) rownames(db) <- r.names
    # if (type == ("zsc")) rownames(db) <- substr(rownames(db),5,6)
    
    # marked3 <- subset(db, abs(f3.zsc.bts)>1 & f3.SE<0.4, select =f3.zsc.bts )
  }
  
  #subset
  if (is.numeric(sbset)) 
    db <- db[c(1:min(nrow(db), sbset)), ]
  
  nitems <- nrow(db)
  

  
  #order
  if (sort[1] == "sd") {
    sds <- apply(db[, (1 + nfactors):(2 * nfactors)], 1, 
                 sum)
    db <- db[order(sds), ]
  }
  
  if (sort[1] == "difference") {
    sds <- abs(apply(db[, (1:nfactors)], 1, sd))
    db <- db[order(sds), ]
  }
  
  db$position <- c(1:nitems)
  
  #colors 1:bstrap, 2:standard, 3 marks
  if (is.null(col)) {
    colegend = c(rep("black", nfactors), rep("white", 3))
    dot.col = rep("black", nfactors)
  }  else {
    colegend = c(col[1:nfactors], rep("white", 3))
    dot.col = col[1:nfactors]
  }

  if (plot.std) {
    if (length(pch) >= nfactors) {
      pich = pch[1:nfactors]
    }
    else stop("The vector of symbols provided in 'pch' needs to be at least twice the length of the number of factors, in order to contain (a) a set of symbols for the bootstrap values and (b) a different set of symbols for the standard values.")
  }
  
  #draw
  i = 1
  lbls <- rownames(db)
  
  par(mar=c(4,2,0,0)+0.0)
  
  # declarations at the left
  # if (type=="zsc" &  labdec==TRUE){
  #   labs <- 
  # }
  
  my.dotchart(db[, i], 
           labels = stringr::str_trunc(rownames(db),60,side = "right"), #adjust the max number of characters
           adj=1,
           pch = NA, 
           xlim = limits, 
           xlab = xlab,
           ylab= ylab,
           lcolor = "transparent", 
           # lwd = 1, 
           cex = cex, 
           gcolor = "black", 
           color = "black",
           col.axis="black")
  

  
  #inner lines
  
  par(xpd = TRUE)
  abline(h = c(0.7:(nitems + 0.7)), 
         col = grid.col, 
         lty = 3,
         lwd = 0.4)
  
  par(xpd = FALSE)
  abline(v = seq(ceiling(limits[1]*2)/2, ceiling(limits[2]*2)/2, 0.5),
         col = grid.col, 
         lty = 3, 
         lwd = 0.2)
  abline(v=0, col='grey50')

  
  for (i in 1:nfactors) {
    segments(x0 = db[, i], 
             y0 = db[, "position"] + (vertdist * (i - 1)), 
             x1 = db[, i] + db[, nfactors + i], 
             y1 = db[,"position"] + (vertdist * (i - 1)), 
             lwd = errbar.lwd, 
             lty = errbar.lty,
             col = errbar.col, cex = cex)
    
    segments(x0 = db[, i], 
             y0 = db[, "position"] + (vertdist *(i - 1)), 
             x1 = db[, i] - db[, nfactors + i], 
             y1 = db[,"position"] + (vertdist * (i - 1)), 
             lwd = errbar.lwd, 
             lty = errbar.lty, 
             col = errbar.col, cex = cex)
    
    points(x = db[, i], 
           db[, "position"] + (vertdist * (i -1)), 
           pch = pch[i], 
           type = "p", 
           # lwd = lwd, 
           cex = cex * 0.9, 
           col = dot.col[i],
           bg=dot.col[i])
    
    if (plot.std) {
      points(x = db[, (2 * nfactors) + i], 
             db[, "position"] + (vertdist * (i - 1)), 
             pch = pch[i], 
             type = "p", 
             # lwd = lwd, 
             cex = cex, 
             col = dot.col[i], 
             bg=rgb(0,0,0,0)
             )
      
      
    }
    
    if (plot.mark) {
      if (type=="zsc"){
        x = db[db[,11+i]==TRUE,i]
        y= db[db[,11+i]==TRUE, "position"] + (vertdist * (i - 1))
      }
      
      if (type=="loa"){
        x = db[db[,12+i]==TRUE,i]
        y= db[db[,12+i]==TRUE, "position"] + (vertdist * (i - 1))
      }
      
      points(x=x,y=y,
             pch = pch[i], 
             type = "p", 
             # lwd = lwd, 
             cex = cex*2, 
             col = dot.col[i],
             bg=rgb(0,0,0,0)
             )
      
    }
    
  
  }
  
  
  if (plot.std) 
    leg.length <- 1:(nfactors + 2)
  else leg.length <- 1:nfactors
  legend(leg.pos, 
         inset=0.01,
         legend = c(paste0("Factor ", 1:nfactors),
                    "Empty symbol: standard",
                    "Filled symbol: bootstrap")[leg.length],
         pch = c(pch[1:nfactors], 0, 0)[leg.length], 
         pt.bg = dot.col,
         cex = cex.leg *cex, 
         pt.cex = cex, 
         col = colegend, 
         bg = "white", 
         # box.col="white",
         bty = bty)
}
