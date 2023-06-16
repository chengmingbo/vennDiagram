# venn for classified taxa
group.venn <- function(vectors, disable.logging=disable.logging, cat.cex=1.5, cex=1,
                       cat.pos=NULL, cat.dist=NULL,
                       alpha = 0.5,
                       label=TRUE, lab.cex=1,
                       lab.col= "black", fill=NULL,
                       file=NULL, ext=NULL, width=8, height=8, ...) {

  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

  save <- !is.null(file)
  if (save) { .get.dev(file, ext, height=height, width=width) }

  if ( !requireNamespace("VennDiagram") ) {
    stop("package 'VennDiagram' is required for this function")
  }
  if ( !requireNamespace("RColorBrewer") ) {
    stop("package 'RColorBrewer' is required for this function")
  }

  if (!requireNamespace("grid")) {
    stop("package 'grid' is required to use this function")
  }

  # Generate plot
  # number of vectors to plot
  len <- length(vectors)
  if ( is.null(fill) ) {
    if ( len == 2 ) {
      fill = c("lightpink", "lightblue")
    } else {
      fill = RColorBrewer::brewer.pal(len, "Pastel1")
    }
  } else {
    if ( length(fill) == len ) {
      fill = fill
    } else if ( length(fill) > len ) {
      warning(paste("more colors being provided than required, will ignore ", length(fill)-len, " colors", sep=""))
      fill = fill[1:len]
    } else {
      warning("not enough colors being provided, will use default")
      if ( len == 2 ) {
        fill = c("lightpink", "lightblue")
      } else {
        fill = RColorBrewer::brewer.pal(len, "Pastel1")
      }
    }
  }

  if ( len > 2 && label )  {
    warning("currently only support 2 groups to have actual item labels; will only use numbers")
  } else if ( len > 5 || len < 2 ) {
    stop("please provide 2 to 5 vectors")
  }

  ialpha = rep(alpha, len)
  if ( !is.null(cat.pos) && !is.null(cat.dist) ) {
    v <- VennDiagram::venn.diagram(vectors, fill = fill, alpha = ialpha,
                                   cat.dist=cat.dist, cat.pos=cat.pos,
                                   cat.fontface = "bold", cat.cex = cat.cex, cex=cex,
                                   filename=NULL, disable.logging=disable.logging, ...)
  } else if ( !is.null(cat.pos) && is.null(cat.dist) ) {
    v <- VennDiagram::venn.diagram(vectors, fill = fill, alpha = ialpha,
                                   cat.pos=cat.pos, cat.fontface = "bold",
                                   cat.cex = cat.cex, cex=cex,
                                   filename=NULL, disable.logging=disable.logging, ...)
  } else if ( is.null(cat.pos) && !is.null(cat.dist) ) {
    v <- VennDiagram::venn.diagram(vectors, fill = fill, alpha = ialpha,
                                   cat.fontface = "bold", cat.dist= cat.dist,
                                   cat.cex = cat.cex, cex=cex,
                                   filename=NULL, disable.logging=disable.logging, ...)
  } else {
    v <- VennDiagram::venn.diagram(vectors, fill = fill, alpha = ialpha,
                                   cat.fontface = "bold", cat.cex = cat.cex, cex=cex,
                                   filename=NULL, disable.logging=disable.logging, ...)
  }

  if ( len==2 ) {
    if ( label ) {
      name <- lapply(v,  names)
      lapply(v, function(i) i$label)

      # plot with labels
      # first find out whether the vectors got rearranged
      v.labels <- lapply(v, function(i) i$label)
      v.lab <- vector()
      for ( i in 1:length(v.labels) ) {
        if ( length(v.labels[[i]] %in% names(vectors)) !=0 &&
               isTRUE(v.labels[[i]] %in% names(vectors))  ) {
          v.lab <- c(v.lab, v.labels[[i]])
        }
      }
      v1 <- vectors[[v.lab[1]]]
      v2 <- vectors[[v.lab[2]]]
      v[[5]]$label  <- paste(c(v[[5]]$label,setdiff(v1, v2)), collapse="\n")
      v[[5]]$gp$cex <- lab.cex
      v[[5]]$gp$col <- lab.col
      # in baa only
      v[[6]]$label <- paste(c(v[[6]]$label,setdiff(v2, v1))  , collapse="\n")
      v[[6]]$gp$cex <- lab.cex
      v[[6]]$gp$col <- lab.col
      # intesection
      v[[7]]$label <- paste(c(v[[7]]$label,intersect(v1, v2)), collapse="\n")
      v[[7]]$gp$cex <- lab.cex
      # plot with labels
      v[[7]]$gp$col <- lab.col
    }
  }
  return(v)
}

##%% require: VennDiagram, RAM
##%% if length(set.list) == 2, use group.venn,
##%% else if length(set.list) > 2, venn.diagram, withnumber working then
venn.label.diagram <- function(set.list,
                               filename = NULL,
                               disable.logging=TRUE,
                               with_number=TRUE,
                               fill=ggsci::pal_simpsons()(length(set.list)),
                               alpha = 0.50,
                               col = "transparent",
                               ...){
  require(VennDiagram)
  if(length(set.list) <= 1){
    return(NULL)
  }
  if(length(set.list) == 2){
      v0 <- group.venn(vectors=set.list,
                       label=TRUE,
                       file=filename,
                       fill = fill,
                       alpha = alpha,
                       lab.cex=1.1,
                       disable.logging=disable.logging,
                       ...)
      return(v0)
  }
  v0 <- venn.diagram(set.list,
                     filename=filename,
                     fill = fill,
                     alpha = alpha,
                     col = col,
                     cat.fontface = "bold",
                     ...)
  overlaps <- calculate.overlap(set.list)
  # extract indexes of overlaps from list names
  indx <- as.numeric(stringr::str_sub(names(overlaps), start = 2))
  ## ---- if there's character(0) overlaps, the position will be shifted.
  ## ---- step 1: find indx of character(0)
  null_ids <- c()
  for(ii in 1:length(indx)){
    if(length(overlaps[[glue("a{ii}")]]) == 0){
       null_ids <- c(null_ids, ii)
    }
  }
  ## ---- step 2: indx=-1 if indx in null_ids else subtracted by how many ids > null_ids
  for(x in 1:length(indx)){
    if(all(indx[x] < null_ids)){
      next
    }else if(indx[x] %in% null_ids){
      indx[x] <- -1
    }else{
      minus <- sum(all(indx[x] > null_ids))
      indx[x] <- indx[x] - minus
    }
  }
  #print(overlaps)
  message("length v0", length(v0), " length setlist ", length(set.list), " length overlap ", length(overlaps))
  for(shift in seq(length(v0), 1, by=-1)){
    if(is.null(v0[[shift]]$label)){
      break
    }
  }
  message("length ", length(set.list), " shift ", shift)
  #indx <- c(4, -1, 3, 5, 1, 2, 6)
  for (i in 1:(length(overlaps))){
    if(indx[i] == -1){
      next
    }
    if(with_number & length(overlaps[[i]]) > 0){ ##
      v0[[shift + indx[i] ]]$label <- paste(c(length(overlaps[[i]]), overlaps[[i]]), collapse = "\n")
    }else {
      v0[[shift + indx[i] ]]$label <- paste(overlaps[[i]], collapse = "\n")
    }
  }
  return(v0)
}

