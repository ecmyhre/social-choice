GetColumns <- function(m, parameter, interaction, incl = FALSE){
  parnames <- dimnames(m)$parameters
  if (incl == TRUE) {
    parameters <- grepl(parameter, parnames)
  } else if (incl == FALSE){
    parameters <- grepl(parameter, parnames) & !grepl(interaction, parnames)
  }
  return(m[,parameters])
}

CollapseInteraction <- function(draws, field, interaction) {
  parnames <- dimnames(draws)$parameters
  mean <- rowSums(draws[,c(field,paste0(interaction,":",field))])
  MMU <- sapply(c("32992","33089","33825","34700"), function(MMU){
    rowSums(draws[,grepl(MMU, parnames)])})
  draws <- cbind(mean,MMU)
  return(draws)
}

shift_draws <- function(d) {
  r <- sweep(d[, -1], MARGIN = 1, STATS = d[, 1], FUN = "+")
  allParms <- data.frame(cbind(d[, 1, drop = FALSE], r))
  return(allParms)
}

mean_sd <- function(x, decimals = 2){
  paste0(round(mean(x), decimals), " (", round(sd(x), decimals), ")")  
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
col <- gg_color_hue(4)
