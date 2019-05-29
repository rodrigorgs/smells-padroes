plot_contingency <- function(tab, xaxes, yaxes, xlab, ylab, basename, nlab, baseline) {
  if (!missing(xaxes))
    rownames(tab) <- xaxes
  if (!missing(yaxes))
  colnames(tab) <- yaxes
  
  z <- t(tab) %>% as.data.frame()
  colnames(z) <- c("status", "score", "freq")
  
  bars <- z %>%
    group_by(score) %>%
    summarise(pbar = freq[status == basename] / sum(freq),
              n = sum(freq)) %>%
    mutate(se = sqrt(pbar * (1-pbar)/n),
           e = qnorm(0.975)*se,
           min = pbar - e,
           max = pbar + e)
  
  labels <- z %>%
    group_by(score) %>%
    summarise(n = sum(freq)) %>%
    mutate(prop = format(round(100 * n / sum(n), 2), nsmall=2) %>% paste0("%"))
  
  x <- prop.table(t(tab), 2) %>% as.data.frame()
  colnames(x) <- c("status", "score", "freq")
  x$status <- factor(x$status,
                     levels=levels(x$status)[order(levels(x$status), decreasing = FALSE)])
  x <- x %>% arrange(score, status)
  x$status <- factor(x$status, levels=rev(levels(x$status)))
  percentages <- rep(paste0('', labels$prop, ''), each=2)
  absolutes <- rep(paste(prettyNum(labels$n, big.mark=",", preserve.width = "none")), each=2)
  g <- ggplot(x, aes(x=score, fill=status, y=freq)) +
    geom_bar(position="fill", stat="identity") +
    geom_errorbar(aes(ymin = pmax(0, rep(bars$min, each=2)), ymax = pmin(1, rep(bars$max, each=2))), width=0.25)
  
  if (!missing(baseline))
    g <- g + geom_hline(yintercept = baseline, linetype=2)
  
  g <- g +
    geom_text(aes(label=absolutes, y = 1), vjust=-0.2, size = 3.5) +
    geom_text(aes(label=percentages, y = 0), vjust=1.2, size=3.5) +
    scale_fill_manual(values=c("#00b55e", "#bd3a31"), name=ylab) +
    scale_y_continuous(labels = scales::percent, expand=c(0, 0.07))
  if (!missing(nlab))
    g <- g + ylab(nlab)
  if (!missing(xlab))
    g <- g + xlab(xlab)
  
  g
}

analyze_contingency <- function(tab) {
  t(tab) %>% print()
  prop.table(t(tab), 2) %>% print()
  chisq.test(tab) %>% print()
  assocstats(tab) %>% print()
}
