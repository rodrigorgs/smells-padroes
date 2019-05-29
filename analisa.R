# TODO: analise por sistema

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
rm(list=ls(all = TRUE))
library(dplyr)
library(readr)
library(tidyr)
library(vcd)
library(pander)

pdf.options(width = 5, height = 5)

sistemas <- c("axion-1.0-M2", "collections-3.2.1", "colt-1.2.0", "displaytag-1.2", "drawswf-1.2.9", "emma-2.0.5312", "fitjava-1.1", "FreeCS", "itext-5.0.3", "jena-2.6.3", "jext-5.0", "jgraph-5.13.0.0", "jgrapht-0.8.1", "jmoney-0.4.4", "jOggPlayer114s", "jparse-0.96", "jpf-1.5.1", "junit-4.10", "nekohtml-1.9.14", "oscache-2.3", "picocontainer-2.10.2", "pooka-3.0-080505", "proguard-4.9", "quickserver-1.4.7", "sablecc-3.2")
# sistemas <- "log4j"
source('carrega.R', chdir = T)

mymosaic <- function(tab, ...) {
  tab <- tab[order(tab[,1] / rowSums(tab)),]
  mosaic(tab, shade=T,  direction = "v",
         rot_labels=c(45,0,0,90),
         just_labels = c("left", 
                         "center", 
                         "center", 
                         "right"),
         gp_labels=(gpar(fontsize=8)), ...)
}

######################################################################

# assocstats(tab): Cramer's V
# interpreting V < 0.3 as small, V < 0.5 as medium, and
# V > 0.5 as large effect size

tab <- xtabs(~ padrao + smell, data = padroes_smells)
t(tab) %>% as.data.frame.matrix() %>% pander()

###########

#' Estatística descritiva

n <- length(unique(classes$classe))
n
length(unique(padroes$classe))
length(unique(padroes$classe)) / n
length(unique(smells$classe))
length(unique(smells$classe)) / n

sort(table(padroes$padrao))
sort(table(smells$smell))

####################################

#' # QP1: Existe associação entre ocorrência de padrões e ocorrência de smells? 
tab <- xtabs(~ hasPadrao + hasSmell, data=classes)
#mosaicplot(tab, shade = T)
tab %>% pander()
mymosaic(tab)
# chisq.test(tab, correct = F)
assocstats(tab) %>% print()
#
mcnemar.test(tab)
prop.test(tab)
fisher.test(tab)

#' --------------

#' # QP2: Quais são os smells mais frequentes nas classes que possuem padrões?
tab <- xtabs(~ smell + hasPadrao, data=smells)
mean(tab < 5)
names(dimnames(tab)) <- c(" ", "Possui Padrao?")
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

pdf("pdf/fig_smellsFrequentesPadroes.pdf")
mymosaic(tab)
dev.off()

#' # QP2: Quais são as categorias de smell mais frequentes nas classes que possuem padrões?
tab <- xtabs(~ catSmell + hasPadrao, data=smells)
tab %>% pander()
mean(tab < 5)
names(dimnames(tab)) <- c(" ", "Possui Padrao?")
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

pdf("pdf/fig_catSmellsApresentarPadroes.pdf")
mymosaic(tab)
dev.off()


#' --------------

#' # QP3: Quais padrões possuem maior propensão a apresentar smells?
tab <- xtabs(~ padrao + hasSmell, data=padroes)
tab %>% pander()
mean(tab < 5)
names(dimnames(tab)) <- c(" ", "Possui Smell?")
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

pdf("pdf/fig_propenApresentarSmells.pdf")
mymosaic(tab)
dev.off()


#' # QP3: Quais categorias de padrão possuem maior propensão a apresentar smell?
tab <- xtabs(~ catPadrao + hasSmell, data=padroes)
tab %>% pander()
mean(tab < 5)
names(dimnames(tab)) <- c(" ", "Possui Smell?")
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

pdf("pdf/fig_cadaCategPadraoComSmells.pdf")
mymosaic(tab)
dev.off()


#' --------------

#' # QP4: Cada categoria de padrao com cada categoria de smell
tab <- xtabs(~ catPadrao + catSmell, data=padroes_smells_simultaneos)
tab %>% pander()
mean(tab < 5)
names(dimnames(tab)) <- c(" ", "  ")
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

pdf("pdf/fig_catPadraoCatSmells.pdf")
mymosaic(tab)
dev.off()



#' # QP4: Cada padrao com cada smell
tab <- xtabs(~ padrao + smell, data=padroes_smells_simultaneos)
tab %>% pander()
mean(tab < 5)
mymosaic(tab)
mymosaic(t(tab))
chisq.test(tab)
assocstats(tab) %>% print()
#chisq.test(tab)$residuals %>% View()
#chisq.test(tab)$stdres %>% View()
# x <- tab %>% as.data.frame() %>%
#   inner_join(chisq.test(tab)$residuals %>% as.data.frame(), by=c("smell", "padrao"))
# x


# tab
# chisq.test(tab)$expected %>% mymosaic()
#' --------------

#' # Questoes que ficaram de fora

#' # Cada categoria de padrao com cada smell
tab <- xtabs(~ catPadrao + smell, data=padroes_smells_simultaneos)
tab %>% pander()
mean(tab < 5)
mymosaic(t(tab))
chisq.test(tab)
assocstats(tab) %>% print()
chisq.test(tab)$residuals %>% pander()

#' # Cada categoria de smell com cada padrao
tab <- xtabs(~ catSmell + padrao, data=padroes_smells_simultaneos)
tab %>% pander()
mean(tab < 5)
mymosaic(t(tab))
chisq.test(tab)
assocstats(tab) %>% print()


