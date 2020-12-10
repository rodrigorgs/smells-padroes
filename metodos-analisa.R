# TODO:
# - Cada padrao so possui um papel relacionado a metodos?
# - Rodar em repo de design patterns de exemplo para ver se ele localiza os design patterns e se esses design patterns possuem smells
#   - https://github.com/iluwatar/java-design-patterns
#   - https://github.com/kamranahmedse/design-patterns-for-humans
#   - https://github.com/clarketm/java-design-patterns
# - Idem para smells
#   - https://github.com/nerdschoolbergen/code-smells (nao sao exatamente os mesmos smells)

#
# Para ver:
# axion - LeafWhereNode.evaluate

# TODO: analise por sistema, para identificar relacoes especificas de determinados dominios

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

# sistemas <- c("axion-1.0-M2", "collections-3.2.1", "colt-1.2.0", "displaytag-1.2", "drawswf-1.2.9", "emma-2.0.5312", "fitjava-1.1", "FreeCS", "itext-5.0.3", "jena-2.6.3", "jext-5.0", "jgraph-5.13.0.0", "jgrapht-0.8.1", "jmoney-0.4.4", "jOggPlayer114s", "jparse-0.96", "jpf-1.5.1", "junit-4.10", "nekohtml-1.9.14", "oscache-2.3", "picocontainer-2.10.2", "pooka-3.0-080505", "proguard-4.9", "quickserver-1.4.7", "sablecc-3.2")
# sistemas <- "log4j"
source('metodos-carrega.R', chdir = T)

mymosaic_unordered <- function(tab, ...) {
  mosaic(tab, shade=T,  direction = "v",
         rot_labels=c(45,0,0,0), # rot_labels=c(45,0,0,90),
         just_labels = c("left", 
                         "center", 
                         "center", 
                         "right"),
         gp_labels=(gpar(fontsize=8)), ...)
}

mymosaic <- function(tab, ...) {
  tab <- tab[order(tab[,1] / rowSums(tab)),]
  mymosaic_unordered(tab, ...)
}

# Remove classes de teste
metodos <- metodos %>%
  filter(!grepl(".*Test.*", classe))

######################################################################

# assocstats(tab): Cramer's V
# interpreting V < 0.3 as small, V < 0.5 as medium, and
# V > 0.5 as large effect size

tab <- xtabs(~ padrao + smell, data = metodos)
t(tab) %>% as.data.frame.matrix() %>% pander()

###########

#' Estatística descritiva

# Classes:
nrow(distinct(metodos, sistema, classe))
nrow(distinct(metodos, sistema, elemento))

# Dados por sistema

x <- metodos %>%
  group_by(sistema, classe, elemento) %>%
  summarise()

y <- x %>%
  group_by(sistema) %>%
  summarise(n_classes = n_distinct(classe),
            n_metodos = n_distinct(elemento)) %>%
  arrange(tolower(sistema))

y %>% data.frame()
sum(y$n_classes)
sum(y$n_metodos)

###

x <- metodos %>%
  group_by(sistema, elemento) %>%
  summarise(hasPadrao = any(hasPadrao),
            hasSmell = any(hasSmell)) %>%
  replace_na(list(hasPadrao = FALSE, hasSmell = FALSE))

# N´úmero de m´étodos com padr˜ão/smell
sum(x$hasPadrao)
100 * mean(x$hasPadrao)
sum(x$hasSmell)
100 * mean(x$hasSmell)

tab <- xtabs(~ hasPadrao + hasSmell, data = x)
addmargins(tab)

# Padroes e smells mais frequentes

metodos %>%
  count(padrao) %>%
  arrange(desc(n)) %>% head(4)

metodos %>%
  count(smell) %>%
  arrange(desc(n)) %>% head(4)


# n <- length(unique(classes$classe))
# n
# length(unique(padroes$classe))
# length(unique(padroes$classe)) / n
# length(unique(smells$classe))
# length(unique(smells$classe)) / n
# 
# sort(table(padroes$padrao))
# sort(table(smells$smell))

####################################

#' # QP1: Existe associação entre ocorrência de padrões e ocorrência de smells? 
x <- metodos %>%
  group_by(sistema, elemento) %>%
  summarise(hasPadrao = any(hasPadrao),
            hasSmell = any(hasSmell)) %>%
  replace_na(list(hasPadrao = FALSE, hasSmell = FALSE))

tab <- xtabs(~ hasPadrao + hasSmell, data=x)
addmargins(tab)
tab
prop.table(tab, margin = 1) * 100
#mosaicplot(tab, shade = T)
tab %>% pander()
mymosaic(tab)
# chisq.test(tab, correct = F)
assocstats(tab) %>% print()
#
mcnemar.test(tab)
chisq.test(tab)
prop.test(tab)
fisher.test(tab)

pdf("pdf/fig_temPadraoTemSmell.pdf")
mymosaic(tab)
dev.off()


#' --------------

#' # QP2: Quais padrões possuem maior propensão a apresentar smells?
tab <- xtabs(~ padrao + hasSmell, data=metodos)
tab %>% pander()
mean(tab < 5)
names(dimnames(tab)) <- c(" ", "Possui Smell?")
mymosaic(tab)
chisq.test(tab)
assocstats(tab) %>% print()

pdf("pdf/fig_propenApresentarSmells.pdf")
mymosaic(tab)
dev.off()

#' ### Amostra

# metodos %>%
#   filter(hasSmell,
#          padrao == "Adapter" | padrao == 'State') %>%
#   View()

#' # QP3: Cada padrao com cada smell
m1 <- metodos
m1$padrao <- factor(m1$padrao, ordered = TRUE, levels = c("Adapter", "Decorator", "State", "Template Method", "Factory Method", "Proxy", "Observer",  "Bridge", "Visitor", "Composite", "Command"))
m1$smell <- factor(m1$smell, ordered = TRUE, levels = c("Feature Envy", "Shotgun Surgery", "Intensive Coupling", "Dispersed Coupling", "Brain Method") %>% rev())
tab <- xtabs(~ padrao + smell, data=m1)
tab %>% pander()
mean(tab < 5)
mymosaic_unordered(tab)
#mymosaic(t(tab))
chisq.test(tab)
assocstats(tab) %>% print()

pdf("pdf/fig_todosPadroesTodosSmells.pdf")
mymosaic_unordered(tab)
dev.off()


#chisq.test(tab)$residuals %>% View()
#chisq.test(tab)$stdres %>% View()
# x <- tab %>% as.data.frame() %>%
#   inner_join(chisq.test(tab)$residuals %>% as.data.frame(), by=c("smell", "padrao"))
# x

#' # Extra
#' 
#' Smells mais comuns em m´étodos que implementam padr˜ões
#' 

# com padrao
metodos %>%
  filter(!is.na(padrao)) %>%
  count(smell) %>%
  arrange(desc(n))

# sem padrao
metodos %>%
  filter(is.na(padrao)) %>%
  count(smell) %>%
  arrange(desc(n))

# tem featureEnvy ou não
tab <- metodos %>%
  # filter(is.na(smell) | smell == "Feature Envy") %>%
  mutate(hasFeatureEnvy = smell == "Feature Envy") %>%
  xtabs(~ padrao + hasFeatureEnvy, data = .)
mymosaic(tab)


tab <- metodos %>%
  # filter(!is.na(padrao)) %>%
  group_by(elemento) %>%
  # summarise(hasAdapter = any(padrao == "Adapter"),
  #           hasFeatureEnvy = any(smell == "Feature Envy")) %>%
  summarise(hasAdapter = any(padrao == "Template Method" | padrao == "Factory Method"),
            hasFeatureEnvy = any(smell == "Shotgun Surgery")) %>%
  replace_na(list(hasAdapter = FALSE, hasFeatureEnvy = FALSE)) %>%
  xtabs(~ hasAdapter + hasFeatureEnvy, data = .)
nrow(metodos)
mymosaic(tab)
tab
chisq.test(tab)
fisher.test(tab)
assocstats(tab)
oddsratio(tab, log=F)


  # filter(is.na(smell) | smell == "Feature Envy") %>%
  mutate(hasFeatureEnvy = smell == "Feature Envy") %>%
  xtabs(~ padrao + hasFeatureEnvy, data = .)


#' Para inspecao manual:

inspecionar <- metodos %>%
  filter(padrao == "Template Method",
         smell == "Shotgun Surgery")
inspecionar %>% select(elemento) %>% pander()

inspecionar <- metodos %>%
  filter(padrao == "Factory Method",
         smell == "Shotgun Surgery")
inspecionar %>% select(elemento) %>% pander()

inspecionar <- metodos %>%
  filter(padrao == "Adapter",
         smell == "Feature Envy")
inspecionar %>% select(elemento) %>% pander()

# inspecionar <- metodos %>%
#   filter(padrao == "State",
#          smell == "Feature Envy")
# inspecionar %>% select(elemento) %>% sample_frac() %>% pander()
# 
# inspecionar <- metodos %>%
#   filter(padrao == "State",
#          is.na(smell))
# inspecionar %>% select(elemento) %>% sample_frac() %>% pander()
