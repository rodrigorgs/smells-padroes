rm(list=ls())
library(readr)
library(dplyr)

# project <- "displaytag"
# project <- "commons-io"
project <- "fastjson"

# BASE_PATH <- "~/Dropbox/ARTIGOS/arquivos_2020/17-06-2020/"
BASE_PATH <- "/tmp/fastjson/"
padroes_orig <- read.csv(path.expand(paste0(BASE_PATH, project, "_padrao.txt")), sep = "\t", header = F, stringsAsFactors = F,
  col.names = c("commit", "parent", "ancestor",  "classe", "metodo", "padrao", "operacao"))
smells_orig <- read.csv(path.expand(paste0(BASE_PATH, project, "_smells.txt")), sep = "\t", header = F, stringsAsFactors = F,
  col.names = c("commit", "parent", "ancestor", "classe", "metodo", "smell", "operacao"))

padroes <- padroes_orig %>%
  filter(ancestor != 'null' & parent != 'null') %>%
  filter(parent == ancestor) %>%
  filter(operacao != 'Passado') %>%
  select(-ancestor)

smells <- smells_orig %>%
  mutate(smell = trimws(smell)) %>%
  filter(ancestor != 'null' & parent != 'null') %>%
  filter(parent == ancestor) %>%
  filter(operacao != 'Passado') %>%
  select(-ancestor)

##########

x <- padroes %>% filter(operacao != 'Passado')
#' Quantas adições/remoções de padrões?
padroes %>% filter(operacao != 'Passado') %>% tally()

#' Quantas adições/remoções de smells?
smells %>% filter(operacao != 'Passado') %>% tally()


##########

data <- padroes %>%
  rename(operacao_padrao = operacao) %>%
  filter(operacao_padrao == 'Adicionado') %>%
  left_join(smells)

xtabs(~ padrao + smell, data=data, exclude=NULL, na.action=na.pass)

###########

#' Agrupando smells em sim/não

data2 <- data %>%
  group_by(commit, parent, classe, metodo, padrao) %>%
  summarise(added_smell = any(operacao == 'Adicionado') %>% coalesce(FALSE),
            removed_smell = any(operacao == 'Removido') %>% coalesce(FALSE))

#' Ao adicionar um determinado padrão P, quantos % das vezes estamos adicionando algum smell (no mesmo commit)?
t <- xtabs(~ padrao + added_smell, data=data2, exclude=NULL, na.action=na.pass)
t
mosaicplot(t, shade=T)
chisq.test(t)

# t <- xtabs(~ padrao + removed_smell, data=data2, exclude=NULL, na.action=na.pass)
# t
# mosaicplot(t, shade=T)
# chisq.test(t)
