rm(list=ls())
library(readr)
library(dplyr)

project <- "displaytag"
BASE_PATH <- "/tmp/ederson/"
padroes_orig <- read.csv(path.expand(paste0(BASE_PATH, project, "_padrao.txt")), sep = "\t", header = F, stringsAsFactors = F,
                         col.names = c("commit", "parent", "ancestor",  "classe", "metodo", "padrao", "operacao"))
smells_orig <- read.csv(path.expand(paste0(BASE_PATH, project, "_smells.txt")), sep = "\t", header = F, stringsAsFactors = F,
                        col.names = c("commit", "parent", "ancestor", "classe", "metodo", "smell", "operacao"))

# Quais padrões estão presentes em cada commit
padroes <- padroes_orig %>%
  filter(operacao != 'Removido') %>%
  select(-operacao)

# Quais smells estão presentes em cada commit
smells <- smells_orig %>%
  mutate(smell = trimws(smell)) %>%
  filter(operacao != 'Removido') %>%
  select(-operacao)

##########

padroes2 <- padroes %>%
  filter(ancestor != 'null' & parent != 'null') %>%
  filter(parent == ancestor) %>%
  select(-ancestor)

# child, parent
commits_elegiveis_padroes <- padroes2 %>%
  select(child = commit, parent) %>%
  distinct()

padroes_do_pai <- commits_elegiveis_padroes %>%
  inner_join(padroes %>% select(-parent, -ancestor), by = c("parent" = "commit")) %>%
  rename(commit = child)

padroes_adicionados <- anti_join(padroes2, padroes_do_pai)
padroes_removidos <- anti_join(padroes_do_pai, padroes2)

delta_padroes <- padroes_adicionados %>%
  mutate(operacao = 'Adicionado') %>%
  rbind(padroes_removidos %>% mutate(operacao = 'Removido'))

#' Quantas adições/remoções de padrões?
nrow(delta_padroes)

###

smells2 <- smells %>%
  filter(ancestor != 'null' & parent != 'null') %>%
  filter(parent == ancestor) %>%
  select(-ancestor)

# child, parent
commits_elegiveis_smells <- smells2 %>%
  select(child = commit, parent) %>%
  distinct()

smells_do_pai <- commits_elegiveis_smells %>%
  inner_join(smells %>% select(-parent, -ancestor), by = c("parent" = "commit")) %>%
  rename(commit = child)

smells_adicionados <- anti_join(smells2, smells_do_pai)
smells_removidos <- anti_join(smells_do_pai, smells2)

delta_smells <- smells_adicionados %>%
  mutate(operacao = 'Adicionado') %>%
  rbind(smells_removidos %>% mutate(operacao = 'Removido'))

#' Quantas adições/remoções de smells?
nrow(delta_smells)


###

x <- delta_padroes %>%
  inner_join(delta_smells, by = c("commit", "classe", "metodo"))

#

unique(padroes$padrao)
unique(smells$smell)

z <- xtabs(~ padrao + smell, data=x)
z
library(vcd)
mosaic(z, shade=T, direction="v")
chisq.test(z)
