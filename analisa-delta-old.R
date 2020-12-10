rm(list=ls())
library(readr)
library(dplyr)

project <- "displaytag"
BASE_PATH <- "~/Dropbox/ARTIGOS/arquivos_2020/29-06-2020/displaytag_result/"
padroes_orig <- read.csv(path.expand(paste0(BASE_PATH, project, "_padrao.txt")), sep = "\t", header = F, stringsAsFactors = F,
                         col.names = c("commit", "parent", "ancestor",  "classe", "metodo", "padrao", "operacao"))
smells_orig <- read.csv(path.expand(paste0(BASE_PATH, project, "_smells.txt")), sep = "\t", header = F, stringsAsFactors = F,
                        col.names = c("commit", "parent", "ancestor", "classe", "metodo", "smell", "operacao"))
releases_orig <- read.csv(path.expand(paste0(BASE_PATH, project, "_releases.txt")), sep = "\t", header = F, stringsAsFactors = F,
                     col.names = c("commit", "release"))

releases <- releases_orig %>%
  mutate(parent = lag(commit))

# Quais padrões estão presentes em cada commit
padroes <- padroes_orig %>%
  filter(operacao != 'Removido') %>%
  select(-operacao) %>%
  filter(commit %in% releases$commit) # ***

# Quais smells estão presentes em cada commit
smells <- smells_orig %>%
  mutate(smell = trimws(smell)) %>%
  filter(operacao != 'Removido') %>%
  select(-operacao) %>%
  filter(commit %in% releases$commit) # ***

##########

padroes2 <- padroes %>%
  # filter(ancestor != 'null' & parent != 'null') %>%
  # filter(parent == ancestor) %>%
  select(-ancestor, -parent) %>%
  inner_join(releases) %>%
  filter(!is.na(parent))

# child, parent
commits_elegiveis_padroes <- padroes2 %>%
  select(child = commit, parent) %>%
  distinct()

padroes_do_pai <- commits_elegiveis_padroes %>%
  inner_join(padroes2 %>% select(-parent), by = c("parent" = "commit")) %>%
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
  select(-ancestor, -parent) %>%
  inner_join(releases) %>%
  filter(!is.na(parent))

# child, parent
commits_elegiveis_smells <- smells2 %>%
  select(child = commit, parent) %>%
  distinct()

smells_do_pai <- commits_elegiveis_smells %>%
  inner_join(smells2 %>% select(-parent), by = c("parent" = "commit")) %>%
  rename(commit = child)

smells_adicionados <- anti_join(smells2, smells_do_pai)
smells_removidos <- anti_join(smells_do_pai, smells2)

delta_smells <- smells_adicionados %>%
  mutate(operacao = 'Adicionado') %>%
  rbind(smells_removidos %>% mutate(operacao = 'Removido'))

#' Quantas adições/remoções de smells?
nrow(delta_smells)


###
# E os casos em que se adiciona um padrão mas não há alteração nos smells?

x <- delta_padroes %>%
  inner_join(delta_smells, by = c("commit", "classe", "metodo")) %>%
  filter(operacao.x == 'Adicionado' & operacao.y == 'Adicionado')

z <- xtabs(~ padrao + smell, data=x)
z
library(vcd)
mosaic(z, shade=T, direction="v")
chisq.test(z)


###

x <- delta_padroes %>%
  inner_join(delta_smells, by = c("commit", "classe", "metodo")) %>%
  filter(operacao.x == 'Adicionado' & operacao.y == 'Removido')

z <- xtabs(~ padrao + smell, data=x)
z
library(vcd)
mosaic(z, shade=T, direction="v")
chisq.test(z)


