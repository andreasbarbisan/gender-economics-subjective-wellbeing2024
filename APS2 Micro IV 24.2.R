#' ---
#' title: "APS 2 Micro IV 2024.2"
#' author: Andreas Azambuja Barbisan, Bruno Frasão Brazil Leiros, Diogo Roecker Cardozo
#'   e Lorena Liz Giusti e Santos
#' date: "2024-10-07"
#' output:
#'   pdf_document:
#'     toc: yes
#'   html_document:
#'     highlight: textmate
#'     theme: flatly
#'     number_sections: yes
#'     toc: yes
#'     toc_float:
#'       collapsed: no
#'       smooth_scroll: no
#' lang: pt-BR
#' fontsize: 11pt 
#' geometry: margin=0.9in
#' ---
#' # WD, Setup & Data
#' 
#' Este chunk define as configurações iniciais do ambiente de trabalho para a análise dos dados. Primeiro, o diretório de trabalho é definido para o caminho específico onde os arquivos estão localizados. Em seguida, as bibliotecas necessárias são carregadas, que são fundamentais para a manipulação dos dados, análise de regressão e geração de relatórios. A base de dados "WVS_Cross-National_Wave_7_csv_v6_0.csv" é então carregada, restringindo as colunas selecionadas para incluir variáveis relevantes ao estudo, como variáveis demográficas e questões específicas relacionadas ao tema da pesquisa. Finalmente, o comando `pdim()` é utilizado para verificar a estrutura de balanceamento do painel de dados, assegurando a correta organização para as análises subsequentes.
#' 
#' 
## ----setup, include=FALSE----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8")
# Definição de WD
setwd("C:/Andreas/Insper/Semestre 5/Microeconomia 4/APS2")

# Bibliotecas
library(dplyr)
library(haven)
library(plm) 
library(tidyverse)
library(pastecs) 
library(fastDummies)
library(stargazer) 
library(sandwich) 
library(tidyr)
library(survey)
library(forcats)
library(nnet)


# Base

base = read.csv("WVS_Cross-National_Wave_7_csv_v6_0.csv") |> select(A_YEAR, B_COUNTRY, N_REGION_WVS, Q1, Q3, Q5, Q28, Q32, Q46, Q260, Q270, Q271, Q273, X003R2, Q275, Q279, Q280, Q290, Q249, PWGHT, N_REGION_WVS, I_PSU, W_WEIGHT)

# Balanceamento

pdim(base)


#' 
#' 
#' 
#' 
#' # Análise Descritiva
#' 
#' Este chunk de código realiza a análise da importância atribuída a diferentes aspectos da vida (família, trabalho, tempo livre e gratificação pelo trabalho doméstico) por gênero. Primeiro, ele prepara os dados provenientes de um banco de dados global sobre valores e atitudes. Para cada aspecto da vida, cria-se um subconjunto de dados onde as respostas são categorizadas de acordo com sua relevância (como "Muito Importante", "Pouco Importante", etc.) e o gênero do respondente (Masculino ou Feminino).Para cada variável analisada, são geradas contagens e porcentagens que indicam o quanto cada grupo (homens ou mulheres) considera aquele aspecto relevante. Em seguida, são criados gráficos de barras para visualizar comparativamente as percepções entre homens e mulheres, facilitando a interpretação visual das diferenças nas atitudes. Finalmente, cada gráfico é salvo como um arquivo PNG, tornando possível a inclusão das análises em relatórios e apresentações.
#' 
#' 
## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------

# Importância da Familia

dados_familia = base |>
  mutate(Genero = ifelse(Q260 == 2, "Feminino", "Masculino"),
         ImportanciaTexto = factor(Q1, 
                                   levels = c("-5", "-4", "-2", "-1", "1", "2", "3", "4"),
                                   labels = c("NA", "Not asked in the country", "No answer", "Don't know", "Muito Importante", "Importante", "Pouco Importante", "Não Importante"))) |>
  filter(ImportanciaTexto %in% c("Muito Importante", "Importante", "Pouco Importante", "Não Importante")) |>
  mutate(ImportanciaTexto = recode(ImportanciaTexto, 
                                   "Pouco Importante" = "Pouco/Não Importante", 
                                   "Não Importante" = "Pouco/Não Importante")) |>
  group_by(Genero, ImportanciaTexto) |>
  summarise(Contagem = n(), .groups = 'drop') |>
  group_by(Genero) |>  # Agrupa por gênero para calcular a porcentagem dentro de cada grupo
  mutate(Porcentagem = Contagem / sum(Contagem) * 100)

grafico_importancia_familia = ggplot(dados_familia, aes(x = ImportanciaTexto, y = Porcentagem, fill = Genero)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Importância da Família por Gênero",
       x = "Importância", y = "Porcentagem (%)") +
  scale_fill_manual(values = c("Masculino" = "lightblue", "Feminino" = "pink")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = sprintf("%.1f%%", Porcentagem)), position = position_dodge(width = 0.9), vjust = -0.25)

# Salvando o gráfico como PNG
ggsave("importancia_familia_genero.png", plot = grafico_importancia_familia, width = 8, height = 6, dpi = 300)



#' 
#' 
## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------

# Importância do Trabalho por gênero

# Criando o conjunto de dados e o gráfico para Importância do Trabalho
dados_trabalho = base |>
  mutate(Genero = ifelse(Q260 == 2, "Feminino", "Masculino"),
         ImportanciaTexto = factor(Q5, 
                                   levels = c("-5", "-4", "-2", "-1", "1", "2", "3", "4"),
                                   labels = c("NA", "Not asked in the country", "No answer", "Don't know", "Muito Importante", "Importante", "Pouco Importante", "Não Importante"))) |>
  filter(ImportanciaTexto %in% c("Muito Importante", "Importante", "Pouco Importante", "Não Importante")) |>
  mutate(ImportanciaTexto = recode(ImportanciaTexto, 
                                   "Pouco Importante" = "Pouco/Não Importante", 
                                   "Não Importante" = "Pouco/Não Importante")) |>
  group_by(Genero, ImportanciaTexto) |>
  summarise(Contagem = n(), .groups = 'drop') |>
  group_by(Genero) |>  # Agrupa por gênero para calcular a porcentagem dentro de cada grupo
  mutate(Porcentagem = Contagem / sum(Contagem) * 100)

grafico_importancia_trabalho = ggplot(dados_trabalho, aes(x = ImportanciaTexto, y = Porcentagem, fill = Genero)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Importância do Trabalho por Gênero",
       x = "Importância", y = "Porcentagem (%)") +
  scale_fill_manual(values = c("Masculino" = "lightblue", "Feminino" = "pink")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = sprintf("%.1f%%", Porcentagem)), position = position_dodge(width = 0.9), vjust = -0.25)

# Salvando o gráfico como PNG
ggsave("importancia_trabalho_genero.png", plot = grafico_importancia_trabalho, width = 8, height = 6, dpi = 300)


#' 
## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------

# Importância de Tempo Livre por Gênero

dados_tempo_livre = base |>
  mutate(Genero = ifelse(Q260 == 2, "Feminino", "Masculino"),
         ImportanciaTexto = factor(Q3, 
                                   levels = c("-5", "-4", "-2", "-1", "1", "2", "3", "4"),
                                   labels = c("NA", "Not asked in the country", "No answer", "Don't know", "Muito Importante", "Importante", "Pouco Importante", "Não Importante"))) |>
  filter(ImportanciaTexto %in% c("Muito Importante", "Importante", "Pouco Importante", "Não Importante")) |>
  mutate(ImportanciaTexto = recode(ImportanciaTexto, 
                                   "Pouco Importante" = "Pouco/Não Importante", 
                                   "Não Importante" = "Pouco/Não Importante")) |>
  group_by(Genero, ImportanciaTexto) |>
  summarise(Contagem = n(), .groups = 'drop') |>
  group_by(Genero) |>  # Agrupa por gênero para calcular a porcentagem dentro de cada grupo
  mutate(Porcentagem = Contagem / sum(Contagem) * 100)

grafico_importancia_tempo_livre = ggplot(dados_tempo_livre, aes(x = ImportanciaTexto, y = Porcentagem, fill = Genero)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Importância do Tempo Livre por Gênero",
       x = "Importância", y = "Porcentagem (%)") +
  scale_fill_manual(values = c("Masculino" = "lightblue", "Feminino" = "pink")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = sprintf("%.1f%%", Porcentagem)), position = position_dodge(width = 0.9), vjust = -0.25)

# Salvando o gráfico como PNG
ggsave("importancia_tempo_livre_genero.png", plot = grafico_importancia_tempo_livre, width = 8, height = 6, dpi = 300)



#' 
## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------

# Aprovação de ser gratificante ser dona de casa

dados_trabalho_domestico = base |>
  mutate(Genero = ifelse(Q260 == 2, "Feminino", "Masculino"),
         OpiniaoTexto = factor(Q32, 
                               levels = c("-5", "-4", "-2", "-1", "1", "2", "3", "4"),
                               labels = c("NA", "Not asked in the country", "No answer", "Don't know", "Concordo Fortemente", "Concordo", "Discordo", "Discordo Fortemente"))) |>
  filter(OpiniaoTexto %in% c("Concordo Fortemente", "Concordo", "Discordo", "Discordo Fortemente")) |>
  group_by(Genero, OpiniaoTexto) |>
  summarise(Contagem = n(), .groups = 'drop') |>
  group_by(Genero) |>  # Agrupa por gênero para calcular a porcentagem dentro de cada grupo
  mutate(Porcentagem = Contagem / sum(Contagem) * 100)

grafico_trabalho_domestico = ggplot(dados_trabalho_domestico, aes(x = OpiniaoTexto, y = Porcentagem, fill = Genero)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Aprovação da afirmação que ser dona de casa é tão gratificante quanto trabalhar ",
       x = "Opinião sobre Trabalho Doméstico", y = "Porcentagem (%)") +
  scale_fill_manual(values = c("Masculino" = "lightblue", "Feminino" = "pink")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = sprintf("%.1f%%", Porcentagem)), position = position_dodge(width = 0.9), vjust = -0.25)

# Salvando o gráfico como PNG
ggsave("trabalho_domestico_genero.png", plot = grafico_trabalho_domestico, width = 8, height = 6, dpi = 300)



#' 
## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------

# Nível de felicidade por gênero

dados_felicidade_familia = base |>
  mutate(Genero = ifelse(Q260 == 2, "Feminino", "Masculino"),
         ImportanciaTexto = factor(Q1, 
                                   levels = c("-5", "-4", "-2", "-1", "1", "2", "3", "4"),
                                   labels = c("NA", "Not asked in the country", "No answer", "Don't know", "Muito Importante", "Importante", "Pouco Importante", "Não Importante")),
         FelicidadeTexto = factor(Q46, 
                                  levels = c("1", "2", "3", "4"),
                                  labels = c("Muito Feliz", "Feliz", "Pouco Feliz", "Nada Feliz"))) |>
  filter(ImportanciaTexto %in% c("Muito Importante", "Importante", "Pouco Importante", "Não Importante")) |>
  group_by(Genero, ImportanciaTexto, FelicidadeTexto) |>
  summarise(Contagem = n(), .groups = 'drop') |>
  group_by(Genero, ImportanciaTexto) |>  # Agrupa por gênero e importância para calcular a porcentagem dentro de cada grupo
  mutate(Porcentagem = Contagem / sum(Contagem) * 100)

# Criando o gráfico
grafico_felicidade_familia = ggplot(dados_felicidade_familia, aes(x = ImportanciaTexto, y = Porcentagem, fill = FelicidadeTexto)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", Porcentagem)), 
            position = position_stack(vjust = 0.5), size = 3) +
  facet_wrap(~Genero) +
  labs(title = "Níveis de Felicidade por Importância da Família e Gênero",
       x = "Importância da Família", y = "Porcentagem (%)", fill = "Nível de Felicidade") +
  scale_fill_manual(values = c("Muito Feliz" = "#C3B1E1",  # Roxo pastel
                               "Feliz" = "#FDFD96",         # Amarelo pastel
                               "Pouco Feliz" = "#AEC6CF",   # Azul pastel
                               "Nada Feliz" = "#CFCFC4")) + # Cinza claro
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Salvando o gráfico como PNG
ggsave("felicidade_importancia_familia_genero.png", plot = grafico_felicidade_familia, width = 10, height = 6, dpi = 300)


#' 
#' # Análise Econométrica
#' 
#' Este código realiza o tratamento e análise dos dados da World Values Survey, começando pela filtragem da base para remover valores inválidos e criar novas variáveis, como a felicidade binária (1 para "Feliz" e 0 para "Não Feliz"). Em seguida, são criadas variáveis categóricas para a importância da família e do trabalho, o número de pessoas na casa e a percepção de igualdade de direitos. O plano amostral é definido usando o peso amostral e estratos geográficos para garantir a representatividade dos resultados. Em seguida, são ajustados modelos de regressão (MQO, Probit e Logit) considerando o plano amostral e corrigindo para estratos com uma única unidade primária de amostragem (PSU). Por fim, os resultados das estimativas são apresentados em HTML utilizando o pacote Stargazer.
#'   
## ----message=FALSE, warning=FALSE--------------------------------------------------------------------------------
dados_filtrados = base |>
  filter(Q46 != "-2" & Q46 != "-1") |>  # Remover valores inválidos de felicidade
  mutate(
    felicidade_bin = ifelse(Q46 %in% c("1", "2"), 1, 0),  # Feliz = 1, Não Feliz = 0
    import_fem_familia = factor(Q1, levels = c("1", "2", "3", "4"), labels = c("Muito Importante", "Importante", "Pouco Importante", "Não Importante")),
    import_fem_trab = factor(Q5, levels = c("1", "2", "3", "4"), labels = c("Muito Importante", "Importante", "Pouco Importante", "Não Importante")),
    pessoascasa = as.numeric(Q270),
    igualdade_direitos = factor(Q249, levels = c("1", "2"), labels = c("Concorda", "Não Concorda"))
  ) |>
  filter(
    !is.na(felicidade_bin) & !is.na(import_fem_familia) & !is.na(import_fem_trab) &
    !is.na(pessoascasa) & !is.na(igualdade_direitos)
  )

# Ajuste para estratos com apenas uma PSU
options(survey.lonely.psu = "adjust")

# Definir o plano amostral com peso, estrato e UPA
survey.design <- svydesign(
  id = ~I_PSU,             # Unidade Primária de Amostragem (PSU)
  strata = ~N_REGION_WVS,  # Estrato geográfico
  weights = ~W_WEIGHT,     # Peso amostral
  data = dados_filtrados,  # Dados
  nest = TRUE              # Evitar erros com clusters
)

# Estimação do Modelo MQO (Mínimos Quadrados Ordinários) com o plano amostral para felicidade binária
modelo_MPL_survey = svyglm(
  felicidade_bin ~ import_fem_familia + import_fem_trab + pessoascasa + igualdade_direitos,
  design = survey.design
)

# Estimação do Modelo Probit com o plano amostral para felicidade binária
modelo_probit_survey = svyglm(
  felicidade_bin ~ import_fem_familia + import_fem_trab + pessoascasa + igualdade_direitos,
  design = survey.design,
  family = binomial(link = "probit")
)

# Estimação do Modelo Logit com o plano amostral para felicidade binária
modelo_logit_survey = svyglm(
  felicidade_bin ~ import_fem_familia + import_fem_trab + pessoascasa + igualdade_direitos,
  design = survey.design,
  family = binomial(link = "logit")
)

# Apresentação dos Resultados com Stargazer em HTML
stargazer(modelo_MPL_survey, modelo_probit_survey, modelo_logit_survey, type = "html",
          title = "Resultados da Estimação dos Modelos MPL, Probit e Logit (Ajustado para Survey)",
          dep.var.labels = c("Felicidade (Binária)"),
          covariate.labels = c("Família: Importante", "Família: Pouco Importante", "Família: Não Importante", 
                               "Trabalho: Importante", "Trabalho: Pouco Importante", "Trabalho: Não Importante", 
                               "Pessoas na Casa", "Igualdade de Direitos: Não Concorda"),
          omit.stat = c("f", "ser"),
          out = "resultados_modelos_survey_binaria.html")


