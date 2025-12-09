library(bslib)
library(purrr)
library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)


##############################################################################
##                         TRATAMENTO DE DADOS                              ##
##############################################################################

dados_originais = readRDS("/home/gallisi/Documents/2025.2/nomes_lista")

dados_tratados = map_df(dados_originais, function(i){
  map_df(i$res, function(r){
    data.frame(
      nomes = i$nome,
      periodo = r$periodo,
      frequencia = r$frequencia
    )
  })
}) %>%
  group_by(nomes) %>%
  mutate(total = sum(frequencia)) %>%
  pivot_wider(
    names_from = periodo,
    values_from = frequencia,
    values_fill = 0 #usando 0 no lugar de NA para facilitar os cálculos
  ) 

nomes_colunas = c("Nomes", "Total","Antes de 1930", "1930-1940", "1940-1950", "1950-1960", 
                  "1960-1970", "1970-1980", "1980-1990", "1990-2000", 
                  "2000-2010")

names(dados_tratados) = nomes_colunas

freq_relativa = function(freq_absoluta, casas_decimais){
  x = freq_absoluta/sum(freq_absoluta)
  return(round(x, casas_decimais))
}

##############################################################################
##                                   UI                                     ##
##############################################################################

ui = page_navbar(
  title = "Nomes da TTA",
  theme = bs_theme(version = 5, bootswatch = "lux"),
    
    #Visão Geral
  nav_panel("Visão Geral",
           layout_columns(
             col_widths = c(4,8),
             
               selectInput(
                 "periodo",
                 "Escolha o período:",
                 choices = nomes_colunas[3:11], #posiçẽs com nomes de colunas
                 multiple = TRUE),
               
               radioButtons("ordenacao",
                            "Selecione a ordenação",
                            choices = c("Crescente","Decrescente","A-Z","Z-A"),
                            inline = TRUE),
             
             card(
               #card_header("Ranking de Nomes"),
               tableOutput("tabela_ranking"),
             ),
             
             card(
               #card_header("Gráfico de Nomes"),
               class = "card bg-light mb-3",
               plotOutput("plot_ranking"),
             )
             
    )),
    
    #Visão por nome
    nav_panel("Dados Por Nome",
              layout_columns(
                col_widths = c(4,4,4),
                
                selectInput("busca_nome", 
                            "Digite um nome:",
                            choices = dados_tratados$Nomes, 
                            multiple = FALSE),
                
                selectInput(
                  "periodo_2",
                  "Escolha o período:",
                  choices = nomes_colunas[3:11], #posiçẽs com nomes de colunas
                  multiple = TRUE),
                
                NULL,
                
                card(class="card text-white bg-info mb-3",
                  textOutput("ranking")),
                card(class="card text-white bg-info mb-3",
                  textOutput("pop_total")),
                card(class="card text-white bg-info mb-3",
                  textOutput("pop_relativa")),
                card(plotOutput("plot_nome"))
              )
    )
)


server = function(input,output,session){
  
  ##############################################################################
  ##                                  ABA 1                                   ##
  ##############################################################################
  
  #Aplicando os filtros de periodo escolhidos pelo usuário
  dados_periodos_selecionados = reactive({
    #se nenhum periodo é selecionado, mostra os dados totais e acrescenta colunas de frequencia
    if(length(input$periodo) == 0){ 
      data.frame(Nomes = dados_tratados$Nomes, 
                 Frequencia = dados_tratados$Total, 
                 Porcentagem = round(dados_tratados$Total/sum(dados_tratados$Total),5)*100)
    }else{
      #dados_tratados %>% mutate(output = list(input$periodo))
      soma_periodos = rowSums(dados_tratados[, c(input$periodo)])
      data.frame(Nomes = dados_tratados$Nomes, 
                 Frequencia = soma_periodos, 
                 Porcentagem = round(soma_periodos/sum(soma_periodos),5)*100) 
    }
  })
  
  #Aplicando os filtros de ordenação escolhidos pelo usuário
  dados_ordenados = reactive({
    if(input$ordenacao == "Crescente"){
      dados_periodos_selecionados() %>% arrange(across(2))
    }else if(input$ordenacao == "Decrescente"){
      dados_periodos_selecionados() %>% arrange(desc(across(2)))
    }else if(input$ordenacao == "A-Z"){
      dados_periodos_selecionados() %>% arrange(Nomes)
    }else if(input$ordenacao == "Z-A"){
      dados_periodos_selecionados() %>% arrange(desc(Nomes))
    }
  })
  
  output$tabela_ranking = renderTable({
    dados_ordenados() %>%
      rename("Nº de Registros" = Frequencia,
             "%" = Porcentagem)},
    striped = TRUE
    )
  
  output$plot_ranking = renderPlot({
    
    #para manter a ordem dos filtros no gráfico, já que o ggplot automaticamente ordena por ordem alfabetica
    dados_grafico <- dados_ordenados() %>%
      mutate(
        Nomes_Ordenado = factor(Nomes, levels = Nomes)
      )
    
    ggplot(data = dados_grafico, aes(x = Nomes_Ordenado, y = Frequencia)) + 
      geom_col(fill = "#42c1a8", width = 0.7) + 
      labs(title = "Número de registros por nome ao longo do tempo", x = "Nomes", y = "Número de registros") +
      theme(
        axis.text.x = element_text(
          angle = 90,      # Ângulo de inclinação
          hjust = 1,       # Alinhamento horizontal 
          vjust = 1        # Alinhamento vertical
        ),
        panel.background = element_rect(fill = "transparent", color = NA),
        
        # Fundo do plot (área total) transparente
        plot.background = element_rect(fill = "transparent", color = NA)
        
        # Remove o grid se quiser
        #panel.grid = element_blank()
      )
    
  })
  
  
  ##############################################################################
  ##                                  ABA 2                                   ##
  ##############################################################################
  
  nome_selecionado = reactive(input$busca_nome)
  
  dados_periodos_selecionados_2 = reactive({
    #cria df apenas com o periodo selecionado
    #não etou agrupando os valores desta vez porque irei usar os dados para gerar o gráfico de histórico de nome
    if(length(input$periodo_2) == 0){
      dados_tratados %>% 
        mutate(Porcentagem = round(Total/sum(.$Total), 5)*100) %>%
        arrange(desc(Total))
    }else{
      dados_tratados %>%
        select(Nomes, all_of(c(input$periodo_2))) %>%
        mutate(Total = rowSums(across(where(is.numeric)))) %>%
        mutate(Porcentagem = round(Total/sum(.$Total), 5)*100) %>%
        arrange(desc(Total)) 
    }
  })
  
  dados_nome_selecionado = reactive({
    filter(dados_periodos_selecionados_2(), Nomes == nome_selecionado())
  })
  
  
  
  
  output$aux2 = renderTable(dados_nome_selecionado())
  
  output$ranking = renderText({
    posicao_nome = which(dados_periodos_selecionados_2()$Nomes == nome_selecionado()) 
    
    paste(nome_selecionado(),"foi o",posicao_nome, "º nome mais usado para nomear bebês que nasceram neste período")
  })
  
  output$aux1 = renderTable(dados_periodos_selecionados_2())
  
  output$pop_total = renderText({
    paste("Foram",dados_nome_selecionado()$Total,"pessoas nascidas com o nome",nome_selecionado())
  })
  
  output$pop_relativa = renderText({
    porcentagem_nome = dados_nome_selecionado()$Porcentagem
    paste("Isto corresponde a",porcentagem_nome, "% do total de pessoas nascidas com os nomes contidos nessa lista no mesmo período")
  })
  
  output$plot_nome = renderPlot({
    
    dados_grafico2 = select(dados_nome_selecionado(), -Porcentagem, -Total) %>%
      pivot_longer(
        cols = -Nomes,
        names_to = "Periodo",
        values_to = "Frequencia") %>%
      mutate(Periodos_Ordenados = factor(Periodo, levels = nomes_colunas[3:11])) 
    #usando nomes colunas aqui pra que o gráfico sempre siga a ordem cronológica dos períodos, independente da ordem que o usuário escolher
    
    ggplot(dados_grafico2, aes(x = Periodos_Ordenados, y = Frequencia, group = Nomes)) +
      geom_line(color = "#a842c1", size = 1) +
      geom_point(color = "#6f42c1", size = 2) +
      labs(title = "Número de pessoas registradas com o nome selecionado ao longo do tempo", x = "Períodos", y = "Número de registros")
  })
}

shinyApp(ui, server)

