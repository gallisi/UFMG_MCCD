library(gt)
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

ui = fluidPage(
  titlePanel("Nomes da TTA"),
  
  tabsetPanel(
    
    #Visão Geral
    tabPanel("Visão Geral",
             fluidRow(
               column(4, selectInput(
                 "periodo",
                 "Escolha o período:",
                 choices = nomes_colunas[3:11], #posiçẽs com nomes de colunas
                 multiple = TRUE
               )),
               
               column(8, radioButtons("ordenacao",
                                      "Selecione a ordenação",
                                      choices = c("Crescente","Decrescente","A-Z","Z-A"),
                                      inline = TRUE))
             ),
             
             fluidRow(
               column(4, gt_output("tabela_ranking")),
               column(8, plotOutput("plot_ranking"))
             )
         ),
    
    #Visão por nome
    tabPanel("Dados Por Nome",
             fluidRow(
               column(6,selectInput("busca_nome", 
                                  "Digite um nome:",
                                  choices = dados_tratados$Nomes, 
                                  multiple = FALSE
                                  )),
               
               column(6, selectInput(
                 "periodo_2",
                 "Escolha o período:",
                 choices = nomes_colunas[3:11], #posiçẽs com nomes de colunas
                 multiple = TRUE
               ))),
             
             fluidRow(
               column(4, textOutput("ranking")),
               column(4,textOutput("pop_total")),
               column(4,textOutput("pop_relativa"))
               ),
             #textOutput("significado"), 
             #tableOutput("aux1"),
             #tableOutput("aux2"),
             
             plotOutput("plot_nome")
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
                 Porcentagem = round(dados_tratados$Total/sum(dados_tratados$Total),5))
    }else{
      #dados_tratados %>% mutate(output = list(input$periodo))
      soma_periodos = rowSums(dados_tratados[, c(input$periodo)])
      data.frame(Nomes = dados_tratados$Nomes, 
                 Frequencia = soma_periodos, 
                 Porcentagem = round(soma_periodos/sum(soma_periodos),5)) 
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
  
  output$tabela_ranking = render_gt({
    
    dados_ordenados() %>%
      gt(
        rowname_col = "Nomes",
        groupname_col = NULL
      ) %>%
      tab_header(
        title = "Nomes da turma TTA",
        subtitle = "..."
      ) 
    
    
  })
  
  output$plot_ranking = renderPlot({
    
    #para manter a ordem dos filtros no gráfico, já que o ggplot automaticamente ordena por ordem alfabetica
    dados_grafico <- dados_ordenados() %>%
      mutate(
        Nomes_Ordenado = factor(Nomes, levels = Nomes)
      )
    
    ggplot(data = dados_grafico, aes(x = Nomes_Ordenado, y = Porcentagem)) + 
      geom_col(fill = "steelblue", width = 0.7) + 
      labs(title = "placeholder", x = "Nomes", y = "Frequência Absoluta")+
      theme(
        axis.text.x = element_text(
          angle = 90,      # Ângulo de inclinação
          hjust = 1,       # Alinhamento horizontal (1 = direita)
          vjust = 1        # Alinhamento vertical (1 = baixo)
        )
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
        mutate(Porcentagem = Total/sum(.$Total)) %>%
        arrange(desc(Total))
    }else{
      dados_tratados %>%
        select(Nomes, all_of(c(input$periodo_2))) %>%
        mutate(Total = rowSums(across(where(is.numeric)))) %>%
        mutate(Porcentagem = Total/sum(.$Total)) %>%
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
    porcentagem_nome = round(dados_nome_selecionado()$Porcentagem*100, 5)
    paste("Isto corresponde a",porcentagem_nome, "% do total de pessoas nascidas com os nomes contidos nessa lista no mesmo período")
  })
  
  output$plot_nome = renderPlot({
    
    dados_grafico2 = select(dados_nome_selecionado(), -Porcentagem, -Total) %>%
      pivot_longer(
        cols = -Nomes,
        names_to = "Periodo",
        values_to = "Frequencia") %>%
      mutate(Periodos_Ordenados = factor(Periodo, levels = Periodo))
    
    ggplot(dados_grafico2, aes(x = Periodos_Ordenados, y = Frequencia, group = Nomes)) +
      geom_point(color = "darkblue", size = 2) +
      geom_line(color = "steelblue", size = 1) 
    
  })
  
  
  
}

shinyApp(ui, server)

