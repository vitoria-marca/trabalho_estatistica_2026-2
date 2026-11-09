library(shiny)
library(DT)

# carregamento do dataset
dados <- read.csv2("dataset.csv", stringsAsFactors = FALSE)
dados$A <- factor(dados$A, levels = c("ETL Batch", "SQL Query", "ML Training"))
dados$D <- factor(dados$D, levels = c("SATA SSD", "NVMe PCIe3", "NVMe PCIe5"))

# funções
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")

calc_momentos <- function(x) {
  n <- length(x)
  m <- mean(x)
  m2 <- sum((x - m)^2) / n
  m3 <- sum((x - m)^3) / n
  m4 <- sum((x - m)^4) / n
  
  a3 <- (m3 / m2^1.5) * sqrt(n * (n - 1)) / (n - 2)
  a4 <- ((n - 1) / ((n - 2) * (n - 3))) * ((n + 1) * (m4 / m2^2 - 3) + 6)
  c(a3 = a3, a4 = a4)
}

# user interface
ui <- navbarPage(
  title = "Trabalho EBC - Grupo 12 T2, Vitória Santa Lucia",
  
  tabPanel("Resolução",
           fluidPage(
             h3("Questão 1: Classificação das Variáveis"),
             p(strong("Unidade de observação:"), "Cada execução de teste na arquitetura do computador."),
             tableOutput("q1_tab"),
             
             hr(),
             h3("Questão 2: Medidas Descritivas"),
             tableOutput("q2_tab"),
             
             hr(),
             h3("Questão 3: Tabela de Médias Cruzadas (ML Training - Consumo de Processamento)"),
             uiOutput("q3_tab_html"),
             tags$ul(
               tags$li("A maior média marginal por tipo de armazenamento ocorreu no NVMe PCIe 3.0 (92,40%)."),
               tags$li("A maior média de ocupação foi a de 8 núcleos e PCIe3 (92,70%), sugerindo que essas configurações podem formar gargalos de CPU na tarefa de Machine Learning.")
             ),
             
             hr(),
             h3("Questão 4: Resumo 5 Números e Box Plot (Tempo Total por RAM)"),
             fluidRow(
               column(6, tableOutput("q4_cinco")),
               column(6, tableOutput("q4_cercas"))
             ),
             plotOutput("q4_box", height = "400px"),
             tags$ul(
               tags$li("Os tempos demonstram assimetria positiva forte (os bigodes superiores são consideravelmente mais longos)."),
               tags$li("Foram identificados valores atípicos (outliers) que ultrapassaram as cercas superiores em todas as capacidades de RAM.")
             ),
             
             hr(),
             h3("Questão 5: Distribuição de Frequências (SQL Query - Eficiência de Memória)"),
             p(textOutput("q5_info")),
             tableOutput("q5_tab"),
             plotOutput("q5_hist", height = "400px"),
             uiOutput("q5_class")
           )
  ),
  
  tabPanel("Base de Dados",
           fluidPage(
             DTOutput("tabela_dados")
           )
  )
)

## servidor shiny apps
server <- function(input, output, session) {
  
  # aba 2 _ dataset
  output$tabela_dados <- renderDT({
    datatable(dados, rownames = FALSE, options = list(pageLength = 25))
  })
  
  # questão 1
  output$q1_tab <- renderTable({
    data.frame(
      "Variável" = c("A - Tipo de tarefa", "B - CPU Cores", "C - Memória RAM", "D - Armazenamento", "E - Co-processamento", "X - Transferência", "Y - Consumo CPU", "Z - Eficiência Memória", "W - Tempo"),
      "Classificação" = c("Qualitativa Nominal", "Quantitativa Discreta", "Quantitativa Discreta", "Qualitativa Nominal", "Qualitativa Nominal (Binária)", "Quantitativa Contínua", "Quantitativa Contínua", "Quantitativa Contínua", "Quantitativa Contínua"),
      "Un. Medida ou Níveis" = c("ETL, SQL, ML", "Núcleos (8, 16, 32)", "GB (16, 32, 64)", "SATA, PCIe3, PCIe5", "0, 1", "GB/s", "%", "%", "segundos")
    )
  }, striped = TRUE, bordered = TRUE)
  
  # questão 2
  output$q2_tab <- renderTable({
    vars <- c("X" = "GB/s", "Y" = "%", "Z" = "%", "W" = "s")
    do.call(rbind, lapply(names(vars), function(v) {
      x <- dados[[v]]
      m <- calc_momentos(x)
      data.frame(
        "Variável" = v,
        "Média" = fmt(mean(x)),
        "Variância" = fmt(var(x)),
        "Desvio Padrão" = fmt(sd(x)),
        "CV (%)" = fmt(100 * sd(x) / mean(x)),
        "Assimetria" = fmt(m["a3"]),
        "Curtose" = fmt(m["a4"]),
        "Assimetria" = ifelse(abs(m["a3"]) <= 0.5, "Simétrica", ifelse(m["a3"] > 0, "Assimétrica Positiva", "Assimétrica Negativa")),
        "Curtose" = ifelse(abs(m["a4"]) <= 0.5, "Mesocúrtica", ifelse(m["a4"] > 0, "Leptocúrtica", "Platicúrtica"))
      )
    }))
  }, striped = TRUE, bordered = TRUE)
  
  # questão 3
  output$q3_tab_html <- renderUI({
    ml <- subset(dados, A == "ML Training")
    ml$B <- factor(ml$B, levels = c(8, 16, 32))
    
    tabela <- tapply(ml$Y, list(ml$D, ml$B), mean)
    mar_linha <- tapply(ml$Y, ml$D, mean)
    mar_col <- tapply(ml$Y, ml$B, mean)
    
    tags$table(class = "table table-bordered",
               tags$tr(tags$th("Armazenamento / Núcleos"), tags$th("8"), tags$th("16"), tags$th("32"), tags$th("Marginal")),
               lapply(levels(ml$D), function(d) {
                 tags$tr(tags$th(d), tags$td(fmt(tabela[d, "8"])), tags$td(fmt(tabela[d, "16"])), tags$td(fmt(tabela[d, "32"])), tags$td(strong(fmt(mar_linha[d]))))
               }),
               tags$tr(tags$th("Marginal"), tags$td(strong(fmt(mar_col["8"]))), tags$td(strong(fmt(mar_col["16"]))), tags$td(strong(fmt(mar_col["32"]))), tags$td(strong(fmt(mean(ml$Y)))))
    )
  })
  
  # questão 4
  res_q4 <- reactive({
    lapply(c(16, 32, 64), function(g) {
      w <- dados$W[dados$C == g]
      q <- quantile(w, c(0.25, 0.5, 0.75), type = 7)
      aiq <- q[3] - q[1]
      ci <- q[1] - 1.5 * aiq
      cs <- q[3] + 1.5 * aiq
      outliers <- w[w < ci | w > cs]
      
      list(g = g, min = min(w), q1 = q[1], med = q[2], q3 = q[3], max = max(w), ci = ci, cs = cs, out = length(outliers))
    })
  })
  
  output$q4_cinco <- renderTable({
    do.call(rbind, lapply(res_q4(), function(r) data.frame("RAM (GB)" = r$g, "Mín" = fmt(r$min), "Q1" = fmt(r$q1), "Mediana" = fmt(r$med), "Q3" = fmt(r$q3), "Máx" = fmt(r$max))))
  }, striped = TRUE, bordered = TRUE)
  
  output$q4_cercas <- renderTable({
    do.call(rbind, lapply(res_q4(), function(r) data.frame("RAM (GB)" = r$g, "Cerca Inf." = fmt(r$ci), "Cerca Sup." = fmt(r$cs), "Qtd. Outliers" = r$out)))
  }, striped = TRUE, bordered = TRUE)
  
  output$q4_box <- renderPlot({
    boxplot(W ~ C, data = dados, col = "lightblue", main = "Box Plot do Tempo (W) por RAM", xlab = "Capacidade de RAM (GB)", ylab = "Tempo de Processamento (s)")
  })
  
  # questão 5
  res_q5 <- reactive({
    z <- dados$Z[dados$A == "SQL Query"]
    n <- length(z)
    k <- ceiling(1 + 3.322 * log10(n))
    amp <- max(z) - min(z)
    h <- round(ceiling(((amp + 0.1) / k) / 0.1) * 0.1, 1)
    quebras <- min(z) + h * (0:k)
    freqs <- as.numeric(table(cut(z, breaks = quebras, right = FALSE)))
    list(z = z, k = k, h = h, quebras = quebras, freqs = freqs, n = n)
  })
  
  output$q5_info <- renderText({
    r <- res_q5()
    paste0("Utilizou-se a Fórmula de Sturges: n = ", r$n, ", resultando em k = ", r$k, " classes com amplitude h = ", fmt(r$h, 1), " %.")
  })
  
  output$q5_tab <- renderTable({
    r <- res_q5()
    li <- r$quebras[-(r$k + 1)]; ls <- r$quebras[-1]
    tab <- data.frame(
      "Classes (%)" = paste0("[", fmt(li, 1), " ; ", fmt(ls, 1), ")"),
      "fi" = r$freqs,
      "fr (%)" = fmt(100 * r$freqs / r$n),
      "Fi" = cumsum(r$freqs),
      "Fr (%)" = fmt(100 * cumsum(r$freqs) / r$n),
      check.names = FALSE
    )
    rbind(tab, data.frame("Classes (%)" = "Total", "fi" = r$n, "fr (%)" = "100", "Fi" = "-", "Fr (%)" = "100", check.names = FALSE))
  }, striped = TRUE, bordered = TRUE)
  
  output$q5_hist <- renderPlot({
    r <- res_q5()
    hist(r$z, breaks = r$quebras, right = FALSE, col = "steelblue", border = "white", main = "Histograma - Eficiência de Memória", xlab = "Eficiência (%)", ylab = "Frequência")
  })
  
  output$q5_class <- renderUI({
    a3 <- calc_momentos(res_q5()$z)["a3"]
    p(strong(paste0("Classificação: Assimétrica Positiva (a3 = ", fmt(a3), "). ")), "O gráfico apresenta cauda alongada para a direita, indicando concentração nas primeiras classes.")
  })
}

shinyApp(ui, server)