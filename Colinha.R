### Criado por Vitor Luciano em 30/06/2022
### Código contendo comandos básicos para ir bem na disciplina de Bioestatística (GES105)
### Mãe e vó, amo vocês! 


#######################
#####   Colinha   #####
#######################

### Tipos de variáveis:

  # Qualitativa nominal: Atributo (cultivar, espécie, entre outros)
  # Qualitativa ordinal: Atributo com ordem (Muito bom, bom, regular, entre outros)
  # Quantitativa discreta: Contagem de inteiros (Quantidade de frutos, quantidade de aluno, entre outros)
  # Quantitativa contínua: Escalas/instrumentos, medidas reais (Peso, altura, temperatura, entre outros)

### Frequência absoluta:

x <- c(VALOR_NUMÉRICO, VALOR_NUMÉRICO)
x <- c("VALOR_NÃO_NUMÉRICO", "VALOR_NÃO_NUMÉRICO")
table(x)

### Frequência relativa:

table(x)/length(x)

### Porcentagem da frequência: 

table(x)/length(x)*100

### Gráfico de barras da tabela de frequência:

barplot(table(x))

  # Modificar o eixo x: xlab = ""
  # Modificar o eixo y: ylab = ""
  # Modificar a cor: col = ""

### Gráfico de pizza da tabela de frequência: 

pie(table(x))

  # Adicionar legenda: labels = c("LEGENDA")

### Histograma: 

x <- hist (VARIÁVEL_NUMÉRICA) # Essa variável será o HISTOGRAMA a ser utilizado posteriormente

  # Polígono de frequência: lines (x$mids, x$counts)

### Ogiva (Gráfico de acúmulo):

x <- HISTOGRAMA$breaks
y <- c(0, cumsum(HISTOGRAMA$counts)/length(HISTOGRAMA))
plot (x,y)

### Criar tabela para múltiplas variáveis:

x <- matrix(c(VALORES_NUMÉRICOS), ncol = NÚMERO_DE_COLUNAS, byrow = TRUE)
colnames(x) <- c("NOME_DA_COLUNA", "NOME_DA_COLUNA")
rownames(x) <- c("NOME_DA_LINHA", "NOME_DA_LINHA")
x <- as.table(x)

### Criar tabela para variáveis com a mesma quantidade de elementos: 

x <- c(VALORES) # Podem ser não numéricos, contanto que se use ""
y <- c(VALORES)
cbind (x,y)

### Média

mean (x)

### Mediana 

median (x)

### Moda

CalcularModa <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
CalcularModa (x)

### Variância:

  # Mesmo que: (sd(x))^2
  # var = (((x1 - media)^2) + ((x2 - media)^2) + ((...)^2) + ((xn - media)^2))/n

var(x) # S²

### Desvio padrão:
  
  # Mesmo que: sqrt(var(x))

sd (x) # S; 
 
### Coeficiente de variação: 

100*sd(x)/mean(x) # Resultado em %

### Coeficiente de assimetria: 

  # Se = 0, distribuição simétrica; Se > 0, assimetria positiva, ou a direita; Se < 0, assimetria negativa, ou a esquerda
  # Na distribuição simétrica: Moda, média e mediana são iguais
  # Na distribuição assimétrica negativa: Mediana e média estão à esquerda da moda
  # Na distribuição assimétrica positiva: Mediana e média estão à direita da moda

(mean(x)-median(x))/sd(x) # Caso o desvio padrão, ou outras partes da fórmula já tenham sido disponibilizadas, trocar os valores removendo o "sd"

### Amplitude:

max(x)-min(x)

### Fatorial:

factorial(x)

### Combinação:

choose (n, k) # n: Quantidade de elementos; k: Quantidade de elementos tomados

### Distribuição binomial (VA qualitativa nominal/ordinária):

dbinom (x, n, p) # x: Todas as possibilidades de sucesso (Ex: x <- seq(0,10)); n: Quantidade de elementos; 
                 # p: Probabilidade de sucesso

  # Para plotar, definir a distribuição binomial como uma varíavel (Ex: k <- dbinom (x, 10, 0.8)), em seguida
  # utilizar cbind (Ex: dp <- cbind (x, k)) e plot (Ex: plot (x, k))

### Distribuição de Poisson (VA quantitativa discreta, contagens):

dpois (x, u) # x: Quantidade analisada; u: Parâmetro, intensidade

### Aproximação da binomial pela Poisson (Para n grande e p pequeno!): 

u = n*p


### Distribuição normal (VA quantitativa contínua, valores instrumentados):

  # Quando for disponibilizado um valor e pedir a probabilidade/área de probabilidade:
  # Em outras palavras, têm-se os quantis e quer-se obter a probabilidade:

pnorm (z, m, sd) # z: Valor alvo; m: Média; sd: Desvio padrão

  # Quando for disponibilizada uma área de probabilidade e pedir um valor:
  # Têm-se a probabilidade e quer-se obter os quantis:

qnorm (p, m, sd) # p: probabilidade 

### Teorema do limite central

  # A distribuição normal mostrada acima, serve exclusivamente para determinação de estimativas de AMOSTRAS INDIVIDUAIS
  # Para obtenção de estimativas de AMOSTRAS AGRUPADAS, deve-se utilizar o teorema do limite central
  # Consiste na divisão do desvio padrão pela raiz da n quantidade de elementos no grupo:
    # Desvio padrão populacional (σ)/sqrt(n_grupo)
  # Ex: X̅ ~ N (média (µ), desvio padrão populacional (σ)/sqrt(n_grupo)), o segundo argumento pode ser lido como "erro padrão"

  # Ainda nesse contexto, é possível encontrar problemas relacionados à proporção populacional
  # Nesse caso, deve-se considerar a média (m) como µ = n*p e desvio padrão (sd) como σ = sqrt(n*p*(1-p))
  # Pode-se obter um z como proporção amostral. Para isso: z = p^*n
  # Além disso, é possível que N SEJA GRANDE, exigindo a seguinte fórmula: z = (proporção amostral - proporção populacional)/sqrt(proporção populacional*(1-proporção populacional)/n)
  # Verificar z na tabela de distribuição normal

  # Para encontrar o valor de z, deve-se verificar os valores presentes em cada célula da tabela de distribuição normal

#################### Prova 2 ####################

### Utilidades: 

  # Parâmetros: Medidas de descrição, ex: média populacional (µ), desvio padrão populacional (σ) e proporção populacional (p)
  
  # Estimador/Estatística: Fórmula de cálculo para obter um parâmetro, ex: média amostral (x̅), desvio padrão amostral (s), proporção amostral (p^)
  
  # Estimativa: Resultado obtido a partir do estimador, ex: valor da média amostral, valor da variância amostral, valor da proporção amostral
  
  # Nivel de confiança: 1-α, probabilidade de conter a média da população 
  
  # Nível de significância: α, probabilidade de não conter a média da população

  # Calculo do z: qnorm (alfa/2, 0, 1)

### Interpretações:
  
  # Formal: A probabilidade de que o verdadeiro valor da média do poluente na represa encontra-se entre 279.5 ppm e 320.5 ppm é de 0,90.
  # Usual: Com 90% de confiança podemos afirmar que o verdadeiro valor da média do poluente na represa está entre 279.5 ppm e 320.5 ppm.
  # Artigo científico: O poluente na represa apresenta média igual a 300±20.5 ppm, com 90% de confiança. OBS: É comum encontrar, erroneamente, o desvio-padrão no lugar do erro amostral nesta notação.
  # TV e jornais: A média do poluente encontrada na represa foi de 300 ppm, com um erro de 20.5 ppm, para mais ou para menos, com 90% de confiança.

### Funções: 

calcintconf_media <- function(alfa, n, media_amostral, sd) {
  z <- abs(qnorm(alfa/2, 0, 1)); 
  max <- media_amostral + z*sd/sqrt(n);
  min <- media_amostral - z*sd/sqrt(n);
  valores <- list("Erro amostral:", z*sd/sqrt(n),"Limite inferior:", min, "Limite superior:", max);
  return (valores)}

calcintconfstudent <- function(alfa, n, media_amostral, sd_amostral) {
  conf <- (1-alfa);
  t <- abs(qt (alfa/2, n-1));
  max <- media_amostral + t*sd_amostral/sqrt(n);
  min <- media_amostral - t*sd_amostral/sqrt(n);
  erro <-  abs(t*sd_amostral/sqrt(n));
  valores <- list("Confiança:", conf, "Valor de t:", t, "Erro amostral:", erro, "Intervalo superior:", max,  "Intervalo inferior", min);
  return(valores)}

calcintconf_prop <- function(alfa, n, prop_amostral) {
  z <- abs(qnorm(alfa/2, 0, 1))
  max <- prop_amostral + z*sqrt((prop_amostral*(1-prop_amostral))/n);
  min <- prop_amostral - z*sqrt((prop_amostral*(1-prop_amostral))/n);
  valores <- list("Erro amostral:", z*sqrt((prop_amostral*(1-prop_amostral))/n), "Limite inferior:", min, "Limite superior:", max);
  return (valores)}

tamanho_n <- function(alfa, erro, sd_pop){
  z <- qnorm(alfa/2, 0, 1)
  n <- ((z*sd_pop)/erro)^2
  cat("O tamanho da amostra deverá ser: ", round(n))    
}

hipotest_propuni <- function (prop_amostral, hipo, n, alfa){
  z <- (prop_amostral-hipo)/sqrt((hipo*(1-hipo))/n)
  vc <- qnorm (alfa, 0, 1)
  pz <- 1-pnorm(abs(z), 0, 1)
  pt <- 1-pt(abs(z), n-1)
  t <- qt(alfa, n-1)
  if(abs(z) < abs(vc)) {
    cat("Caso n seja < 25, considerar distribuição t de Student")
    cat("\n######################################################")
    cat("\nAceita-se H0")
    cat("\nValor de Z: ", abs(z))
    cat("\nValor crítico na distribuição normal: ", abs(vc))
    cat("\nValor crítico na distribuição t de Student: ", abs(t))
    cat("\nValor p na distribuição normal: ", pz)
    cat("\nValor p na distribuição t de Student: ", abs(pt))
  }
  else {
    cat("Caso n seja < 25, considerar distribuição t de Student")
    cat("\n######################################################")
    cat("\nRejeita-se H0")
    cat("\nValor de Z: ", abs(z))
    cat("\nValor crítico na distribuição normal: ", abs(vc))
    cat("\nValor crítico na distribuição t de Student: ", abs(t))
    cat("\nValor p na distribuição normal: ", pz) 
    cat("\nValor p na distribuição t de Student: ", abs(pt))
  }
}

hipotest_propbi <- function (prop_amostral, hipo, n, alfa){
  z <- (prop_amostral-hipo)/sqrt((hipo*(1-hipo))/n)
  vc <- qnorm (alfa/2, 0, 1)
  pz <- 2*(1-pnorm(abs(z), 0, 1))
  pt <- 2*(1-pt(abs(z), n-1))
  t <- qt(alfa/2, n-1)
  if(abs(z) < abs(vc)) {
    cat("Caso n seja < 25, considerar distribuição t de Student")
    cat("\n######################################################")
    cat("\nAceita-se H0")
    cat("\nValor de Z: ", abs(z))
    cat("\nValor crítico na distribuição normal: ", abs(vc))
    cat("\nValor crítico na distribuição t de Student: ", abs(t))
    cat("\nValor p na distribuição normal: ", pz)
    cat("\nValor p na distribuição t de Student: ", abs(pt))
  }
  else {
    cat("Caso n seja < 25, considerar distribuição t de Student")
    cat("\n######################################################")
    cat("\nRejeita-se H0")
    cat("\nValor de Z: ", abs(z))
    cat("\nValor crítico na distribuição normal: ", abs(vc))
    cat("\nValor crítico na distribuição t de Student: ", abs(t))
    cat("\nValor p na distribuição normal: ", pz) 
    cat("\nValor p na distribuição t de Student: ", abs(pt))
  }
}

hipotest_mediauni <- function (media_amostral, hipo, sd_amostral, n, alfa){
  z <- (media_amostral-hipo)/(sd_amostral/sqrt(n))
  vc <- qnorm (alfa, 0, 1)
  pz <- 1-pnorm(abs(z), 0, 1)
  pt <- 1-pt(abs(z), n-1)
  t <- qt(alfa, n-1)
  if(abs(z) < abs(vc)) {
    cat("Caso n seja < 25, considerar distribuição t de Student")
    cat("\n######################################################")
    cat("\nAceita-se H0")
    cat("\nValor de Z: ", abs(z))
    cat("\nValor crítico na distribuição normal: ", abs(vc))
    cat("\nValor crítico na distribuição t de Student: ", abs(t))
    cat("\nValor p na distribuição normal: ", pz)
    cat("\nValor p na distribuição t de Student: ", abs(pt))
  }
  else {
    cat("Caso n seja < 25, considerar distribuição t de Student")
    cat("\n######################################################")
    cat("\nRejeita-se H0")
    cat("\nValor de Z: ", abs(z))
    cat("\nValor crítico na distribuição normal: ", abs(vc))
    cat("\nValor crítico na distribuição t de Student: ", abs(t))
    cat("\nValor p na distribuição normal: ", pz) 
    cat("\nValor p na distribuição t de Student: ", abs(pt))
  }
}

hipotest_mediabi <- function (media_amostral, hipo, sd_amostral, n, alfa){
  z <- (media_amostral-hipo)/(sd_amostral/sqrt(n))
  vc <- qnorm (alfa/2, 0, 1)
  pz <- 2*(1-pnorm(abs(z), 0, 1))
  pt <- 2*(1-pt(abs(z), n-1))
  t <- qt(alfa/2, n-1)
  if(abs(z) < abs(vc)) {
    cat("Caso n seja < 25, considerar distribuição t de Student")
    cat("\n######################################################")
    cat("\nAceita-se H0")
    cat("\nValor de Z: ", abs(z))
    cat("\nValor crítico na distribuição normal: ", abs(vc))
    cat("\nValor crítico na distribuição t de Student: ", abs(t))
    cat("\nValor p na distribuição normal: ", pz)
    cat("\nValor p na distribuição t de Student: ", abs(pt))
  }
  else {
    cat("Caso n seja < 25, considerar distribuição t de Student")
    cat("\n######################################################")
    cat("\nRejeita-se H0")
    cat("\nValor de Z: ", abs(z))
    cat("\nValor crítico na distribuição normal: ", abs(vc))
    cat("\nValor crítico na distribuição t de Student: ", abs(t))
    cat("\nValor p na distribuição normal: ", pz) 
    cat("\nValor p na distribuição t de Student: ", abs(pt))
  }
}

### Intervalo de confiança: 

  # Média populacional (µ) = média amostral (x̅) + erro (e)

  # P(x̅- erro =< µ =< x̅+ erro) = 1-α

### Intervalo de confiança para média populacional, com desvio padrão conhecido (tamanho pequeno ou grande): 

  # Média amostral (x̅) +- z * Desvio padrão populacional (σ)/sqrt(n_grupo). Nesse caso, z trata-se do quantil da distribuição normal, em que é α/2 (verificar tabela)

### Intervalo de confiança para média populacional, com desvio padrão desconhecido (tamanho grande, n >= 25):

  # Média amostral (x̅) +- z * Desvio padrão amostral(S)/sqrt(n_grupo). Tamanho de amostra deve ser >= 25. Se for menor, a distribuição normal não pode ser utilizada
  # Intervalo de confiança com desvio padrão desconhecido e tamanho da amostra  pequeno: distribuição "t" de Student 
    # Supõe-se que a população possui distribuição normal

### Intervalo de confiança para média populacional, com desvio padrão desconhecido (tamanho pequeno, n < 25):

  # Usar calcintconfstudent
  # Se a média populacional encontrar-se entre os intervalos de confiança, pode-se dizer que "a produção está sob controle"
  # Média amostral (x̅) +- t * S/sqrt(n_grupo) 
  # Para encontrar t, deve-se consultar a tabela de distribuição t de Student
  # No eixo x está o valor de alfa e no eixo Y os graus de liberdade (n-1)
  # Também pode-se utilizar o comando: qt (alfa/2, n-1)

### Cálculo do tamanho da amostra: 

  # erro (e) = z * σ/sqrt(n_grupo), em que z é α/2
  # Não esquecer de dividir o alfa!!!
  # n = ((z*σ)/e)^2
  # Se o desvio padrão populacional for desconhecido, basta utilizar o desvio padrão amostral

### Intervalo de confiança para proporção populacional: 

  # Proporção AMOSTRAL (p^) +- z*sqrt((p^*(1-p^))/n)

### Tamanho de amostra para estimar proporção populacional:

  # n = ((z^2)/(e^2))*p^*(1-p^)
  # É possível obter o valor de p^ através de estudos prévios, amostra piloto e utilizando o tamanho máximo da amostra
  # Para obter a amostra com número máximo, pode-se utilizar: n = ((z^2)/(e^2))*0.25
  # Proporção amostral (p^): Número de sucessos/Espaço amostral

### TESTES DE HIPOTESE:

### Teste de hipótese paramétrico para proporção populacional:

  # Primeiro deve-se definir a hipótese nula (H0) e a hipótese alternativa (H1) 
    # H0: Valor suposto para o parâmetro, SEMPRE SERÀ A IGUALDADE! 
    # H1: Valor complementar de H0, pode ser de 3 tipos: unilateral (H0:P=x e H1:P<x), unilateral (H0:P=x e H1:P>x) e bilateral (H0:P=x e H1:P!=x)
  
  # Em seguida, deve-se calcular a estatística de teste
    # z <- (Prop. amostral - Prop. populacional ou hipótese)/sqrt((prop. populacional*(1-prop. populacional))/n)

  # Após isso, deve-se estabelecer a região crítica, onde a H0 é rejeitada. A área de rejeição é igual à significância que estabelece a probabilidade de rejeitar a Ho quando ela é verdadeira
    # NÃO É O VALOR DA ÁREA CENTRAL, é o quantil tabelado de alfa (significância). A área central é o nivel de confiança
    # Valor crítico: qnorm (alfa, 0, 1) para testes unilaterais
    # Valor crítico: qnorm (alfa/2, 0, 1) para testes bilaterais 

  # Se o valor da estatística do teste cair fora da região crítica, aceita-se H0. Se o valor da estatística do teste cair dentro da região crítica, não aceita-se H0
    # Caso se aceite H0, a hipótese nula não pode ser rejeitada
    # Caso H0 seja rejeitado, existem evidências estatísticas para rejeitá-lo com um erro (alfa)

  # Erro do tipo I: rejeição da H0 quando ela é verdadeira

### Teste de hipótese paramétrico para média populacional com variância desconhecida:

  # Primeiro deve-se definir a hipótese nula (H0) e a hipótese alternativa (H1) 
  
  # Em seguida, calcular a estatística do teste
    # Caso o desvio padrão ou variância sejam desconhecidos e tenha-se a média populacional: z <- (media_amostral-media_pop)/(sd_amostral/sqrt(n))

### Valor-p: probabilidade de se obter um valor maior que o valor observado na estatística de teste

  # t.test(DADOS, mu=media_pop, alternative = "greater OU less OU two.sided", conf.level = 1-alfa)

### Valor-p para testes unilaterais: 

  # Para distribuições t de Student, deve-se olhar a linha correspondente aos graus de liberdade na amostra analisada
  # Em seguida, escolher o valor mais próximo ao valor obtido na estatística de teste
  # Após isso, subir a coluna até chegar na primeira linha (eixo x), o valor correspondente será o valor-p
  # Caso o valor-p seja maior que alfa, aceita-se a H0
  # Para obter o p: 1-pt(abs(ESTATÍSTICA DO TESTE), n-1)
  # Afim de comparação, pode-se fazer: tc <- qt(alfa, n-1). Caso |p| > |tc|, rejeita-se H0 

  # Para distribuição normal Z, obtêm-se o valor de |z|
  # Em seguida, encontra-se a célula correspondente a esse mesmo valor na tabela (através dos eixos) 
  # A célula encontrada deve ser subtraida de 1 O resultado da operação será o valor-p, que sendo maior que alfa, permite a aceitação da H0
  # 1-pnorm(abs(z), 0, 1)

  # REJEITA-SE H0 SE P É MUITO < QUE ALFA!

### Valor-p para testes bilaterais:

  # Basta multiplicar por 2 o valor-p obtido em um teste unilateral

### Teste de hipótese para comparação de duas proporções populacionais:

  # z = (prop_amostral1(pa1) - prop_amostral2(pa2))/sqrt(((pa1*(1-pa1))/n1)+((pa2*(1-pa2))/n2))

### Teste de hipótese para comparação de duas médias de populações independentes: 

  # Primeiro deve-se verificar se as médias são estatísticamente iguais: Se maior_var_amostral/menor_var_amostral < 4, não há diferença estatística entre as variâncias
  # Ficar atento se a informação fornecida é desvio padrão (S), ou variância (S²)!  var

  # Caso as variâncias sejam iguais, calcula-se a variância combinada: Var_comb = (((n1-1)*(var1)+((n2-1)*(var2)))/(n1+n2-2))
  # Em seguida, calcula-se a estatística do teste: t = (media_amostral1(ma1) - media_amostral2(ma2))/(sd_comb*sqrt((1/n1)+(1/n2)))
  
  # Caso as variâncias sejam diferentes, usa-se: t = (ma1 - ma2)/sqrt((var1/n1)+(var2/n2))

  # Para obtenção dos graus de liberdade: gl = n1+n2-2

  # A área de rejeição é calculada da seguinte forma: 
    # Unilateral: qt(alfa, (n1+n2-2))
    # Bilateral: qt(alfa/2, (n1+n2-2))

### Teste de hipótese para comparação de duas médias de populações dependentes: 

  # Nessa situação, trabalha-se com a diferença entre os pares de valores, a média e os desvio padrão dessas diferenças
  # Primeiro deve-se definir a H0, que prediz que a média das diferenças é igual a 0
  # Em seguida, faz-se a estatística de teste: t = media_diferenças/(sd_diferenças/sqrt(n))
  # Após isso, define-se a área de rejeição: qt(alfa, n-1) ou qt(alfa/2, n-1)

### Teste de aderência do qui-quadrado:

  # Estabelecer as H0 e H1
    # H0: Os dados são iguais (se aderem) à teoria estipulada
    # H1: Os dados não são iguais (não se aderem) à teoria estipulada 

  # Fazer a estatística de teste
    # x = (((O1 - E1)^2)/E1) + (((O2 - E2)^2)/E2) + (((On - En)^2)/En)
    # x = sum((ob-es)^2/es)
  
  # Definir o qui-quadrado crítico
    # r = Quantidade de classes
    # graus de liberdade (gl) = r-1
    # qchisq(alfa, gl, lower.tail = FALSE)

  # Se a estatística de teste for menor que o qui-quadrado crítico, aceita-se H0 

  # Valor-p: 1-pchisq(x, gl)

### Teste de independência do qui-quadrado:

  # Estabelecer as H0 e H1:
    # H0: A ocorrência de uma variável independe da ocorrência da outra variável.
    # H1: : A ocorrência de uma variável depende da ocorrência da outra variável.

  # Calcular a estatística de teste:
    # É possível estabelecer os valores esperados multiplicando a soma da primeira linha pela soma da primeira coluna e dividindo pela soma total. 
    # Prosseguir com x = (((O1 - E1)^2)/E1) + (((O2 - E2)^2)/E2) + (((On - En)^2)/En) para cada célula da tabela
    # dados <- c(27, 42, 34, 47);
    # ob <- matrix(dados, nrow=2, byrow=TRUE); 
    # E <- outer(rowSums(ob), colSums(ob), "*")/sum(ob); 
    # q <- sum((ob-E)^2/E); 


  # Calcular a região crítica: 
    # gl = (r-1)*(c-1)
      # r = número de linhas, c = número de colunas

  # Para tabelas 2x2, deve-se aplicar a correção de Yates:
    # x = (((abs(O1 - E1)-0.5)^2)/E1) + (((abs(O2 - E2)-0.5)^2)/E2) + (((abs(On - En)-0.5)^2)/En)
    











  
