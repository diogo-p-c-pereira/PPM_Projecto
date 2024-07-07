GrupoJLS32 – Diogo Pereira, Mariana Andrade, Nádia Gavancha

- Class CommandLineOption

Esta classe é utilizada para se poder fazer a interação com o utilizador na TUI.

- Controller.fxml

Para sermos capazes de manter os divisores da SplitPane no mesmo sítio no caso do utilizador os tentar mexer, fomos à parte de texto deste ficheiro, e copiámos as suas posições para as podermos utilizar numa outra função definida na Class Controller, a lockPaneDividers.

- Class Controller

A variável correctButtons é uma lista de Toggle Buttons que está inicialmente vazia. Ela é o que nos permite que na GUI seja apenas possível selecionar letras que estejam juntas umas às outras, ou seja, letras contíguas.
O nosso temporizador está a utilizar o tempo atual, e a subtrair-lhe o tempo inicial do jogo. Desta forma, podemos exibir o tempo de jogo no tabuleiro de uma forma constante, para o utilizador o poder ver.

- Object Direction

Este enumerado começou por estar dentro do Object ZigZag, mas como ele era usado em várias outras partes do projeto, colocámo-lo no seu próprio ficheiro SCALA.

- Object IO_Utils

Esta é a classe onde temos as funções que fazem print para a consola. Das outras vezes em que é necessário imprimir algo, chamam-se as funções desta classe.

- Object Main

A única exceção onde existia um print que não fosse chamado através do Object IO_Utils era neste Main, estando igual ao que foi feito em aula. Entretanto, isto foi alterado, visto que os prints deveriam ficar todos num único ficheiro.
Parte do código foi copiado dos exercícios feitos na semana 6, sendo posteriormente adaptado às nossas necessidades.

- Class MainGUI

Nesta classe utilizámos o que fizemos na aula prática da semana 8, e adaptámos o código ao do nosso projeto.

- Case Class MyRandom

O código daqui foi praticamente copiado da ficha da semana 6, com apenas algumas diferenças, nomeadamente a ausência do trait.

- Package Object ZigZagGame

Algo que usamos para definir constantes globais do projeto, que neste caso são ambos ficheiros de texto com informações relacionadas com as seeds, e com as palavras que irão fazer parte do tabuleiro.

- Case Class ZigZag

Esta classe está no mesmo ficheiro SCALA que o Object ZigZag, e serve maioritariamente como suporte à GUI.
Tivemos de adicionar uma variável serialVersionUID, devido ao facto de termos de carregar a classe dos ficheiros de jogos gravados. A variável força o ID da classe a ser o número que lhe foi atribuído, e assim, apesar de apenas resolver o problema parte das vezes, quando compilamos a classe novamente, o ID já não muda, e já não ocorre um erro ao tentarmos fazer load de um save de uma compilação antiga.
O erro que aparece é o seguinte: java.io.InvalidClassException: ZigZagGame.ZigZag; local class incompatible: stream classdesc serialVersionUID = 6087090209548627722, local class serialVersionUID = 869904675760693134

- Object ZigZag

Este objeto está no mesmo ficheiro SCALA que a Case Class ZigZag.
Várias das funções entregues na parte 1 foram alteradas com o intuito de as tornar mais eficientes, ou de as adaptar à GUI. Contudo, não foram apagadas, simplesmente não são usadas.
Uma das funções, fillOneCell (alínea T2), foi feita de duas maneiras diferentes, tendo nós optado por apenas uma delas.
Os pontos são calculados em função do tempo total de jogo e do número de tentativas falhadas, começando em 300 e indo decrementando conforme a fórmula estabelecida.
Sentimos a necessidade de alterar a nossa função play (T5) para que retornasse as coordenadas das palavras corretas, para depois as podermos utilizar na TUI.
Na alínea T6, da maneira como tinha sido feita antes, apenas verificava se todas as palavras existiam, e se elas não estavam repetidas, mas não se uma das palavras se bifucarva a meio, dando origem a outra palavra. Nós alterámos a função com o intuito de lidar com este problema. Infelizmente, apercebemo-nos de que nem sempre funcionava, visto que o último dos testes efetuados no Object ZigZag (o que utiliza o board10) era suposto retornar false, mas retorna true. Felizmente, a função não está a retornar falsos negativos.
Tínhamos criado uma nova função play adaptada à GUI, mas visto que o objetivo era usar as funções da TUI adaptadas à GUI, acabámos por não utilizar esta função.
Aqui existem várias instâncias de prints, mas estes são apenas usados para efeitos de teste.