<h1 align="center"> Diegod - O Camingo da Goiaba </h1>
<p align="center">
  Projetos para Paradigmas de Linguagem de Programação - UFCG - 2021.2
  </p>
<p align="center">
  </p>

<div align="center">

<h3 align="center"> Diegod e a sua Goiaba Dourada </h3>
<img src = "https://user-images.githubusercontent.com/72308168/185725359-5c4e7229-67e6-4b9f-981b-63e4e019e2cb.png" width="200"px/>
</div>


 <h1 align="center"> Haskell </h1>

# Instalação

### Para instalar em distros linux:
É necessário ter [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Após instalar
o stack, utilize os seguintes comandos:

```bash
git clone https://github.com/anthony-s-ferreira/Diegod-O-Caminho-da-Goiaba.git
cd Diegod-O-Caminho-da-Goiaba
cd Haskell
sudo apt install libncurses-dev
sudo apt install cabal-install
export PATH=~/.local/bin:$PATH
echo $PATH
PATH=$PATH stack install ghc-mod hlint hasktags
stack install diegod
```

### ~~Windows~~
Como stack **_não funciona no Windows_**, será necessário baixar o [teminal ubuntu](https://www.microsoft.com/store/productId/9PDXGNCFSCZV)
e executar os comandos acima.

## Comandos
Para jogar, use o comando `diegod` no seu terminal.   
Para conseguir ajuda, execute `diegod --help`.  
Para ver seu maior score, execute `diegod --high-score`.

## Como jogar

* **W/S** ou 	**&uarr;/&darr;** para pular/abaixar
* **p** para pausar
* **r** para resetar 
* **q** para sair

<h1 align="center"> Prolog </h1>

# Instalação

### Para instalar em distros linux:
É necessário ter [swipl](https://www.swi-prolog.org/versions.txt). Após instalar
o swipl, utilize os seguintes comandos:

### Para jogar no Windows/Linux:
```bash
git clone https://github.com/anthony-s-ferreira/Diegod-O-Caminho-da-Goiaba.git
cd Diegod-O-Caminho-da-Goiaba
cd Prolog
swipl
consult("Main.pl").
main.
```

## Comandos
Para jogar, digite main. e depois siga as instruções do jogo.

## Como jogar

* **0.** para ver os créditos. (No menu)
* **1.** para fazer sua jogada.(Rodar o dado)
* **2.** para abrir a caixa.


