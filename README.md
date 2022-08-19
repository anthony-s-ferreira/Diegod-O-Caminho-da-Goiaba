<h1 align="center"> Diegod - O Camingo da Goiaba </h1>
<p align="center">
  Projetos para Paradigmas de Linguagem de Programação - UFCG - 2021.2
  </p>
<p align="center">
  </p>
  
  
 <h1 align="center"> Haskell </h1>

# Instalação

### Para instalar no distros linux:
É necessário ter [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install). Após instalar
o stack, utilize os seguintes comandos:

```bash
git clone https://github.com/anthony-s-ferreira/Diegod-O-Caminho-da-Goiaba.git
cd Diegod-O-Caminho-da-Goiaba
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

# Como jogar

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
Para jogar,digite main. e depois siga as instruções do jogo.

## Como jogar

* **J.** para fazer sua jogada.(Rodar o dado)
* **"ABRIR."** para abrir a caixa.


