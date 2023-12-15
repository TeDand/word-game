# word-game
Word game implemented in Haskell for CSE 230.

## Project Brief
This repository includes a terminal based game where users can type target words to get points. The aim of the game would be to type as many words as possible without losing all your health.

## Project Structure

The project structure is given below (note that most of the game logic is implemented in the `src` folder):

```bash
word-game
├── app
│   └── Main.hs
├── data
│   └── words.txt
│         "contains words to load into game"
├── demo
│     "contains gifs for readme"
├── src
│   ├── Attributes.hs
│   │     "contains style attributes"
│   ├── Dataloader.hs
│   │     "contains functions to load words"
│   ├── EventHandler.hs
│   │     "handles all user events for the game"
│   ├── GameState.hs
│   │     "contains game-state constructor"
│   ├── Menu.hs
│   │     "contains apps for every menu page"
│   ├── RenderState.hs
│   │     "renders game based on current state"
│   ├── Scoreboard.hs
│   │     "contains functions for scoreboard"
│   ├── Ships.hs
│   │     "contains ascii-art for ships and gameover screen"
│   ├── Shuffle.hs
│   │     "contains randomizer for word selecton/generation"
│   └── Tui.hs
│         "contains function call to generate game user interface"
├── test
│     "work in progress"
├── .gitignore
├── LICENSE
├── makefile
│     "contains useful make commands for running project" 
├── package.yaml
├── README.md
├── sb.txt
│     "contains store of saved high-scores" 
├── stack.yaml
├── stack.yaml.lock
└── word-game.cabal
```

## Game Setup & Rules

To start the game run the following command in the root directory:

```
make all
```

The rules of the game are as follows:

-	Type words and press return to get rid of the attacking word bullet
-	When you type a word incorrectly, or get hit you will lose health
-	When you run out of health then it is game over

<img src="https://github.com/TeDand/word-game/blob/main/demo/game.png?raw=true" width="2300" height="250">

## Game Features

### Menu

Upon starting the code, you are greeted with a menu that includes the option to play the game, as well as view scoreboard page and select the game difficulty:

<img src="https://github.com/TeDand/word-game/blob/main/demo/menu.png?raw=true" width="2300" height="250">

<img src="https://github.com/TeDand/word-game/blob/main/demo/scoreboard.png?raw=true" width="2300" height="250">

<img src="https://github.com/TeDand/word-game/blob/main/demo/difficulty.png?raw=true" width="2300" height="250">

When you lose the game, you will be met with a game over screen, after which you can input your name to add to the scoreboard:

<img src="https://github.com/TeDand/word-game/blob/main/demo/gameover.png?raw=true" width="2300" height="250">

<img src="https://github.com/TeDand/word-game/blob/main/demo/score_entry.png?raw=true" width="2300" height="250">

### Difficulties

There are three difficulties available to play. The first is easy, with a single target word:

![Easy](https://github.com/TeDand/word-game/blob/main/demo/easy.gif?raw=true "Easy")

The next is hard, with three target words:

![Hard](https://github.com/TeDand/word-game/blob/main/demo/hard.gif?raw=true "Hard")

And the final is nightmare, with three randomly generated target words:

![Nightmare](https://github.com/TeDand/word-game/blob/main/demo/nightmare.gif?raw=true "Nightmare")

### Input Detection

Detects and hightights input, which is done for all target words in hard/nightmare difficulties. There are points awarded for correct entries:

![Correct Input](https://github.com/TeDand/word-game/blob/main/demo/correct_input.gif?raw=true "Correct Input")

The game also penalizes incorrect entries with an error message and health deduction:

![Incorrect Input](https://github.com/TeDand/word-game/blob/main/demo/incorrect_input.gif?raw=true "Incorrect Input")

### Hit Detection

When you are too slow to type the words, they will hit your ship. There is a health loss penalty in these cases:

![Hits](https://github.com/TeDand/word-game/blob/main/demo/hits.gif?raw=true "Hits")

### Timer and Speedup

As you can see, there is a timer implemented in the game. The longer you survive, the faster the words will shoot towards your ship. Here is the word speed at the start of the game:

![Speed 1](https://github.com/TeDand/word-game/blob/main/demo/speed_1.gif?raw=true "Speed 1")

And here is the faster word speed you are challenged with as the timer increases:

![Speed 2](https://github.com/TeDand/word-game/blob/main/demo/speed_2.gif?raw=true "Speed 2")

## Milestone 1 (Registration and Proposal)

- Display word and prompt user input
- Add gameplay system
  - Points system (or life system)
  - Timer
  - Type log that keeps track of user input and allows for submission 
  - Different words pop up on the screen

- Add difficulty system
  - As you survive longer in the game, words get longer or words disappear quicker, more words pop up on the screen

- Add high score/ scorekeeping
  - Allows players to see top 5 best games that is kept track of between different games and users
  - Keeps track of different users, to see different user scores 

- Graphical effects for losing lives and typing correct letters etc.
- Let multiple words appear and move on screen at same time
- Make words move (possibly in random directions/ rotate)
- Add graphics interface
  - Asteroids and gun
  - Adding menu system


## Milestone 2 (Updates)

This application currently has a few key components:

- User input and evaluation (scoring)
- Random word generation
- Time limit to enter word
- Game graphics for words moving across the screen
- File reading for persistent storage of leaderboard 

There have been some challenges along the way:

- Recent changes to the brick API, meaning lots of previous documentation and examples are obsolete, but knowledge of monads came in very handy
- Organisation of largescale Haskell project is initially difficult, but once we managed to get a skeleton up and running we were more able to effectively paralellize work
  
We believe that given our current progress we will be able to meet most/all of the main goals we initially set out. The scope of the game may change somewhat, as we currently have words being shot horizontally across the screen. We may be unable to add some of the finer details such as words moving randomly/rotating.
