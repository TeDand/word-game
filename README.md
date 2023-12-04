# word-game
Word game implemented in Haskell for CSE 230.

## Project Brief
Create a terminal based game where users can type target words to get points. The aim of the game would be to type as many words as possible without losing lives.

## Milestones

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


## Updates

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
