module Ships
    ( ship,
    evilShip
    )
    where

import Brick

asciShip :: Widget ()
asciShip = str "                     /~~~~~|"
        <=> str "               .__./''''''|"
        <=> str "._____________/   |/^^^^^^^\\____"
        <=> str "|             `===\"\\_______/"
        <=> str "`.             .___/^^^^^^^^\\___"
        <=> str "  `------------'~~~\\________/"
        <=> str "                   `........\\"
        <=> str "                     `-------'"
        <=> padTop (Pad 2) (str "           DEFEND YOUR SHIP!     ")


ship :: Widget()
ship =  (padLeft Max asciShip)

evilShip :: Widget ()
evilShip = str "[=====>"
            <=> str "[  (    _____"
            <=> str " \\__\\,-'//   `--._"
            <=> str "  [_/~||,-----.\\@_\\___"
            <=> str "  [_) |||()()()   ~[|||>"
            <=> str "  [_\\_||`-----'   //"
            <=> str " /  /`-.\\\\___,--'==(-"
            <=> str "[  ("
            <=> str "[=====>"
            <=> padTop (Pad 1) (str "TYPE THE ENEMY WORD TO DESTROY IT!")
