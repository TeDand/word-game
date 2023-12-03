module Ship
    ( ship
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


ship :: Widget()
ship =  (padLeft (Pad 20) asciShip)