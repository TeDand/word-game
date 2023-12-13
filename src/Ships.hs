module Ships
    ( ship,
    evilShip
    , gameOver
    )
    where

import Brick
type ResourceName = String
asciShip :: Widget ResourceName
asciShip = str "                     /~~~~~|"
        <=> str "               .__./''''''|"
        <=> str "._____________/   |/^^^^^^^\\____"
        <=> str "|             `===\"\\_______/"
        <=> str "`.             .___/^^^^^^^^\\___"
        <=> str "  `------------'~~~\\________/"
        <=> str "                   `........\\"
        <=> str "                     `-------'"
        <=> padTop (Pad 2) (str "           DEFEND YOUR SHIP!     ")


ship :: Widget ResourceName
ship =  (padLeft Max asciShip)

evilShip :: Widget ResourceName
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

gameOver :: Widget ResourceName
gameOver =  str "  _____          __  __ ______ ______      ________ _____ "
        <=> str " / ____|   /\\   |  \\/  |  ____/ __ \\ \\    / /  ____|  __ \\"
        <=> str "| |  __   /  \\  | \\  / | |__ | |  | \\ \\  / /| |__  | |__) |"
        <=> str "| | |_ | / /\\ \\ | |\\/| |  __|| |  | |\\ \\/ / |  __| |  _  /"
        <=> str "| |__| |/ ____ \\| |  | | |___| |__| | \\  /  | |____| | \\ \\"
        <=> str " \\_____/_/    \\_\\_|  |_|______\\____/   \\/   |______|_|  \\_\\"
