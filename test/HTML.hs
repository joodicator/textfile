{-# LANGUAGE QuasiQuotes, OverloadedStrings, NoMonomorphismRestriction #-}

import Indent

html = [indent|
    <!DOCTYPE html>
    <head>
        <title>:[title]:</title>
    </head>
    <body>
        <ul>:[[:<li>:[i]:</li>:] | i <- items]:</ul>
        <table>
            :[unlines $ map tr table]:
        </table>
    </body>|]
  where
    title = "Title"
    items = ["One", "Two", "Three"]
    table = [
        [1, 0, 0],
        [0, 1, 0],
        [0, 0, 1]]

tr row = [indent|
    <tr>:[concat [[:<td> :[show x]: </td>:] | x <- row]]:</tr>|]
