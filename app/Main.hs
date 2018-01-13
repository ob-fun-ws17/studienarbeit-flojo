
-- / A Main module
module Main where

import Server

-- / Start the application
main :: IO ()
main  = start "/home/osboxes/serverconf.txt"
