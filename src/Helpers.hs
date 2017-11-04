module Helpers where


toJvmFile :: String -> String
toJvmFile file = reverse $ toJVM (reverse file)

toJVM :: String -> String
toJVM ('.':xs) = "j." ++ xs
toJVM (x:xs) = toJVM xs

toLlvmFile :: String -> String
toLlvmFile file = reverse $ toLLVM (reverse file)

toLLVM :: String -> String
toLLVM ('.':xs) = "ll." ++ xs
toLLVM (x:xs) = toLLVM xs

fileName :: String -> String
fileName file = reverse $ toName (reverse $ directName  file)

directName:: String -> String
directName ('/':xs)= xs
directName (x:xs) = directName xs

toName :: String -> String
toName ('.':xs) = xs
toName (x:xs) = toName xs
