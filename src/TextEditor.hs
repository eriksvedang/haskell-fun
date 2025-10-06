module TextEditor where

data EditorState = EditorState String String

instance Show EditorState where
  show (EditorState before after) = "'" ++ before ++ "|" ++ after ++ "'"

emptyState :: EditorState
emptyState =
  EditorState "" ""

data Action
  = MoveLeft
  | MoveRight
  | Insert Char
  | Erase

execute :: EditorState -> Action -> EditorState
execute (EditorState "" after) MoveLeft = EditorState "" after
execute (EditorState before after) MoveLeft = EditorState (init before) (last before : after)
execute (EditorState before "") MoveRight = EditorState before ""
execute (EditorState before after) MoveRight = EditorState (before ++ [head after]) (tail after)
execute (EditorState before after) (Insert c) = EditorState (before ++ [c]) after
execute (EditorState before after) Erase = EditorState (init before) after

session :: [Action]
session =
  [ Insert 'i',
    MoveLeft,
    Insert 'h',
    MoveRight,
    Insert 'x',
    Erase,
    Insert '!'
  ]

main = do
  print (foldl execute emptyState session)
  mapM print (scanl execute emptyState session)
