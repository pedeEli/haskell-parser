module Text.Parsel.Pos where

type SourceName = String
type Line = Int
type Column = Int

data SourcePos = SourcePos SourceName !Line !Column
  deriving (Eq, Ord)

instance Show SourcePos where
  show (SourcePos name line column) = name ++ ":" ++ show line ++ ":" ++ show column

initialPos :: SourceName -> SourcePos
initialPos name = SourcePos name 1 1

updatePosChar :: SourcePos -> Char -> SourcePos
updatePosChar (SourcePos name line column) c = case c of
  '\n' -> SourcePos name (line + 1) 1
  '\t' -> SourcePos name line (column + 8 - ((column - 1) `mod` 8))
  _    -> SourcePos name line (column + 1)

updatePosString :: SourcePos -> String -> SourcePos
updatePosString = foldl updatePosChar