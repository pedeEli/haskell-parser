module Structure where


-- used for useful error messages
data Token = Token String Int Int Int Int
        deriving (Show, Eq)

-- this is the root of the ast
data Module = Named Token (Maybe [Export]) Body
            | Main Body -- when there is no 'module <name> where' on the top of the file, it is automaticly the main module
        deriving (Show, Eq)

-- defines different possible exports
data Export = Exp'Module Token
            | Exp'QCon Token (Maybe [Token]) -- used for types and classes
            | Exp'QVar Token
        deriving (Show, Eq)

-- represents an import
data Import = Import
            Token -- module which is imported
            Bool -- true if import is qualified
            (Maybe Token) -- alias
            (Maybe ( -- specific values are imported
                Bool, -- true if tha values are hidden
                [ImportSpec] -- specific values to be imported
            ))
        deriving (Show, Eq)

-- same as Export
data ImportSpec = Imp'Var Token
                | Imp'Con Token (Maybe [Token])
        deriving (Show, Eq)

data Body = Body [Import] [TopDecl]
        deriving (Show, Eq)

newtype TopDecl = TopDecl Token
        deriving (Show, Eq)