{-# OPTIONS_GHC -Wall #-}
module AST.Expression.Optimized
    ( Def(..), Facts(..), dummyFacts
    , Expr(..)
    , Decider(..), Choice(..)
    ) where

import qualified AST.Expression.General as Expr
import qualified AST.Literal as Literal
import qualified AST.Module.Name as ModuleName
import qualified AST.Pattern as P
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Optimize.DecisionTree as DT
import qualified Reporting.Region as R



-- DEFINITIONS

data Def
    = Def Facts String Expr
    | TailDef Facts String [String] Expr
    deriving (Show)

data Facts = Facts
    { home :: Maybe ModuleName.Canonical
    }
--  deriving (Show)
instance Show Facts where
  show f =
    case (home f) of
      Just c -> "(Just " ++ show c ++ ")"
      Nothing -> "Nothing"

dummyFacts :: Facts
dummyFacts =
  Facts Nothing


-- EXPRESSIONS

data Expr
    = Literal Literal.Literal
    | Var Var.Canonical
    | ExplicitList [Expr]
    | Binop Var.Canonical Expr Expr
    | Function [String] Expr
    | Call Expr [Expr]
    | TailCall String [String] [Expr]
    | If [(Expr, Expr)] Expr
    | Let [Def] Expr
    | Case String (Decider Choice) [(Int, Expr)]
    | PlainCase String [(Int, Expr)] (DT.DecisionTree)
    | ElmCase Expr [(P.OPattern Var.Canonical, Expr)]
    | Data String [Expr]
    | DataAccess Expr Int
    | Access Expr String
    | Update Expr [(String, Expr)]
    | Record [(String, Expr)]
    | Cmd ModuleName.Canonical
    | Sub ModuleName.Canonical
    | OutgoingPort String Type.Canonical
    | IncomingPort String Type.Canonical
    | Program (Expr.Main Type.Canonical) Expr
    | GLShader String String Literal.GLShaderTipe
    | Crash ModuleName.Canonical R.Region (Maybe Expr)
  deriving (Show)

data Decider a
    = Leaf a
    | Chain
        { testChain :: [(DT.Path, DT.Test)]
        , success :: Decider a
        , failure :: Decider a
        }
    | FanOut
        { path :: DT.Path
        , tests :: [(DT.Test, Decider a)]
        , fallback :: Decider a
        }
    deriving (Show,Eq)


data Choice
    = Inline Expr
    | Jump Int
 deriving (Show)
