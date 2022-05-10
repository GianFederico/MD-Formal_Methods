{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ByeSPACE where

import Control.Applicative
import Data.Char
import System.IO
import Data.List

--definition of environment as list of variable@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
data Variable = Variable
  { name :: String, --name of var
    vtype :: String, -- type of var
    value :: Int -- value of var, list of list of int for array and matrices
  }
  deriving (Show)

type Env = [Variable]

updateEnv :: Variable -> Parser [Char]
updateEnv var =
  P
    ( \env input -> case input of
        xs -> [(modifyEnv env var, "", xs)]
    )

modifyEnv :: Env -> Variable -> Env
modifyEnv [] var = [var]
modifyEnv (x : xs) newVar =
  if name x == name newVar
    then [newVar] ++ xs
    else [x] ++ modifyEnv xs newVar

--simply reads a variable returning its value
readVariable :: String -> Parser Int
readVariable name =
  P ( \env input -> case searchVariable env name of
        [] -> []
        [value] -> [(env, value, input)])

--scans the environment in order to find a var with the same identifier
searchVariable :: Env -> String -> [Int]
searchVariable [] queryname = []
searchVariable (x : xs) queryname =
  if (name x) == queryname
    then [(value x)]
    else searchVariable xs queryname

-- @@@@@@@@@@@@@@@@@ ARRAY @@@@@@@@@@@@@@@@@@@@@

saveArray :: String -> [Int] -> Parser String
saveArray var val = P(\env input -> [(updateArray env var val, "", input)])

updateArray :: Env -> String -> [Int] -> Env
updateArray env var val = foldl (modifyEnv) env l
                          where l = zipWith (\a i ->
                                   Variable { name=var ++ "{" ++ (show i) ++ "}"
                                            , vtype="array"
                                            , value= a}) val [0..]


searchArray :: Env -> String -> [Int]
searchArray env array =
     case searchVariable env x of
          []    -> []
          value -> concat([value] ++ map (\var -> searchVariable env var) xs)
     where (x:xs) = map (\i -> (array ++ "{" ++ (show i) ++ "}")) [0..l]
           l = countElem env
           countElem [] = 0
           countElem (x:xs) = if (array ++ "{") `isPrefixOf` (name x)
                                then 1 + countElem xs
                                else countElem xs
                                             
                                                          
readArray :: String -> Parser [Int]
readArray name = P(\env input -> case searchArray env name of
                    [] -> []
                    value -> [(env, value, input)])
                    



--definition of parser @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
newtype Parser a = P (Env -> String -> [(Env, a, String)])

parse :: Parser a -> Env -> String -> [(Env, a, String)]
parse (P p) inp = p inp

--parser implemented as instance of Functor, Application and Monad and Alternative
instance Functor Parser where
  -- fmap :: (a->b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \env inp -> case parse p env inp of
          [] -> []
          [(env, v, out)] -> [(env, g v, out)]
      )

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\env inp -> [(env, v, inp)])

  -- <*> :: Parser (a->b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \env inp -> case parse pg env inp of
          [] -> []
          [(env, g, out)] -> parse (fmap g px) env out
      )

instance Monad Parser where
  return v = P (\env inp -> [(env, v, inp)])
  p >>= f =
    P
      ( \env inp -> case parse p env inp of
          [] -> []
          [(env, v, out)] -> parse (f v) env out
      )

instance Alternative Parser where 
--empty ::Parser a
  empty = P (\env inp -> [])

  -- (<|>) :: Parser a ->  Parser a ->  Parser a
  p <|> q =
    P
      ( \env inp -> case parse p env inp of
          [] -> parse q env inp
          [(envout, v, out)] -> [(envout, v, out)]
      )

item :: Parser Char
item =
  P
    ( \env inp -> case inp of
        [] -> []
        (x : xs) -> [(env, x, xs)]
    )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

digit :: Parser Int
digit = do
  x <- sat isDigit
  return $ digitToInt x

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isLetter

integer :: Parser Int
integer =
  ( do
      symbol "-"
      n <- natural
      return (- n)
  )
    <|> natural

natural :: Parser Int
natural =
  ( do
      d <- digit
      n <- natural
      return (read (show d ++ show n) :: Int)
  )
    <|> digit

alphanum :: Parser Char
alphanum = sat isAlphaNum

identifier :: Parser String
identifier = do
  space
  x <- lower
  xs <- many alphanum
  space
  return (x : xs)

symbol :: String -> Parser String
symbol [] = return ""
symbol (x : xs) = do {
                    sat (x ==);
                    symbol xs;
                    return (x : xs);}

consumeAexp :: Parser String
consumeAexp = do
  do
    space
    t <- consumeAterm
    space
    symbol "+"
    space
    a <- consumeAexp
    return (t ++ "+" ++ a)
    <|> do
      space
      t <- consumeAterm
      space
      symbol "-"
      space
      a <- consumeAexp
      return (t ++ "-" ++ a)
    <|> do
      consumeAterm

aexp :: Parser Int
aexp =
  ( do
      space
      t <- aterm
      space
      symbol "+"
      space
      e <- aexp
      return (t + e)
  )
    <|> ( do
            space
            t <- aterm
            space
            symbol "-"
            space
            e <- aexp
            return (t - e)
        )
    <|> aterm

--defining how to consume terms
consumeAterm :: Parser String
consumeAterm = do
  do
    space
    f <- consumeAfactor
    space
    symbol "*"
    space
    t <- consumeAterm
    return (f ++ "*" ++ t)
    <|> do
      space
      f <- consumeAfactor
      space
      symbol "/"
      space
      t <- consumeAterm
      return (f ++ "/" ++ t)
    <|> do
      consumeAfactor

aterm :: Parser Int
aterm = do
  f <- factor
  t <- aterm1 f
  return t

aterm1 :: Int -> Parser Int
aterm1 t1 =
  do
    space
    symbol "*"
    space
    f <- factor
    t <- aterm1 (t1 * f)
    return (t)
    <|> do
      space
      symbol "/"
      space
      f <- factor
      t <- aterm1 (div t1 f)
      return (t)
    <|> return t1

--defining how to consume factors
consumeAfactor :: Parser String
consumeAfactor =
  do
    space
    symbol "("
    space
    e <- consumeAexp
    space
    symbol ")"
    return ("(" ++ e ++ ")")
    <|> do
      space
      symbol "-"
      space
      f <- consumeAfactor
      return ("-" ++ f)
    <|> do
      i <- identifier
      space
      symbol "{"
      space
      index <- consumeAexp
      space
      symbol "}"
      return $ i ++ "{" ++ index ++ "}"
    <|> do
      i <- identifier
      return i
    <|> do
      i <- integer
      return (show i)

factor :: Parser Int
factor =
  do
    space
    symbol "("
    space
    e <- aexp
    space
    symbol ")"
    return e
    <|> do
      space
      i <- identifier
      space
      symbol "{"
      space
      index <- aexp
      space
      symbol "}"
      readVariable $ i ++ "{" ++ (show index) ++ "}"
    <|> do
      i <- identifier
      readVariable i
    <|> integer

consumeBexp :: Parser String
consumeBexp = do
  do
    space
    b0 <- consumeBterm
    space
    symbol "OR"
    space
    b1 <- consumeBexp
    return (b0 ++ "OR" ++ b1)
    <|> do
      consumeBterm

bexp :: Parser Bool
bexp =
  ( do
      space
      bt <- bterm
      space
      symbol "OR"
      space
      b1 <- bexp
      return (bt || b1)
  )
    <|> bterm

consumeBterm :: Parser String
consumeBterm = do
  do
    space
    f0 <- consumeBfactor
    space
    symbol "AND"
    space
    f1 <- consumeBterm
    return (f0 ++ "AND" ++ f1)
    <|> do
      consumeBfactor

bterm :: Parser Bool
bterm =
  ( do
      space
      bf <- bfactor
      space
      symbol "AND"
      space
      bt <- bterm
      return (bf && bt)
  )
    <|> bfactor

consumeBfactor :: Parser String
consumeBfactor = do
  do
    space
    symbol "True"
    return "True"
    <|> do
      space
      symbol "False"
      return "False"
    <|> do
      space
      symbol "!"
      space
      b <- consumeBfactor
      return ("!" ++ b)
    <|> do
      space
      symbol "("
      space
      b <- consumeBexp
      space
      symbol ")"
      return ("(" ++ b ++ ")")
    <|> do
      consumeBcomparison

bfactor :: Parser Bool
bfactor =
  ( do
      space
      symbol "!"
      space
      b <- bfactor
      return $ not b
  )
    <|> ( do
            space
            symbol "("
            space
            b <- bexp
            space
            symbol ")"
            return b
        )
    <|> ( do
            space
            symbol "True"
            return True
        )
    <|> ( do
            space
            symbol "False"
            return False
        )
    <|> bcomparison

consumeBcomparison :: Parser String
consumeBcomparison = do
  do
    space
    a0 <- consumeAexp
    space
    symbol "<"
    space
    a1 <- consumeAexp
    return (a0 ++ "<" ++ a1)
    <|> do
      space
      a0 <- consumeAexp
      space
      symbol "<="
      space
      a1 <- consumeAexp
      return (a0 ++ "<=" ++ a1)
    <|> do
      space
      a0 <- consumeAexp
      space
      symbol ">"
      space
      a1 <- consumeAexp
      return (a0 ++ ">" ++ a1)
    <|> do
      space
      a0 <- consumeAexp
      space
      symbol ">="
      space
      a1 <- consumeAexp
      return (a0 ++ ">=" ++ a1)
    <|> do
      space
      a0 <- consumeAexp
      space
      symbol "=="
      space
      a1 <- consumeAexp
      return (a0 ++ "==" ++ a1)
    <|> do
      a1 <- consumeAexp
      symbol "=/="
      a2 <- consumeAexp
      return (a1 ++ "=/=" ++ a2)

bcomparison :: Parser Bool
bcomparison =
  ( do
      space
      a <- aexp
      space
      symbol "=="
      space
      b <- aexp
      return $ a == b
  )
    <|> ( do
            space
            a <- aexp
            space
            symbol "=/="
            space
            b <- aexp
            return $ a /= b
        )
    <|> ( do
            space
            a <- aexp
            space
            symbol "<="
            space
            b <- aexp
            return $ a <= b
        )
    <|> ( do
            space
            a <- aexp
            space
            symbol ">="
            space
            b <- aexp
            return $ a >= b
        )
    <|> ( do
            space
            a <- aexp
            space
            symbol ">"
            space
            b <- aexp
            return $ a > b
        )
    <|> ( do
            space
            a <- aexp
            space
            symbol "<"
            space
            b <- aexp
            return $ a < b
        )

consumeAssignment :: Parser String
consumeAssignment = 
              (do 
                  space
                  id <- identifier
                  space
                  symbol "="
                  space
                  a <- consumeAexp
                  space
                  symbol ";"
                  space
                  return $ id ++ "=" ++ a ++ ";"
                  )
              <|>
              -- x{1} = y{1}
              (do id <- identifier
                  symbol "{"
                  space
                  index <- consumeAexp
                  space
                  symbol "}"
                  space
                  symbol "="
                  space
                  id2 <- identifier
                  space
                  symbol "{"
                  space
                  index2 <- consumeAexp
                  space
                  symbol "}"
                  space
                  symbol ";"
                  space
                  return $ id ++ "{" ++ index ++ "}=" ++ id2 ++ "{" ++ index2 ++ "}" )
              <|>
              (do -- y = x{1}
                  space
                  id <- identifier
                  space
                  symbol "="
                  space
                  id2 <- identifier
                  space
                  symbol "{"
                  space
                  index <- consumeAexp
                  space
                  symbol "}"
                  space
                  symbol ";"
                  space
                  return $ id ++ "=" ++ id2 ++ "{" ++ index ++ "}"
                  )
              <|>
            -- x{1} = y
              (do
                  space
                  id <- identifier
                  space
                  symbol "{"
                  space
                  index <- consumeAexp
                  space
                  symbol "}"
                  space
                  symbol "="
                  space
                  symbol ";"
                  space
                  val <- consumeAexp
                  space
                  array <- readArray id
                  space
                  return $ id ++ "{" ++ index ++ "}=" ++ val )
            <|>
              -- x={1,2,3}
              (do 
                  space
                  id <- identifier
                  space
                  symbol "="
                  space
                  symbol ";"
                  space
                  arr <- consumeArray
                  space
                  return $ id ++ "=" ++ arr  )
             <|>
              -- x = y++z
              (do 
                  space
                  id <- identifier
                  space
                  symbol "="
                  space
                  ar1 <- consumeArray
                  space
                  symbol "++"
                  space
                  symbol ";"
                  space
                  ar2 <- consumeArray
                  space
                  return $ id ++ "=" ++ ar1 ++ "++" ++ ar2)  


assignment :: Parser String
assignment =
  ( do 
      space
      id <- identifier
      space
      symbol "="
      space
      a <- aexp
      space
      symbol ";"
      space
      updateEnv Variable {name = id, vtype = "int", value = a}
  )
    <|>
    -- x{1} = y
    ( do 
        space
        id <- identifier
        space
        symbol "{"
        space
        index <- aexp
        space
        symbol "}"
        space
        symbol "="
        space
        val <- aexp
        space
        symbol ";"
        space
        array <- readArray id
        if length array <= index
          then empty
          else
            updateEnv
              Variable
                { name = (id ++ "{" ++ (show index) ++ "}"),
                  vtype = "array",
                  value = val
                }
    )
    <|>
    -- y = x{1}
    ( do
        space
        id <- identifier
        space
        symbol "="
        space
        id2 <- identifier
        space
        symbol "{"
        space
        index <- aexp
        space
        symbol "}"
        space
        symbol ";"
        space
        val <- readVariable (id2 ++ "{" ++ (show index) ++ "}")
        updateEnv Variable {name = id, vtype = "int", value = val}
    )
    <|>
    -- x{1} = y{1}
    ( do
        space
        id <- identifier
        space
        symbol "{"
        space
        index <- aexp
        space
        symbol "}"
        space
        symbol "="
        space
        id2 <- identifier
        space
        symbol "{"
        space
        index2 <- aexp
        space
        symbol "}"
        space
        symbol ";"
        space
        val <- readVariable (id2 ++ "{" ++ (show index2) ++ "}")
        updateEnv Variable {name = (id ++ "{" ++ (show index) ++ "}"), vtype = "array", value = val}
    )
    <|>
    -- x={1,2,3}
    ( do
        space
        id <- identifier
        space
        symbol "="
        space
        arr <- array
        space
        symbol ";"
        space
        saveArray id arr
    )
    <|>
    -- x = y++z
    ( do
        space
        id <- identifier
        space
        symbol "="
        space
        ar1 <- array
        space
        symbol "++"
        space
        ar2 <- array
        space
        symbol ";"
        space
        saveArray id (ar1 ++ ar2)
    )


consumeifThenElse :: Parser String
consumeifThenElse = do
  space
  symbol "if"
  space
  b <- consumeBexp
  space
  symbol "then"
  space
  x <- consumeProgram
  space
  symbol "else"
  space
  y <- consumeProgram
  space
  symbol "endif;"
  return $ "if" ++ b ++ "then" ++ x ++ "else" ++ y ++ "endif;"

ifThenElse :: Parser String
ifThenElse =do  {space;
                symbol "if";
                space;
                b <- bexp;
                space;
                symbol "then";
                space;
                if (b)
                  then
                    ( do
                        program
                        space
                        symbol "else"
                        space
                        consumeProgram
                        space
                        symbol "endif;"
                        space
                        return ""
                    )
                  else
                    ( do
                        consumeProgram
                        space
                        symbol "else"
                        space
                        program
                        space
                        symbol "endif;"
                        space
                        return ""
                    )}
                    <|> do{
                      space;
                      symbol "if";
                      space;
                      b <- bexp;
                      space;
                      symbol "then";
                      space;
                      if(b)
                        then
                        (do 
                          program
                          space
                          symbol "endif;"
                          space
                          return "")
                       else
                         (do
                          consumeProgram
                          space
                          symbol "endif;"
                          space
                          return "")                
                    }
consumeWhile :: Parser String
consumeWhile = do
  space
  symbol "while"
  space
  b <- consumeBexp
  space
  symbol "do"
  space
  x <- consumeProgram
  space
  symbol "end"
  space
  symbol ";"
  space
  return $ "while" ++ b ++ "do" ++ x ++ "end" ++ ";"

executeWhile :: String -> Parser String
executeWhile c = P (\env input -> [(env, "", c ++ input)])

while :: Parser String
while = do
  space
  w <- consumeWhile
  executeWhile w
  space
  symbol "while"
  space
  b <- bexp
  space
  symbol "do"
  space
  if (b)
    then
      ( do
          space
          program
          space
          symbol "end"
          space
          symbol ";"
          space
          executeWhile w
          while
      )
    else
      ( do
          space
          consumeProgram
          space
          symbol "end"
          space
          symbol ";"
          space
          return ""
      )

consumeTernary :: Parser String
consumeTernary = do
  space
  symbol "("
  space
  b <- consumeBexp
  space
  symbol ")"
  space
  symbol "="
  space
  x <- consumeProgram
  space
  symbol ":"
  space
  y <- consumeProgram
  return $ "(" ++ b ++ ")" ++ " = " ++ x ++ " : " ++ y

ternary :: Parser String
ternary = do
  space
  symbol "("
  space
  b <- bexp
  space
  symbol ")"
  space
  symbol "="
  space
  if (b)
    then
      ( do
          space
          program
          space
          symbol ":"
          space
          consumeProgram
          return ""
      )
    else
      ( do
          space
          consumeProgram
          space
          symbol ":"
          space
          program
          return ""
      )




consumeConcArray :: Parser String
consumeConcArray = do 
                      space
                      a <- consumeArray
                      space
                      symbol "++"
                      space
                      b <- consumeArray
                      space
                      return ( a ++ b)
                 <|>
                 consumeArray 


consumeArrayItems :: Parser String
consumeArrayItems = do 
                       space
                       a <- consumeAexp
                       space
                       symbol ","
                       space
                       b <- consumeArrayItems
                       space
                       return (a ++ "," ++ b)
                <|> consumeAexp

consumeArray :: Parser String
consumeArray = do{ space;
                 symbol "{";
                 space;
                 a <- consumeArrayItems;
                 space;
                 symbol "}";
                 space;
                 return ("{" ++ a ++ "}");}
              <|> identifier
                 
                  
concArray :: Parser [Int]
concArray = do space
               a <- array
               space
               symbol "++"
               space
               b <- concArray
               space
               return (a ++ b)
            <|> array
            
arrayItems :: Parser [Int]
arrayItems = do space
                a <- aexp
                space
                symbol ","
                space
                as <- arrayItems
                space
                return ([a] ++ as)
             <|>
             do a <- aexp
                return [a]
                              
                 
array :: Parser [Int]
array =  do symbol "{"
            space
            a <- arrayItems
            space
            symbol "}"
            space
            return a
          <|>
          do i <- identifier
             readArray i   




eval :: String -> Env
eval c = case parse program [] c of
  [] -> error "Invalid input"
  [(e, _, [])] -> e
  [(e, _, out)] -> error $ "Invalid input: unused '" ++ out ++ "'"

consumeCommand :: Parser String
consumeCommand = consumeAssignment 
                 <|> consumeifThenElse 
                 <|> consumeTernary  <|> symbol "skip"  <|> consumeWhile

command :: Parser String
command =
   assignment 
  <|> ifThenElse 
  <|> ternary 
  <|> while
  <|> symbol "skip"
        
consumeProgram :: Parser String
consumeProgram =
  (do
    x <- consumeCommand
    y <- consumeProgram
    return $ x ++ y)
    <|> 
    ( do
            x <- consumeCommand
            return $ x 
        )
    <|> consumeCommand

program :: Parser String
program =
  ( do
      command
      program
  )
    <|> ( do
            command
        )
    <|> command


logo :: IO String
logo = do
  putStrLn "     __________________________________________________________________"
  putStrLn "                             __     ____     __       __     _____     "
  putStrLn "         /                 /    )   /    )   / |    /    )   /    '    "
  putStrLn "     ---/__----------__----\\-------/____/---/__|---/--------/__--------"
  putStrLn "       /   ) /   / /___)    \\     /        /   |  /        /           "
  putStrLn "     _(___/_(___/_(___ _(____/___/________/____|_(____/___/____ _______"
  putStrLn "               /                                                       "
  putStrLn "           (_ /                                                        "
  putStrLn "     __________________________________________________________________"
  putStrLn ""
  putStrLn "                      by Giovanni Federico Poli"
  putStrLn ""
  putStrLn "      Please type the code to be evaluated, or type 'quit' to quit.\n"
  menu    

menu :: IO String
menu = do
  putStr "byeSPACE> "
  hFlush stdout
  input <- getLine

  if input == "quit"
    then return "Bye!"
    else do
      print (eval input)
      menu
