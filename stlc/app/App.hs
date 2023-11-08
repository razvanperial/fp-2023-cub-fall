module App where

-- optparse-applicative
import Options.Applicative
import Parser (parseLambdaTerm, ParsingError)
import TypeCheck
import Data.Text (Text)

data Transformation
  = TypeCheck
  | Parse

data Action = Action
  { transformation :: Transformation
  , input :: Text
  }

data Args = Args
  { transformationArg :: Transformation
  , inputArg :: Text
  }

actionParser :: Parser Args
actionParser =
  Args <$> parseTransformation
       <*> inputParser

inputParser :: Parser Text
inputParser = strOption
  (  long "input"
  <> short 'i'
  <> metavar "INPUT"
  <> help "String input"
  )

parseTransformation :: Parser Transformation
parseTransformation =
      typeCheckParser
  <|> parserParser

parserParser :: Parser Transformation
parserParser = flag' Parse
  (  long "parse"
  <> short 'p'
  <> help "Parse the term"
  )


typeCheckParser :: Parser Transformation
typeCheckParser = flag' TypeCheck
  (  long "typeCheck"
  <> short 'c'
  <> help "Type check the term"
  )

transform :: Args -> IO Action
transform (Args transformation input) = do
  return $ Action transformation input

printEither :: Show a => Either String a -> IO ()
printEither (Left err) = do
  putStrLn "Error"
  putStrLn err
printEither (Right x) = do
  putStrLn "Ok"
  print x

runAction :: Args -> IO ()
runAction args = do
  action <- transform args
  case transformation action of
    TypeCheck -> do
      let parsed = parseLambdaTerm (input action)
      case parsed of
        Left err -> print $ showError err
        Right term -> do
          let checked = typeCheckEmpty term
          case checked of
            Left err -> print err
            Right _ -> print "Type check successful"
    Parse -> do
      let parsed = parseLambdaTerm (input action)
      case parsed of
        Left err -> print $ showError err
        Right term -> print term
  where
    showError :: Parser.ParsingError -> String
    showError = show

runApp :: IO ()
runApp = do
    runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "STLC type checker"
      <> header "stlc"
      )