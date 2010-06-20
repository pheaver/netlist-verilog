--------------------------------------------------------------------------------

module Language.Verilog.ParserTest where

import Prelude hiding (catch)

import System.Directory             (getDirectoryContents)
import System.FilePath              ((</>), replaceExtension, takeExtension)
import Text.PrettyPrint             (render)
import Text.Parsec                  (parse)
import Text.Parsec.ByteString       (parseFromFile)

import Language.Verilog.Parser
import Language.Verilog.PrettyPrint (ppVerilog)
import Language.Verilog.Syntax      (Verilog)

--------------------------------------------------------------------------------

examples_dir :: FilePath
examples_dir = "./verilog/examples"

check_all :: IO ()
check_all
  = do files <- getDirectoryContents examples_dir
       sequence_ [ check f
                   | f <- files
                   , takeExtension f == ".v"
                 ]
       return ()

check :: FilePath -> IO ()
check baseName
  = do result1 <- parseFromFile verilogFile fp
       case result1 of
         Left err1  -> do writeFile fp_err1 (show err1 ++ "\n")
                          putStrLn ("Fail1: " ++ baseName)
         Right ast1 -> do let str1 = render (ppVerilog ast1)
                          writeFile fp1 str1
                          result2 <- parseFromFile verilogFile fp1
                          case result2 of
                            Left err2  -> do writeFile fp_err2 (show err2 ++ "\n")
                                             putStrLn ("Fail2: " ++ baseName)
                            Right ast2 -> do let str2 = render (ppVerilog ast2)
                                             writeFile fp2 str2
                                             if (ast1 == ast2)
                                                then putStrLn ("Match: " ++ baseName)
                                                else do writeFile fp_err3 ""
                                                        putStrLn ("No match: " ++ baseName)
  where
    fp      = examples_dir </> baseName
    fp1     = replaceExtension fp "v.2"
    fp2     = replaceExtension fp "v.3"
    fp_err1 = replaceExtension fp "err1"
    fp_err2 = replaceExtension fp "err2"
    fp_err3 = replaceExtension fp "err3"

parseVerilog :: String -> Verilog
parseVerilog x
  = case parse verilogFile "" x of
      Left err -> error (show err)
      Right y  -> y

test :: FilePath -> IO ()
test fp
  = do x <- run fp
       putStrLn (render (ppVerilog x))

run :: FilePath -> IO Verilog
run fp
  = do x <- parseFromFile verilogFile fp
       case x of
         Left err  -> error (show err)
         Right y   -> return y

--------------------------------------------------------------------------------

