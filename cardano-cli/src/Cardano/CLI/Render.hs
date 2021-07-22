module Cardano.CLI.Render
  ( customRenderHelp
  ) where

import           Cardano.Prelude
import           Data.Function (id)
import           Options.Applicative
import           Options.Applicative.Help.Ann
import           Options.Applicative.Help.Types (helpText)
import           Prelude (String)
import           Prettyprinter
import           Prettyprinter.Render.Util.SimpleDocTree

import qualified Data.Text as T
import qualified System.Environment as IO
import qualified System.IO.Unsafe as IO

cliHelpTraceEnabled :: Bool
cliHelpTraceEnabled = IO.unsafePerformIO $ do
  mValue <- IO.lookupEnv "CLI_HELP_TRACE"
  return $ mValue == Just "1"
{-# NOINLINE cliHelpTraceEnabled #-}

-- | Convert a help text to 'String'.
customRenderHelp :: Int -> ParserHelp -> String
customRenderHelp cols
  = T.unpack
  . wrapper
  . renderSimplyDecorated id renderElement
  . treeForm
  . layoutSmart (LayoutOptions (AvailablePerLine cols 1.0))
  . helpText
  where
    renderElement = if cliHelpTraceEnabled
      then \(AnnTrace _ name) x -> "<span name=" <> show name <> ">" <> x <> "</span>"
      else flip const
    wrapper = if cliHelpTraceEnabled
      then id
        . ("<html>\n" <>)
        . ("<body>\n" <>)
        . ("<pre>\n" <>)
        . (<> "\n</html>")
        . (<> "\n</body>")
        . (<> "\n</pre>")
      else id
