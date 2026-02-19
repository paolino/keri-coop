-- | CLI entry point for the keri-coop server.
module Main (main) where

import KeriCoop.Server (Options (..), run)
import Options.Applicative (
    Parser,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    showDefault,
    strOption,
    value,
    (<**>),
 )

optionsParser :: Parser Options
optionsParser =
    Options
        <$> option
            auto
            ( long "port"
                <> metavar "PORT"
                <> help "Port to listen on"
                <> value 3001
                <> showDefault
            )
        <*> strOption
            ( long "db"
                <> metavar "PATH"
                <> help "SQLite database file"
                <> value "keri-coop.db"
                <> showDefault
            )
        <*> strOption
            ( long "static-dir"
                <> metavar "DIR"
                <> help "Directory to serve static files from"
                <> value "./client/dist"
                <> showDefault
            )

main :: IO ()
main = do
    opts <-
        execParser $
            info
                (optionsParser <**> helper)
                ( fullDesc
                    <> progDesc "KERI-based collective purchasing server"
                    <> header "keri-coop-server"
                )
    run opts
