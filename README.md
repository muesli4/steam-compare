# Purpose

Trading Steam games often requires filtering huge lists of offers. While there exist tools for this purpose, all of them are browser-based JavaScript programs and are horribly slow. `steam-compare` provides a tool that is relatively fast as it manages its own database locally. At the moment only a CLI is provided.

# Configuration

To prevent entering your information over and over `steam-compare` uses a ini configuration file, located at the user configuration directory (as `steam-compare/default-user.conf`) or within the programs path. Either `steamid` or `steamid64` is required.

# Usage

```
Usage: steam-compare [-c|--config-file CONFIG_PATH]
                     [-d|--database-file DATABASE_PATH] COMMAND
  Manage a local steam database and filter a list of games you don't own with
  your criteria.

Available options:
  -c,--config-file CONFIG_PATH
                           Override default configuration file
  -d,--database-file DATABASE_PATH
                           Override default database file
  -h,--help                Show this help text

Available commands:
  update                   Update applications and owned games
  dump-blacklist           Dump the blacklist
  blacklist                Hide games that should not appear in future matching
  match                    Find matches in a list of games with your criteria
                           that you don't own
  query                    Query games for their appids
```

`match` and `query` also offer variety of options (mostly in the way the input is matched or how the result is displayed). For example, to replace lines of games with markdown links of the corresponding shop page you can use `steam-compare query -l --output-markdown --display-unmatched`.

## Working with lists

In some situations `steam-compare` will read a list of games from the standard input (`match`, `query` and `blacklist`). In Linux games can easily be pasted to terminal. To terminate the list hit `Ctrl+D`, to cancel hit `Ctrl+C`.

## Matching criteria

At the moment criteria are fixed to Linux games. If there is interest for other criteria please let me know.

