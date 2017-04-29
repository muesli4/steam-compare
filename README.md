# Purpose

Trading Steam games often requires filtering huge lists of offers. While there exist tools for this purpose, all of them are browser-based JavaScript programs and are horribly slow. `steam-compare` provides a tool that is relatively fast as it manages its own database locally. At the moment only a CLI is provided.

# Configuration

To prevent entering your information over and over `steam-compare` uses a ini configuration file, located at the user configuration directory (as `steam-compare/default-user.conf`) or within the programs path. Either `steamid` or `steamid64` is required.

# Usage

Updating the database of owned games:
```
steam-compare update
```
Query application identifier and shop URL (`%` acts as wildcard):
```
steam-compare appid 'Civ%'
```
## Working with lists

In some situations `steam-compare` will read a list of games from the standard input. In Linux games can easily be pasted to terminal. To terminate the list hit `Ctrl+D`, to cancel hit `Ctrl+C`.

Adding games to your blacklist (these games will be ignored in future queries):
```
steam-compare blacklist
```

Matching a list of games against your criteria, excluding owned games:
```
steam-compare match
```

At the moment criteria are fixed to Linux games. If there is interest for other criteria please let me know.

