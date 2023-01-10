# aneta-cbtc

## Dev environment

```
$ nix develop
```

## Running executable

```
[nix develop:~/aneta-cbtc]$ cabal run
```

## Running hoogle

```
[nix develop:~/aneta-cbtc]$ hoogle server --local --port=8085
```

## Precommits

### Run `,format` before commits cabal *.hs *.nix *.cabal

```
[nix develop:~/aneta-cbtc]$ ,format 
```

### Format check
```
[nix develop:~/aneta-cbtc]$ ,format check
```
