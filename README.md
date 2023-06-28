# lib-tezos-dex-v3
Library of a Uniswap-v3 implemented in Cameligo

## WARNING

This version is not meant for production purposes.
TWAP has not been verified yet !

## How to use this library

### Install

Install the library with ligo CLI (with docker)
```
ligo install dex-v3
```

This command will add a dependency in a package.json file.

Here is an example of the resulting package.json file.
```
{ "dependencies": { "dex-v3": "^0.1.0" } }
```


## How to play with this repository

### Compiling

To produce the Michelson (TZ file) from Cameligo source code
```
make compile
```

### Tests

To launch tests (It may take 30 minutes)
```
make test
```

One can launch tests on a specific configuration (CONFIG={ fa2_fa12 | fa12_fa2 | fa2_fa2 | fa12_fa12 | fa2_ctez | fa12_ctez | sft_fa2 | sft_fa12 })
One can launch tests on a specific configuration (FILE={ set_position_$(CONFIG) | update_position_$(CONFIG) | x_to_y_$(CONFIG) | y_to_x_$(CONFIG) | complex_position_$(CONFIG) })
```
make test CONFIG=fa12_fa2
make test FILE=x_to_y_fa2_fa12
```


### Publishing to Ligolang

To publish this repository on https://packages.ligolang.org/packages
```
ligo login
ligo publish
```

The local `ligo` compiler must be used. 