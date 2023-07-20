# Segmented CFMM Tests (in LIGO) and helpers

This folder includes:
* a test suite, implemented with the [ligo test framework](https://ligolang.org) in the [./cfmm](./cfmm) directory
* a cfmm contract for each configuration in the [./dex](./dex) directory
* helpers to setup test contracts and execute tests in the [./helpers](./helpers) directory
* token implementations (standard FA2 and FA1.2) using ligo packages in the [./tokens](./tokens) directory

## Running the tests

### Ligo packages dependencies

In order to run these tests you'll need to install Ligo packages. you can manually launch the installation with the command 
```bash
make install
```

### Running some tests

Two optional arguments (FILE, CONFIG) can be provided to reduce the scope of the tests. 

```bash
# Run emulator tests
make test CONFIG=fa2_fa2
```

For example, `make test CONFIG=fa2_fa12` command will launch tests on a configuration where token X follows a FA2 standard and token Y follows a FA1.2 standard.
The available configurations are :
CONFIG = sft_fa12 | sft_fa2 | fa2_fa12 | fa2_fa2 | fa12_fa12 | fa12_fa2 | fa12_ctez | fa2_ctez

For example, `make test FILE=x_to_y_fa12_ctez` command will launch a single test file (which contains multiple tests) where the file is located at `./cfmm/${FILE}.test.mligo`.

There are two sets of tests not launched by default (sft_fa12 or sft_fa2). These configurations are similar to (fa2_fa12, fa2_fa2) where the token X implements a semi-fungible token (following FA2 standard). Tests on these configurations can be launched with following commands: 
`make test CONFIG=sft_fa12`
`make test CONFIG=sft_fa2`

### Running all tests

The most straightforward way to run them is to execute the following command at the repository's root.

It will run all tests on the Ligo emulator (it may take a long time).

```bash
# Run emulator tests
make test
```
