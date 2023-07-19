# lib-tezos-dex-v3
Library of a Uniswap-v3 implemented in Cameligo

## WARNING

This version is not meant for production purposes.
TWAP has not been verified yet !


## How to use this library

This library must be specified in the dependencies of your project. this is done by installing the dex-v3 package.
The package.json file must have the following dependency
```
{ "dependencies": { "dex-v3": "^0.1.2" } }
```

This library can be used to a liquidity pool (uniswap-v3-like) very easily



#### Configuration consideration

Since the library must handle multiple format (fa2, fa1.2) the transfer format is different so when creating a liquidity pool, one must specify which type of token it is dealing with. Many configurations are provided (all derived from the main.mligo). The `*_ctez` configurations takes protocol fees into account.

#### Example


##### Create a basic liquidity pool contract

For example, let's create a liquidity pool contract between a FA2 (implementing a SemiFungibleToken) and a FA12 (implementing an old school FungibleToken).

- import the library (here we use the configuration fa2_fa12). 
- define the `main` function (here we use the one provide by the library)
- (optional) create alias for useful types of the library

```
#import "dex-v3/lib/cfmm/main_fa2_fa12.mligo" "Dex"

let main = Dex.main

type storage = Dex.storage
type parameter = Dex.parameter
type metadata_map = Dex.metadata_map
```

This basic liquidity pool (let's call it "dex" from now on) needs to be tested. 


##### Testing - Prepare initial storage

The library provides a helper to generate default dex storage which can be imported:
```
    #import "dex-v3/lib/cfmm/defaults.mligo" "Default"
    #import "<path-to-dex-main>.mligo" "Dex"
```

The test can prepare the initial storage by using the `Default.default_storage` function. At origination the ratio token Y/X is one. 
```
    let constants = { 
        fee_bps = fee_bps; 
        ctez_burn_fee_bps = proto_fee_bps;
        x_token_id = 0n;
        y_token_id = 0n;
        x_token_address = x_token_address; // ("KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn" : address);
        y_token_address = y_token_address; // ("KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn" : address);
        tick_spacing = tick_spacing;
	} in
    let init_cumulatives_buffer_extra_slots = 0n in
    let metadata_map = (Big_map.empty : Dex.metadata_map) in
    let init_storage = Default.default_storage constants init_cumulatives_buffer_extra_slots metadata_map in
```

##### Testing - Originate

Origination of the dex can be done in Cameligo Test framework with the `Test.originate_from_file` which will provide the address of the dex. Here is a snippet of code from a test deploying a dex.
```
    let f = "../../lib/cfmm/main_fa2_fa12.mligo" in
    let v_mich = Test.run (fun (x:Dex.storage) -> x) init_storage in
    let (addr, _, _) = Test.originate_from_file f "main" ["v_get_constants"] v_mich 0tez in
```
Tests and espacially helpers are good examples (check this helper `/test/helpers/cfmm.mligo` that wraps the dex and can be easily used in tests) 

##### Testing - Custom storage

If at the origination of the dex the initial ratio Y/X different than 1 then the storage must be customized.
Here is a snippet of code that describes a full override of the storage. It defines a `custom_storage` function that takes extra `current_tick` parameter. (This "tick" corresponds to the ratio Y/X in exponential scale following the formula : Y/X = 1.0001^tick)

```
#import "dex-v3/lib/cfmm/main.mligo" "Dex"
#import "dex-v3/lib/cfmm/defaults.mligo" "Default"

let default_ladder : Dex.ladder = Default.default_ladder

let custom_storage
    (constants : Dex.constants)
    (init_cumulatives_buffer_extra_slots : nat)
    (metadata_map : Dex.metadata_map) 
    (current_tick : int) : Dex.storage =
  let minus_max_tick : int = 0n - Dex.const_max_tick in
  let minus_impossible_tick : int = 0n - Dex.impossible_tick in 
  let min_tick_state =
    { prev = { i = minus_impossible_tick }
    ; next = { i = int(Dex.const_max_tick) }
    ; liquidity_net = 0
    ; n_positions = 1n (* prevents garbage collection *)
    ; seconds_outside = 0n
    ; tick_cumulative_outside = 0
    ; fee_growth_outside = {x = { x128 = 0n } ; y = { x128 = 0n }}
    ; seconds_per_liquidity_outside = {x128 = 0n}
    ; sqrt_price = Dex.half_bps_pow (minus_max_tick, default_ladder)
    } in

  let max_tick_state =
    { prev = { i = minus_max_tick }
    ; next = { i = int(Dex.impossible_tick) }
    ; liquidity_net = 0
    ; n_positions = 1n (* prevents garbage collection *)
    ; seconds_outside = 0n
    ; tick_cumulative_outside = 0
    ; fee_growth_outside = {x = { x128 = 0n } ; y = { x128 = 0n }}
    ; seconds_per_liquidity_outside = {x128 = 0n}
    ; sqrt_price = Dex.half_bps_pow (int Dex.const_max_tick, default_ladder)
    } in

  let ticks = Big_map.literal [
      ({ i = minus_max_tick }, min_tick_state);
      ({ i = int(Dex.const_max_tick) }, max_tick_state)
  ] in

  { liquidity = 0n
  ; sqrt_price = Dex.half_bps_pow (current_tick, default_ladder)
  ; cur_tick_index = { i = current_tick }
  ; cur_tick_witness  = { i = minus_max_tick }
  ; fee_growth = { x = { x128 = 0n }; y = { x128 = 0n } }
  ; ticks = ticks
  ; positions = (Big_map.empty : Dex.position_map)
  ; cumulatives_buffer = Dex.init_cumulatives_buffer init_cumulatives_buffer_extra_slots
  ; metadata = metadata_map
  ; new_position_id = 0n
  ; operators = (Big_map.empty : Dex.operators)
  ; constants = constants
  ; ladder = default_ladder
  }
```

This `custom_storage` function can be used to generate an initial storage:
```
    let constants = { 
        fee_bps = fee_bps; 
        ctez_burn_fee_bps = proto_fee_bps;
        x_token_id = 0n;
        y_token_id = 0n;
        x_token_address = x_token_address; 
        y_token_address = y_token_address;
        tick_spacing = tick_spacing;
	} in
    let init_cumulatives_buffer_extra_slots = 0n in
    let metadata_map = (Big_map.empty : Cfmm.metadata_map) in
    let initial_storage = custom_storage constants init_cumulatives_buffer_extra_slots metadata_map initial_tick in
```

## How to use this repository

### Install

Install the library with ligo CLI (with docker)
```
ligo install dex-v3
```

This command will add a dependency in a package.json file.

Here is an example of the resulting package.json file.
```
{ "dependencies": { "dex-v3": "^0.1.1" } }
```

### Compiling

To produce the Michelson (TZ file) from Cameligo source code
```
make compile
```

### Tests

One can launch tests on a specific configuration or on a specific file
CONFIG= fa2_fa12 | fa12_fa2 | fa2_fa2 | fa12_fa12 | fa2_ctez | fa12_ctez | sft_fa2 | sft_fa12
FILE= set_position_$(CONFIG) | update_position_$(CONFIG) | x_to_y_$(CONFIG) | y_to_x_$(CONFIG) | complex_position_$(CONFIG)
```
make test CONFIG=fa12_fa2
make test FILE=x_to_y_fa2_fa12
```

To all launch tests (It may take 30-45 minutes)
```
make test
```

### Publishing to Ligolang

To publish this repository on https://packages.ligolang.org/packages
```
ligo login
ligo publish
```

The local `ligo` compiler must be used. 