#import "../../lib/cfmm/main.mligo" "Cfmm"
let ceildiv (numerator : nat) (denominator : nat) : nat = abs ((0n - numerator) / (int denominator))

let _280 : nat = Bitwise.shift_left 1n 80n

let calcSwapFee(feeBps, tokensDelta : nat * nat) : nat =
    // let version2_hs = tokensDelta * feeBps / 10000n
    let version1 = ceildiv (tokensDelta * feeBps) 10000n in 
    version1

let calcNewPriceX(sqrt_price_old, liquidity, dx : Cfmm.x80n * nat * nat) : nat =
    // from smart contract
    let version1 = (Bitwise.shift_left (liquidity * sqrt_price_old.x80) 80n) / 
            ((Bitwise.shift_left liquidity 80n) + dx * sqrt_price_old.x80) in
    //from haskell: _280 / (dx / liquidity + _280 / sqrt_price_old.x80) 
    // let version2_hs = _280 * 10000000000n / (10000000000n * dx / liquidity + 10000000000n * _280 / sqrt_price_old.x80) in 
    // let () = assert(version1 = version2_hs) in
    version1
    
let calcReceivedY(sqrt_price_old, sqrt_price_new, liquidity, protoFees : nat * nat * nat * nat) : nat =
    // from smart contract:  
    // let dy = Bitwise.shift_right ((assert_nat (p.s.sqrt_price.x80 - sqrt_price_new.x80, internal_bad_sqrt_price_move_x_direction)) * p.s.liquidity) 80n in
    let temp_a =  Bitwise.shift_right (sqrt_price_old * 100000000000000000000000n) 80n in 
    let temp_b =  Bitwise.shift_right (sqrt_price_new * 100000000000000000000000n) 80n in 
    let temp = abs(temp_a - temp_b) * liquidity in
    let version1 = temp / 100000000000000000000000n in
    // remove protofees
    let version1 = version1 * abs(10000n - protoFees) / 10000n in

    // from test in haskell
    // (old - new) because swapping will make decrease sqrt_price_new 
    // let diff  = sqrt_price_old - sqrt_price_new in
    // let temp_dy = diff * liquidity / _280 in
    // let version2_hs :nat = abs(temp_dy * abs(10000n - protoFees) / 10000) in
    // let () = assert(version1 = version2_hs) in
    version1


// calcNewPriceY
// NewPriceY = 2^80 * dy / liquidity + sqrt_price_old
let calcNewPriceY(sqrt_price_old, liquidity, dy, proto_fee_bps : Cfmm.x80n * nat * nat * nat) : nat =
    let dy = abs(dy * (10_000 - proto_fee_bps) / 10_000) in
    // from smart contract
    let version1 = ceildiv (Bitwise.shift_left dy 80n) liquidity + sqrt_price_old.x80 in
    //from haskell (2^80 * dy / liquidity + sqrt_price_old)
    // let version2_hs = ceildiv (_280 * dy) liquidity + sqrt_price_old.x80 in
    // let () = assert(version1 = version2_hs) in
    version1


let calcReceivedX(sqrt_price_old, sqrt_price_new, liquidity : nat * nat * nat) : nat =
    // from smart contract:  
    let () = assert(sqrt_price_new >= sqrt_price_old) in
    let version1 = 
        (liquidity * Bitwise.shift_left (abs(sqrt_price_new - sqrt_price_old)) 80n) / 
        (sqrt_price_new * sqrt_price_old) in
    // from test in haskell
    let dx =
        ((liquidity * _280 * 100000000000000000000000n) / sqrt_price_new)
        -
        ((liquidity * _280 * 100000000000000000000000n) / sqrt_price_old)
    in
    // -- dx is the amount of tokens to add to the pool.
    // -- To calculate how many tokens will be sent to the user, we flip the sign.
    // let version2_hs  = abs(0 - dx) / 100000000000000000000000n in
    // let () = assert(version1 = version2_hs) in
    version1

// let sqrtPriceFor(ti : Cfmm.tick_index) : nat =
//   adjustScale @30 $
//     mkX' @Double @80 (sqrt (exp 0.0001) ^^ i)
let sqrtPriceFor(ti, ladder : int * Cfmm.ladder) : Cfmm.x80n =
    Cfmm.half_bps_pow(ti, ladder)


// -- When adding @liquidity_delta@ to a position, calculate how many tokens will need to be deposited/withdrawn.
// -- Due to the floating-point math used in `sqrtPriceFor`, this function has a certain margin of error.
let liquidityDeltaToTokensDelta(liquidityDelta, lowerTickIndex, upperTickIndex, currentTickIndex, sqrtPrice, ladder : int * int * int * int * Cfmm.x80n * Cfmm.ladder) : (int * int) =
    let sqrtPriceLower : Cfmm.x80n = sqrtPriceFor(lowerTickIndex, ladder) in
    let sqrtPriceUpper : Cfmm.x80n = sqrtPriceFor(upperTickIndex, ladder) in
    // -- Equation 6.29
    // let () = assert(sqrtPrice.x80 >= sqrtPriceLower.x80) in
    // let () = assert(sqrtPriceUpper.x80 >= sqrtPriceLower.x80) in
    let delta_y = 
        if currentTickIndex < lowerTickIndex then
            0
        else if lowerTickIndex <= currentTickIndex && currentTickIndex < upperTickIndex then
            liquidityDelta * (sqrtPrice.x80 - sqrtPriceLower.x80) / _280
        else
            liquidityDelta * (sqrtPriceUpper.x80 - sqrtPriceLower.x80) / _280
    in
    // -- Equation 6.30
    // let () = assert(sqrtPriceUpper.x80 >= sqrtPriceLower.x80) in
    // let () = assert(sqrtPrice.x80 >= sqrtPriceLower.x80) in
    let delta_x = 
        if currentTickIndex < lowerTickIndex then
            // (liquidityDelta * _280 * (-sqrtPriceLower + sqrtPriceUpper)) `divUp` (sqrtPriceLower * sqrtPriceUpper)
            (liquidityDelta * _280 * (sqrtPriceUpper.x80 - sqrtPriceLower.x80)) / (sqrtPriceLower.x80 * sqrtPriceUpper.x80)
        else if lowerTickIndex <= currentTickIndex && currentTickIndex < upperTickIndex then
            // (liquidityDelta * _280 * (sqrtPriceUpper - sqrtPrice)) `divUp` (sqrtPrice * sqrtPriceUpper)
            (liquidityDelta * _280 * (sqrtPriceUpper.x80 - sqrtPrice.x80)) / (sqrtPrice.x80 * sqrtPriceUpper.x80)
        else
            0
    in
    (delta_x, delta_y)