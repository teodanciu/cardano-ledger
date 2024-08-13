{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Evaluate" #-}

module Test.Cardano.Ledger.Core.Binary.CDDL where

import Codec.CBOR.Cuddle.Huddle
import Data.Function (($))
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Show (Show (show))

--------------------------------------------------------------------------------
-- Crypto
--------------------------------------------------------------------------------

hash28 :: Rule
hash28 = "$hash28" =:= VBytes `sized` (28 :: Word64)

hash32 :: Rule
hash32 = "$hash32" =:= VBytes `sized` (32 :: Word64)

vkey :: Rule
vkey = "$vkey" =:= VBytes `sized` (32 :: Word64)

vrf_vkey :: Rule
vrf_vkey = "$vrf_vkey" =:= VBytes `sized` (32 :: Word64)

vrf_cert :: Rule
vrf_cert = "$vrf_cert" =:= arr [a VBytes, a (VBytes `sized` (80 :: Word64))]

kes_vkey :: Rule
kes_vkey = "$kes_vkey" =:= VBytes `sized` (32 :: Word64)

kes_signature :: Rule
kes_signature = "$kes_signature" =:= VBytes `sized` (448 :: Word64)

signkeyKES :: Rule
signkeyKES = "signkeyKES" =:= VBytes `sized` (64 :: Word64)

signature :: Rule
signature = "$signature" =:= VBytes `sized` (64 :: Word64)

--------------------------------------------------------------------------------
-- Extras
--------------------------------------------------------------------------------

-- Conway era introduces an optional 258 tag for sets, which will become mandatory in the
-- second era after Conway. We recommend all the tooling to account for this future breaking
-- change sooner rather than later, in order to provide a smooth transition for their users.

set :: IsType0 t0 => t0 -> GRuleCall
set = binding $ \x -> "set" =:= tag 258 (arr [0 <+ a x]) / sarr [0 <+ a x]

nonempty_set :: IsType0 t0 => t0 -> GRuleCall
nonempty_set = binding $ \x ->
  "nonempty_set"
    =:= tag 258 (arr [1 <+ a x])
    / sarr [1 <+ a x]

positive_int :: Rule
positive_int = "positive_int" =:= 1 ... 18446744073709551615

unit_interval :: Rule
unit_interval = "unit_interval" =:= tag 30 (arr [1, 2])

-- unit_interval = tag 0 [uint, uint]
--
-- Comment above depicts the actual definition for `unit_interval`.
--
-- Unit interval is a number in the range between 0 and 1, which
-- means there are two extra constraints:
-- \* numerator <= denominator
-- \* denominator > 0
--
-- Relation between numerator and denominator cannot be expressed in CDDL, which
-- poses a problem for testing. We need to be able to generate random valid data
-- for testing implementation of our encoders/decoders. Which means we cannot use
-- the actual definition here and we hard code the value to 1/2

-- nonnegative_interval = tag 0 [uint, positive_int]
nonnegative_interval :: Rule
nonnegative_interval = "nonnegative_interval" =:= tag 30 (arr [a VUInt, a positive_int])

address :: Rule
address =
  "address"
    =:= bstr
      "001000000000000000000000000000000000000000000000000000000011000000000000000000000000000000000000000000000000000000"
    / bstr
      "102000000000000000000000000000000000000000000000000000000022000000000000000000000000000000000000000000000000000000"
    / bstr
      "203000000000000000000000000000000000000000000000000000000033000000000000000000000000000000000000000000000000000000"
    / bstr
      "304000000000000000000000000000000000000000000000000000000044000000000000000000000000000000000000000000000000000000"
    / bstr "405000000000000000000000000000000000000000000000000000000087680203"
    / bstr "506000000000000000000000000000000000000000000000000000000087680203"
    / bstr "6070000000000000000000000000000000000000000000000000000000"
    / bstr "7080000000000000000000000000000000000000000000000000000000"

reward_account :: Rule
reward_account =
  "reward_account"
    =:= bstr "E090000000000000000000000000000000000000000000000000000000"
    / bstr "F0A0000000000000000000000000000000000000000000000000000000"

bounded_bytes :: Rule
bounded_bytes = "bounded_bytes" =:= VBytes `sized` (0 :: Word64, 64 :: Word64)

-- the real bounded_bytes does not have this limit. it instead has a different
-- limit which cannot be expressed in CDDL.
-- The limit is as follows:
--  - bytes with a definite-length encoding are limited to size 0..64
--  - for bytes with an indefinite-length CBOR encoding, each chunk is
--    limited to size 0..64
--  ( reminder: in CBOR, the indefinite-length encoding of bytestrings
--    consists of a token #2.31 followed by a sequence of definite-length
--    encoded bytestrings and a stop code )

-- a type for distinct values.
-- The type parameter must support .size, for example: bytes or uint
distinct :: IsSizeable s => Value s -> Rule
distinct x =
  "distinct_"
    <> T.pack (show x)
      =:= (x `sized` (8 :: Word64))
      / (x `sized` (16 :: Word64))
      / (x `sized` (20 :: Word64))
      / (x `sized` (24 :: Word64))
      / (x `sized` (30 :: Word64))
      / (x `sized` (32 :: Word64))