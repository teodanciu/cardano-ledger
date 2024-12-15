{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | In cardano-ledger, hashing a type @X@ is based upon the serialization of @X@. Serialization is
--   based upon the 'EncCBOR' and DecCBOR type classes, and the values produced by 'EncCBOR' instances for a
--   particular type, are not necessarily unique. For this reason, when an @X@ object comes
--   over the network in serialized form, we must preserve the original bytes that arrived over
--   the network, otherwise when the system hashes that object, the hash in the ledger, and the hash of
--   that object from the other side of the network may not agree. The module 'Cardano.Ledger.SafeHash'
--   introduces the 'SafeToHash' type class that ensures that types with a @(SafeToHash X)@ instance store the
--   original bytes that arrived over the network for the value of @X@. The recommended way to store the
--   original bytes is to use the type 'MemoBytes', although there are
--   a few types that store their original bytes in other ways. In order to encourage the use of 'MemoBytes'
--   newtypes defined as a 'MemoBytes' get the to derive 'SafeToHash' instances for free.
module Cardano.Ledger.SafeHash (
  -- * SafeHash and SafeToHash

  --
  -- $SAFE
  SafeHash,
  SafeToHash (..),

  -- * Creating SafeHash

  --
  -- $MAKE
  HashAnnotated,
  hashAnnotated,
  unsafeMakeSafeHash,

  -- * Other operations

  --
  -- $OTHER
  castSafeHash,
  extractHash,
  indexProxy,
)
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.HeapWords (HeapWords (..))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Plain (FromCBOR (..), ToCBOR (..))
import Cardano.Ledger.Crypto
import Cardano.Ledger.Orphans ()
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import Data.ByteString.Short (ShortByteString, fromShort)
import qualified Data.ByteString.Short as SBS (length)
import Data.Default (Default (..))
import Data.Typeable
import NoThunks.Class (NoThunks (..))

-- ==========================================================

-- SAFE

-- | A 'SafeHash' is a hash of something that is safe to hash. Such types store
--     their own serialisation bytes. The prime example is @('MemoBytes' t)@, but other
--     examples are things that consist of only ByteStrings (i.e. they are their own serialization)
--     or for some other reason store their original bytes.
--
--     We do NOT export the constructor 'SafeHash', but instead export other functions
--     such as 'hashAnnotated' and 'extractHash' which have constraints
--     that limit their application to types which preserve their original serialization
--     bytes.
newtype SafeHash i = SafeHash (Hash.Hash HASH i)
  deriving
    (Show, Eq, Ord, NoThunks, NFData, SafeToHash, HeapWords, ToCBOR, FromCBOR, EncCBOR, DecCBOR)

deriving instance ToJSON (SafeHash i)

deriving instance FromJSON (SafeHash i)

instance Default (SafeHash i) where
  def = unsafeMakeSafeHash def

-- | Extract the hash out of a 'SafeHash'
extractHash :: SafeHash i -> Hash.Hash HASH i
extractHash (SafeHash h) = h

-- MAKE

-- | Don't use this except in Testing to make Arbitrary instances, etc.
--   Defined here, only because the Constructor is in scope here.
unsafeMakeSafeHash :: Hash.Hash HASH i -> SafeHash i
unsafeMakeSafeHash = SafeHash

-- =====================================================================

-- | Only Types that preserve their serialisation bytes are members of the
--   class 'SafeToHash'. There are only a limited number of primitive direct
--   instances of 'SafeToHash', all but two of them are present in this file. Instead
--   of making explicit instances, we almost always use a newtype (around a type @S@)
--   where their is already an instance @(SafeToHash S)@. In that case the newtype
--   has its SafeToHash instance derived using newtype deriving. The prime example of @s@ is 'MemoBytes'.
--   The only exceptions are the legacy Shelley types: @Metadata@ and @ShelleyTx@, that
--   preserve their serialization bytes
--   using a different mechanism than the use of 'MemoBytes'.  'SafeToHash' is a superclass
--   requirement of the classes 'HashAnnotated' which
--   provide more convenient ways to construct SafeHashes than using 'makeHashWithExplicitProxys'.
class SafeToHash t where
  -- | Extract the original bytes from 't'
  originalBytes :: t -> ByteString

  originalBytesSize :: t -> Int
  originalBytesSize = BS.length . originalBytes

  makeHashWithExplicitProxys :: Proxy i -> t -> SafeHash i

  -- | Build a @(SafeHash index)@ value given a proxy determining @i@, and the
  --   value to be hashed.
  makeHashWithExplicitProxys _ x = SafeHash $ Hash.castHash (Hash.hashWith originalBytes x)

-- There are a limited number of direct instances. Everything else should come
-- from newtype deriving.

instance SafeToHash ShortByteString where
  originalBytes = fromShort
  originalBytesSize = SBS.length

instance SafeToHash ByteString where
  originalBytes x = x

-- If one looks at the deriving clause in the definitions of SafeHash, we see that we
-- derive that it is SafeToHash. We can derive this instance because SafeHash is
-- a newtype around (Hash.Hash c i) which is a primitive SafeToHash type.

instance Hash.HashAlgorithm h => SafeToHash (Hash.Hash h i) where
  originalBytes = Hash.hashToBytes

-- | Types that are 'SafeToHash', AND have both of the following two invariants,
--   are made members of the HashAnnotated class. The preconditions are:
--
--   1. The type uniquely determines the 'index' type tag of (SafeHashrypto index)
--   2. The type uniquely determines the 'crypto' type of (SafeHashrytop index)
--
--   The 'SafeToHash' and the 'HashAnnotated' classes are designed so that their
--   instances can be easily derived (because their methods have default methods
--   when the type is a newtype around a type that is 'SafeToHash'). For example,
class SafeToHash x => HashAnnotated x i | x -> i where
  indexProxy :: x -> Proxy i
  indexProxy _ = Proxy @i

  -- | Create a @('SafeHash' i crypto)@,
  -- given @(Hash.HashAlgorithm (HASH crypto))@
  -- and  @(HashAnnotated x i crypto)@ instances.
  hashAnnotated :: x -> SafeHash i
  hashAnnotated = makeHashWithExplicitProxys (Proxy @i)
  {-# INLINE hashAnnotated #-}

-- OTHER

-- | To change the index parameter of SafeHash (which is a phantom type) use castSafeHash
castSafeHash :: forall i j. SafeHash i -> SafeHash j
castSafeHash (SafeHash h) = SafeHash (Hash.castHash h)
