module Contract Types

--------------------------------------------------
--               contract structs
BlockNum : Type
Address : Type
UTCTime : Type
Balance : Type

BlockNum = Int
Address = Int
UTCTime = Int
Balance = Int

data ImmutBCData = MkImmutBCData BlockNum UTCTime
data ImmutContractData = MkImmutContractData Address
data MutContractData a = MkMutContractData Balance a
