--Selfmade three valued logic datatype
--emulating basic truth tables





data ThreeValuedLogic = Tru
                      | Fals
                      | Ukn deriving (Show)


ternaryNOT :: ThreeValuedLogic -> ThreeValuedLogic
ternaryNOT Tru = Fals
ternaryNOT Fals = Tru
ternaryNOT Ukn = Ukn
     

ternaryAND :: ThreeValuedLogic -> ThreeValuedLogic -> ThreeValuedLogic
ternaryAND Tru Tru = Tru
ternaryAND Fals _ = Fals
ternaryAND _ Fals = Fals
ternaryAND _ _ = Ukn


ternaryOR :: ThreeValuedLogic -> ThreeValuedLogic -> ThreeValuedLogic
ternaryOR Tru _ = Tru
ternaryOR _ Tru = Tru
ternaryOR Fals Fals = Fals
ternaryOR _ _ = Ukn