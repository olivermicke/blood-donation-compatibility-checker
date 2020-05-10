data Patient = Patient { name :: Name
                       , bloodType:: BloodType }
instance Show Patient where
  show (Patient (firstName, lastName) bloodType) =
    mconcat [firstName, " ", lastName, " ", "[", show bloodType, "]"]

data BloodType = BloodType { aboType :: ABOType
                           , rhesusType :: RhesusType }
instance Show BloodType where
  show (BloodType aboType rhesusType) = show aboType ++ show rhesusType

data ABOType = A | B | AB | O deriving(Show)
data RhesusType = Positive | Negative
instance Show RhesusType where
  show Positive = "+"
  show Negative = "-"

type Donor = Patient
type Recipient = Patient
type DonorBloodType = BloodType
type RecipientBloodType = BloodType
type Name = (String, String)

canDonateTo :: Donor -> Recipient -> Bool
canDonateTo donor recipient =
  canDonateTo' (bloodType donor) (bloodType recipient)

canDonateTo' :: DonorBloodType -> RecipientBloodType -> Bool
canDonateTo' (BloodType _  Positive) (BloodType _  Negative) = False
canDonateTo' (BloodType A  _       ) (BloodType A  _       ) = True
canDonateTo' (BloodType A  _       ) (BloodType AB _       ) = True
canDonateTo' (BloodType A  _       ) _                       = False
canDonateTo' (BloodType B  _       ) (BloodType B  _)        = True
canDonateTo' (BloodType B  _       ) (BloodType AB _)        = True
canDonateTo' (BloodType B  _       ) _                       = False
canDonateTo' (BloodType AB _       ) (BloodType AB _)        = True
canDonateTo' (BloodType AB _       ) _                       = False
canDonateTo' (BloodType O  _       ) _                       = True

showCanDonateTo :: Donor -> Recipient -> String
showCanDonateTo donor recipient = mconcat
  [show donor, " ", canDonateStr, " ", show recipient]
 where
  canDonate    = canDonateTo donor recipient
  canDonateStr = (if canDonate then "can" else "can't") ++ " donate to"

main :: IO ()
main = do
  let john =
        Patient { name = ("John", "Doe"), bloodType = BloodType AB Negative }

  let jane =
        Patient { name = ("Jane", "Doe"), bloodType = BloodType A Positive }

  let jürgen =
        Patient { name = ("Jürgen", "Doe"), bloodType = BloodType O Negative }

  let showCanDonateToJohn donor = showCanDonateTo donor john

  putStrLn "== Recipient =="
  mapM_ putStrLn [show john, ""]
  putStrLn "== Donors =="
  mapM_ putStrLn ((map show [jane, jürgen]) ++ [""])
  putStrLn "== Results =="
  mapM_ (putStrLn . showCanDonateToJohn) [jane, jürgen]
