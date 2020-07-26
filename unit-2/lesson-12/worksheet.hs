type FirstName = String
type MiddleName = String
type LastName = String
type PatientName = (FirstName, LastName)
type Age = Int
type Height = Int

data Name = Name { firstName :: FirstName
                 , lastName :: LastName }
          | NameWithMiddle { firstName :: FirstName
                           , middleName :: MiddleName
                           , lastName :: LastName }

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

--firstName :: PatientName -> FirstName
--firstName = fst

--lastName :: PatientName -> LastName
--lastName = snd

patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fName, lName) age height = name ++ " " ++ ageHeight
  where
    name = lName ++ ", " ++ fName
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"


data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

--data Patient = Patient Name Sex Int Int Int BloodType
data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)
janeElizabeth :: Patient
janeElizabeth = Patient (Name "Jane" "Elizabeth") Female 21 54 172 (BloodType O Neg)
jakieSmith :: Patient
jakieSmith = Patient { name = Name "Jakie" "Smith"
                     , age = 43
                     , sex = Male
                     , height = 62
                     , weight = 115
                     , bloodType = BloodType O Neg }
jakieSmithUpdated = jakieSmith { age = 44 }

canPatientDonateTo :: Patient -> Patient -> Bool
canPatientDonateTo d a = bloodType d `canDonateTo` bloodType a

patientSummary :: Patient -> String
patientSummary p =
  boundary ++ "\n" ++
  "Patient Name: " ++ lastName pName ++ ", " ++ firstName pName ++ "\n" ++
  "Sex: " ++ showSex (sex p) ++ "\n" ++
  "Age: " ++ show (age p) ++ "\n" ++
  "Height: " ++ show (height p) ++ " in.\n" ++
  "Weight: " ++ show (weight p) ++ " lbs.\n" ++
  "Blood Type: " ++ showBloodType (bloodType p) ++ "\n" ++
  boundary ++ "\n"
  where boundary = replicate 15 '*'
        pName = name p

