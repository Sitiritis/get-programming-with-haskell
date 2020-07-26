import Data.List

ifEvenIncDup n =
  if even n
  then n + 1
  else n
ifEvenDoubleDup n =
  if even n
  then n * 2
  else n
ifEvenSquareDup n =
  if even n
  then n^2
  else n

ifEven f n =
  if even n
  then f n
  else n

inc n = n + 1
double n = n*2
square n = n^2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n
ifEvenCube n = ifEven (\x -> x^3) n

author = ("Will", "Kurt")

names = [
    ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephen", "Morris")
  ]

compareLastNames name1 name2 =
  if lastName1 > lastName2
  then GT
  else
    if lastName1 < lastName2
    then LT
    else
      if firstName1 > firstName1
      then GT
      else
        if firstName1 < firstName2
        then LT
        else EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2
    firstName1 = fst name1
    firstName2 = fst name2
compareLastNamesComp name1 name2 = if lastNamesComp == EQ
  then firstNamesComp
  else lastNamesComp
  where
    lastName1 = snd name1
    lastName2 = snd name2
    firstName1 = fst name1
    firstName2 = fst name2
    lastNamesComp = compare lastName1 lastName2
    firstNamesComp = compare firstName1 firstName2

addressLetterStraight name location = nameText ++ " - " ++ location
  where nameText = (fst name) ++ " " ++ (snd name)

sfOffice name =
  if lastName < "L"
  then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
  else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName
nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)
renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name
dcOffice name = nameText ++ " - Washington, DC"
  where nameText = (fst name) ++ " " ++ (snd name) ++ ", Esq."

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location
