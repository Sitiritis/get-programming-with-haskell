toPart recipient = "Dear " ++ recipient ++ ",\n"
bodyPart bookTitle = "Congratulations with purchasing " ++ bookTitle ++ ".\n"
fromPart author = "Sincerely yours,\n" ++ author

createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author

main :: IO()
main = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print (createEmail recipient title author)
