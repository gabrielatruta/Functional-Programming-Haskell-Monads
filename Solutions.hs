import Data.Char

passwords :: [String]
passwords = sequenceA (replicate 8 (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']))

data User = User {email :: Email, password :: Password} deriving (Show, Eq)

data Password = Password String deriving (Show, Eq)

data Email = Email {username :: String, domain:: String} deriving (Show, Eq)

validatePassword :: String -> Maybe Password
validatePassword password = 
    let
        helper :: Bool -> Bool -> Bool -> String -> Maybe Password
        helper lower upper digit pass =
            case pass of
                [] -> if lower && upper && digit && (length password >= 8) then Just (Password password) else Nothing
                x:xs -> if isUpper x then helper lower True digit xs
                        else if isLower x then helper True upper digit xs
                        else if isDigit x then helper lower upper True xs
                        else helper lower upper digit xs
    in
        helper False False False password

oneArond :: String -> Bool
oneArond email = length (filter (== '@') email) == 1

usernameEmail :: String -> Bool
usernameEmail email = length (takeWhile (/= '@') email) >= 3

domainName :: String -> Bool
domainName email =
    let 
        domain = reverse (takeWhile (/= '.') (reverse email))
        hostName = reverse (dropWhile (/= '.') (reverse (dropWhile (/= '@') email)))
    in
        length hostName > 2 && length (filter (== '@') hostName) == 1 && length domain >= 2

validateEmail :: String -> Maybe Email
validateEmail email = if oneArond email && usernameEmail email && domainName email then Just (Email (takeWhile (/= '@') email) (drop 1 (dropWhile (/= '@') email))) else Nothing
        
validateUser :: String -> String -> Maybe User
validateUser email password = 
    case (validatePassword password, validateEmail email) of
        (Just _, Just _) -> Just (User (Email (takeWhile (/= '@') email) (drop 1 (dropWhile (/= '@') email))) (Password password))
        (_, _) -> Nothing

main :: IO ()
main = do
    putStrLn "Email:"
    email <- getLine
    putStrLn "Password:"
    password <- getLine
    case validateUser email password of
        (Just _ ) -> putStrLn "Valid username and password"
        Nothing -> putStrLn "Invalid username or password"