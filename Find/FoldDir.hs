
data Iterate seed = Done    { unwrap :: seed }
                  | Skip    { unwrap :: seed }
                  | Continue{ unwrap :: seed }
                    deriving (Show)

type Iterator seed = see -> Info -> Iterate seed


