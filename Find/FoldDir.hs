
data Iterate seed = Done    { unwrap :: seed }
                  | Skip    { unwrap :: seed }
                  | Continue{ unwrap :: seed }
                    deriving (Show)

