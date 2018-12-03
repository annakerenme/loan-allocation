data Cents= Cents Int deriving (Show)
data ID = ID Int deriving (Show)
data LoanPart = LoanPart Int deriving (Show)
data Loan = Loan ID [LoanPart] deriving (Show)

data Name = Name String
data Investor = Investor ID String Int [Int] deriving (Show)

allocate :: Loan -> String -> Int -> Maybe Investor
allocate (Loan loanId  [LoanPart lpCents]) investorName investorSum 
 | investorSum > lpCents = Just (Investor loanId investorName investorSum [lpCents])
 | otherwise = Nothing  
