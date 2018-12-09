import Test.HUnit
  

data Cents= Cents Int deriving (Show, Eq)
data ID = ID Int deriving (Show, Eq)
data LoanPart = LoanPart Int deriving (Show, Eq)
data Loan = Loan ID [LoanPart] deriving (Show, Eq)

data Name = Name String deriving (Show, Eq)
data Investor = Investor ID String Int [Int] deriving (Show, Eq)

allocate :: Loan -> String -> Int -> Maybe Investor
allocate (Loan loanId  [LoanPart lpCents]) investorName investorSum
 | investorSum > lpCents = Just (Investor loanId investorName investorSum [lpCents])
 | otherwise = Nothing

loanPart1 = LoanPart 100
id1 = ID 1
loan = Loan id1 [loanPart1]
test1= TestCase (assertEqual ("allocate loan to investor") (allocate loan "aa" 105) (Just (Investor (ID 1) "aa" 105 [100])))


tests = TestList [TestLabel "test1" test1]
