import Test.HUnit
  

data Cents= Cents Int deriving (Show, Eq)
data ID = ID Int deriving (Show, Eq)
data LoanPart = LoanPart Int deriving (Show, Eq)
data Loan = Loan { loanId :: ID, listOfLoanParts :: [LoanPart] }  deriving (Show, Eq)

data Name = Name String deriving (Show, Eq)
data Investor = Investor { investorId :: ID, investorName :: String, investorFunds :: Int, loanParts :: [Int] } deriving (Show, Eq)

allocate :: Loan -> String -> Int -> Maybe Investor
allocate (Loan loanId  [LoanPart lpCents]) investorName investorSum
 | investorSum > lpCents = Just (Investor loanId investorName investorSum [lpCents])
 | otherwise = Nothing

loanPart1 = LoanPart 100
id1 = ID 1
loan = Loan id1 [loanPart1]
test1 = TestCase (assertEqual ("allocate loan to investor when investor can buy the loan in full") (allocate loan "aa" 105) (Just (Investor (ID 1) "aa" 105 [100])))
test2 = TestCase (assertEqual ("fail to allocate wehn investor does not have enough money to buy loan in full") (allocate loan "aa" 99) (Nothing))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]
