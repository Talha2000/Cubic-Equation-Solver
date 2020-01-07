{- Assignment 1
 - Name: Talha Amjad
 - Date: 2019-09-29
 -}

module Assign_1 where

macid :: String
macid = "amjadt1"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 This function will calculate what cubicQ is going to be when
 you give values for a b c
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3*a*c)-(b**2))/(9*a**2)
{- ----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 Similar to cubicQ, this line of code will calculate what cubicR
 will equal to when you give a b c and d values.
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9*a*b*c)-(27*a^2*d)-(2*b^3))/(54*a^3)

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 Takes a value for q and r and calculate for cubicDisc which will
 later help us use the condition statements. The solutions will determine
 based on the value of the equation q^3 + r^2 and give the discriminants.
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r =  q^3 + r^2 
{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - calculates cubicS given values for q and r. If the value is a negative,
 it will multiply by -1 so that it will be able to do the cube root. 
 -}
cubicS :: Double -> Double -> Double
cubicS q r = do
    if (r + (sqrt((q**3) + (r**2)))) < 0
        then -((abs(r + (sqrt(q**3 + r**2))))**(1/3))
    else (r + (sqrt((q**3) + (r**2))))**(1/3)
{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Calculates cubicT given values for q and r, similar to cubicS,
 in order for haskell to be able to cube root something, it will have to
 be positive instead of a negative number.
 -}
cubicT :: Double -> Double -> Double
cubicT q r = do
    if (r - (sqrt(q**3 + r**2))) < 0
        then -((abs(r - (sqrt(q**3 + r**2))))**(1/3))
    else (r - (sqrt((q**3 + r**2))))**(1/3)

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - This is our main function and all the other functions are implemented in here.
 in this function, we LET all the variables: q, r, s, t, disc, x1 and x2 equal
 to an equation. q, r, s, t and disc will basically equal to the functions previously
 defined. Since discriminant values will come from "cubicDisc q r", we will use "disc"
 and use if statements to determine the output based on the conditions and value of the 
 discriminant.
 -}
cubicRealSolutions a b c d = do
    let 
        q = cubicQ a b c
        r = cubicR a b c d
        s = cubicS q r 
        t = cubicT q r
        disc = cubicDisc q r
        x1 = (s + t) - (b/(3*a))
        x2 = ((-s - t)/2) - (b/(3*a))
        x3 = ((-s - t)/2) - (b/(3*a))
    if (disc < 0) 
        then []  
    else if (disc == 0)
        then [x1, x2, x3]
    else [x1]
{- -----------------------------------------------------------------
 -- Test Cases
 
 - ---------------------------------------------------------------}

