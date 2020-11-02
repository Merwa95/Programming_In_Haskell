{- Projet 230   phase2_11500834_11405067-1.hs   -}
-- Type couleur

import System.Random
import Criterion.Main
import Text.Printf
import Control.Exception
import System.CPUTime
import Control.Parallel.Strategies
import Control.Monad
import System.Environment
--import Formatting
--import Formatting.Clock
import System.Clock
import System.TimeIt
import System.IO
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Char
 
data Couleur = Rouge | Noir deriving(Show)

-- Type pour les Arbres Binaires de recherche (ABR).
data Arbre =  Vide  
           | Noeud Couleur Integer Arbre Arbre
             deriving(Show)

----------------------------------phase3----------------------------------------------------------



-------------------------------------- Question 2-------------------------------------------------
newList:: Int -> Integer -> [Integer] 

newList a 0 = []
newList a n = let (valeurAleatoir,generateur) = randomR (0,n*n) (mkStdGen a) in [valeurAleatoir] ++ newList a (n-1)

-------------------------------------- Question 3-------------------------------------------------


toARN :: [Integer]-> Arbre
toARN [] = Vide
toARN (x : l)  = inserer  (toARN l) x

-------------------------------------- Question 4-------------------------------------------------

toArn :: Int -> Integer -> Arbre

toArn a n = toARN(newList a n)


-------------------------------------- Question 5-------------------------------------------------


main :: IO()
main = do
 	putStrLn "StartFunctionA NewList"
   n <- getLine
   seed <- getLine

	timeIt $ putStrLn ("Result: " ++ show newList (read seed :: Int ) (read n :: Int))
	putStrLn "End"





---------------------------------------------------------------------------------------
--Fonction qui ninverse la couleur d'un noeud
inversCoulr :: Arbre -> Arbre
inversCoulr Vide = Vide
inversCoulr (Noeud Rouge x g d) = (Noeud Noir x g d)
inversCoulr (Noeud Noir x g d) = (Noeud Rouge x g d)

--True si un noeud est noir False sinon
estNoir :: Arbre -> Bool
estNoir Vide = False
estNoir (Noeud Noir r g d) = True
estNoir (Noeud Rouge r g d) = False
---------------------------------------------------------------------------------------
--Fonction equilibrage 

balance :: Arbre -> Integer -> Arbre -> Arbre
balance (Noeud Rouge x a b) y (Noeud Rouge z c d) = (Noeud Rouge y (Noeud Noir x a b) (Noeud Noir z c d))
balance (Noeud Rouge y (Noeud Rouge x a b) c) z d = (Noeud Rouge y (Noeud Noir x a b) (Noeud Noir z c d))
balance (Noeud Rouge x a (Noeud Rouge y b c)) z d = (Noeud Rouge y (Noeud Noir x a b) (Noeud Noir z c d))
balance a x (Noeud Rouge y b (Noeud Rouge z c d)) = (Noeud Rouge y (Noeud Noir x a b) (Noeud Noir z c d))
balance a x (Noeud Rouge z (Noeud Rouge y b c) d) = (Noeud Rouge y (Noeud Noir x a b) (Noeud Noir z c d))
balance a x b = (Noeud Noir x a b)



----------------------------QUESTION 2-------------------------------------------------

-- Fonction qui prend en argument un ABR ainsi qu'un entier et renvoit True si l'entier passé en argument appartient à l'ABR, False sinon.
recherche :: Arbre  -> Integer -> Bool 
recherche Vide _ = False
recherche (Noeud _ r g d) x |x == r = True
                          |x >  r  = recherche d x
                          |x <  r  = recherche g x

----------------------------QUESTION 3-------------------------------------------------

-- Fonction qui prend en argument un ABR et renvoit à la fois l'ABR privé du plus petit élement et l'entier représentant le plus petit élement.
supprimerMinimum :: Arbre -> (Arbre,Integer) 
supprimerMinimum (Noeud _ r Vide d) = (d,r)
supprimerMinimum (Noeud c r g1 d1) = let (g2,petit) = supprimerMinimum g1 in
                                     (Noeud c r g2 d1, petit)

-- Fonction qui prend en argument un ABR et renvoit un ABR qui supprime la racine (dans notre cas la future racine est le plus petit entier de la partie droite de l'arbre).
supprimerRacine :: Arbre -> Arbre 
supprimerRacine (Noeud _ _ Vide Vide) = Vide
supprimerRacine (Noeud _ _ Vide d) = d
supprimerRacine (Noeud _ _ g Vide) = g
supprimerRacine (Noeud c _ g1 d1) = Noeud c r2 g1 d2 where (d2,r2) = supprimerMinimum d1

-- Fonction qui prend en argument un entier ainsi qu'un ABR et renvoit un ABR qui supprime l'entier passé en argument.
supprimer :: Integer -> Arbre -> Arbre 
supprimer _ Vide = Vide
supprimer x (Noeud c r g d) | x == r = supprimerRacine (Noeud c r g d)
                            | x > r  = Noeud c r g (supprimer x d)
                            | x < r  = Noeud c r (supprimer x g) d

----------------------------Modification de toute la fonction supprimer---------------
insNoir :: Arbre -> Arbre
insNoir (Noeud c r g d) = (Noeud Noir r g d)

equilibrage :: Arbre -> Arbre
equilibrage (Noeud Noir z (Noeud Rouge y (Noeud Rouge x xg xd) yd) zd) = Noeud Rouge y (Noeud Noir x xg xd) (Noeud Noir z yd zd)
equilibrage (Noeud Noir z (Noeud Rouge x xg (Noeud Rouge y yg yd)) zd) = Noeud Rouge y (Noeud Noir x xg yg) (Noeud Noir z yd zd)
equilibrage (Noeud Noir x xg (Noeud Rouge z (Noeud Rouge y yg yd) zd)) = Noeud Rouge y (Noeud Noir x xg yg) (Noeud Noir z yd zd)
equilibrage (Noeud Noir x xg (Noeud Rouge y yg (Noeud Rouge z zg zd))) = Noeud Rouge y (Noeud Noir x xg yg) (Noeud Noir z zg zd)
equilibrage (Noeud c x g d) = Noeud c x g d


equilibreG :: Arbre -> Arbre
equilibreG (Noeud Noir y (Noeud Rouge x xg xd) d) = (Noeud Rouge y (Noeud Noir x xg xd) d)
equilibreG (Noeud Noir y g (Noeud Noir x xg xd)) = equilibrage (Noeud Noir y g (Noeud Rouge x xg xd))
equilibreG (Noeud Noir y g (Noeud Rouge x (Noeud Noir w gw dw) (Noeud Noir v gv dv))) = (Noeud Rouge w (Noeud Noir y g gw) (equilibrage (Noeud Noir x dw (Noeud Rouge v gv dv))))


equilibreD  :: Arbre -> Arbre
equilibreD (Noeud Noir y yg (Noeud Rouge x xg xd)) = Noeud Rouge y yg (Noeud Noir x xg xd)
equilibreD (Noeud Noir y (Noeud Noir x xg xd) yd) = equilibrage (Noeud Noir y (Noeud Rouge x xg xd) yd)
equilibreD (Noeud Noir y (Noeud Rouge x (Noeud Noir v vg vd) (Noeud Noir w wg wd)) yd) = (Noeud Rouge w (equilibrage (Noeud Noir x (Noeud Rouge v vg vd) wg)) (Noeud Noir y wd yd))

supprimerBis :: Integer -> Arbre -> Arbre
supprimerBis x Vide = Vide 
supprimerBis x (Noeud c r g d)
         | x < r = supprimerG x (Noeud c r g d)
         | x > r = supprimerD x (Noeud c r g d)
         | x == r = fusionner g d 

supprimerG :: Integer -> Arbre -> Arbre
supprimerG x (Noeud Noir r g d) = equilibreG (Noeud Noir r (supprimerBis x g) d)
supprimerG x (Noeud Rouge r g d) = (Noeud Rouge r (supprimerBis x g) d)

supprimerD :: Integer -> Arbre -> Arbre
supprimerD x (Noeud Noir r g d) = equilibreD (Noeud Noir r g (supprimerBis x d))
supprimerD x (Noeud Rouge r g d) = (Noeud Rouge r g (supprimerBis x d))

supprimerARN :: Integer -> Arbre -> Arbre
supprimerARN x (Noeud c r g d) = insNoir ((supprimerBis x (Noeud c x g d))) 
----------------------------QUESTION 4-------------------------------------------------


inserer :: Arbre -> Integer -> Arbre
inserer Vide x = Noeud Noir x Vide Vide 
inserer a x |recherche a x == True = a
            |recherche a x == False = insererBis a x 


-- Fonction qui prend en argument un ABR ainsi qu'un entier et renvoit un ABR qui insert dans l'ABR l'entier passé en argument.
insererBis :: Arbre  -> Integer -> Arbre 
insererBis Vide x = Noeud Rouge x Vide Vide
insererBis (Noeud c r g d) x |x > r = balance g r (insererBis d x) 
                             |x < r = balance (insererBis g x) r d 

----------------------------QUESTION 5-------------------------------------------------

-- Fonction qui prend en argument un entier ainsi qu'un ABR et renvoit un ABR qui incrémente la valeur de chaque clé selon l'entier passé en argument.
incrementer :: Integer -> Arbre  -> Arbre 
incrementer x Vide =  Vide
incrementer x (Noeud c r g d) = Noeud c (r+x) (incrementer x g) (incrementer x d)

----------------------------QUESTION 6-------------------------------------------------

-- Fonction qui prend en argument un ABR et renvoit un ABR qui multiplie par deux la valeur de chaque clé.
multiple_2 :: Arbre  -> Arbre 
multiple_2 Vide =  Vide
multiple_2 (Noeud c r g d) = Noeud c (r*2) (multiple_2 g) (multiple_2 d)

----------------------------QUESTION 7-------------------------------------------------

-- Fonction qui prend en argument un ABR et renvoit un ABR qui inverse le signe de toute les clés.
-- Comme on a inversé le signe de toute les clés, on doit également réaliser une rotation des sous-arbres gauche et droit afin d'être un ABR.
inverse :: Arbre  -> Arbre 
inverse Vide =  Vide
inverse (Noeud c r g d) = Noeud c (-r) (inverse d) (inverse g)

----------------------------QUESTION 8-------------------------------------------------

-- Fonction qui prend en argument deux ABR et renvoit un ABR qui est la fusion des deux ABR passé en argument.
-- Dans le dernier cas, on insère la racine du deuxième arbre et puis récursivement, on parcourt les sous-arbres gauches et droit.
fusionner :: Arbre -> Arbre -> Arbre 
fusionner Vide Vide = Vide
fusionner Vide (Noeud c r1 g1 d1)= Noeud c r1 g1 d1
fusionner (Noeud c r1 g1 d1) Vide = Noeud c r1 g1 d1
fusionner (Noeud c r1 g1 d1) (Noeud cl r2 g2 d2) = fusionner (fusionner (inserer (Noeud c r1 g1 d1) r2) g2) d2


---------------------------------------------------------------------------------------
--Test si il y a le même nombre de noeud noi rdans tous les chemins

maxNoir :: Arbre -> Integer
maxNoir Vide = 0
maxNoir (Noeud c r g d) | (estNoir (Noeud c r g d)) == True = max (1 + (maxNoir g)) (1 + (maxNoir d)) 
                        | (estNoir (Noeud c r g d)) == False = max (maxNoir g) (maxNoir d)

minNoir :: Arbre -> Integer
minNoir Vide = 0
minNoir (Noeud c r g d) | (estNoir (Noeud c r g d)) == True = min (1 + (minNoir g)) (1 + (minNoir d)) 
                        | (estNoir (Noeud c r g d)) == False = min (minNoir g) (minNoir d)

nbNoir :: Arbre -> Bool 
nbNoir Vide =  False
nbNoir (Noeud c r g d) = (minNoir(Noeud c r g d)) == (maxNoir(Noeud c r g d))

















