import Data.Maybe
----------------------------------------------------------------------------------------------
--------------------- 1. Ecrivez un automate avec le type suivant  ---------------------------
----------------------------------------------------------------------------------------------

type Nom = Int
type Transitions = [(Char,Nom)]
-- une transition = char+nom_etat
data Etat = Etat Bool Transitions
-- terminal et transitions
data AutomateFini = AutomateFini [(Nom,Etat)] Nom -- dictionaire des etats et etat init

--- automate pour le language (a b)*
automate :: AutomateFini
etat1 =  Etat True  [('a',2)]
etat2 =  Etat False [('b',1)]
automate = AutomateFini [(1,etat1), (2,etat2)] 1


----------------------------------------------------------------------------------------------
--------------------- 3. Que fait cet automate ?  --------------------------------------------
----------------------------------------------------------------------------------------------
--- L(A) =   {w appartient à {a,b}* telque |w|a >= 1}U{ ε } 
automate'1 :: AutomateFini
etat'1 = Etat True  [('a',1),('b',2)]
etat'2 = Etat False [('a',3),('b',1)]
etat'3 = Etat False [('a',2),('b',1)]
automate'1 = AutomateFini [(1,etat'1), (2,etat'2), (3,etat'3)] 1

----------------------------------------------------------------------------------------------
--------------------- 4. Ecrire un automate qui lit les mots de la form (abc)*  --------------
----------------------------------------------------------------------------------------------

automate'2 :: AutomateFini
etat''1 = Etat True [('a',2)]
etat''2 = Etat False [('b',3)]
etat''3 = Etat False [('c',1)]
automate'2 = AutomateFini [(1,etat''1), (2,etat''2), (3,etat''3)] 1

----------------------------------------------------------------------------------------------
---------------------5. implémentez reconnait :: AutomateFini -> String -> Bool --------------
----------------------------------------------------------------------------------------------
{--

Algorithme

1.Chercher l'etat initial à travers la methode etatInitial
2. Deux cas
2.1 String s== vide ==> on va retourner le type d'etat initial
2.2 String s/= vide ==> 
  1. Tester si la transition exite ?
  1.1 elle n'existe pas ==> on va retourner false
  1.2 existe ==> appel recursive de la methode reconnait avec l'etat init=etatsuivant de la transition et le reste de la liste 
 --}

reconnait :: AutomateFini -> String -> Bool 

reconnait (AutomateFini l n ) [] = etatType(etat (etatInitial l n))

reconnait (AutomateFini l n ) (c:w) |(transition (transitions (etat (etatInitial l n))) c) ==Nothing = False
                                    |(transition(transitions (etat (etatInitial l n))) c) /=Nothing = reconnait (AutomateFini l (numEtatS ( fromJust (transition (transitions (etat (etatInitial l n))) c )))) w

----------------------------------------------------------------------------------------------
------ 5.1 fonction etatInitial  permet de chercher l'etat initial d'une automate-------------
----------------------------------------------------------------------------------------------

etatInitial :: [(Nom,Etat)] -> Nom -> (Nom,Etat)

etatInitial ((n,e):l)  x | n==x = (n,e)
                         | n /= x = etatInitial l x

----------------------------------------------------------------------------------------------
------ 5.2 fonction etat permet d'extraire un etat d'un pair (Nom,Etat)   --------------------
----------------------------------------------------------------------------------------------

etat :: (Nom,Etat) -> Etat

etat (n,e) = e

----------------------------------------------------------------------------------------------
------ 5.3 fonction etatType permet d'extraire le type d'un etat -----------------------------
----------------------------------------------------------------------------------------------

etatType :: Etat -> Bool

etatType (Etat b l) = b

----------------------------------------------------------------------------------------------
------ 5.4 fonction transitions permet d'extraire les transitions  d'un etat -----------------
----------------------------------------------------------------------------------------------

transitions :: Etat -> Transitions
transitions (Etat b l) = l

----------------------------------------------------------------------------------------------
------ 5.5 fonction transitions permet de rechercher la transitio a partir un cle ----------------
----------------------------------------------------------------------------------------------

transition :: Transitions ->  Char ->Maybe (Char,Nom)

transition [] x = Nothing

transition ((c,n):l) x | c==x = Just (c,n)
                       | c/= x =  transition l x  




numEtatS :: (Char,Nom) -> Nom 
numEtatS (c,n)=n





