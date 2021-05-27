import Data.Maybe

type Nom = Int
type Transitions = ARN Char Nom 
data Etat = Etat Bool Transitions  deriving(Show,Eq)
data AutomateFini = AutomateFini (ARN Nom Etat) Nom deriving(Show,Eq)

data Couleur = Rouge | Noir deriving(Show,Eq)
data ARN cle cont = Feuille | Noeud Couleur cle cont (ARN cle cont) (ARN cle cont) deriving(Show,Eq)

---------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
---------- 1. modifier le module d’ARN récupérer pour matcher l’un de ces cas, L’ARN-------------
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------

--Fonction equilibrage 

balance :: ARN cle cont -> cle -> cont -> ARN cle cont-> ARN cle cont

balance (Noeud Rouge cl ct a b) cle cont (Noeud Rouge c'1 t c d) = (Noeud Rouge cle cont (Noeud Noir cl ct a b) (Noeud Noir c'1 t c d))

balance (Noeud Rouge cl ct (Noeud Rouge c'1 t a b) c) cle cont d = (Noeud Rouge cl ct (Noeud Noir c'1 t a b) (Noeud Noir cle cont c d))

balance (Noeud Rouge c'1 t a (Noeud Rouge cl ct b c)) cle cont d = (Noeud Rouge cl ct (Noeud Noir c'1 t a b) (Noeud Noir cle cont c d))

balance a cle cont (Noeud Rouge cl ct b (Noeud Rouge c'1 t c d)) = (Noeud Rouge cl ct (Noeud Noir cle cont a b) (Noeud Noir c'1 t c d))

balance a cle cont (Noeud Rouge cl ct (Noeud Rouge c'1 t b c) d) = (Noeud Rouge c'1 t (Noeud Noir cle cont a b) (Noeud Noir cl ct c d))

balance a cle cont b = (Noeud Noir cle cont a b)




-- Fonction qui prend en argument un ABR ainsi qu'un entier et renvoit True si l'entier passé en argument appartient à l'ABR, False sinon.
recherche :: Ord cle => ARN cle cont -> cle -> Maybe cont
recherche Feuille _ = Nothing
recherche (Noeud _ cle cont g d) x |x == cle = Just cont
                                   |x > cle  =  recherche d x
                                   |x <  cle  = recherche g x




inserer :: Eq cont => Ord cle =>ARN cle cont -> cle -> cont -> ARN cle cont
inserer Feuille cle cont = Noeud Noir cle cont Feuille Feuille 
inserer a cle cont |recherche a cle /= Nothing = a
                   |recherche a cle == Nothing = insererBis a cle cont


-- Fonction qui prend en argument un ABR ainsi qu'un entier et renvoit un ABR qui insert dans l'ABR l'entier passé en argument.
insererBis :: Ord cle => ARN cle cont -> cle -> cont -> ARN cle cont 
insererBis Feuille cle cont  = Noeud Rouge cle cont Feuille Feuille
insererBis (Noeud c cl ct g d) cle cont |cle >= cl = balance g cl ct (insererBis d cle cont) 
                                        |cle < cl = balance (insererBis g cle cont) cl ct d 

--Fonction qui ninverse la couleur d'un noeud
inversCoulr :: Ord cle => ARN cle cont -> ARN cle cont 
inversCoulr Feuille = Feuille
inversCoulr (Noeud Rouge cle cont g d) = (Noeud Noir cle cont g d)
inversCoulr (Noeud Noir cle cont g d) = (Noeud Rouge cle cont  g d)

--True si un noeud est noir False sinon
estNoir :: Ord cle => ARN cle cont  -> Bool
estNoir Feuille = False
estNoir (Noeud Noir cle cont g d) = True
estNoir (Noeud Rouge cle cont g d) = False 
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-------------------------- 2. implémenter le le type d’automate choisi,--------------------------
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
--- automate pour le language (a b)*

automate ::  AutomateFini
etat1 =  Etat True (inserer Feuille 'a' 2) 
etat2 =  Etat False (inserer Feuille 'b' 1) 
automate = AutomateFini ( inserer (inserer Feuille 1 etat1 ) 2 etat2) 1
{--
3. 

--}

reconnait :: AutomateFini -> String -> Bool 

reconnait (AutomateFini a n ) [] = etatType(fromJust(recherche a n))
reconnait (AutomateFini a n ) (c:w)|(recherche(transitions(fromJust(recherche a n))) c)==Nothing = False
                                   |(recherche(transitions(fromJust(recherche a n))) c)/=Nothing = reconnait(AutomateFini a (fromJust (recherche(transitions(fromJust(recherche a n))) c)) ) w
                
 

----------3.1------------------------------------------------------------------------------------

etatType :: Etat -> Bool

etatType (Etat b a) = b

----------------------------------------------------------------------------------------------
------ 3.2 fonction transitions permet d'extraire les transitions  d'un etat -----------------
----------------------------------------------------------------------------------------------

transitions :: Etat -> Transitions
transitions (Etat b a) = a

-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-- écrire une fonction oublisFinitude :: AutomateFini -> AutomateD qui 
--transporte un automate fini en un automate déterministe du type utilisé
------dans les TPs,--------------------------
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
------------declaration d'une automate pour la question4 on a que la methode inserer dans un ARN permet d'inserer une seule fois  cle
-- donc l'automate est deterministe il faut juste transformer l'automate fini en automateD 
data EtatD     =   EtatD Bool (Char -> EtatD)
                 | Puits Bool
type AutomateD = EtatD


oublisFinitude :: AutomateFini -> AutomateD
oublisFinitude (AutomateFini a n ) = EtatD (etatType (fromJust(recherche a n)) ) (etatSui (AutomateFini a n ) (transitions(fromJust(recherche a n))))


etatSui :: AutomateFini->Transitions -> (char -> EtatD)
etatSui a Feuille = (\_ -> Puits False)
etatSui (AutomateFini a i) (Noeud c cle cont g d) =  (\cle->EtatD (etatType (fromJust(recherche a cont)) ) (etatSui (AutomateFini a i) (transitions(fromJust(recherche a cont))) ) )

{--
tailleAu :: AutomateFini -> Integer
tailleAu (AutomateFini (Feuille) n) = 0
tailleAu (AutomateFini (Noeud c cle cont g d) n) = 1 + (tailleAu (AutomateFini g n)) + (tailleAu (AutomateFini d n))

clef :: ARN cle cont ->cle
clef (Noeud c cle cont g d)= cle

contenu :: ARN cle cont ->cont
contenu (Noeud c cle cont g d)= cont
--}
-------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
--------------------------------------------5. une version non déterministe--------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------


type TransitionsND = ARN Char Nom
data EtatND = EtatND Bool Transitions  deriving(Show,Eq)
data AutomateFiniND = AutomateFiniND (ARN Nom EtatND) [Nom] deriving(Show,Eq)


{-- on ajouter l'egalité dans la condition de la fonction insererBis
 cle > cl = balance g cl ct (insererBis d cle cont) ====> cle >= cl = balance g cl ct (insererBis d cle cont)

pour permettre d'ajouter le meme cle mais cont/= --}

-- il faut creer  une méthode insererND  


insererND :: Eq cont => Ord cle =>ARN cle cont -> cle -> cont -> ARN cle cont
insererND Feuille cle cont = Noeud Noir cle cont Feuille Feuille 
insereNDr a cle cont = insererBis a cle cont

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
------6. (difficile3) écrire une fonction de détermination determinise :: AutomateFiniND -> AutomateFini+--------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

--determinise :: AutomateFiniND -> AutomateFini








