(* -------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2018 ---------------------- *)
(* -------------------------------------------------------------------------- *)
(* Matricule étudiant: .........                                              *)
(* Matricule étudiant: .........                                              *)
(* Matricule étudiant: .........                                              *)
(* -------------------------------------------------------------------------- *)
(* -- PRINCIPALE FICHIER DU TP: FONCTIONS À IMPLANTER ----------------------- *)
(* -------------------------------------------------------------------------- *)

(* Enonces des problemes
   Ils sont representes par un couple contenant:
     - une liste de propositions (les Hi)
     - une proposition (le C)
   Chaque proposition est representee a l'aide du type proposition *)

(* Formes clausales
   Une forme clausale est une liste de clauses;
   implicitement, il y a un et logique entre les clauses.
   Une clause est une liste de litteraux;
   implicitement, il y a un ou logique entre les litteraux.
   Un litteral est une proposition de la forme `Var id' ou `Non (Var id)' *)

#use "parseur.ml";;
#use "resolution.mli";;

(******************************************************************************)
(* Implantation                                                               *)
(******************************************************************************)
module Resolution = (* : RESOLUTION = *)
struct
  include TypesUtiles
  open List
  
  (* @Fonction      : union : a' list -> a' list -> a' list                   *)
  (* @Description   : à partir de deux listes, crée une liste contenant les éléments des deux listes.                 *)
  (* @Precondition  : aucune                                                  *)
  (* @Postcondition :  la liste résultant est sans doublon.                   *)
  let union l1 l2 = 
    fold_left (fun l -> fun x -> if mem x l then l else l@[x]) [] (l1@l2)

  (* @Fonction      : prod : a' list list -> a' list list -> a' list list                   *)
  (* @Description   : combine par union chaque sous-liste d'une liste avec ceux d'une autre *)
  (* @Precondition  : aucune                                                                *)
  (* @Postcondition : aucune                                                                *)
  let prod l1 l2 =
    let f l s1 = fold_left (fun l -> fun s2 -> union l [union s1 s2]) l l2 in
    fold_left f [] l1

  (* @Fonction      : paires : a' list -> (a' * a') * (a' list) list                        *)
  (* @Description   : combine chaque paire d'éléments de liste avec la liste privée des éléments de la paire   *)
  (* @Precondition  : aucune                                                                *)
  (* @Postcondition : aucune                                                                *)
  let paires lst = 
    let exc l i = fun j -> filter (fun x -> x <> i && x <> j) l in
    let rec f r = if r == [] then [] else
      let i = hd r and queue_lst = tl r and lst_sans = exc lst (hd r) in
      union (fold_left (fun l -> fun x -> l @ [((i, x), lst_sans x)]) [] queue_lst) (f queue_lst) in
    f lst  

  (* @Fonction      : enonce2proposition : a' list * a' -> proposition          *)
  (* @Description   : traduit un énoncé de problème en une proposition unique   *)
  (* @Precondition  : aucune                                                                *)
  (* @Postcondition : aucune                                                                *)
  let enonce2proposition enonce =
  fold_right (fun e -> fun p -> Et(e, p)) (fst enonce) (Non(snd enonce))

  (* @Fonction      : mfc : proposition -> proposition list list          *)
  (* @Description   : déconstruit une proposition en une liste de clauses   *)
  (* @Precondition  : aucune                                                                *)
  (* @Postcondition : pas de clauses doublons. *)
  let rec mfc prop = match prop with
  | Vrai -> []
  | Faux -> [[]]
  | Var(x) -> [[Var(x)];]
  | Et(p, p') -> union (mfc p) (mfc p')
  | Ou(p, p') -> prod (mfc p) (mfc p')
  | Imp(p, p') -> mfc (Ou(Non(p), p'))
  | Equ(p, p') -> mfc (Et(Imp(p, p'), Imp(p', p)))
  | Non(Vrai) -> mfc Faux
  | Non(Faux) -> mfc Vrai
  | Non(Var(x)) -> [[Non(Var(x))];]
  | Non(Et(p, p')) -> mfc (Ou(Non(p), Non(p')))
  | Non(Ou(p, p')) -> mfc (Et(Non(p), Non(p')))
  | Non(Imp(p, p')) -> mfc (Non(Ou(Non(p), p')))
  | Non(Equ(p, p')) -> mfc (Equ(Non(p), p'))
  | Non(Non(p)) -> mfc p

  (* @Fonction      : opp : proposition * proposition -> bool          *)
  (* @Description   : Vérifie si deux propositons sont les négations l'une de l'autre   *)
  (* @Precondition  : les propositions sont soit d'un type proposition autre que "Non(_)" ou alors d'un type "Non(_)" simple. *)
  (* @Postcondition : aucune *)
  let opp p p' = (p = Non(p')) || (p' = Non(p))

  let resolutions c c' =
  fold_left (fun l -> fun ((x, y), r) -> if opp x y then l@[r] else l) [] (paires(union c c'))

  (* @Fonction      : dev_once : forme_clause -> forme_clause          *)
  (* @Description   : développe toutes les résolutions possibles entre les clauses d'une forme clausale   *)
  (* @Precondition  : aucune                                                                *)
  (* @Postcondition : aucune *)
  let dev_once fc =
    let elim c = fold_left (fun v -> fun ((p, p'), _) -> not (opp p p') && v) true (paires c) in
    let simp fc = filter elim fc in
  fold_left (fun l -> fun ((c, c'), _) -> resolutions c c' @ l) [] (paires(simp fc))

  let decision prop =
    let rec dev fc = 
    let fc' = dev_once fc in
    if mem [] fc || mem [] fc' then true else if union fc' fc = fc then false else
    dev (union fc' fc) in
  dev (mfc prop)

  let decisionTrace prop = 
    let rec trace fc lst =
    let fc' = dev_once fc in
    let step = union fc' fc in
    if mem [] step then Some (lst @ [[fc] @ [fc']]) else
    if step = fc then None else
    trace step (lst @ [fc])  in
  trace (mfc prop) []

end;;

(* Exploitation des fonctions à implanter (fichier resolution.ml):

# #use "resolution.ml";;
module TypesUtiles :
  sig
    type jeton =
        LexParG
      | LexParD
      | LexVrai
      | LexFaux
      | LexNon
      | LexOu
      | LexEt
      | LexImp
      | LexEqu
      | LexVar of string
      | LexSep
      | LexFDF
    type proposition =
        Vrai
      | Faux
      | Non of proposition
      | Et of proposition * proposition
      | Ou of proposition * proposition
      | Imp of proposition * proposition
      | Equ of proposition * proposition
      | Var of string
    type enonce_probleme = proposition list * proposition
    type clause = proposition list
    type forme_clausale = clause list
  end
module Parseur :
  sig
    type jeton = TypesUtiles.jeton
    type proposition = TypesUtiles.proposition
    type enonce_probleme = TypesUtiles.enonce_probleme
    exception ErrAnalyseLexicale
    exception ErrAnalyseSyntaxique
    val aLex : string -> jeton list
    val aSynt : jeton list -> enonce_probleme
  end
module type RESOLUTION =
  sig
    type proposition = TypesUtiles.proposition
    type enonce_probleme = TypesUtiles.enonce_probleme
    type clause = TypesUtiles.clause
    type forme_clausale = TypesUtiles.forme_clausale
    val union : 'a list -> 'a list -> 'a list
    val prod : 'a list list -> 'a list list -> 'a list list
    val paires : 'a list -> (('a * 'a) * 'a list) list
    val enonce2proposition : enonce_probleme -> proposition
    val mfc : proposition -> forme_clausale
    val resolutions : clause -> clause -> clause list
    val decision : proposition -> bool
    val decisionTrace : proposition -> forme_clausale list option
  end
module Resolution : RESOLUTION

# open TypesUtiles;;
# open Parseur;;
# open Resolution;;

# union [1;2;3;2;4;1] [2;1;5;3;6];;
- : int list = [1; 2; 3; 4; 5; 6]
# union ["a"] ["b"; "a"];;
- : string list = ["a"; "b"]
# union [Non (Var "a"); Var "b"] [Non (Var "b"); Var "a"; Var "b"];;
- : TypesUtiles.proposition list =
[Non (Var "a"); Var "b"; Non (Var "b"); Var "a"]

# prod [[1;2];[3]] [[4];[5;2;3];[1]];;
- : int list list =
[[1; 2; 4]; [1; 2; 5; 3]; [1; 2]; [3; 4]; [3; 5; 2]; [3; 1]]
# prod [[1;2;1]] [[3];[]];;
- : int list list = [[1; 2; 3]; [1; 2]]
# prod [[1;2]] [];;
- : int list list = []
# prod [] [[4;5];[6]];;
- : int list list = []
# prod [[Non (Var "a"); Var "b"]; [Var "a"]] [[Var "b"]; [Var "a"]];;
- : TypesUtiles.proposition list list =
[[Non (Var "a"); Var "b"]; [Non (Var "a"); Var "b"; Var "a"];
 [Var "a"; Var "b"]; [Var "a"]]

# paires [1;2;3;4];;
- : ((int * int) * int list) list =
[((1, 2), [3; 4]); ((1, 3), [2; 4]); ((1, 4), [2; 3]); ((2, 3), [1; 4]);
 ((2, 4), [1; 3]); ((3, 4), [1; 2])]
# paires ["a";"b";"c"];;
- : ((string * string) * string list) list =
[(("a", "b"), ["c"]); (("a", "c"), ["b"]); (("b", "c"), ["a"])]
# paires ["a";"b"];;
- : ((string * string) * string list) list = [(("a", "b"), [])]
# paires ["a"];;
- : ((string * string) * string list) list = []
# paires [];;
- : (('a * 'a) * 'a list) list = []
# paires [[Non (Var "a"); Var "b"]; [Non (Var "b")]; [Var "a"]];;
- : ((TypesUtiles.proposition list * TypesUtiles.proposition list) *
     TypesUtiles.proposition list list)
    list
=
[(([Non (Var "a"); Var "b"], [Non (Var "b")]), [[Var "a"]]);
 (([Non (Var "a"); Var "b"], [Var "a"]), [[Non (Var "b")]]);
 (([Non (Var "b")], [Var "a"]), [[Non (Var "a"); Var "b"]])]

# enonce2proposition ([Imp (Var "a", Var "b"); Non (Var "b")], Non (Var "a"));;
- : Resolution.proposition =
Et (Imp (Var "a", Var "b"), Et (Non (Var "b"), Non (Non (Var "a"))))
# enonce2proposition ([Et (Var "a", Var "b")], Var "a");;
- : Resolution.proposition = Et (Et (Var "a", Var "b"), Non (Var "a"))
# enonce2proposition ([Ou (Var "a", Var "b")], Et (Var "a", Non (Var "b")));;
- : Resolution.proposition =
Et (Ou (Var "a", Var "b"), Non (Et (Var "a", Non (Var "b"))))
# enonce2proposition ([Var "a"], Var "a");;
- : Resolution.proposition = Et (Var "a", Non (Var "a"))

# mfc Vrai;;
- : Resolution.forme_clausale = []
# mfc Faux;;
- : Resolution.forme_clausale = [[]]
# mfc (Var "a");;
- : Resolution.forme_clausale = [[Var "a"]]
# mfc (Non Vrai);;
- : Resolution.forme_clausale = [[]]
# mfc (Non Faux);;
- : Resolution.forme_clausale = []
# mfc (Non (Var "a"));;
- : Resolution.forme_clausale = [[Non (Var "a")]]
# mfc (Non (Non (Var "a")));;
- : Resolution.forme_clausale = [[Var "a"]]
# mfc (Et (Ou (Var "a", Var "b"), Non (Et (Var "a", Non (Var "b")))));;
- : Resolution.forme_clausale =
[[Var "a"; Var "b"]; [Non (Var "a"); Var "b"]]

# let c1 = [Var "p"; Non(Var "q"); Var "r"; Non(Var "s")];;
val c1 : TypesUtiles.proposition list =
  [Var "p"; Non (Var "q"); Var "r"; Non (Var "s")]
# let c2 = [Var "r"; Non(Var "p"); Var "q"; Var "t"];;
val c2 : TypesUtiles.proposition list =
  [Var "r"; Non (Var "p"); Var "q"; Var "t"]
# resolutions c1 c2;;
- : Resolution.clause list =
[[Non (Var "q"); Var "r"; Non (Var "s"); Var "q"; Var "t"];
 [Var "p"; Var "r"; Non (Var "s"); Non (Var "p"); Var "t"]]
# resolutions [Non (Var "a"); Var "b"] [Non (Var "b")];;
- : Resolution.clause list = [[Non (Var "a")]]
# resolutions [Non (Var "a"); Var "b"] [Var "a"];;
- : Resolution.clause list = [[Var "b"]]
# resolutions [Non (Var "b")] [Var "a"];;
- : Resolution.clause list = []
# resolutions [Non (Var "a"); Var "b"] [Non (Var "b"); Var "a"];;
- : Resolution.clause list =
[[Var "b"; Non (Var "b")]; [Non (Var "a"); Var "a"]]
# resolutions [Var "b"] [Non (Var "b")];;
- : Resolution.clause list = [[]]

# decision (enonce2proposition (aSynt (aLex "a => b; non b; non a")));;
- : bool = true
# decision (enonce2proposition (aSynt (aLex "a => b; non b; non b")));;
- : bool = true
# decision (enonce2proposition (aSynt (aLex "a => b; non b; a")));;
- : bool = false
# decision (enonce2proposition (aSynt (aLex "p <=> q; q <=> r; p <=> r")));;
- : bool = true
# decision (enonce2proposition (aSynt (aLex "a => b; a; b")));;
- : bool = true
# decision (Et (Imp (Var "a", Var "b"), Et (Var "a", Non (Var "b"))));;
- : bool = true

# decisionTrace (enonce2proposition (aSynt (aLex "a => b; non b; non a")));;
- : Resolution.forme_clausale list option =
Some
 [[[Non (Var "a"); Var "b"]; [Non (Var "b")]; [Var "a"]];
  [[Non (Var "a")]; [Var "a"]]; [[]]]
# decisionTrace (enonce2proposition (aSynt (aLex "a => b; non b; a")));;
- : Resolution.forme_clausale list option = None
# decisionTrace (enonce2proposition (aSynt (aLex "p <=> q; q <=> r; p <=> r")));;
- : Resolution.forme_clausale list option =
Some
 [[[Non (Var "p"); Var "q"]; [Non (Var "q"); Var "p"];
   [Non (Var "q"); Var "r"]; [Non (Var "r"); Var "q"]; [Var "p"; Var "r"];
   [Non (Var "r"); Non (Var "p")]];
  [[Non (Var "p"); Var "r"]; [Non (Var "q"); Var "p"];
   [Non (Var "r"); Var "q"]; [Var "p"; Var "r"];
   [Non (Var "r"); Non (Var "p")]];
  [[Var "r"]; [Non (Var "q"); Var "p"]; [Non (Var "r"); Var "q"];
   [Non (Var "r"); Non (Var "p")]];
  [[Var "p"; Non (Var "r")]; [Var "r"]; [Non (Var "r"); Non (Var "p")]];
  [[Non (Var "r")]; [Var "r"]]; [[]]]
# decisionTrace (enonce2proposition (aSynt (aLex "a => b; a; b")));;
- : Resolution.forme_clausale list option =
Some
 [[[Non (Var "a"); Var "b"]; [Var "a"]; [Non (Var "b")]];
  [[Var "b"]; [Non (Var "b")]]; [[]]]
*)