(*d'abord definissons le type automate*)

type automate = {
	etat_initial : int;
	ensemble_des_etats : int list;
	alphabets : char list;
	transitions :(int*char*int) list;
	etats_finaux : int list
};;

(*prenons 4 variables a1,a2,a3 et a4 du type automate qu'on a definit precedemment comme exemples*)

let a1={
	etat_initial=1;
	ensemble_des_etats=[1;2;3];
	alphabets=['a';'b'];
	transitions=[(1,'a',1);(1,'b',2);(2,'b',2);(2,'a',3);(3,'b',3);(3,'a',3)];
	etats_finaux=[3]
};;
let a2={
	etat_initial=1;
	ensemble_des_etats=[1;2;3];
	alphabets=['a';'b'];
	transitions=[(1,'b',2);(2,'b',2);(2,'a',3);(3,'b',3);(3,'a',3)];
	etats_finaux=[3]
};;
let a3={
	etat_initial=1;
	ensemble_des_etats=[1;2;3];
	alphabets=['a';'b'];
	transitions=[(1,'a',1);(1,'b',2);(2,'b',2);(2,'a',4);(2,'b',3);(3,'b',3);(3,'a',3);(4,'a',4);(4,'b',4)];
	etats_finaux=[3]
};;
let a4={
	etat_initial=1;
	ensemble_des_etats=[1;2;3];
	alphabets=['a';'b'];
	transitions=[(1,'a',1);(1,'b',2);(2,'b',2);(2,'b',3);(3,'b',3);(3,'a',3)];
	etats_finaux=[3]
};;
(*pour tester si un automate est complet ou non on a besoin de trois fonctions*)
(*la premiere donne l'image d'un couple (etat,caractere) par une transition*)

let rec image e a t= 
match t with 
[]->[]
|x::l-> let (i,j,k)=x in if (i=e && j=a) then k::(image e a l) 
else image e a l ;;

(*la deuxieme donne une liste qui contient tout les couples d'un etat (etat,caractere)
 qui n'ont pas d'image donc si image e la lt=[] cela veut dire que l'automate est n'est pas complet dans cet etat*)

let rec nodef e la lt=
match la with
| [] -> []
| a::la'-> if image e a lt = [] then (e,a)::nodef e la' lt else nodef e la' lt;;

(*la troisieme applique nodef sur tout les element d'une liste*)

let rec est_complet le la lt =
match le with
| [] -> []
| e::le'-> nodef e la lt :: est_complet le' la lt;;

  (*ici c'est l'application directe de toutes les fonctions precedentes sur un automate*)

let complet auto=
if est_complet auto.ensemble_des_etats auto.alphabets auto.transitions = [[]; []; []]
then true else false ;;

(*la partie qui teste est ce qu'un automate est complet termine ici*)
(*on s'interesse maintenant a faire une fonction qui teste si un automate est deterministe*)
(*d'abord on realie la fonction membre qui retourne true si un element donné en parametre est membre d'une liste false sinon*)

let rec membre a l =
	match l with
	| [] -> false
	| x::rl -> x=a || membre a rl;;

(*ensuite on definie deux fonctions dans une*) 

let est_det a=

(*la premiere pour voir si une liste contient des doublons*)

let rec appartient l = match l with 
[] -> false
| x::r -> (membre x r) || (appartient r);

in

(*la deuxieme pour rendre une liste de paires a partir d'une qui contenait des triplets*)

let rec mettre_en_couple l =
match l with
|[]->[]
|(a,b,c)::l1->(a,b)::mettre_en_couple l1;

in

(*et enfin on applique la premiere fonction sur la liste rendu par la deuxieme*)

let l = mettre_en_couple a
in
appartient l;;

let deterministe auto=
if est_det auto.transitions=false then true else false;;

(*ici c'est l'application directe de toutes les fonctions precedentes sur un automate*)

(*pour representer un automate graphiquement en utilisant graphviz on necessite 4 fonctions*)

(*la premiere pour ecrire dans un fichier qui sera passé par la sortie standard stdout vers la fonction dessiner_automate,le fichier fmt
 contiendra la description suivant la condition appliqué a une transition de l'automate du graph en langage dot*)
let fmt_transition auto fmt (inedge,by,outedge)=
	if membre outedge auto.etats_finaux=true then
 Format.fprintf fmt "@[node [shape = doublecircle]%d;@]" outedge;
 if inedge=auto.etat_initial then
 Format.fprintf fmt "@[node [shape = point]start;node [shape = circle];start -> %d ;@]"inedge ;
 Format.fprintf fmt "@[%d -> %d [label=\"%c\"];@]" inedge outedge by;;
(*la deuxieme fonction levera la premiere sur toute les transitions de l'automate *)
let fmt_transitions fmt auto=
 Format.fprintf fmt "@[<v 2>digraph output {@,%a@,@]}@,@."
  (Format.pp_print_list (fmt_transition auto)) auto.transitions
;;
(*la troisieme va lancer la commande d'affichage de l'image genere par le fichier dot passe en parametre dans 
un environement linux depuis ocaml *)
let dessiner_automate auto =
  let cmd = "dot -Tpng | display -" in
  let (sout, sin, serr) as channels =
    Unix.open_process_full cmd (Unix.environment ()) in
  let fmt = Format.formatter_of_out_channel sin in
  Format.fprintf fmt "%a@." fmt_transitions auto;
  channels 

let cleanup channels =
  Unix.close_process_full channels;;
(*la fonction affichage affiche le resultat des fonctions deterministe et complet sur l'automate qu'on va prendre de l'utilisateur*)
let affichage auto =
	if deterministe auto = true && complet auto = true
	then print_string"cet automate est complet et deterministe\n"
else if deterministe auto = true && complet auto = false
then print_string"cet automate est deterministe mais pas complet\n"
else if deterministe auto = false && complet auto = true
then print_string"cet automate n'est pas deterministe mais complet\n"
else if deterministe auto = false && complet auto = false
then print_string"cet automate n'est pas complet ni deterministe\n"
;;
(*la fonction suivante c'est la fonction qui va prendre notre automate de l'entree de l'utilisateur*)
let lire_automate=

let ()=print_string"donnez un etat initial\n"
in
let ei=read_int()
in
let ()=print_string"donnez l'ensemble des etats\n"
in 
let ee=read_line() |> Str.split (Str.regexp " +") |> List.map int_of_string
in
let()=print_string"donnez un alphabet\n"
in
let al=read_line() |> Str.split (Str.regexp " +") |> List.map (fun x -> x.[0])
in
let tr =
 let () = print_string "les transitions \n" in
 let () = print_string "nombre des transitions: " in
 let n=read_int () in
 let rec g a=
 match a with
 0 -> []
 |_ -> let ()= print_string ("Transition no"^string_of_int a^": \n")
 in let ()=print_string "etat de depart: " 
 in let e= read_int()
 in
 let ()= print_string "caractére : "
 in
 let w=read_line()
 in 
 let ()= print_string "etat d'arriveé: "
 in
 let d=read_int () in 
 (e,w.[0],d)::(g (a-1));
 in 
 g n

in
let ()=print_string"donnez les etats finaux \n"
in
let ef = read_line() |> Str.split (Str.regexp " +") |> List.map int_of_string
in
let auto ={
etat_initial=ei;
ensemble_des_etats=ee;
alphabets=al;
transitions=tr;
etats_finaux=ef
}
in
affichage auto;
dessiner_automate auto
;;



 (* dessiner_automate a1 ;;
  dessiner_automate a2 ;;
  dessiner_automate a3 ;;
  dessiner_automate a4 ;;
print_newline() *)
