type ide = string;;

type exp = 
    | Eint of int 
    | Ebool of bool 
    | Den of ide 
    | Prod of exp * exp 
    | Sum of exp * exp 
    | Diff of exp * exp 
    | Eq of exp * exp 
    | Minus of exp 
    | IsZero of exp 
    | Or of exp * exp 
    | And of exp * exp 
    | Not of exp 
    | Ifthenelse of exp * exp * exp 
    | Let of ide * exp * exp 
    | Fun of ide * exp 
    | FunCall of exp * exp 
    | Letrec of ide * exp * exp
    | MyFunRec of ide * exp (* ASTRAZIONE DI UNA FUNZIONE RICORSIVA *)
    | Estring of string (* ESPRESSIONE DICHIARAZIONE STRINGA *)
    | Dict of (ide * exp) list (* ESPRESSIONE DICHIARAZIONE DIZIONARIO *)
    | Select of exp * ide (* ESPRESSIONE SELEZIONE VALORE DA CHIAVE *)
    | Add of exp * ide * exp (* ESPRESSIONE AGGIUNTA COPPIA (CHIAVE,VALORE) NEL DIZIONARIO *)
    | Rm of exp * ide (* ESPRESSIONE RIMOZIONE COPPIA (CHIAVE,VALORE) NEL DIZIONARIO *)
    | Clear of exp (* ESPRESSIONE PULIZIA DIZIONARIO *)
    | ApplyOver of exp * exp (* ESPRESSIONE APPLICAZIONE FUNZIONE AD OGNI VALORE DEL DIZIONARIO *)
    | Rt_eval of exp;; (* ESPRESSIONE VALUTAZIONE ESPRESSIONE CON SCOPING DINAMICO *)

(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = function x -> if x = i then v else applyenv r x;;

(*tipi esprimibili*)
type evT = 
    | Int of int 
    | Bool of bool 
    | Unbound 
    | FunVal of evFun 
    | FunValD of evFunD  (* TIPO ESPRIMIBILE PER FUNZIONE PER SCOPE DINAMICO *)
    | RecFunVal of ide * evFun
    | RecFunValD of ide * evFunD (* TIPO ESPRIMIBILE PER FUNZIONE RICORSIVA PER SCOPE DINAMICO *)
    | DictVal of (ide * evT) list (* AGGIUNTO TIPO ESPRIMIBILE DIZIONARIO *)
    | String of string (* AGGIUNTO TIPO ESPRIMINILE STRINGA *)
and evFun = ide * exp * evT env
and evFunD = ide * exp;;

(*type checking*)
let typecheck (s : string) (v : evT) : bool = match s with
  | "int" -> (match v with
               |Int(_) -> true
               |_ -> false
             ) 
  | "bool" -> (match v with
                |Bool(_) -> true 
                |_ -> false
              ) 
  (* TIPE CHECKING PER LE STRINGHE *)
  | "string" -> (match v with
                  | String(_) -> true
                  | _ -> false )
  | _ -> failwith("not a valid type");;

(*funzioni primitive*)
let prod x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
         |(Int(n),Int(u)) -> Int(n*u)
         | _ -> failwith("WrongTypeChecker")
       )
  else failwith("Type error");;

let sum x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
         | (Int(n),Int(u)) -> Int(n+u)
         | _ -> failwith("WrongTypeChecker")
       )
  else failwith("Type error");;

let diff x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
         | (Int(n),Int(u)) -> Int(n-u)
         | _ -> failwith("WrongTypeChecker")
       )
  else failwith("Type error");;

let eq x y = if (typecheck "int" x) && (typecheck "int" y)
  then (match (x,y) with
         | (Int(n),Int(u)) -> Bool(n=u)
         | _ -> failwith("WrongTypeChecker")
       )
  else failwith("Type error");;

let minus x = if (typecheck "int" x) 
  then (match x with
         | Int(n) -> Int(-n)
         | _ -> failwith("WrongTypeChecker")
       )
  else failwith("Type error");;

let iszero x = if (typecheck "int" x)
  then (match x with
         | Int(n) -> Bool(n=0)
         | _ -> failwith("WrongTypeChecker")
       )
  else failwith("Type error");;

let vel x y = if (typecheck "bool" x) && (typecheck "bool" y)
  then (match (x,y) with
         | (Bool(b),Bool(e)) -> (Bool(b||e))
         | _ -> failwith("WrongTypeChecker")
       )
  else failwith("Type error");;

let et x y = if (typecheck "bool" x) && (typecheck "bool" y)
  then (match (x,y) with
         | (Bool(b),Bool(e)) -> Bool(b&&e)
         | _ -> failwith("WrongTypeChecker")
       )
  else failwith("Type error");;

let non x = if (typecheck "bool" x)
  then (match x with
         | Bool(true) -> Bool(false) 
         | Bool(false) -> Bool(true)
         | _ -> failwith("WrongTypeChecker")
       )
  else failwith("Type error");;


(*interprete*)
let rec eval (e : exp) (r : evT env) : evT = match e with
    Eint n -> Int n 
  | Estring s -> String s 
  | Ebool b -> Bool b 
  | IsZero a -> iszero (eval a r) 
  | Den i -> applyenv r i 
  | Eq(a, b) -> eq (eval a r) (eval b r) 
  | Prod(a, b) -> prod (eval a r) (eval b r) 
  | Sum(a, b) -> sum (eval a r) (eval b r) 
  | Diff(a, b) -> diff (eval a r) (eval b r) 
  | Minus a -> minus (eval a r) 
  | And(a, b) -> et (eval a r) (eval b r) 
  | Or(a, b) -> vel (eval a r) (eval b r) 
  | Not a -> non (eval a r) 
  | Ifthenelse(a, b, c) -> let g = (eval a r) in 
        if (typecheck "bool" g) then 
          (if g = Bool(true) then (eval b r) 
           else (eval c r)) 
        else failwith ("nonboolean guard") 
  | Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) 
  | Fun(i, a) -> FunVal(i, a, r) 
  | FunCall(f, eArg) -> let fClosure = (eval f r) in
        (match fClosure with
            FunVal(arg, fBody, fDecEnv) -> eval fBody (bind fDecEnv arg (eval eArg r)) 
          | RecFunVal(g, (arg, fBody, fDecEnv)) -> let aVal = (eval eArg r) in
              let rEnv = (bind fDecEnv g fClosure) in
              let aEnv = (bind rEnv arg aVal) in eval fBody aEnv
          | _ -> failwith("non functional value")) 
  | Letrec(f, funDef, letBody) -> (match funDef with
                                      Fun(i, fBody) -> let r1 = (bind r f (RecFunVal(f, (i, fBody, r)))) in eval letBody r1 
                                    | _ -> failwith("non functional def")) 

  (* COSTRUTTI AGGIUNTI AL LINGUAGGIO DIDATTICO FUNZIONALE *)

  (* VALUTAZIONE DI UNA DICHIARAZIONE DI UNA FUNZIONE RICORSIVA *)
  | MyFunRec(f, funDef) -> (match funDef with 
                               Fun(i, fBody) -> RecFunVal(f, (i, fBody, r)) 
                             | _ -> failwith("non functional def") )

  (* VALUTAZIONE DI UN DIZIONARIO *)
  | Dict(lst) -> DictVal(evalList lst r)

  (* DATO UN DIZIONARIO E UNA CHIAVE RESTITUISCE IL VALORE ASSOCIATO A QUESTA *)
  | Select(dict, key) -> (match (eval dict r) with 
                             DictVal(pairs) -> lookup key pairs
                           | _ -> failwith("non dictionary def") ) 

  (* DATO UN DIZIONARIO, RESTITUISCE UN NUOVO DIZIONARIO AGGIUNGENDO UNA COPPIA (CHIAVE,VALORE) *)
  | Add(dict, key, e) -> (let v = (eval e r) in 
                            match (eval dict r) with 
                                DictVal(pairs) -> DictVal(add pairs key v) 
                              | _ -> failwith("non dictionary def")) 

  (* DATO UN DIZIONARIO E UNA CHIAVE, RESTITUISCE UN NUOVO DIZIONARIO SENZA LA COPPIA ASSOCIATA ALLA CHIAVE *)
  | Rm(dict, key) -> (match (eval dict r) with 
                         DictVal(pairs) -> DictVal(rm pairs key)
                       | _ -> failwith("non dictionary def")) 

  (* DATO UN DIZIONARIO, RESTITUISCE UN DIZIONARIO VUOTO *)
  | Clear(dict) -> ( match (eval dict r) with
                       DictVal(pairs) -> DictVal([])
                     | _ -> failwith("non dictionary def"))

  (* APPLICA FUNZIONE f AD OGNI VALORE DELLE COPPIE DEL DIZIONARIO dict*)
  | ApplyOver(f, dict) -> (let fClosure = (eval f r) in 
                             match (eval dict r) with 
                                 DictVal(pairs) -> DictVal(ap_over fClosure pairs)
                               | _ -> failwith("non dictionary def"))

  (* VALUTAZIONE DI UNA ESPRESSIONE CON SCOPE DINAMICO *)
  | Rt_eval(e) -> evalD e r

(* OGNI VALORE DEL DIZIONARIO VIENE VALUTATO: DA exp A evT *)
and evalList (lst: (ide * exp) list) (r: evT env) : (ide * evT) list = match lst with 
    [] -> []
  | (i,e)::rest -> (let v = (eval e r) in (i, v)::(evalList rest r) )

(* RESTITUISCE IL VALORE DI UNA CHIAVE key, Unbound SE NON LA TROVO *)
and lookup (key : ide) (p : (ide * evT) list) : evT = match p with
    (k,v)::rest -> if (key = k) then v else (lookup key rest)
  | [] -> Unbound

(* RESTITUISCE UNA NUOVA LISTA (ide * evT) CON AGGIUNTA LA COPPIA (key, vl), SE GIA' PRESENTE -> AGGIORNA IL VALORE *)
and add (d : (ide * evT) list) (key :ide) (vl :evT) : (ide * evT) list = match d with
    [] -> [(key, vl)]
  | (k,v)::rest -> if (key = k) then (k, vl)::rest else (k, v)::(add rest key vl) 

(* RESTITUISCE UNA NUOVA LISTA (ide * evT) CON RIMOSSA LA COPPIA (key, v), SE NON PRESENTE -> ECCEZIONE *)
and rm (d : (ide * evT) list) (key :ide) : (ide * evT) list = match d with
    [] -> failwith("nessun valore trovato")
  | (k, v)::rest -> if (key = k) then rest else (k, v)::(rm rest key)

(* APPLICA LA FUNZIONE DICHIARATA NELLA CHIUSURA cls AD OGNI VALORE DEL DIZIONARIO, SE C'E' UN ERRORE DI TIPO -> ECCEZIONE *)
and ap_over (cls: evT) (d: (ide * evT) list) : (ide * evT) list =
  ( match (cls, d) with 
      (_ , []) -> []
    | (FunVal(arg, fBody, fDecEnv), ((k, v)::rest) ) -> (k, (eval fBody (bind fDecEnv arg v)))::(ap_over cls rest)
    | (RecFunVal(g, (arg, fBody, fDecEnv)), ((k, v)::rest))  -> (let env1 = bind fDecEnv g cls in 
                                                                 let env2 = bind env1 arg v in 
                                                                   (k, (eval fBody env2))::(ap_over cls rest)) 
    | _ -> failwith("non functional def"))

(* APPLICA LA FUNZIONE DICHIARATA NELLA CHIUSURA cls AD OGNI VALORE DEL DIZIONARIO, SE C'E' UN ERRORE DI TIPO -> ECCEZIONE *)
(* OTTIMIZZATA AFFINCHE' FUNZIONI CON SCOPING DINAMICO *)
and ap_overD (cls: evT) (d: (ide * evT) list) (r: evT env) : (ide * evT) list =
  ( match (cls, d) with 
      (_ , []) -> []
    | (FunValD(arg, fBody), ((k, v)::rest) ) -> (k, (evalD fBody (bind r arg v)))::(ap_overD cls rest r)
    | (RecFunValD(g, (arg, fBody)), ((k, v)::rest))  -> (let env1 = bind r g cls in 
                                                         let env2 = bind env1 arg v in 
                                                           (k, (evalD fBody env2))::(ap_overD cls rest r)) 
    | _ -> failwith("non functional def"))

(* INTERPRETE CON SCOPING DINAMICO *)
and evalD (e : exp) (r : evT env) : evT = match e with
    Eint n -> Int n 
  | Estring s -> String s 
  | Ebool b -> Bool b 
  | IsZero a -> iszero (evalD a r) 
  | Den i -> applyenv r i 
  | Eq(a, b) -> eq (evalD a r) (evalD b r) 
  | Prod(a, b) -> prod (evalD a r) (evalD b r) 
  | Sum(a, b) -> sum (evalD a r) (evalD b r) 
  | Diff(a, b) -> diff (evalD a r) (evalD b r) 
  | Minus a -> minus (evalD a r) 
  | And(a, b) -> et (evalD a r) (evalD b r) 
  | Or(a, b) -> vel (evalD a r) (evalD b r) 
  | Not a -> non (evalD a r) 
  | Ifthenelse(a, b, c) -> let g = (evalD a r) in 
        if (typecheck "bool" g) then 
          (if g = Bool(true) then (evalD b r) 
           else (evalD c r)) 
        else failwith ("nonboolean guard") 
  | Let(i, e1, e2) -> evalD e2 (bind r i (evalD e1 r)) 
  | Fun(i, a) -> FunValD(i, a) 
  | FunCall(f, eArg) -> let fClosure = (evalD f r) in
        (match fClosure with
            FunValD(arg, fBody) -> evalD fBody (bind r arg (evalD eArg r)) 
          | RecFunValD(g, (arg, fBody)) -> let aVal = (evalD eArg r) in
              let rEnv = (bind r g fClosure) in
              let aEnv = (bind rEnv arg aVal) in evalD fBody aEnv
          | _ -> failwith("non functional value")) 
  | Letrec(f, funDef, letBody) -> (match funDef with
                                      Fun(i, fBody) -> let r1 = (bind r f (RecFunValD(f, (i, fBody)))) in evalD letBody r1 
                                    | _ -> failwith("non functional def")) 
  | MyFunRec(f, funDef) -> (match funDef with 
                               Fun(i, fBody) -> RecFunValD(f, (i, fBody)) 
                             | _ -> failwith("non functional def") )
  | Dict(lst) -> DictVal(evalList lst r)
  | Select(dict, key) -> (match (evalD dict r) with 
                             DictVal(pairs) -> lookup key pairs
                           | _ -> failwith("non dictionary def") ) 
  | Add(dict, key, e) -> (let v = (evalD e r) in 
                            match (evalD dict r) with 
                                DictVal(pairs) -> DictVal(add pairs key v) 
                              | _ -> failwith("non dictionary def")) 
  | Rm(dict, key) -> (match (evalD dict r) with 
                         DictVal(pairs) -> DictVal(rm pairs key)
                       | _ -> failwith("non dictionary def")) 
  | Clear(dict) -> ( match (evalD dict r) with
                       DictVal(pairs) -> DictVal([])
                     | _ -> failwith("non dictionary def"))
  | ApplyOver(f, dict) -> (let fClosure = (evalD f r) in 
                             match (evalD dict r) with 
                                 DictVal(pairs) -> DictVal(ap_overD fClosure pairs r)
                               | _ -> failwith("non dictionary def"))
  | Rt_eval(e) -> evalD e r ;;


(* ======================================== TESTS ======================================== *)

let env0 = emptyenv Unbound;;




(* ================== Creazione di un dizionario vuoto ================== *)
let e1 = Dict([]);;
eval e1 env0;;





(* =============== Creazione di un dizionario inizializzato =============== *)
let e2 = Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]);;
eval e2 env0;;





(* ============== Creazione di un dizionario con un valore da valutare ============== *)
let e3 = Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Sum(Eint 123400, Eint 56)) ]);;
eval e3 env0;;





(* ================ Selezione di un elemento presente in un dizionario =============== *)
let e4 = Let( 
           "myDict" , 
           Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]) ,
           Select(Den "myDict", "nome")
         );;
eval e4 env0;;





(* ================ Selezione di un elemento NON presente in un dizionario ================ *)
let e5 = Let( 
           "myDict" , 
           Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]) ,
           Select(Den "myDict", "cognome")
         );;
eval e5 env0;;





(* =========== Aggiunta di un elemento in un dizionario (operazione immutable) =========== *)

(* Dimostro che myDict1 rimane invariato *)
let e6 = Let( 
           "myDict1" , 
           Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]) ,
           Let (
             "myDict2",
             Add(Den "myDict1", "cognome", Estring "Rossi"),
             Den "myDict1"
           )
         );;
eval e6 env0;;

(* Dimostro che myDict2 = myDict1 + elemento *)
let e7 = Let( 
           "myDict1" , 
           Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]) ,
           Let (
             "myDict2",
             Add(Den "myDict1", "cognome", Estring "Rossi"),
             Den "myDict2"
           )
         );;
eval e7 env0;;

(* Se si aggiunge una coppia la cui chiave è già presente, restituisce un nuovo dizionario con il valore aggiornato *)
let e8 = Let( 
           "myDict1" , 
           Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]) ,
           Let (
             "myDict2",
             Add(Den "myDict1", "nome", Estring "Mario"),
             Den "myDict2"
           )
         );;
eval e8 env0;;






(* =========== Rimozione di un elemento in un dizionario (operazione immutable) =========== *)

(* Dimostro che myDict1 rimane invariato *)
let e9 = Let( 
           "myDict1" , 
           Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]) ,
           Let (
             "myDict2",
             Rm(Den "myDict1", "matricola"),
             Den "myDict1"
           )
         );;
eval e9 env0;;

(* Dimostro che myDict2 = myDict1 - elemento *)
let e10 = Let( 
            "myDict1" , 
            Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]) ,
            Let (
              "myDict2",
              Rm(Den "myDict1", "matricola"),
              Den "myDict2"
            )
          );;
eval e10 env0;;





(* =========== Clear di in un dizionario (operazione immutable) =========== *)

(* Dimostro che myDict1 rimane invariato *)
let e11 = Let( 
            "myDict1" , 
            Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]) ,
            Let (
              "myDict2",
              Clear(Den "myDict1"),
              Den "myDict1"
            )
          );;
eval e11 env0;;

(* Dimostro che myDict2 = myDict1 - elemento *)
let e12 = Let( 
            "myDict1" , 
            Dict([ ("nome", Estring "Giovanni") ; ("matricola" , Eint 123456) ]) ,
            Let (
              "myDict2",
              Clear(Den "myDict1"),
              Den "myDict2"
            )
          );;
eval e12 env0;;





(* ====================== ApplyOver in un dizionario ====================== *)

(* Apply di una funzione NON ricorsiva in un dizionario OMOGENEO *)
let e13 = Let(
            "myDict" , 
            Dict([ ("chiave1", Eint 1) ; ("chiave2" , Eint 2) ; ("chiave3" , Eint 3) ]) ,
            ApplyOver(
              Fun( "n" , Sum(Den "n", Eint 1) ),
              Den "myDict"
            )
          );;
eval e13 env0;;

(* Apply di una funzione ricorsiva in un dizionario OMOGENEO *)
let e14 = Let(
            "myDict" , 
            Dict([ ("chiave1", Eint 3) ; ("chiave2" , Eint 4) ; ("chiave3" , Eint 5) ]) ,
            ApplyOver(
              MyFunRec( "fact" , Fun("n", Ifthenelse( IsZero(Den "n") ,
                                                      Eint 1,
                                                      Prod(Den "n", FunCall(Den "fact",
                                                                            Diff(Den "n", Eint 1)))
                                                    )
                                    )
                      ),
              Den "myDict"
            )
          );;
eval e14 env0;;

(* Apply di una funzione NON ricorsiva in un dizionario NON omogeneo *)
(*
let e15 = Let(
"myDict" , 
Dict([ ("chiave1", Eint 1) ; ("chiave2" , Eint 2) ; ("chiave3" , Estring "parola") ]) ,
ApplyOver(
Fun( "n" , Sum(Den "n", Eint 1) ),
Den "myDict"
)
);;
eval e15 env0;;
*)




(* ====================== Rt_eval: valutazione di una exp con scoping dinamico ====================== *)

(* Funzione NON ricorsiva con scoping statico:

   let x = 4;;
   let foo n = x+n;;
   let x = 9;;
   foo 6;;


   Risutlato = 10

*)

let e16 = Let(
            "x",
            Eint 4, 
            Let (
              "foo",
              Fun("n", Sum(Den "x", Den "n")),
              Let (
                "x",
                Eint 9,
                FunCall(Den "foo", Eint 6)
              )
            )
          );;

eval e16 env0;;

(* Stessa funzione con scoping dinamico: Risultato = 15 *)
eval (Rt_eval(e16)) env0;;


(* Funzione ricorsiva con scoping statico

   let x = 1;;
   let prod n = if n = 0 then 0 else x + prod(n-1);;
   let x = 2;;
   prod 4;;


   Risultato = 4
*)

let e17 = Let(
            "x",
            Eint 1, 
            Letrec (
              "prod",
              Fun("n", Ifthenelse( IsZero(Den "n") , 
                                   Eint 0, 
                                   Sum(Den "x", FunCall(Den "prod", Diff( Den "n", Eint 1)) )
                                 )
                 ),
              Let (
                "x",
                Eint 2,
                FunCall(Den "prod", Eint 4)
              )
            )
          );;

eval e17 env0;;

(* Stessa funzione con scoping dinamico: Risultato = 8 *)
eval (Rt_eval(e17)) env0;;
