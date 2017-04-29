(** Type des expressions. *)
type t =
  | Var of string
  | Int of int
  | String of string
  | Let of string * t * t
  | App of string * t list
  | Case of t * (t * t) list
  | Any

(** Utilitaire pour afficher des listes. *)
let pp_list pp_item pp_sep chan l =
  let rec aux = function
    | [] -> ()
    | [e] -> pp_item chan e
    | hd::tl -> pp_item chan hd ; pp_sep chan ; aux tl
  in aux l

let pp_list2 pp_item pp_sep chan l =
  let pp_pair chan (e,e') =
    Format.fprintf chan "(%a,%a)" pp_item e pp_item e'
  in
  Format.fprintf chan "[%a]" (pp_list pp_pair pp_sep) l


(** Afficheur d'expressions. *)
let rec pp_expr chan = function
  | Var v -> Format.fprintf chan "Var %s" v
  | Int i -> Format.fprintf chan "Int %d" i
  | String s -> Format.fprintf chan "String %s" s
  | Any -> Format.fprintf chan "Any"
  | Let (x,v,b) ->
      Format.fprintf chan
        "@[<2>let %s = @,%a@, in @,%a@]" 
        x
        pp_expr v
        pp_expr b
  | App (f,args) ->
      Format.fprintf chan
        "%s(%a)"
        f
        (pp_list pp_expr (fun chan -> Format.fprintf chan ",@,")) args
  | Case(e,m) ->
      Format.fprintf chan
        "%s(%a,%a)"
        "case"
        pp_expr e
        (pp_list2 pp_expr (fun chan -> Format.fprintf chan ",@,")) m
