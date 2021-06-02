Require Import Coq.Strings.String.
Require Import Coq.Lists.List.
Import ListNotations.

Open Scope string_scope.
(* Abstract syntax tree for grammmar*)
Section Grammar.

Inductive symbol: Type :=
| T: string -> symbol
| NT: string -> symbol
| U: string -> symbol. (* U -> undecide T or NT*)

Inductive expr: Type :=
| base: (list symbol) -> expr
| combine: expr -> expr -> expr.

Definition rule: Type := symbol * expr.
Definition grammar: Type:= list rule.

End Grammar.

Section Reify.
(* test grammmar*)
Definition test: grammar := 
    [   ((U "E"), combine (base [(U "E"); (U "+"); (U "T")]) (base [(U "T")]));
        ((U "T"), combine (base [(U "T"); (U "*"); (U "F")]) (base [(U "F")]));
        ((U "F"), combine (base [(U "("); (U "E"); (U ")")]) (base [(U "i")]))
    ].

Fixpoint get_string_from_symbol (a: symbol): string :=
    match a with
    | T x => x
    | NT x => x
    | U x => x
    end.

Fixpoint symbol_dec (a b:symbol): bool :=
    match a with
    | T s1 => match b with
              | T s2 => if string_dec s1 s2 then true else false
              | _ => false
              end
    | NT s1 => match b with
               | NT s2 => if string_dec s1 s2 then true else false
               | _ => false
               end
    | U s1 => match b with
              | U s2 => if string_dec s1 s2 then true else false
              | _ => false
              end
    end.

Fixpoint In_symbol (t: symbol)(l: list symbol): bool :=
    match l with
    | nil => false
    | x::xs => if (symbol_dec t x) then true else In_symbol t xs
    end.

Fixpoint In_string (l: list string)(t: string): bool :=
    match l with
    | nil => false
    | x::xs => if (string_dec t x) then true else In_string xs t
    end.

Fixpoint find_string_in_expr (e: expr): list string :=
    match e with
    | base xs => map get_string_from_symbol xs
    | combine a b => (find_string_in_expr a) ++ (find_string_in_expr b)
    end. 

Fixpoint find_string_in_grammar (target: grammar): list string :=
    match target with
    | nil => nil
    | (_, e)::xs => let l:= find_string_in_expr e in l ++ (find_string_in_grammar xs)
    end.

Fixpoint find_NT (target: grammar): list string :=
    match target with
    | nil => nil
    | (x, _)::xs => let l := find_NT xs in
                        let s := get_string_from_symbol x in
                            if negb (In_string l s) then s::l else l
    end.

(* x not in find_NT iff. x is T *)
Definition find_T (target: grammar): list string := filter (fun x => negb (In_string (find_NT target) x)) (find_string_in_grammar target).

Compute (find_NT test). (* = ["E"; "T"; "F"] : list string *)
Compute (find_T test). (* = ["+"; "*"; "("; ")"; "i"] : list string *)

Definition reify_symbol (nt: list string)(s: symbol): symbol :=
    let str:= get_string_from_symbol s in
        if (In_string nt str) then NT str else T str.

Fixpoint reify_expr (nt: list string)(t: expr): expr:=
    match t with
    | base l => base (map (reify_symbol nt) l)
    | combine a b => combine (reify_expr nt a) (reify_expr nt b)
    end.

Fixpoint _reify (nt: list string)(target: grammar): grammar:=
    match target with
    | nil => nil
    | (s, expr)::xs => (reify_symbol nt s, reify_expr nt expr)::(_reify nt xs)
    end.

Definition reify t:= _reify (find_NT t) t.

Definition test_reify := reify test.

Compute test_reify. 
(* = [(NT "E", combine (base [NT "E"; T "+"; NT "T"]) (base [NT "T"]));
      (NT "T", combine (base [NT "T"; T "*"; NT "F"]) (base [NT "F"]));
      (NT "F", combine (base [T "("; NT "E"; T ")"]) (base [T "i"]))]
    : grammar *)

(* TODO *)
(*  multi_rule not handle, shown as follows
    (NT "E", combine (base [NT "E"; T "+"; NT "T"]) (base [NT "T"]));
    (NT "E", combine (base [NT "T"; T "*"; NT "F"]) (base [NT "F"]));
    (NT "E", combine (base [T "("; NT "E"; T ")"]) (base [T "i"])) *)

End Reify.

(* Check the grammar *)
Section Tyck.
(* TODO *)
End Tyck.

Section FIRSTVT_and_LASTVT.

Fixpoint get_expr (target: symbol)(g: grammar): option expr :=
    match g with
    | nil => None
    | (x, e)::xs => if (symbol_dec x target) then Some e else get_expr target xs
    end.

Definition default := U "ERROR".

Definition look_ahead (l: list symbol): list symbol:=
    match (nth_default default l 1) with
    | T x => [T x]
    | _ => nil (* default is 'U "ERROR"', which is in this clause *)
    end.

Fixpoint find_first_ (target: symbol)(g: grammar)(dec: nat): list symbol :=
    match dec with
    | S n =>
        match (get_expr target g) with
        | Some e => _find_first target g e n
        | None => nil
        end
    | O => nil
    end
with _find_first (target: symbol)(g: grammar)(e: expr)(dec: nat):=
    match dec with
    | S n =>
        match e with
        | base l => let fst:= (nth_default default l 0) in 
                        if (symbol_dec fst target) 
                            then look_ahead l
                            else (
                                match fst with
                                | T x => [T x] ++ look_ahead l
                                | NT _ => (find_first_ fst g n) ++ look_ahead l
                                | U _ => nil
                                end
                            )
        | combine a b => (_find_first target g a n) ++ (_find_first target g b n)
        end
    | O => nil
    end.

(* max execution depth: 100 *)
(* because the depth maybe infinite... *)
(* to pass the Gallina's recursion check *)
Definition MAX_EXECUTION_DEPTH := 100.

Definition find_first (target: symbol)(g: grammar) := find_first_ target g MAX_EXECUTION_DEPTH.

Fixpoint delete_multi_occur (l: list symbol): list symbol :=
    match l with
    | nil => nil
    | x::xs => if (In_symbol x xs)
                    then delete_multi_occur xs 
                    else x::(delete_multi_occur xs)
    end.

(* the first element is all NT symbols, their first set is following in order *)
Definition firstvt (g: grammar) := 
let nt := find_NT g in    
    let l := map (fun s => map get_string_from_symbol (delete_multi_occur (find_first (NT s) g))) nt in
        (nt :: l).

(* reverse the grammar, then use firstvt() to define lastvt() *)
Fixpoint rev_expr (e: expr): expr :=
    match e with
    | base l => base (rev l)
    | combine a b => combine (rev_expr a) (rev_expr b)
    end.

Definition lastvt (g: grammar):= 
    let rev_g:= map (fun x => (fst x, rev_expr (snd x))) g in
        firstvt rev_g.

Compute (firstvt test_reify).
(* = [["E"; "T"; "F"]; ["+"; "*"; "("; "i"]; ["*"; "("; "i"]; ["("; "i"]]
: list (list string) *)
Compute (lastvt test_reify).
(* = [["E"; "T"; "F"]; ["+"; "*"; ")"; "i"]; ["*"; ")"; "i"]; [")"; "i"]]
: list (list string) *)

End FIRSTVT_and_LASTVT.

