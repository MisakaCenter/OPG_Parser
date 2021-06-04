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
Definition EOF: symbol:= T "$".

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

Fixpoint reify_EOF (l: list string): list string :=
    match l with
    | nil => nil
    | x::xs => if eqb x "$" then reify_EOF xs else x::(reify_EOF xs)
    end.

Fixpoint delete_multi_occur_str (l: list string): list string :=
    match l with
    | nil => nil
    | x::xs => if (In_string xs x)
                    then delete_multi_occur_str xs 
                    else x::(delete_multi_occur_str xs)
    end.

(* x not in find_NT iff. x is T *)
Definition find_T (target: grammar): list string := 
    (reify_EOF
        (delete_multi_occur_str 
            (filter (fun x => negb (In_string (find_NT target) x)) (find_string_in_grammar target))))++["$"].

(*
Compute (find_NT test). (* = ["E"; "T"; "F"] : list string *)
Compute (find_T test). (* = ["+"; "*"; "("; ")"; "i"] : list string *)
*)

Definition reify_symbol (nt: list string)(s: symbol): symbol :=
    let str:= get_string_from_symbol s in
        if (In_string nt str) then NT str else T str.

Fixpoint reify_expr (nt: list string)(t: expr): expr:=
    match t with
    | base l => base (map (reify_symbol nt) l)
    | combine a b => combine (reify_expr nt a) (reify_expr nt b)
    end.

Fixpoint _reify (nt: list string)(target: grammar): grammar :=
    match target with
    | nil => nil
    | (s, expr)::xs => (reify_symbol nt s, reify_expr nt expr)::(_reify nt xs)
    end.

Definition add_EOF (g: grammar): grammar := 
    match g with
    | (s, e)::xs => (s, combine e (base [EOF ; s ; EOF]))::xs
    | nil => nil
    end.

Definition reify t:= add_EOF (_reify (find_NT t) t).

Definition test_reify := reify test.

(* Compute test_reify.  *)
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

Definition is_EOF (s: symbol): bool :=
    match s with
    | T s' => eqb s' "$" 
    | _ => false
    end.

(* the first element is all NT symbols, their first set is following in order *)
Definition firstvt (g: grammar) := 
let nt := find_NT g in    
    let l := map (fun s => map get_string_from_symbol (delete_multi_occur (find_first (NT s) g))) nt in
        (nt :: map reify_EOF l).

(* reverse the grammar, then use firstvt() to define lastvt() *)
Fixpoint rev_expr (e: expr): expr :=
    match e with
    | base l => base (rev l)
    | combine a b => combine (rev_expr a) (rev_expr b)
    end.

Definition lastvt (g: grammar):= 
    let rev_g:= map (fun x => (fst x, rev_expr (snd x))) g in
        firstvt rev_g.

(* Compute (firstvt test_reify). *)
(* = [["E"; "T"; "F"]; ["+"; "*"; "("; "i"]; ["*"; "("; "i"]; ["("; "i"]]
: list (list string) *)
(* Compute (lastvt test_reify). *)
(* = [["E"; "T"; "F"]; ["+"; "*"; ")"; "i"]; ["*"; ")"; "i"]; [")"; "i"]]
: list (list string) *)

End FIRSTVT_and_LASTVT.

Section OPG_Matrix.

Inductive relation: Type :=
| le : relation (* < *)
| ge : relation (* > *)
| eq : relation (* = *)
| emp : relation. (* ? undecided *)

Definition matrix: Type := list (list relation).

Definition is_emp (r: relation): bool :=
    match r with
    | emp => true
    | _ => false
    end.

Fixpoint list_n_times {T: Type}(x: T)(n: nat): list T :=
    match n with
    | S n => x :: (list_n_times x n)
    | O => nil
    end.

Definition matrix_gen_emp (g: grammar): matrix :=
    let len:= length (find_T g) in
        let row:= list_n_times emp len in
            list_n_times row len.

Definition default_relation: list relation := nil.

Fixpoint change_m_to_x (l: list relation)(m: nat)(x: relation): list relation :=
    match l with
    | nil => nil
    | x'::xs => match m with
                | 0 => x::xs
                | S m => x'::(change_m_to_x xs m x)
                end
    end.

Fixpoint change_n_m_to_x (mtx: matrix)(n m:nat)(x: relation): matrix :=
    match mtx with
    | nil => nil
    | x'::xs => match n with
                | 0 => (change_m_to_x x' m x)::xs
                | S n => x'::(change_n_m_to_x xs n m x)
                end
    end.

Definition is_T (s: symbol): bool :=
    match s with
    | T _ => true
    | _ => false
    end.

Definition is_NT (s: symbol): bool :=
    match s with
    | NT _ => true
    | _ => false
    end.

Fixpoint _find_x_in_g (l: list string)(x: string): nat :=
    match l with
    | x'::xs => if string_dec x' x then O else S (_find_x_in_g xs x)
    | nil => O
    end.

Definition find_x_in_g_symbol (g: grammar)(x: symbol) := _find_x_in_g (find_T g) (get_string_from_symbol x).
Definition find_x_in_g_str (g: grammar)(x: string) := _find_x_in_g (find_T g) x.

(* find => aQb | ab => a equal b*)
Fixpoint equal_list_eval (m: matrix)(l: list symbol)(g: grammar): matrix :=
    match l with
    | nil => m
    | x::xs => if is_T x 
                    then (let (snd, trd):= (nth_default default xs 0, nth_default default xs 1) in
                            if (andb (is_NT snd) (is_T trd)) (* aQb *)
                                then (let (a, b):= (find_x_in_g_symbol g x, find_x_in_g_symbol g trd) in 
                                        equal_list_eval (change_n_m_to_x m a b eq) xs g)
                                else if is_T snd (* ab *)
                                        then (let (a, b):= (find_x_in_g_symbol g x, find_x_in_g_symbol g snd) in 
                                                equal_list_eval (change_n_m_to_x m a b eq) xs g)
                                        else equal_list_eval m xs g)
                    else equal_list_eval m xs g
    end.

Fixpoint equal_expr_eval (m: matrix)(expr: expr)(g: grammar): matrix :=
    match expr with
    | base l => equal_list_eval m l g
    | combine a b => equal_expr_eval (equal_expr_eval m a g) b g
    end.

Fixpoint _equal_eval (m: matrix)(exprs: list expr)(g: grammar): matrix :=
    match exprs with
    | nil => m
    | x::xs => _equal_eval (equal_expr_eval m x g) xs g
    end.

Definition firstvt_gen (g: grammar)(nt: symbol): list string := 
    let fst:= firstvt g in
        nth_default nil fst (_find_x_in_g (nth_default nil fst 0) (get_string_from_symbol nt) + 1).

Fixpoint fold_le (g: grammar)(l: list string)(m: matrix)(left: nat): matrix :=
    match l with
    | nil => m
    | x::xs => let a:= find_x_in_g_str g x in 
                    fold_le g xs (change_n_m_to_x m left a le) left
    end.

(* find => aQ => a < firstvt(Q)*)
Fixpoint less_list_eval (m: matrix)(l: list symbol)(g: grammar): matrix :=
    match l with
    | nil => m
    | x::xs => if is_T x 
                    then (let snd:= nth_default default xs 0 in
                            if (is_NT snd) (* aQ *)
                                then (let fstq:= firstvt_gen g snd in 
                                        less_list_eval (fold_le g fstq m (find_x_in_g_symbol g x)) xs g)
                                else less_list_eval m xs g)
                    else less_list_eval m xs g
    end.

Definition lastvt_gen (g: grammar)(nt: symbol): list string := 
    let fst:= lastvt g in
        nth_default nil fst (_find_x_in_g (nth_default nil fst 0) (get_string_from_symbol nt) + 1).

Fixpoint fold_ge (g: grammar)(l: list string)(m: matrix)(right: nat): matrix :=
    match l with
    | nil => m
    | x::xs => let a:= find_x_in_g_str g x in 
                    fold_ge g xs (change_n_m_to_x m a right ge) right
    end.

(* find => Qa => firstvt(Q) > a*)
Fixpoint greater_list_eval (m: matrix)(l: list symbol)(g: grammar): matrix :=
    match l with
    | nil => m
    | x::xs => if is_NT x 
                    then (let snd:= nth_default default xs 0 in
                            if (is_T snd) (* Qa *)
                                then (let lstq:= lastvt_gen g x in 
                                        greater_list_eval (fold_ge g lstq m (find_x_in_g_symbol g snd)) xs g)
                                else greater_list_eval m xs g)
                    else greater_list_eval m xs g
    end.

(* meta-programming! for less, greater and equal generation *)
Fixpoint meta_expr_eval (f: matrix -> list symbol -> grammar -> matrix)(m: matrix)(e: expr)(g: grammar): matrix :=
    match e with
    | base l => f m l g
    | combine a b => meta_expr_eval f (meta_expr_eval f m a g) b g
    end.

Fixpoint _meta_eval (f: matrix -> list symbol -> grammar -> matrix)(m: matrix)(exprs: list expr)(g: grammar): matrix :=
    match exprs with
    | nil => m
    | x::xs => _meta_eval f (meta_expr_eval f m x g) xs g
    end.

Definition meta_eval (f: matrix -> list symbol -> grammar -> matrix)(g: grammar):=
    _meta_eval f (matrix_gen_emp g) (map snd g) g.

(* OPG matrix can be generated using following 3 functions *)
Definition equal_eval (g: grammar):= meta_eval equal_list_eval g.
Definition less_eval (g: grammar):= meta_eval less_list_eval g.
Definition greater_eval (g: grammar):= meta_eval greater_list_eval g.

(* merge three matrix into one *)
Fixpoint merge_list (l1 l2: list relation): list relation :=
    if Nat.eqb (length l1) (length l2) then
        match l1, l2 with
        | x1::xs1, x2::xs2 => if is_emp x1 then x2::(merge_list xs1 xs2)
                                           else if is_emp x2 then x1::(merge_list xs1 xs2)
                                                             else nil (* both are not empty, should not happen! *)
        | _, _ => nil
        end
    else nil.

Fixpoint merge (m1 m2: matrix): matrix :=
    if Nat.eqb (length m1) (length m2) then
        match m1, m2 with
        | x1::xs1, x2::xs2 => (merge_list x1 x2)::(merge xs1 xs2)
        | _, _ => nil
        end
    else nil.

Definition get_opg_matrix (g: grammar) := merge (merge (less_eval g) (equal_eval g)) (greater_eval g).

(* Compute test_reify.
Compute find_T test_reify.
Compute get_opg_matrix test_reify. *)

End OPG_Matrix.

(* G => reify G => get_opg_matrix (reify G) *)
(* Interface *)
Section OPG_Analyzer.
Definition output_t (g: grammar):= find_T (reify g).
Definition output_nt (g: grammar):= find_NT (reify g).
Definition output_matrix (g: grammar):= get_opg_matrix (reify g).
End OPG_Analyzer.

(* Extraction to Haskell for I/O *)
Require Coq.extraction.Extraction.
Extraction Language Haskell.
Require Coq.extraction.ExtrHaskellString.
Require Import ExtrHaskellBasic.
Extraction "./Module/OPG_core.hs" output_t output_nt output_matrix test.