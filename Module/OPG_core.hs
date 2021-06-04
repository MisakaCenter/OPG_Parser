module Module.OPG_core where

import qualified Prelude

data Nat =
   O
 | S Nat

fst :: ((,) a1 a2) -> a1
fst p =
  case p of {
   (,) x _ -> x}

snd :: ((,) a1 a2) -> a2
snd p =
  case p of {
   (,) _ y -> y}

length :: (([]) a1) -> Nat
length l =
  case l of {
   ([]) -> O;
   (:) _ l' -> S (length l')}

app :: (([]) a1) -> (([]) 
       a1) -> ([]) a1
app l m =
  case l of {
   ([]) -> m;
   (:) a l1 -> (:) a (app l1 m)}

add :: Nat -> Nat -> Nat
add n m =
  case n of {
   O -> m;
   S p -> S (add p m)}

eqb :: Nat -> Nat ->
       Prelude.Bool
eqb n m =
  case n of {
   O ->
    case m of {
     O -> Prelude.True;
     S _ -> Prelude.False};
   S n' ->
    case m of {
     O -> Prelude.False;
     S m' -> eqb n' m'}}

nth_error :: (([]) a1) -> Nat
             -> Prelude.Maybe
             a1
nth_error l n =
  case n of {
   O ->
    case l of {
     ([]) -> Prelude.Nothing;
     (:) x _ -> Prelude.Just x};
   S n0 ->
    case l of {
     ([]) -> Prelude.Nothing;
     (:) _ l0 ->
      nth_error l0 n0}}

nth_default :: a1 -> (([]) 
               a1) -> Nat -> a1
nth_default default1 l n =
  case nth_error l n of {
   Prelude.Just x -> x;
   Prelude.Nothing -> default1}

rev :: (([]) a1) -> ([]) a1
rev l =
  case l of {
   ([]) -> ([]);
   (:) x l' ->
    app (rev l') ((:) x ([]))}

map :: (a1 -> a2) -> (([]) 
       a1) -> ([]) a2
map f l =
  case l of {
   ([]) -> ([]);
   (:) a t -> (:) (f a)
    (map f t)}

filter :: (a1 -> Prelude.Bool)
          -> (([]) a1) -> ([])
          a1
filter f l =
  case l of {
   ([]) -> ([]);
   (:) x l0 ->
    case f x of {
     Prelude.True -> (:) x
      (filter f l0);
     Prelude.False ->
      filter f l0}}

data Symbol =
   T Prelude.String
 | NT Prelude.String
 | U Prelude.String

data Expr =
   Base (([]) Symbol)
 | Combine Expr Expr

type Rule = (,) Symbol Expr

type Grammar = ([]) Rule

eOF :: Symbol
eOF =
  T ((:) '$' ([]))

default0 :: Symbol
default0 =
  U ((:) 'E' ((:) 'R' ((:) 'R'
    ((:) 'O' ((:) 'R' ([]))))))

test :: Grammar
test =
  (:) ((,) (U ((:) 'E' ([])))
    (Combine (Base ((:) (U ((:)
    'E' ([]))) ((:) (U ((:) '+'
    ([]))) ((:) (U ((:) 'T'
    ([]))) ([]))))) (Base ((:)
    (U ((:) 'T' ([]))) ([])))))
    ((:) ((,) (U ((:) 'T'
    ([]))) (Combine (Base ((:)
    (U ((:) 'T' ([]))) ((:) (U
    ((:) '*' ([]))) ((:) (U
    ((:) 'F' ([]))) ([])))))
    (Base ((:) (U ((:) 'F'
    ([]))) ([]))))) ((:) ((,)
    (U ((:) 'F' ([]))) (Combine
    (Base ((:) (U ((:) '('
    ([]))) ((:) (U ((:) 'E'
    ([]))) ((:) (U ((:) ')'
    ([]))) ([]))))) (Base ((:)
    (U ((:) 'i' ([]))) ([])))))
    ([])))

get_string_from_symbol :: 
  Symbol -> Prelude.String
get_string_from_symbol a =
  case a of {
   T x -> x;
   NT x -> x;
   U x -> x}

symbol_dec :: Symbol -> Symbol
              -> Prelude.Bool
symbol_dec a b =
  case a of {
   T s1 ->
    case b of {
     T s2 ->
      case (Prelude.==) s1 s2 of {
       Prelude.True ->
        Prelude.True;
       Prelude.False ->
        Prelude.False};
     _ -> Prelude.False};
   NT s1 ->
    case b of {
     NT s2 ->
      case (Prelude.==) s1 s2 of {
       Prelude.True ->
        Prelude.True;
       Prelude.False ->
        Prelude.False};
     _ -> Prelude.False};
   U s1 ->
    case b of {
     U s2 ->
      case (Prelude.==) s1 s2 of {
       Prelude.True ->
        Prelude.True;
       Prelude.False ->
        Prelude.False};
     _ -> Prelude.False}}

in_symbol :: Symbol -> (([])
             Symbol) ->
             Prelude.Bool
in_symbol t l =
  case l of {
   ([]) -> Prelude.False;
   (:) x xs ->
    case symbol_dec t x of {
     Prelude.True ->
      Prelude.True;
     Prelude.False ->
      in_symbol t xs}}

in_string :: (([])
             Prelude.String) ->
             Prelude.String ->
             Prelude.Bool
in_string l t =
  case l of {
   ([]) -> Prelude.False;
   (:) x xs ->
    case (Prelude.==) t x of {
     Prelude.True ->
      Prelude.True;
     Prelude.False ->
      in_string xs t}}

find_string_in_expr :: 
  Expr -> ([]) Prelude.String
find_string_in_expr e =
  case e of {
   Base xs ->
    map get_string_from_symbol
      xs;
   Combine a b ->
    app (find_string_in_expr a)
      (find_string_in_expr b)}

find_string_in_grammar :: 
  Grammar -> ([])
  Prelude.String
find_string_in_grammar target =
  case target of {
   ([]) -> ([]);
   (:) r xs ->
    case r of {
     (,) _ e ->
      let {
       l = find_string_in_expr
             e}
      in
      app l
        (find_string_in_grammar
          xs)}}

find_NT :: Grammar -> ([])
           Prelude.String
find_NT target =
  case target of {
   ([]) -> ([]);
   (:) r xs ->
    case r of {
     (,) x _ ->
      let {l = find_NT xs} in
      let {
       s = get_string_from_symbol
             x}
      in
      case Prelude.not
             (in_string l s) of {
       Prelude.True -> (:) s l;
       Prelude.False -> l}}}

reify_EOF :: (([])
             Prelude.String) ->
             ([])
             Prelude.String
reify_EOF l =
  case l of {
   ([]) -> ([]);
   (:) x xs ->
    case (Prelude.==) x ((:)
           '$' ([])) of {
     Prelude.True ->
      reify_EOF xs;
     Prelude.False -> (:) x
      (reify_EOF xs)}}

delete_multi_occur_str :: 
  (([]) Prelude.String) -> ([])
  Prelude.String
delete_multi_occur_str l =
  case l of {
   ([]) -> ([]);
   (:) x xs ->
    case in_string xs x of {
     Prelude.True ->
      delete_multi_occur_str xs;
     Prelude.False -> (:) x
      (delete_multi_occur_str
        xs)}}

find_T :: Grammar -> ([])
          Prelude.String
find_T target =
  app
    (reify_EOF
      (delete_multi_occur_str
        (filter (\x ->
          Prelude.not
            (in_string
              (find_NT target)
              x))
          (find_string_in_grammar
            target)))) ((:)
    ((:) '$' ([])) ([]))

reify_symbol :: (([])
                Prelude.String)
                -> Symbol ->
                Symbol
reify_symbol nt s =
  let {
   str = get_string_from_symbol
           s}
  in
  case in_string nt str of {
   Prelude.True -> NT str;
   Prelude.False -> T str}

reify_expr :: (([])
              Prelude.String)
              -> Expr -> Expr
reify_expr nt t =
  case t of {
   Base l -> Base
    (map (reify_symbol nt) l);
   Combine a b -> Combine
    (reify_expr nt a)
    (reify_expr nt b)}

_reify :: (([]) Prelude.String)
          -> Grammar -> Grammar
_reify nt target =
  case target of {
   ([]) -> ([]);
   (:) r xs ->
    case r of {
     (,) s expr -> (:) ((,)
      (reify_symbol nt s)
      (reify_expr nt expr))
      (_reify nt xs)}}

add_EOF :: Grammar -> Grammar
add_EOF g =
  case g of {
   ([]) -> ([]);
   (:) r xs ->
    case r of {
     (,) s e -> (:) ((,) s
      (Combine e (Base ((:) eOF
      ((:) s ((:) eOF
      ([]))))))) xs}}

reify :: Grammar -> Grammar
reify t =
  add_EOF
    (_reify (find_NT t) t)

check_expr :: (([]) Symbol) ->
              (([]) Symbol) ->
              Prelude.Bool
check_expr nt l =
  case l of {
   ([]) -> Prelude.True;
   (:) x xs ->
    case in_symbol x nt of {
     Prelude.True ->
      case in_symbol
             (nth_default
               default0 xs O)
             nt of {
       Prelude.True ->
        Prelude.False;
       Prelude.False ->
        check_expr nt xs};
     Prelude.False ->
      check_expr nt xs}}

check_expr_l :: (([]) Symbol)
                -> Expr ->
                Prelude.Bool
check_expr_l nt e =
  case e of {
   Base l -> check_expr nt l;
   Combine a b ->
    (Prelude.&&)
      (check_expr_l nt a)
      (check_expr_l nt b)}

_type_checker :: (([]) 
                 Symbol) ->
                 Grammar ->
                 Prelude.Bool
_type_checker nt g =
  case g of {
   ([]) -> Prelude.True;
   (:) r xs ->
    case r of {
     (,) _ e ->
      (Prelude.&&)
        (check_expr_l nt e)
        (_type_checker nt xs)}}

type_checker :: Grammar ->
                Prelude.Bool
type_checker g =
  _type_checker
    (map (\x -> NT x)
      (find_NT g)) g

get_expr :: Symbol -> Grammar
            -> Prelude.Maybe
            Expr
get_expr target g =
  case g of {
   ([]) -> Prelude.Nothing;
   (:) r xs ->
    case r of {
     (,) x e ->
      case symbol_dec x target of {
       Prelude.True ->
        Prelude.Just e;
       Prelude.False ->
        get_expr target xs}}}

look_ahead :: (([]) Symbol) ->
              ([]) Symbol
look_ahead l =
  case nth_default default0 l
         (S O) of {
   T x -> (:) (T x) ([]);
   _ -> ([])}

find_first_ :: Symbol ->
               Grammar -> Nat
               -> ([]) 
               Symbol
find_first_ target g dec =
  case dec of {
   O -> ([]);
   S n ->
    case get_expr target g of {
     Prelude.Just e ->
      _find_first target g e n;
     Prelude.Nothing -> ([])}}

_find_first :: Symbol ->
               Grammar -> Expr
               -> Nat -> ([])
               Symbol
_find_first target g e dec =
  case dec of {
   O -> ([]);
   S n ->
    case e of {
     Base l ->
      let {
       fst0 = nth_default
                default0 l O}
      in
      case symbol_dec fst0
             target of {
       Prelude.True ->
        look_ahead l;
       Prelude.False ->
        case fst0 of {
         T x ->
          app ((:) (T x) ([]))
            (look_ahead l);
         NT _ ->
          app
            (find_first_ fst0 g
              n) (look_ahead l);
         U _ -> ([])}};
     Combine a b ->
      app
        (_find_first target g a
          n)
        (_find_first target g b
          n)}}

mAX_EXECUTION_DEPTH :: Nat
mAX_EXECUTION_DEPTH =
  S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S
    O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

find_first :: Symbol -> Grammar
              -> ([]) Symbol
find_first target g =
  find_first_ target g
    mAX_EXECUTION_DEPTH

delete_multi_occur :: (([])
                      Symbol)
                      -> ([])
                      Symbol
delete_multi_occur l =
  case l of {
   ([]) -> ([]);
   (:) x xs ->
    case in_symbol x xs of {
     Prelude.True ->
      delete_multi_occur xs;
     Prelude.False -> (:) x
      (delete_multi_occur xs)}}

firstvt :: Grammar -> ([])
           (([])
           Prelude.String)
firstvt g =
  let {nt = find_NT g} in
  let {
   l = map (\s ->
         map
           get_string_from_symbol
           (delete_multi_occur
             (find_first (NT s)
               g))) nt}
  in
  (:) nt (map reify_EOF l)

rev_expr :: Expr -> Expr
rev_expr e =
  case e of {
   Base l -> Base (rev l);
   Combine a b -> Combine
    (rev_expr a) (rev_expr b)}

lastvt :: Grammar -> ([])
          (([]) Prelude.String)
lastvt g =
  let {
   rev_g = map (\x -> (,)
             (fst x)
             (rev_expr (snd x)))
             g}
  in
  firstvt rev_g

data Relation =
   Le
 | Ge
 | Eq
 | Emp

type Matrix =
  ([]) (([]) Relation)

is_emp :: Relation ->
          Prelude.Bool
is_emp r =
  case r of {
   Emp -> Prelude.True;
   _ -> Prelude.False}

list_n_times :: a1 -> Nat ->
                ([]) a1
list_n_times x n =
  case n of {
   O -> ([]);
   S n0 -> (:) x
    (list_n_times x n0)}

matrix_gen_emp :: Grammar ->
                  Matrix
matrix_gen_emp g =
  let {len = length (find_T g)}
  in
  let {
   row = list_n_times Emp len}
  in
  list_n_times row len

change_m_to_x :: (([])
                 Relation) ->
                 Nat ->
                 Relation ->
                 ([]) Relation
change_m_to_x l m x =
  case l of {
   ([]) -> ([]);
   (:) x' xs ->
    case m of {
     O -> (:) x xs;
     S m0 -> (:) x'
      (change_m_to_x xs m0 x)}}

change_n_m_to_x :: Matrix ->
                   Nat -> Nat
                   -> Relation
                   -> Matrix
change_n_m_to_x mtx n m x =
  case mtx of {
   ([]) -> ([]);
   (:) x' xs ->
    case n of {
     O -> (:)
      (change_m_to_x x' m x) xs;
     S n0 -> (:) x'
      (change_n_m_to_x xs n0 m
        x)}}

is_T :: Symbol -> Prelude.Bool
is_T s =
  case s of {
   T _ -> Prelude.True;
   _ -> Prelude.False}

is_NT :: Symbol -> Prelude.Bool
is_NT s =
  case s of {
   NT _ -> Prelude.True;
   _ -> Prelude.False}

_find_x_in_g :: (([])
                Prelude.String)
                ->
                Prelude.String
                -> Nat
_find_x_in_g l x =
  case l of {
   ([]) -> O;
   (:) x' xs ->
    case (Prelude.==) x' x of {
     Prelude.True -> O;
     Prelude.False -> S
      (_find_x_in_g xs x)}}

find_x_in_g_symbol :: Grammar
                      -> Symbol
                      -> Nat
find_x_in_g_symbol g x =
  _find_x_in_g (find_T g)
    (get_string_from_symbol x)

find_x_in_g_str :: Grammar ->
                   Prelude.String
                   -> Nat
find_x_in_g_str g x =
  _find_x_in_g (find_T g) x

equal_list_eval :: Matrix ->
                   (([])
                   Symbol) ->
                   Grammar ->
                   Matrix
equal_list_eval m l g =
  case l of {
   ([]) -> m;
   (:) x xs ->
    case is_T x of {
     Prelude.True ->
      let {
       snd0 = nth_default
                default0 xs O}
      in
      let {
       trd = nth_default
               default0 xs (S
               O)}
      in
      case (Prelude.&&)
             (is_NT snd0)
             (is_T trd) of {
       Prelude.True ->
        let {
         a = find_x_in_g_symbol
               g x}
        in
        let {
         b = find_x_in_g_symbol
               g trd}
        in
        equal_list_eval
          (change_n_m_to_x m a
            b Eq) xs g;
       Prelude.False ->
        case is_T snd0 of {
         Prelude.True ->
          let {
           a = find_x_in_g_symbol
                 g x}
          in
          let {
           b = find_x_in_g_symbol
                 g snd0}
          in
          equal_list_eval
            (change_n_m_to_x m
              a b Eq) xs g;
         Prelude.False ->
          equal_list_eval m xs
            g}};
     Prelude.False ->
      equal_list_eval m xs g}}

firstvt_gen :: Grammar ->
               Symbol -> ([])
               Prelude.String
firstvt_gen g nt =
  let {fst0 = firstvt g} in
  nth_default ([]) fst0
    (add
      (_find_x_in_g
        (nth_default ([]) fst0
          O)
        (get_string_from_symbol
          nt)) (S O))

fold_le :: Grammar -> (([])
           Prelude.String) ->
           Matrix -> Nat ->
           Matrix
fold_le g l m left =
  case l of {
   ([]) -> m;
   (:) x xs ->
    let {
     a = find_x_in_g_str g x}
    in
    fold_le g xs
      (change_n_m_to_x m left a
        Le) left}

less_list_eval :: Matrix ->
                  (([]) 
                  Symbol) ->
                  Grammar ->
                  Matrix
less_list_eval m l g =
  case l of {
   ([]) -> m;
   (:) x xs ->
    case is_T x of {
     Prelude.True ->
      let {
       snd0 = nth_default
                default0 xs O}
      in
      case is_NT snd0 of {
       Prelude.True ->
        let {
         fstq = firstvt_gen g
                  snd0}
        in
        less_list_eval
          (fold_le g fstq m
            (find_x_in_g_symbol
              g x)) xs g;
       Prelude.False ->
        less_list_eval m xs g};
     Prelude.False ->
      less_list_eval m xs g}}

lastvt_gen :: Grammar -> Symbol
              -> ([])
              Prelude.String
lastvt_gen g nt =
  let {fst0 = lastvt g} in
  nth_default ([]) fst0
    (add
      (_find_x_in_g
        (nth_default ([]) fst0
          O)
        (get_string_from_symbol
          nt)) (S O))

fold_ge :: Grammar -> (([])
           Prelude.String) ->
           Matrix -> Nat ->
           Matrix
fold_ge g l m right =
  case l of {
   ([]) -> m;
   (:) x xs ->
    let {
     a = find_x_in_g_str g x}
    in
    fold_ge g xs
      (change_n_m_to_x m a
        right Ge) right}

greater_list_eval :: Matrix ->
                     (([])
                     Symbol) ->
                     Grammar ->
                     Matrix
greater_list_eval m l g =
  case l of {
   ([]) -> m;
   (:) x xs ->
    case is_NT x of {
     Prelude.True ->
      let {
       snd0 = nth_default
                default0 xs O}
      in
      case is_T snd0 of {
       Prelude.True ->
        let {
         lstq = lastvt_gen g x}
        in
        greater_list_eval
          (fold_ge g lstq m
            (find_x_in_g_symbol
              g snd0)) xs g;
       Prelude.False ->
        greater_list_eval m xs
          g};
     Prelude.False ->
      greater_list_eval m xs g}}

meta_expr_eval :: (Matrix ->
                  (([]) 
                  Symbol) ->
                  Grammar ->
                  Matrix) ->
                  Matrix ->
                  Expr ->
                  Grammar ->
                  Matrix
meta_expr_eval f m e g =
  case e of {
   Base l -> f m l g;
   Combine a b ->
    meta_expr_eval f
      (meta_expr_eval f m a g)
      b g}

_meta_eval :: (Matrix -> (([])
              Symbol) ->
              Grammar ->
              Matrix) -> Matrix
              -> (([]) 
              Expr) -> Grammar
              -> Matrix
_meta_eval f m exprs g =
  case exprs of {
   ([]) -> m;
   (:) x xs ->
    _meta_eval f
      (meta_expr_eval f m x g)
      xs g}

meta_eval :: (Matrix -> (([])
             Symbol) -> Grammar
             -> Matrix) ->
             Grammar -> Matrix
meta_eval f g =
  _meta_eval f
    (matrix_gen_emp g)
    (map snd g) g

equal_eval :: Grammar -> Matrix
equal_eval g =
  meta_eval equal_list_eval g

less_eval :: Grammar -> Matrix
less_eval g =
  meta_eval less_list_eval g

greater_eval :: Grammar ->
                Matrix
greater_eval g =
  meta_eval greater_list_eval g

merge_list :: (([]) Relation)
              -> (([])
              Relation) -> ([])
              Relation
merge_list l1 l2 =
  case eqb (length l1)
         (length l2) of {
   Prelude.True ->
    case l1 of {
     ([]) -> ([]);
     (:) x1 xs1 ->
      case l2 of {
       ([]) -> ([]);
       (:) x2 xs2 ->
        case is_emp x1 of {
         Prelude.True -> (:) x2
          (merge_list xs1 xs2);
         Prelude.False ->
          case is_emp x2 of {
           Prelude.True -> (:)
            x1
            (merge_list xs1
              xs2);
           Prelude.False ->
            ([])}}}};
   Prelude.False -> ([])}

merge :: Matrix -> Matrix ->
         Matrix
merge m1 m2 =
  case eqb (length m1)
         (length m2) of {
   Prelude.True ->
    case m1 of {
     ([]) -> ([]);
     (:) x1 xs1 ->
      case m2 of {
       ([]) -> ([]);
       (:) x2 xs2 -> (:)
        (merge_list x1 x2)
        (merge xs1 xs2)}};
   Prelude.False -> ([])}

get_opg_matrix :: Grammar ->
                  Matrix
get_opg_matrix g =
  merge
    (merge (less_eval g)
      (equal_eval g))
    (greater_eval g)

output_t :: Grammar -> ([])
            Prelude.String
output_t g =
  find_T (reify g)

output_nt :: Grammar -> ([])
             Prelude.String
output_nt g =
  find_NT (reify g)

output_matrix :: Grammar ->
                 Matrix
output_matrix g =
  get_opg_matrix (reify g)