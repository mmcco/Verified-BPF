type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type unit0 =
| Tt

type bool =
| True
| False

(** val implb : bool -> bool -> bool **)

let implb b1 b2 =
  match b1 with
  | True -> b2
  | False -> True

(** val negb : bool -> bool **)

let negb = function
| True -> False
| False -> True

type nat =
| O
| S of nat

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

(** val fst : ('a1, 'a2) prod -> 'a1 **)

let fst = function
| Pair (x, y) -> x

(** val snd : ('a1, 'a2) prod -> 'a2 **)

let snd = function
| Pair (x, y) -> y

type 'a list =
| Nil
| Cons of 'a * 'a list

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | Nil -> m
  | Cons (a, l1) -> Cons (a, (app l1 m))

type comparison =
| Eq
| Lt
| Gt

(** val compOpp : comparison -> comparison **)

let compOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt

type compareSpecT =
| CompEqT
| CompLtT
| CompGtT

(** val compareSpec2Type : comparison -> compareSpecT **)

let compareSpec2Type = function
| Eq -> CompEqT
| Lt -> CompLtT
| Gt -> CompGtT

type 'a compSpecT = compareSpecT

(** val compSpec2Type : 'a1 -> 'a1 -> comparison -> 'a1 compSpecT **)

let compSpec2Type x y c =
  compareSpec2Type c

type 'a sig0 =
  'a
  (* singleton inductive, whose constructor was exist *)

type ('a, 'p) sigT =
| ExistT of 'a * 'p

(** val projT1 : ('a1, 'a2) sigT -> 'a1 **)

let projT1 = function
| ExistT (a, p) -> a

(** val projT2 : ('a1, 'a2) sigT -> 'a2 **)

let projT2 = function
| ExistT (x0, h) -> h

type sumbool =
| Left
| Right

type 'a sumor =
| Inleft of 'a
| Inright

(** val plus : nat -> nat -> nat **)

let rec plus n0 m =
  match n0 with
  | O -> m
  | S p -> S (plus p m)

(** val max : nat -> nat -> nat **)

let rec max n0 m =
  match n0 with
  | O -> m
  | S n' ->
    (match m with
     | O -> n0
     | S m' -> S (max n' m'))

(** val min : nat -> nat -> nat **)

let rec min n0 m =
  match n0 with
  | O -> O
  | S n' ->
    (match m with
     | O -> O
     | S m' -> S (min n' m'))

(** val nat_iter : nat -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

let rec nat_iter n0 f x =
  match n0 with
  | O -> x
  | S n' -> f (nat_iter n' f x)

type reflect =
| ReflectT
| ReflectF

(** val iff_reflect : bool -> reflect **)

let iff_reflect = function
| True -> ReflectT
| False -> ReflectF

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| Nil -> Nil
| Cons (x, l') -> app (rev l') (Cons (x, Nil))

(** val rev_append : 'a1 list -> 'a1 list -> 'a1 list **)

let rec rev_append l l' =
  match l with
  | Nil -> l'
  | Cons (a, l0) -> rev_append l0 (Cons (a, l'))

(** val rev' : 'a1 list -> 'a1 list **)

let rev' l =
  rev_append l Nil

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| Nil -> Nil
| Cons (a, t0) -> Cons ((f a), (map f t0))

(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | Nil -> a0
  | Cons (b, t0) -> fold_left f t0 (f a0 b)

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| Nil -> a0
| Cons (b, t0) -> f b (fold_right f a0 t0)

(** val forallb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb f = function
| Nil -> True
| Cons (a, l0) ->
  (match f a with
   | True -> forallb f l0
   | False -> False)

(** val nat_compare : nat -> nat -> comparison **)

let rec nat_compare n0 m =
  match n0 with
  | O ->
    (match m with
     | O -> Eq
     | S n1 -> Lt)
  | S n' ->
    (match m with
     | O -> Gt
     | S m' -> nat_compare n' m')

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module type EqLtLe = 
 sig 
  type t 
 end

module type OrderedType = 
 sig 
  type t 
  
  val compare : t -> t -> comparison
  
  val eq_dec : t -> t -> sumbool
 end

module type OrderedType' = 
 sig 
  type t 
  
  val compare : t -> t -> comparison
  
  val eq_dec : t -> t -> sumbool
 end

module OT_to_Full = 
 functor (O:OrderedType') ->
 struct 
  type t = O.t
  
  (** val compare : t -> t -> comparison **)
  
  let compare =
    O.compare
  
  (** val eq_dec : t -> t -> sumbool **)
  
  let eq_dec =
    O.eq_dec
 end

module MakeOrderTac = 
 functor (O:EqLtLe) ->
 functor (P:sig 
  
 end) ->
 struct 
  
 end

module OT_to_OrderTac = 
 functor (OT:OrderedType) ->
 struct 
  module OTF = OT_to_Full(OT)
  
  module TO = 
   struct 
    type t = OTF.t
    
    (** val compare : t -> t -> comparison **)
    
    let compare =
      OTF.compare
    
    (** val eq_dec : t -> t -> sumbool **)
    
    let eq_dec =
      OTF.eq_dec
   end
 end

module OrderedTypeFacts = 
 functor (O:OrderedType') ->
 struct 
  module OrderTac = OT_to_OrderTac(O)
  
  (** val eq_dec : O.t -> O.t -> sumbool **)
  
  let eq_dec =
    O.eq_dec
  
  (** val lt_dec : O.t -> O.t -> sumbool **)
  
  let lt_dec x y =
    let c = compSpec2Type x y (O.compare x y) in
    (match c with
     | CompLtT -> Left
     | _ -> Right)
  
  (** val eqb : O.t -> O.t -> bool **)
  
  let eqb x y =
    match eq_dec x y with
    | Left -> True
    | Right -> False
 end

module Pos = 
 struct 
  type t = positive
  
  (** val succ : positive -> positive **)
  
  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH
  
  (** val add : positive -> positive -> positive **)
  
  let rec add x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XO p ->
      (match y with
       | XI q -> XI (add p q)
       | XO q -> XO (add p q)
       | XH -> XI p)
    | XH ->
      (match y with
       | XI q -> XO (succ q)
       | XO q -> XI q
       | XH -> XO XH)
  
  (** val add_carry : positive -> positive -> positive **)
  
  and add_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XI (add_carry p q)
       | XO q -> XO (add_carry p q)
       | XH -> XI (succ p))
    | XO p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XH ->
      (match y with
       | XI q -> XI (succ q)
       | XO q -> XO (succ q)
       | XH -> XI XH)
  
  (** val pred_double : positive -> positive **)
  
  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH
  
  (** val pred : positive -> positive **)
  
  let pred = function
  | XI p -> XO p
  | XO p -> pred_double p
  | XH -> XH
  
  (** val pred_N : positive -> n **)
  
  let pred_N = function
  | XI p -> Npos (XO p)
  | XO p -> Npos (pred_double p)
  | XH -> N0
  
  type mask =
  | IsNul
  | IsPos of positive
  | IsNeg
  
  (** val mask_rect : 'a1 -> (positive -> 'a1) -> 'a1 -> mask -> 'a1 **)
  
  let mask_rect f f0 f1 = function
  | IsNul -> f
  | IsPos x -> f0 x
  | IsNeg -> f1
  
  (** val mask_rec : 'a1 -> (positive -> 'a1) -> 'a1 -> mask -> 'a1 **)
  
  let mask_rec f f0 f1 = function
  | IsNul -> f
  | IsPos x -> f0 x
  | IsNeg -> f1
  
  (** val succ_double_mask : mask -> mask **)
  
  let succ_double_mask = function
  | IsNul -> IsPos XH
  | IsPos p -> IsPos (XI p)
  | IsNeg -> IsNeg
  
  (** val double_mask : mask -> mask **)
  
  let double_mask = function
  | IsPos p -> IsPos (XO p)
  | x0 -> x0
  
  (** val double_pred_mask : positive -> mask **)
  
  let double_pred_mask = function
  | XI p -> IsPos (XO (XO p))
  | XO p -> IsPos (XO (pred_double p))
  | XH -> IsNul
  
  (** val pred_mask : mask -> mask **)
  
  let pred_mask = function
  | IsPos q ->
    (match q with
     | XH -> IsNul
     | _ -> IsPos (pred q))
  | _ -> IsNeg
  
  (** val sub_mask : positive -> positive -> mask **)
  
  let rec sub_mask x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double_mask (sub_mask p q)
       | XO q -> succ_double_mask (sub_mask p q)
       | XH -> IsPos (XO p))
    | XO p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XH ->
      (match y with
       | XH -> IsNul
       | _ -> IsNeg)
  
  (** val sub_mask_carry : positive -> positive -> mask **)
  
  and sub_mask_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XO p ->
      (match y with
       | XI q -> double_mask (sub_mask_carry p q)
       | XO q -> succ_double_mask (sub_mask_carry p q)
       | XH -> double_pred_mask p)
    | XH -> IsNeg
  
  (** val sub : positive -> positive -> positive **)
  
  let sub x y =
    match sub_mask x y with
    | IsPos z0 -> z0
    | _ -> XH
  
  (** val mul : positive -> positive -> positive **)
  
  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y
  
  (** val iter : positive -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)
  
  let rec iter n0 f x =
    match n0 with
    | XI n' -> f (iter n' f (iter n' f x))
    | XO n' -> iter n' f (iter n' f x)
    | XH -> f x
  
  (** val pow : positive -> positive -> positive **)
  
  let pow x y =
    iter y (mul x) XH
  
  (** val square : positive -> positive **)
  
  let rec square = function
  | XI p0 -> XI (XO (add (square p0) p0))
  | XO p0 -> XO (XO (square p0))
  | XH -> XH
  
  (** val div2 : positive -> positive **)
  
  let div2 = function
  | XI p0 -> p0
  | XO p0 -> p0
  | XH -> XH
  
  (** val div2_up : positive -> positive **)
  
  let div2_up = function
  | XI p0 -> succ p0
  | XO p0 -> p0
  | XH -> XH
  
  (** val size_nat : positive -> nat **)
  
  let rec size_nat = function
  | XI p0 -> S (size_nat p0)
  | XO p0 -> S (size_nat p0)
  | XH -> S O
  
  (** val size : positive -> positive **)
  
  let rec size = function
  | XI p0 -> succ (size p0)
  | XO p0 -> succ (size p0)
  | XH -> XH
  
  (** val compare_cont : positive -> positive -> comparison -> comparison **)
  
  let rec compare_cont x y r =
    match x with
    | XI p ->
      (match y with
       | XI q -> compare_cont p q r
       | XO q -> compare_cont p q Gt
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q -> compare_cont p q Lt
       | XO q -> compare_cont p q r
       | XH -> Gt)
    | XH ->
      (match y with
       | XH -> r
       | _ -> Lt)
  
  (** val compare : positive -> positive -> comparison **)
  
  let compare x y =
    compare_cont x y Eq
  
  (** val min : positive -> positive -> positive **)
  
  let min p p' =
    match compare p p' with
    | Gt -> p'
    | _ -> p
  
  (** val max : positive -> positive -> positive **)
  
  let max p p' =
    match compare p p' with
    | Gt -> p
    | _ -> p'
  
  (** val eqb : positive -> positive -> bool **)
  
  let rec eqb p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> eqb p0 q0
       | _ -> False)
    | XO p0 ->
      (match q with
       | XO q0 -> eqb p0 q0
       | _ -> False)
    | XH ->
      (match q with
       | XH -> True
       | _ -> False)
  
  (** val leb : positive -> positive -> bool **)
  
  let leb x y =
    match compare x y with
    | Gt -> False
    | _ -> True
  
  (** val ltb : positive -> positive -> bool **)
  
  let ltb x y =
    match compare x y with
    | Lt -> True
    | _ -> False
  
  (** val sqrtrem_step :
      (positive -> positive) -> (positive -> positive) -> (positive, mask)
      prod -> (positive, mask) prod **)
  
  let sqrtrem_step f g = function
  | Pair (s, y) ->
    (match y with
     | IsPos r ->
       let s' = XI (XO s) in
       let r' = g (f r) in
       (match leb s' r' with
        | True -> Pair ((XI s), (sub_mask r' s'))
        | False -> Pair ((XO s), (IsPos r')))
     | _ -> Pair ((XO s), (sub_mask (g (f XH)) (XO (XO XH)))))
  
  (** val sqrtrem : positive -> (positive, mask) prod **)
  
  let rec sqrtrem = function
  | XI p0 ->
    (match p0 with
     | XI p1 -> sqrtrem_step (fun x -> XI x) (fun x -> XI x) (sqrtrem p1)
     | XO p1 -> sqrtrem_step (fun x -> XO x) (fun x -> XI x) (sqrtrem p1)
     | XH -> Pair (XH, (IsPos (XO XH))))
  | XO p0 ->
    (match p0 with
     | XI p1 -> sqrtrem_step (fun x -> XI x) (fun x -> XO x) (sqrtrem p1)
     | XO p1 -> sqrtrem_step (fun x -> XO x) (fun x -> XO x) (sqrtrem p1)
     | XH -> Pair (XH, (IsPos XH)))
  | XH -> Pair (XH, IsNul)
  
  (** val sqrt : positive -> positive **)
  
  let sqrt p =
    fst (sqrtrem p)
  
  (** val gcdn : nat -> positive -> positive -> positive **)
  
  let rec gcdn n0 a b =
    match n0 with
    | O -> XH
    | S n1 ->
      (match a with
       | XI a' ->
         (match b with
          | XI b' ->
            (match compare a' b' with
             | Eq -> a
             | Lt -> gcdn n1 (sub b' a') a
             | Gt -> gcdn n1 (sub a' b') b)
          | XO b0 -> gcdn n1 a b0
          | XH -> XH)
       | XO a0 ->
         (match b with
          | XI p -> gcdn n1 a0 b
          | XO b0 -> XO (gcdn n1 a0 b0)
          | XH -> XH)
       | XH -> XH)
  
  (** val gcd : positive -> positive -> positive **)
  
  let gcd a b =
    gcdn (plus (size_nat a) (size_nat b)) a b
  
  (** val ggcdn :
      nat -> positive -> positive -> (positive, (positive, positive) prod)
      prod **)
  
  let rec ggcdn n0 a b =
    match n0 with
    | O -> Pair (XH, (Pair (a, b)))
    | S n1 ->
      (match a with
       | XI a' ->
         (match b with
          | XI b' ->
            (match compare a' b' with
             | Eq -> Pair (a, (Pair (XH, XH)))
             | Lt ->
               let Pair (g, p) = ggcdn n1 (sub b' a') a in
               let Pair (ba, aa) = p in
               Pair (g, (Pair (aa, (add aa (XO ba)))))
             | Gt ->
               let Pair (g, p) = ggcdn n1 (sub a' b') b in
               let Pair (ab, bb) = p in
               Pair (g, (Pair ((add bb (XO ab)), bb))))
          | XO b0 ->
            let Pair (g, p) = ggcdn n1 a b0 in
            let Pair (aa, bb) = p in Pair (g, (Pair (aa, (XO bb))))
          | XH -> Pair (XH, (Pair (a, XH))))
       | XO a0 ->
         (match b with
          | XI p ->
            let Pair (g, p0) = ggcdn n1 a0 b in
            let Pair (aa, bb) = p0 in Pair (g, (Pair ((XO aa), bb)))
          | XO b0 -> let Pair (g, p) = ggcdn n1 a0 b0 in Pair ((XO g), p)
          | XH -> Pair (XH, (Pair (a, XH))))
       | XH -> Pair (XH, (Pair (XH, b))))
  
  (** val ggcd :
      positive -> positive -> (positive, (positive, positive) prod) prod **)
  
  let ggcd a b =
    ggcdn (plus (size_nat a) (size_nat b)) a b
  
  (** val coq_Nsucc_double : n -> n **)
  
  let coq_Nsucc_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)
  
  (** val coq_Ndouble : n -> n **)
  
  let coq_Ndouble = function
  | N0 -> N0
  | Npos p -> Npos (XO p)
  
  (** val coq_lor : positive -> positive -> positive **)
  
  let rec coq_lor p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> XI (coq_lor p0 q0)
       | XO q0 -> XI (coq_lor p0 q0)
       | XH -> p)
    | XO p0 ->
      (match q with
       | XI q0 -> XI (coq_lor p0 q0)
       | XO q0 -> XO (coq_lor p0 q0)
       | XH -> XI p0)
    | XH ->
      (match q with
       | XO q0 -> XI q0
       | _ -> q)
  
  (** val coq_land : positive -> positive -> n **)
  
  let rec coq_land p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Nsucc_double (coq_land p0 q0)
       | XO q0 -> coq_Ndouble (coq_land p0 q0)
       | XH -> Npos XH)
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (coq_land p0 q0)
       | XO q0 -> coq_Ndouble (coq_land p0 q0)
       | XH -> N0)
    | XH ->
      (match q with
       | XO q0 -> N0
       | _ -> Npos XH)
  
  (** val ldiff : positive -> positive -> n **)
  
  let rec ldiff p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (ldiff p0 q0)
       | XO q0 -> coq_Nsucc_double (ldiff p0 q0)
       | XH -> Npos (XO p0))
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (ldiff p0 q0)
       | XO q0 -> coq_Ndouble (ldiff p0 q0)
       | XH -> Npos p)
    | XH ->
      (match q with
       | XO q0 -> Npos XH
       | _ -> N0)
  
  (** val coq_lxor : positive -> positive -> n **)
  
  let rec coq_lxor p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (coq_lxor p0 q0)
       | XO q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | XH -> Npos (XO p0))
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | XO q0 -> coq_Ndouble (coq_lxor p0 q0)
       | XH -> Npos (XI p0))
    | XH ->
      (match q with
       | XI q0 -> Npos (XO q0)
       | XO q0 -> Npos (XI q0)
       | XH -> N0)
  
  (** val shiftl_nat : positive -> nat -> positive **)
  
  let shiftl_nat p n0 =
    nat_iter n0 (fun x -> XO x) p
  
  (** val shiftr_nat : positive -> nat -> positive **)
  
  let shiftr_nat p n0 =
    nat_iter n0 div2 p
  
  (** val shiftl : positive -> n -> positive **)
  
  let shiftl p = function
  | N0 -> p
  | Npos n1 -> iter n1 (fun x -> XO x) p
  
  (** val shiftr : positive -> n -> positive **)
  
  let shiftr p = function
  | N0 -> p
  | Npos n1 -> iter n1 div2 p
  
  (** val testbit_nat : positive -> nat -> bool **)
  
  let rec testbit_nat p n0 =
    match p with
    | XI p0 ->
      (match n0 with
       | O -> True
       | S n' -> testbit_nat p0 n')
    | XO p0 ->
      (match n0 with
       | O -> False
       | S n' -> testbit_nat p0 n')
    | XH ->
      (match n0 with
       | O -> True
       | S n1 -> False)
  
  (** val testbit : positive -> n -> bool **)
  
  let rec testbit p n0 =
    match p with
    | XI p0 ->
      (match n0 with
       | N0 -> True
       | Npos n1 -> testbit p0 (pred_N n1))
    | XO p0 ->
      (match n0 with
       | N0 -> False
       | Npos n1 -> testbit p0 (pred_N n1))
    | XH ->
      (match n0 with
       | N0 -> True
       | Npos p0 -> False)
  
  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)
  
  let rec iter_op op p a =
    match p with
    | XI p0 -> op a (iter_op op p0 (op a a))
    | XO p0 -> iter_op op p0 (op a a)
    | XH -> a
  
  (** val to_nat : positive -> nat **)
  
  let to_nat x =
    iter_op plus x (S O)
  
  (** val of_nat : nat -> positive **)
  
  let rec of_nat = function
  | O -> XH
  | S x ->
    (match x with
     | O -> XH
     | S n1 -> succ (of_nat x))
  
  (** val of_succ_nat : nat -> positive **)
  
  let rec of_succ_nat = function
  | O -> XH
  | S x -> succ (of_succ_nat x)
 end

module Coq_Pos = 
 struct 
  type t = positive
  
  (** val succ : positive -> positive **)
  
  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH
  
  (** val add : positive -> positive -> positive **)
  
  let rec add x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XO p ->
      (match y with
       | XI q -> XI (add p q)
       | XO q -> XO (add p q)
       | XH -> XI p)
    | XH ->
      (match y with
       | XI q -> XO (succ q)
       | XO q -> XI q
       | XH -> XO XH)
  
  (** val add_carry : positive -> positive -> positive **)
  
  and add_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XI (add_carry p q)
       | XO q -> XO (add_carry p q)
       | XH -> XI (succ p))
    | XO p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XH ->
      (match y with
       | XI q -> XI (succ q)
       | XO q -> XO (succ q)
       | XH -> XI XH)
  
  (** val pred_double : positive -> positive **)
  
  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH
  
  (** val pred : positive -> positive **)
  
  let pred = function
  | XI p -> XO p
  | XO p -> pred_double p
  | XH -> XH
  
  (** val pred_N : positive -> n **)
  
  let pred_N = function
  | XI p -> Npos (XO p)
  | XO p -> Npos (pred_double p)
  | XH -> N0
  
  type mask = Pos.mask =
  | IsNul
  | IsPos of positive
  | IsNeg
  
  (** val mask_rect : 'a1 -> (positive -> 'a1) -> 'a1 -> mask -> 'a1 **)
  
  let mask_rect f f0 f1 = function
  | IsNul -> f
  | IsPos x -> f0 x
  | IsNeg -> f1
  
  (** val mask_rec : 'a1 -> (positive -> 'a1) -> 'a1 -> mask -> 'a1 **)
  
  let mask_rec f f0 f1 = function
  | IsNul -> f
  | IsPos x -> f0 x
  | IsNeg -> f1
  
  (** val succ_double_mask : mask -> mask **)
  
  let succ_double_mask = function
  | IsNul -> IsPos XH
  | IsPos p -> IsPos (XI p)
  | IsNeg -> IsNeg
  
  (** val double_mask : mask -> mask **)
  
  let double_mask = function
  | IsPos p -> IsPos (XO p)
  | x0 -> x0
  
  (** val double_pred_mask : positive -> mask **)
  
  let double_pred_mask = function
  | XI p -> IsPos (XO (XO p))
  | XO p -> IsPos (XO (pred_double p))
  | XH -> IsNul
  
  (** val pred_mask : mask -> mask **)
  
  let pred_mask = function
  | IsPos q ->
    (match q with
     | XH -> IsNul
     | _ -> IsPos (pred q))
  | _ -> IsNeg
  
  (** val sub_mask : positive -> positive -> mask **)
  
  let rec sub_mask x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double_mask (sub_mask p q)
       | XO q -> succ_double_mask (sub_mask p q)
       | XH -> IsPos (XO p))
    | XO p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XH ->
      (match y with
       | XH -> IsNul
       | _ -> IsNeg)
  
  (** val sub_mask_carry : positive -> positive -> mask **)
  
  and sub_mask_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> succ_double_mask (sub_mask_carry p q)
       | XO q -> double_mask (sub_mask p q)
       | XH -> IsPos (pred_double p))
    | XO p ->
      (match y with
       | XI q -> double_mask (sub_mask_carry p q)
       | XO q -> succ_double_mask (sub_mask_carry p q)
       | XH -> double_pred_mask p)
    | XH -> IsNeg
  
  (** val sub : positive -> positive -> positive **)
  
  let sub x y =
    match sub_mask x y with
    | IsPos z0 -> z0
    | _ -> XH
  
  (** val mul : positive -> positive -> positive **)
  
  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y
  
  (** val iter : positive -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)
  
  let rec iter n0 f x =
    match n0 with
    | XI n' -> f (iter n' f (iter n' f x))
    | XO n' -> iter n' f (iter n' f x)
    | XH -> f x
  
  (** val pow : positive -> positive -> positive **)
  
  let pow x y =
    iter y (mul x) XH
  
  (** val square : positive -> positive **)
  
  let rec square = function
  | XI p0 -> XI (XO (add (square p0) p0))
  | XO p0 -> XO (XO (square p0))
  | XH -> XH
  
  (** val div2 : positive -> positive **)
  
  let div2 = function
  | XI p0 -> p0
  | XO p0 -> p0
  | XH -> XH
  
  (** val div2_up : positive -> positive **)
  
  let div2_up = function
  | XI p0 -> succ p0
  | XO p0 -> p0
  | XH -> XH
  
  (** val size_nat : positive -> nat **)
  
  let rec size_nat = function
  | XI p0 -> S (size_nat p0)
  | XO p0 -> S (size_nat p0)
  | XH -> S O
  
  (** val size : positive -> positive **)
  
  let rec size = function
  | XI p0 -> succ (size p0)
  | XO p0 -> succ (size p0)
  | XH -> XH
  
  (** val compare_cont : positive -> positive -> comparison -> comparison **)
  
  let rec compare_cont x y r =
    match x with
    | XI p ->
      (match y with
       | XI q -> compare_cont p q r
       | XO q -> compare_cont p q Gt
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q -> compare_cont p q Lt
       | XO q -> compare_cont p q r
       | XH -> Gt)
    | XH ->
      (match y with
       | XH -> r
       | _ -> Lt)
  
  (** val compare : positive -> positive -> comparison **)
  
  let compare x y =
    compare_cont x y Eq
  
  (** val min : positive -> positive -> positive **)
  
  let min p p' =
    match compare p p' with
    | Gt -> p'
    | _ -> p
  
  (** val max : positive -> positive -> positive **)
  
  let max p p' =
    match compare p p' with
    | Gt -> p
    | _ -> p'
  
  (** val eqb : positive -> positive -> bool **)
  
  let rec eqb p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> eqb p0 q0
       | _ -> False)
    | XO p0 ->
      (match q with
       | XO q0 -> eqb p0 q0
       | _ -> False)
    | XH ->
      (match q with
       | XH -> True
       | _ -> False)
  
  (** val leb : positive -> positive -> bool **)
  
  let leb x y =
    match compare x y with
    | Gt -> False
    | _ -> True
  
  (** val ltb : positive -> positive -> bool **)
  
  let ltb x y =
    match compare x y with
    | Lt -> True
    | _ -> False
  
  (** val sqrtrem_step :
      (positive -> positive) -> (positive -> positive) -> (positive, mask)
      prod -> (positive, mask) prod **)
  
  let sqrtrem_step f g = function
  | Pair (s, y) ->
    (match y with
     | IsPos r ->
       let s' = XI (XO s) in
       let r' = g (f r) in
       (match leb s' r' with
        | True -> Pair ((XI s), (sub_mask r' s'))
        | False -> Pair ((XO s), (IsPos r')))
     | _ -> Pair ((XO s), (sub_mask (g (f XH)) (XO (XO XH)))))
  
  (** val sqrtrem : positive -> (positive, mask) prod **)
  
  let rec sqrtrem = function
  | XI p0 ->
    (match p0 with
     | XI p1 -> sqrtrem_step (fun x -> XI x) (fun x -> XI x) (sqrtrem p1)
     | XO p1 -> sqrtrem_step (fun x -> XO x) (fun x -> XI x) (sqrtrem p1)
     | XH -> Pair (XH, (IsPos (XO XH))))
  | XO p0 ->
    (match p0 with
     | XI p1 -> sqrtrem_step (fun x -> XI x) (fun x -> XO x) (sqrtrem p1)
     | XO p1 -> sqrtrem_step (fun x -> XO x) (fun x -> XO x) (sqrtrem p1)
     | XH -> Pair (XH, (IsPos XH)))
  | XH -> Pair (XH, IsNul)
  
  (** val sqrt : positive -> positive **)
  
  let sqrt p =
    fst (sqrtrem p)
  
  (** val gcdn : nat -> positive -> positive -> positive **)
  
  let rec gcdn n0 a b =
    match n0 with
    | O -> XH
    | S n1 ->
      (match a with
       | XI a' ->
         (match b with
          | XI b' ->
            (match compare a' b' with
             | Eq -> a
             | Lt -> gcdn n1 (sub b' a') a
             | Gt -> gcdn n1 (sub a' b') b)
          | XO b0 -> gcdn n1 a b0
          | XH -> XH)
       | XO a0 ->
         (match b with
          | XI p -> gcdn n1 a0 b
          | XO b0 -> XO (gcdn n1 a0 b0)
          | XH -> XH)
       | XH -> XH)
  
  (** val gcd : positive -> positive -> positive **)
  
  let gcd a b =
    gcdn (plus (size_nat a) (size_nat b)) a b
  
  (** val ggcdn :
      nat -> positive -> positive -> (positive, (positive, positive) prod)
      prod **)
  
  let rec ggcdn n0 a b =
    match n0 with
    | O -> Pair (XH, (Pair (a, b)))
    | S n1 ->
      (match a with
       | XI a' ->
         (match b with
          | XI b' ->
            (match compare a' b' with
             | Eq -> Pair (a, (Pair (XH, XH)))
             | Lt ->
               let Pair (g, p) = ggcdn n1 (sub b' a') a in
               let Pair (ba, aa) = p in
               Pair (g, (Pair (aa, (add aa (XO ba)))))
             | Gt ->
               let Pair (g, p) = ggcdn n1 (sub a' b') b in
               let Pair (ab, bb) = p in
               Pair (g, (Pair ((add bb (XO ab)), bb))))
          | XO b0 ->
            let Pair (g, p) = ggcdn n1 a b0 in
            let Pair (aa, bb) = p in Pair (g, (Pair (aa, (XO bb))))
          | XH -> Pair (XH, (Pair (a, XH))))
       | XO a0 ->
         (match b with
          | XI p ->
            let Pair (g, p0) = ggcdn n1 a0 b in
            let Pair (aa, bb) = p0 in Pair (g, (Pair ((XO aa), bb)))
          | XO b0 -> let Pair (g, p) = ggcdn n1 a0 b0 in Pair ((XO g), p)
          | XH -> Pair (XH, (Pair (a, XH))))
       | XH -> Pair (XH, (Pair (XH, b))))
  
  (** val ggcd :
      positive -> positive -> (positive, (positive, positive) prod) prod **)
  
  let ggcd a b =
    ggcdn (plus (size_nat a) (size_nat b)) a b
  
  (** val coq_Nsucc_double : n -> n **)
  
  let coq_Nsucc_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)
  
  (** val coq_Ndouble : n -> n **)
  
  let coq_Ndouble = function
  | N0 -> N0
  | Npos p -> Npos (XO p)
  
  (** val coq_lor : positive -> positive -> positive **)
  
  let rec coq_lor p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> XI (coq_lor p0 q0)
       | XO q0 -> XI (coq_lor p0 q0)
       | XH -> p)
    | XO p0 ->
      (match q with
       | XI q0 -> XI (coq_lor p0 q0)
       | XO q0 -> XO (coq_lor p0 q0)
       | XH -> XI p0)
    | XH ->
      (match q with
       | XO q0 -> XI q0
       | _ -> q)
  
  (** val coq_land : positive -> positive -> n **)
  
  let rec coq_land p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Nsucc_double (coq_land p0 q0)
       | XO q0 -> coq_Ndouble (coq_land p0 q0)
       | XH -> Npos XH)
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (coq_land p0 q0)
       | XO q0 -> coq_Ndouble (coq_land p0 q0)
       | XH -> N0)
    | XH ->
      (match q with
       | XO q0 -> N0
       | _ -> Npos XH)
  
  (** val ldiff : positive -> positive -> n **)
  
  let rec ldiff p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (ldiff p0 q0)
       | XO q0 -> coq_Nsucc_double (ldiff p0 q0)
       | XH -> Npos (XO p0))
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (ldiff p0 q0)
       | XO q0 -> coq_Ndouble (ldiff p0 q0)
       | XH -> Npos p)
    | XH ->
      (match q with
       | XO q0 -> Npos XH
       | _ -> N0)
  
  (** val coq_lxor : positive -> positive -> n **)
  
  let rec coq_lxor p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (coq_lxor p0 q0)
       | XO q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | XH -> Npos (XO p0))
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Nsucc_double (coq_lxor p0 q0)
       | XO q0 -> coq_Ndouble (coq_lxor p0 q0)
       | XH -> Npos (XI p0))
    | XH ->
      (match q with
       | XI q0 -> Npos (XO q0)
       | XO q0 -> Npos (XI q0)
       | XH -> N0)
  
  (** val shiftl_nat : positive -> nat -> positive **)
  
  let shiftl_nat p n0 =
    nat_iter n0 (fun x -> XO x) p
  
  (** val shiftr_nat : positive -> nat -> positive **)
  
  let shiftr_nat p n0 =
    nat_iter n0 div2 p
  
  (** val shiftl : positive -> n -> positive **)
  
  let shiftl p = function
  | N0 -> p
  | Npos n1 -> iter n1 (fun x -> XO x) p
  
  (** val shiftr : positive -> n -> positive **)
  
  let shiftr p = function
  | N0 -> p
  | Npos n1 -> iter n1 div2 p
  
  (** val testbit_nat : positive -> nat -> bool **)
  
  let rec testbit_nat p n0 =
    match p with
    | XI p0 ->
      (match n0 with
       | O -> True
       | S n' -> testbit_nat p0 n')
    | XO p0 ->
      (match n0 with
       | O -> False
       | S n' -> testbit_nat p0 n')
    | XH ->
      (match n0 with
       | O -> True
       | S n1 -> False)
  
  (** val testbit : positive -> n -> bool **)
  
  let rec testbit p n0 =
    match p with
    | XI p0 ->
      (match n0 with
       | N0 -> True
       | Npos n1 -> testbit p0 (pred_N n1))
    | XO p0 ->
      (match n0 with
       | N0 -> False
       | Npos n1 -> testbit p0 (pred_N n1))
    | XH ->
      (match n0 with
       | N0 -> True
       | Npos p0 -> False)
  
  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)
  
  let rec iter_op op p a =
    match p with
    | XI p0 -> op a (iter_op op p0 (op a a))
    | XO p0 -> iter_op op p0 (op a a)
    | XH -> a
  
  (** val to_nat : positive -> nat **)
  
  let to_nat x =
    iter_op plus x (S O)
  
  (** val of_nat : nat -> positive **)
  
  let rec of_nat = function
  | O -> XH
  | S x ->
    (match x with
     | O -> XH
     | S n1 -> succ (of_nat x))
  
  (** val of_succ_nat : nat -> positive **)
  
  let rec of_succ_nat = function
  | O -> XH
  | S x -> succ (of_succ_nat x)
  
  (** val eq_dec : positive -> positive -> sumbool **)
  
  let rec eq_dec p y0 =
    match p with
    | XI p0 ->
      (match y0 with
       | XI p1 -> eq_dec p0 p1
       | _ -> Right)
    | XO p0 ->
      (match y0 with
       | XO p1 -> eq_dec p0 p1
       | _ -> Right)
    | XH ->
      (match y0 with
       | XH -> Left
       | _ -> Right)
  
  (** val peano_rect : 'a1 -> (positive -> 'a1 -> 'a1) -> positive -> 'a1 **)
  
  let rec peano_rect a f p =
    let f2 = peano_rect (f XH a) (fun p0 x -> f (succ (XO p0)) (f (XO p0) x))
    in
    (match p with
     | XI q -> f (XO q) (f2 q)
     | XO q -> f2 q
     | XH -> a)
  
  (** val peano_rec : 'a1 -> (positive -> 'a1 -> 'a1) -> positive -> 'a1 **)
  
  let peano_rec =
    peano_rect
  
  type coq_PeanoView =
  | PeanoOne
  | PeanoSucc of positive * coq_PeanoView
  
  (** val coq_PeanoView_rect :
      'a1 -> (positive -> coq_PeanoView -> 'a1 -> 'a1) -> positive ->
      coq_PeanoView -> 'a1 **)
  
  let rec coq_PeanoView_rect f f0 p = function
  | PeanoOne -> f
  | PeanoSucc (p1, p2) -> f0 p1 p2 (coq_PeanoView_rect f f0 p1 p2)
  
  (** val coq_PeanoView_rec :
      'a1 -> (positive -> coq_PeanoView -> 'a1 -> 'a1) -> positive ->
      coq_PeanoView -> 'a1 **)
  
  let rec coq_PeanoView_rec f f0 p = function
  | PeanoOne -> f
  | PeanoSucc (p1, p2) -> f0 p1 p2 (coq_PeanoView_rec f f0 p1 p2)
  
  (** val peanoView_xO : positive -> coq_PeanoView -> coq_PeanoView **)
  
  let rec peanoView_xO p = function
  | PeanoOne -> PeanoSucc (XH, PeanoOne)
  | PeanoSucc (p0, q0) ->
    PeanoSucc ((succ (XO p0)), (PeanoSucc ((XO p0), (peanoView_xO p0 q0))))
  
  (** val peanoView_xI : positive -> coq_PeanoView -> coq_PeanoView **)
  
  let rec peanoView_xI p = function
  | PeanoOne -> PeanoSucc ((succ XH), (PeanoSucc (XH, PeanoOne)))
  | PeanoSucc (p0, q0) ->
    PeanoSucc ((succ (XI p0)), (PeanoSucc ((XI p0), (peanoView_xI p0 q0))))
  
  (** val peanoView : positive -> coq_PeanoView **)
  
  let rec peanoView = function
  | XI p0 -> peanoView_xI p0 (peanoView p0)
  | XO p0 -> peanoView_xO p0 (peanoView p0)
  | XH -> PeanoOne
  
  (** val coq_PeanoView_iter :
      'a1 -> (positive -> 'a1 -> 'a1) -> positive -> coq_PeanoView -> 'a1 **)
  
  let rec coq_PeanoView_iter a f p = function
  | PeanoOne -> a
  | PeanoSucc (p0, q0) -> f p0 (coq_PeanoView_iter a f p0 q0)
  
  (** val eqb_spec : positive -> positive -> reflect **)
  
  let eqb_spec x y =
    iff_reflect (eqb x y)
  
  (** val switch_Eq : comparison -> comparison -> comparison **)
  
  let switch_Eq c = function
  | Eq -> c
  | x -> x
  
  (** val mask2cmp : mask -> comparison **)
  
  let mask2cmp = function
  | IsNul -> Eq
  | IsPos p0 -> Gt
  | IsNeg -> Lt
  
  (** val leb_spec0 : positive -> positive -> reflect **)
  
  let leb_spec0 x y =
    iff_reflect (leb x y)
  
  (** val ltb_spec0 : positive -> positive -> reflect **)
  
  let ltb_spec0 x y =
    iff_reflect (ltb x y)
  
  module Private_Tac = 
   struct 
    
   end
  
  module Private_Dec = 
   struct 
    (** val max_case_strong :
        positive -> positive -> (positive -> positive -> __ -> 'a1 -> 'a1) ->
        (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
    
    let max_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat n0 (max n0 m) __ (hl __)
       | _ -> compat m (max n0 m) __ (hr __))
    
    (** val max_case :
        positive -> positive -> (positive -> positive -> __ -> 'a1 -> 'a1) ->
        'a1 -> 'a1 -> 'a1 **)
    
    let max_case n0 m x x0 x1 =
      max_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)
    
    (** val max_dec : positive -> positive -> sumbool **)
    
    let max_dec n0 m =
      max_case n0 m (fun x y _ h0 -> h0) Left Right
    
    (** val min_case_strong :
        positive -> positive -> (positive -> positive -> __ -> 'a1 -> 'a1) ->
        (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
    
    let min_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat m (min n0 m) __ (hr __)
       | _ -> compat n0 (min n0 m) __ (hl __))
    
    (** val min_case :
        positive -> positive -> (positive -> positive -> __ -> 'a1 -> 'a1) ->
        'a1 -> 'a1 -> 'a1 **)
    
    let min_case n0 m x x0 x1 =
      min_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)
    
    (** val min_dec : positive -> positive -> sumbool **)
    
    let min_dec n0 m =
      min_case n0 m (fun x y _ h0 -> h0) Left Right
   end
  
  (** val max_case_strong :
      positive -> positive -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let max_case_strong n0 m x x0 =
    Private_Dec.max_case_strong n0 m (fun x1 y _ x2 -> x2) x x0
  
  (** val max_case : positive -> positive -> 'a1 -> 'a1 -> 'a1 **)
  
  let max_case n0 m x x0 =
    max_case_strong n0 m (fun _ -> x) (fun _ -> x0)
  
  (** val max_dec : positive -> positive -> sumbool **)
  
  let max_dec =
    Private_Dec.max_dec
  
  (** val min_case_strong :
      positive -> positive -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let min_case_strong n0 m x x0 =
    Private_Dec.min_case_strong n0 m (fun x1 y _ x2 -> x2) x x0
  
  (** val min_case : positive -> positive -> 'a1 -> 'a1 -> 'a1 **)
  
  let min_case n0 m x x0 =
    min_case_strong n0 m (fun _ -> x) (fun _ -> x0)
  
  (** val min_dec : positive -> positive -> sumbool **)
  
  let min_dec =
    Private_Dec.min_dec
 end

module N = 
 struct 
  type t = n
  
  (** val zero : n **)
  
  let zero =
    N0
  
  (** val one : n **)
  
  let one =
    Npos XH
  
  (** val two : n **)
  
  let two =
    Npos (XO XH)
  
  (** val succ_double : n -> n **)
  
  let succ_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)
  
  (** val double : n -> n **)
  
  let double = function
  | N0 -> N0
  | Npos p -> Npos (XO p)
  
  (** val succ : n -> n **)
  
  let succ = function
  | N0 -> Npos XH
  | Npos p -> Npos (Coq_Pos.succ p)
  
  (** val pred : n -> n **)
  
  let pred = function
  | N0 -> N0
  | Npos p -> Coq_Pos.pred_N p
  
  (** val succ_pos : n -> positive **)
  
  let succ_pos = function
  | N0 -> XH
  | Npos p -> Coq_Pos.succ p
  
  (** val add : n -> n -> n **)
  
  let add n0 m =
    match n0 with
    | N0 -> m
    | Npos p ->
      (match m with
       | N0 -> n0
       | Npos q -> Npos (Coq_Pos.add p q))
  
  (** val sub : n -> n -> n **)
  
  let sub n0 m =
    match n0 with
    | N0 -> N0
    | Npos n' ->
      (match m with
       | N0 -> n0
       | Npos m' ->
         (match Coq_Pos.sub_mask n' m' with
          | Coq_Pos.IsPos p -> Npos p
          | _ -> N0))
  
  (** val mul : n -> n -> n **)
  
  let mul n0 m =
    match n0 with
    | N0 -> N0
    | Npos p ->
      (match m with
       | N0 -> N0
       | Npos q -> Npos (Coq_Pos.mul p q))
  
  (** val compare : n -> n -> comparison **)
  
  let compare n0 m =
    match n0 with
    | N0 ->
      (match m with
       | N0 -> Eq
       | Npos m' -> Lt)
    | Npos n' ->
      (match m with
       | N0 -> Gt
       | Npos m' -> Coq_Pos.compare n' m')
  
  (** val eqb : n -> n -> bool **)
  
  let rec eqb n0 m =
    match n0 with
    | N0 ->
      (match m with
       | N0 -> True
       | Npos p -> False)
    | Npos p ->
      (match m with
       | N0 -> False
       | Npos q -> Coq_Pos.eqb p q)
  
  (** val leb : n -> n -> bool **)
  
  let leb x y =
    match compare x y with
    | Gt -> False
    | _ -> True
  
  (** val ltb : n -> n -> bool **)
  
  let ltb x y =
    match compare x y with
    | Lt -> True
    | _ -> False
  
  (** val min : n -> n -> n **)
  
  let min n0 n' =
    match compare n0 n' with
    | Gt -> n'
    | _ -> n0
  
  (** val max : n -> n -> n **)
  
  let max n0 n' =
    match compare n0 n' with
    | Gt -> n0
    | _ -> n'
  
  (** val div2 : n -> n **)
  
  let div2 = function
  | N0 -> N0
  | Npos p0 ->
    (match p0 with
     | XI p -> Npos p
     | XO p -> Npos p
     | XH -> N0)
  
  (** val even : n -> bool **)
  
  let even = function
  | N0 -> True
  | Npos p ->
    (match p with
     | XO p0 -> True
     | _ -> False)
  
  (** val odd : n -> bool **)
  
  let odd n0 =
    negb (even n0)
  
  (** val pow : n -> n -> n **)
  
  let pow n0 = function
  | N0 -> Npos XH
  | Npos p0 ->
    (match n0 with
     | N0 -> N0
     | Npos q -> Npos (Coq_Pos.pow q p0))
  
  (** val square : n -> n **)
  
  let square = function
  | N0 -> N0
  | Npos p -> Npos (Coq_Pos.square p)
  
  (** val log2 : n -> n **)
  
  let log2 = function
  | N0 -> N0
  | Npos p0 ->
    (match p0 with
     | XI p -> Npos (Coq_Pos.size p)
     | XO p -> Npos (Coq_Pos.size p)
     | XH -> N0)
  
  (** val size : n -> n **)
  
  let size = function
  | N0 -> N0
  | Npos p -> Npos (Coq_Pos.size p)
  
  (** val size_nat : n -> nat **)
  
  let size_nat = function
  | N0 -> O
  | Npos p -> Coq_Pos.size_nat p
  
  (** val pos_div_eucl : positive -> n -> (n, n) prod **)
  
  let rec pos_div_eucl a b =
    match a with
    | XI a' ->
      let Pair (q, r) = pos_div_eucl a' b in
      let r' = succ_double r in
      (match leb b r' with
       | True -> Pair ((succ_double q), (sub r' b))
       | False -> Pair ((double q), r'))
    | XO a' ->
      let Pair (q, r) = pos_div_eucl a' b in
      let r' = double r in
      (match leb b r' with
       | True -> Pair ((succ_double q), (sub r' b))
       | False -> Pair ((double q), r'))
    | XH ->
      (match b with
       | N0 -> Pair (N0, (Npos XH))
       | Npos p ->
         (match p with
          | XH -> Pair ((Npos XH), N0)
          | _ -> Pair (N0, (Npos XH))))
  
  (** val div_eucl : n -> n -> (n, n) prod **)
  
  let div_eucl a b =
    match a with
    | N0 -> Pair (N0, N0)
    | Npos na ->
      (match b with
       | N0 -> Pair (N0, a)
       | Npos p -> pos_div_eucl na b)
  
  (** val div : n -> n -> n **)
  
  let div a b =
    fst (div_eucl a b)
  
  (** val modulo : n -> n -> n **)
  
  let modulo a b =
    snd (div_eucl a b)
  
  (** val gcd : n -> n -> n **)
  
  let gcd a b =
    match a with
    | N0 -> b
    | Npos p ->
      (match b with
       | N0 -> a
       | Npos q -> Npos (Coq_Pos.gcd p q))
  
  (** val ggcd : n -> n -> (n, (n, n) prod) prod **)
  
  let ggcd a b =
    match a with
    | N0 -> Pair (b, (Pair (N0, (Npos XH))))
    | Npos p ->
      (match b with
       | N0 -> Pair (a, (Pair ((Npos XH), N0)))
       | Npos q ->
         let Pair (g, p0) = Coq_Pos.ggcd p q in
         let Pair (aa, bb) = p0 in
         Pair ((Npos g), (Pair ((Npos aa), (Npos bb)))))
  
  (** val sqrtrem : n -> (n, n) prod **)
  
  let sqrtrem = function
  | N0 -> Pair (N0, N0)
  | Npos p ->
    let Pair (s, m) = Coq_Pos.sqrtrem p in
    (match m with
     | Coq_Pos.IsPos r -> Pair ((Npos s), (Npos r))
     | _ -> Pair ((Npos s), N0))
  
  (** val sqrt : n -> n **)
  
  let sqrt = function
  | N0 -> N0
  | Npos p -> Npos (Coq_Pos.sqrt p)
  
  (** val coq_lor : n -> n -> n **)
  
  let coq_lor n0 m =
    match n0 with
    | N0 -> m
    | Npos p ->
      (match m with
       | N0 -> n0
       | Npos q -> Npos (Coq_Pos.coq_lor p q))
  
  (** val coq_land : n -> n -> n **)
  
  let coq_land n0 m =
    match n0 with
    | N0 -> N0
    | Npos p ->
      (match m with
       | N0 -> N0
       | Npos q -> Coq_Pos.coq_land p q)
  
  (** val ldiff : n -> n -> n **)
  
  let rec ldiff n0 m =
    match n0 with
    | N0 -> N0
    | Npos p ->
      (match m with
       | N0 -> n0
       | Npos q -> Coq_Pos.ldiff p q)
  
  (** val coq_lxor : n -> n -> n **)
  
  let coq_lxor n0 m =
    match n0 with
    | N0 -> m
    | Npos p ->
      (match m with
       | N0 -> n0
       | Npos q -> Coq_Pos.coq_lxor p q)
  
  (** val shiftl_nat : n -> nat -> n **)
  
  let shiftl_nat a n0 =
    nat_iter n0 double a
  
  (** val shiftr_nat : n -> nat -> n **)
  
  let shiftr_nat a n0 =
    nat_iter n0 div2 a
  
  (** val shiftl : n -> n -> n **)
  
  let shiftl a n0 =
    match a with
    | N0 -> N0
    | Npos a0 -> Npos (Coq_Pos.shiftl a0 n0)
  
  (** val shiftr : n -> n -> n **)
  
  let shiftr a = function
  | N0 -> a
  | Npos p -> Coq_Pos.iter p div2 a
  
  (** val testbit_nat : n -> nat -> bool **)
  
  let testbit_nat = function
  | N0 -> (fun x -> False)
  | Npos p -> Coq_Pos.testbit_nat p
  
  (** val testbit : n -> n -> bool **)
  
  let testbit a n0 =
    match a with
    | N0 -> False
    | Npos p -> Coq_Pos.testbit p n0
  
  (** val to_nat : n -> nat **)
  
  let to_nat = function
  | N0 -> O
  | Npos p -> Coq_Pos.to_nat p
  
  (** val of_nat : nat -> n **)
  
  let of_nat = function
  | O -> N0
  | S n' -> Npos (Coq_Pos.of_succ_nat n')
  
  (** val iter : n -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)
  
  let iter n0 f x =
    match n0 with
    | N0 -> x
    | Npos p -> Coq_Pos.iter p f x
  
  (** val eq_dec : n -> n -> sumbool **)
  
  let eq_dec n0 m =
    match n0 with
    | N0 ->
      (match m with
       | N0 -> Left
       | Npos p -> Right)
    | Npos x ->
      (match m with
       | N0 -> Right
       | Npos p0 -> Coq_Pos.eq_dec x p0)
  
  (** val discr : n -> positive sumor **)
  
  let discr = function
  | N0 -> Inright
  | Npos p -> Inleft p
  
  (** val binary_rect :
      'a1 -> (n -> 'a1 -> 'a1) -> (n -> 'a1 -> 'a1) -> n -> 'a1 **)
  
  let binary_rect f0 f2 fS2 n0 =
    let f2' = fun p -> f2 (Npos p) in
    let fS2' = fun p -> fS2 (Npos p) in
    (match n0 with
     | N0 -> f0
     | Npos p ->
       let rec f = function
       | XI p1 -> fS2' p1 (f p1)
       | XO p1 -> f2' p1 (f p1)
       | XH -> fS2 N0 f0
       in f p)
  
  (** val binary_rec :
      'a1 -> (n -> 'a1 -> 'a1) -> (n -> 'a1 -> 'a1) -> n -> 'a1 **)
  
  let binary_rec =
    binary_rect
  
  (** val peano_rect : 'a1 -> (n -> 'a1 -> 'a1) -> n -> 'a1 **)
  
  let peano_rect f0 f n0 =
    let f' = fun p -> f (Npos p) in
    (match n0 with
     | N0 -> f0
     | Npos p -> Coq_Pos.peano_rect (f N0 f0) f' p)
  
  (** val peano_rec : 'a1 -> (n -> 'a1 -> 'a1) -> n -> 'a1 **)
  
  let peano_rec =
    peano_rect
  
  (** val leb_spec0 : n -> n -> reflect **)
  
  let leb_spec0 x y =
    iff_reflect (leb x y)
  
  (** val ltb_spec0 : n -> n -> reflect **)
  
  let ltb_spec0 x y =
    iff_reflect (ltb x y)
  
  module Private_BootStrap = 
   struct 
    
   end
  
  (** val recursion : 'a1 -> (n -> 'a1 -> 'a1) -> n -> 'a1 **)
  
  let recursion x =
    peano_rect x
  
  module Private_OrderTac = 
   struct 
    module IsTotal = 
     struct 
      
     end
    
    module Tac = 
     struct 
      
     end
   end
  
  module Private_NZPow = 
   struct 
    
   end
  
  module Private_NZSqrt = 
   struct 
    
   end
  
  (** val sqrt_up : n -> n **)
  
  let sqrt_up a =
    match compare N0 a with
    | Lt -> succ (sqrt (pred a))
    | _ -> N0
  
  (** val log2_up : n -> n **)
  
  let log2_up a =
    match compare (Npos XH) a with
    | Lt -> succ (log2 (pred a))
    | _ -> N0
  
  module Private_NZDiv = 
   struct 
    
   end
  
  (** val lcm : n -> n -> n **)
  
  let lcm a b =
    mul a (div b (gcd a b))
  
  (** val eqb_spec : n -> n -> reflect **)
  
  let eqb_spec x y =
    iff_reflect (eqb x y)
  
  (** val b2n : bool -> n **)
  
  let b2n = function
  | True -> Npos XH
  | False -> N0
  
  (** val setbit : n -> n -> n **)
  
  let setbit a n0 =
    coq_lor a (shiftl (Npos XH) n0)
  
  (** val clearbit : n -> n -> n **)
  
  let clearbit a n0 =
    ldiff a (shiftl (Npos XH) n0)
  
  (** val ones : n -> n **)
  
  let ones n0 =
    pred (shiftl (Npos XH) n0)
  
  (** val lnot : n -> n -> n **)
  
  let lnot a n0 =
    coq_lxor a (ones n0)
  
  module Private_Tac = 
   struct 
    
   end
  
  module Private_Dec = 
   struct 
    (** val max_case_strong :
        n -> n -> (n -> n -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1)
        -> 'a1 **)
    
    let max_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat n0 (max n0 m) __ (hl __)
       | _ -> compat m (max n0 m) __ (hr __))
    
    (** val max_case :
        n -> n -> (n -> n -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
    
    let max_case n0 m x x0 x1 =
      max_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)
    
    (** val max_dec : n -> n -> sumbool **)
    
    let max_dec n0 m =
      max_case n0 m (fun x y _ h0 -> h0) Left Right
    
    (** val min_case_strong :
        n -> n -> (n -> n -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1)
        -> 'a1 **)
    
    let min_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat m (min n0 m) __ (hr __)
       | _ -> compat n0 (min n0 m) __ (hl __))
    
    (** val min_case :
        n -> n -> (n -> n -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
    
    let min_case n0 m x x0 x1 =
      min_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)
    
    (** val min_dec : n -> n -> sumbool **)
    
    let min_dec n0 m =
      min_case n0 m (fun x y _ h0 -> h0) Left Right
   end
  
  (** val max_case_strong : n -> n -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let max_case_strong n0 m x x0 =
    Private_Dec.max_case_strong n0 m (fun x1 y _ x2 -> x2) x x0
  
  (** val max_case : n -> n -> 'a1 -> 'a1 -> 'a1 **)
  
  let max_case n0 m x x0 =
    max_case_strong n0 m (fun _ -> x) (fun _ -> x0)
  
  (** val max_dec : n -> n -> sumbool **)
  
  let max_dec =
    Private_Dec.max_dec
  
  (** val min_case_strong : n -> n -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let min_case_strong n0 m x x0 =
    Private_Dec.min_case_strong n0 m (fun x1 y _ x2 -> x2) x x0
  
  (** val min_case : n -> n -> 'a1 -> 'a1 -> 'a1 **)
  
  let min_case n0 m x x0 =
    min_case_strong n0 m (fun _ -> x) (fun _ -> x0)
  
  (** val min_dec : n -> n -> sumbool **)
  
  let min_dec =
    Private_Dec.min_dec
 end

module Z = 
 struct 
  type t = z
  
  (** val zero : z **)
  
  let zero =
    Z0
  
  (** val one : z **)
  
  let one =
    Zpos XH
  
  (** val two : z **)
  
  let two =
    Zpos (XO XH)
  
  (** val double : z -> z **)
  
  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (XO p)
  | Zneg p -> Zneg (XO p)
  
  (** val succ_double : z -> z **)
  
  let succ_double = function
  | Z0 -> Zpos XH
  | Zpos p -> Zpos (XI p)
  | Zneg p -> Zneg (Coq_Pos.pred_double p)
  
  (** val pred_double : z -> z **)
  
  let pred_double = function
  | Z0 -> Zneg XH
  | Zpos p -> Zpos (Coq_Pos.pred_double p)
  | Zneg p -> Zneg (XI p)
  
  (** val pos_sub : positive -> positive -> z **)
  
  let rec pos_sub x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double (pos_sub p q)
       | XO q -> succ_double (pos_sub p q)
       | XH -> Zpos (XO p))
    | XO p ->
      (match y with
       | XI q -> pred_double (pos_sub p q)
       | XO q -> double (pos_sub p q)
       | XH -> Zpos (Coq_Pos.pred_double p))
    | XH ->
      (match y with
       | XI q -> Zneg (XO q)
       | XO q -> Zneg (Coq_Pos.pred_double q)
       | XH -> Z0)
  
  (** val add : z -> z -> z **)
  
  let add x y =
    match x with
    | Z0 -> y
    | Zpos x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> Zpos (Coq_Pos.add x' y')
       | Zneg y' -> pos_sub x' y')
    | Zneg x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> pos_sub y' x'
       | Zneg y' -> Zneg (Coq_Pos.add x' y'))
  
  (** val opp : z -> z **)
  
  let opp = function
  | Z0 -> Z0
  | Zpos x0 -> Zneg x0
  | Zneg x0 -> Zpos x0
  
  (** val succ : z -> z **)
  
  let succ x =
    add x (Zpos XH)
  
  (** val pred : z -> z **)
  
  let pred x =
    add x (Zneg XH)
  
  (** val sub : z -> z -> z **)
  
  let sub m n0 =
    add m (opp n0)
  
  (** val mul : z -> z -> z **)
  
  let mul x y =
    match x with
    | Z0 -> Z0
    | Zpos x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zpos (Coq_Pos.mul x' y')
       | Zneg y' -> Zneg (Coq_Pos.mul x' y'))
    | Zneg x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zneg (Coq_Pos.mul x' y')
       | Zneg y' -> Zpos (Coq_Pos.mul x' y'))
  
  (** val pow_pos : z -> positive -> z **)
  
  let pow_pos z0 n0 =
    Coq_Pos.iter n0 (mul z0) (Zpos XH)
  
  (** val pow : z -> z -> z **)
  
  let pow x = function
  | Z0 -> Zpos XH
  | Zpos p -> pow_pos x p
  | Zneg p -> Z0
  
  (** val square : z -> z **)
  
  let square = function
  | Z0 -> Z0
  | Zpos p -> Zpos (Coq_Pos.square p)
  | Zneg p -> Zpos (Coq_Pos.square p)
  
  (** val compare : z -> z -> comparison **)
  
  let compare x y =
    match x with
    | Z0 ->
      (match y with
       | Z0 -> Eq
       | Zpos y' -> Lt
       | Zneg y' -> Gt)
    | Zpos x' ->
      (match y with
       | Zpos y' -> Coq_Pos.compare x' y'
       | _ -> Gt)
    | Zneg x' ->
      (match y with
       | Zneg y' -> compOpp (Coq_Pos.compare x' y')
       | _ -> Lt)
  
  (** val sgn : z -> z **)
  
  let sgn = function
  | Z0 -> Z0
  | Zpos p -> Zpos XH
  | Zneg p -> Zneg XH
  
  (** val leb : z -> z -> bool **)
  
  let leb x y =
    match compare x y with
    | Gt -> False
    | _ -> True
  
  (** val ltb : z -> z -> bool **)
  
  let ltb x y =
    match compare x y with
    | Lt -> True
    | _ -> False
  
  (** val geb : z -> z -> bool **)
  
  let geb x y =
    match compare x y with
    | Lt -> False
    | _ -> True
  
  (** val gtb : z -> z -> bool **)
  
  let gtb x y =
    match compare x y with
    | Gt -> True
    | _ -> False
  
  (** val eqb : z -> z -> bool **)
  
  let rec eqb x y =
    match x with
    | Z0 ->
      (match y with
       | Z0 -> True
       | _ -> False)
    | Zpos p ->
      (match y with
       | Zpos q -> Coq_Pos.eqb p q
       | _ -> False)
    | Zneg p ->
      (match y with
       | Zneg q -> Coq_Pos.eqb p q
       | _ -> False)
  
  (** val max : z -> z -> z **)
  
  let max n0 m =
    match compare n0 m with
    | Lt -> m
    | _ -> n0
  
  (** val min : z -> z -> z **)
  
  let min n0 m =
    match compare n0 m with
    | Gt -> m
    | _ -> n0
  
  (** val abs : z -> z **)
  
  let abs = function
  | Zneg p -> Zpos p
  | x -> x
  
  (** val abs_nat : z -> nat **)
  
  let abs_nat = function
  | Z0 -> O
  | Zpos p -> Coq_Pos.to_nat p
  | Zneg p -> Coq_Pos.to_nat p
  
  (** val abs_N : z -> n **)
  
  let abs_N = function
  | Z0 -> N0
  | Zpos p -> Npos p
  | Zneg p -> Npos p
  
  (** val to_nat : z -> nat **)
  
  let to_nat = function
  | Zpos p -> Coq_Pos.to_nat p
  | _ -> O
  
  (** val to_N : z -> n **)
  
  let to_N = function
  | Zpos p -> Npos p
  | _ -> N0
  
  (** val of_nat : nat -> z **)
  
  let of_nat = function
  | O -> Z0
  | S n1 -> Zpos (Coq_Pos.of_succ_nat n1)
  
  (** val of_N : n -> z **)
  
  let of_N = function
  | N0 -> Z0
  | Npos p -> Zpos p
  
  (** val to_pos : z -> positive **)
  
  let to_pos = function
  | Zpos p -> p
  | _ -> XH
  
  (** val iter : z -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)
  
  let iter n0 f x =
    match n0 with
    | Zpos p -> Coq_Pos.iter p f x
    | _ -> x
  
  (** val pos_div_eucl : positive -> z -> (z, z) prod **)
  
  let rec pos_div_eucl a b =
    match a with
    | XI a' ->
      let Pair (q, r) = pos_div_eucl a' b in
      let r' = add (mul (Zpos (XO XH)) r) (Zpos XH) in
      (match ltb r' b with
       | True -> Pair ((mul (Zpos (XO XH)) q), r')
       | False -> Pair ((add (mul (Zpos (XO XH)) q) (Zpos XH)), (sub r' b)))
    | XO a' ->
      let Pair (q, r) = pos_div_eucl a' b in
      let r' = mul (Zpos (XO XH)) r in
      (match ltb r' b with
       | True -> Pair ((mul (Zpos (XO XH)) q), r')
       | False -> Pair ((add (mul (Zpos (XO XH)) q) (Zpos XH)), (sub r' b)))
    | XH ->
      (match leb (Zpos (XO XH)) b with
       | True -> Pair (Z0, (Zpos XH))
       | False -> Pair ((Zpos XH), Z0))
  
  (** val div_eucl : z -> z -> (z, z) prod **)
  
  let div_eucl a b =
    match a with
    | Z0 -> Pair (Z0, Z0)
    | Zpos a' ->
      (match b with
       | Z0 -> Pair (Z0, Z0)
       | Zpos p -> pos_div_eucl a' b
       | Zneg b' ->
         let Pair (q, r) = pos_div_eucl a' (Zpos b') in
         (match r with
          | Z0 -> Pair ((opp q), Z0)
          | _ -> Pair ((opp (add q (Zpos XH))), (add b r))))
    | Zneg a' ->
      (match b with
       | Z0 -> Pair (Z0, Z0)
       | Zpos p ->
         let Pair (q, r) = pos_div_eucl a' b in
         (match r with
          | Z0 -> Pair ((opp q), Z0)
          | _ -> Pair ((opp (add q (Zpos XH))), (sub b r)))
       | Zneg b' ->
         let Pair (q, r) = pos_div_eucl a' (Zpos b') in Pair (q, (opp r)))
  
  (** val div : z -> z -> z **)
  
  let div a b =
    let Pair (q, x) = div_eucl a b in q
  
  (** val modulo : z -> z -> z **)
  
  let modulo a b =
    let Pair (x, r) = div_eucl a b in r
  
  (** val quotrem : z -> z -> (z, z) prod **)
  
  let quotrem a b =
    match a with
    | Z0 -> Pair (Z0, Z0)
    | Zpos a0 ->
      (match b with
       | Z0 -> Pair (Z0, a)
       | Zpos b0 ->
         let Pair (q, r) = N.pos_div_eucl a0 (Npos b0) in
         Pair ((of_N q), (of_N r))
       | Zneg b0 ->
         let Pair (q, r) = N.pos_div_eucl a0 (Npos b0) in
         Pair ((opp (of_N q)), (of_N r)))
    | Zneg a0 ->
      (match b with
       | Z0 -> Pair (Z0, a)
       | Zpos b0 ->
         let Pair (q, r) = N.pos_div_eucl a0 (Npos b0) in
         Pair ((opp (of_N q)), (opp (of_N r)))
       | Zneg b0 ->
         let Pair (q, r) = N.pos_div_eucl a0 (Npos b0) in
         Pair ((of_N q), (opp (of_N r))))
  
  (** val quot : z -> z -> z **)
  
  let quot a b =
    fst (quotrem a b)
  
  (** val rem : z -> z -> z **)
  
  let rem a b =
    snd (quotrem a b)
  
  (** val even : z -> bool **)
  
  let even = function
  | Z0 -> True
  | Zpos p ->
    (match p with
     | XO p0 -> True
     | _ -> False)
  | Zneg p ->
    (match p with
     | XO p0 -> True
     | _ -> False)
  
  (** val odd : z -> bool **)
  
  let odd = function
  | Z0 -> False
  | Zpos p ->
    (match p with
     | XO p0 -> False
     | _ -> True)
  | Zneg p ->
    (match p with
     | XO p0 -> False
     | _ -> True)
  
  (** val div2 : z -> z **)
  
  let div2 = function
  | Z0 -> Z0
  | Zpos p ->
    (match p with
     | XH -> Z0
     | _ -> Zpos (Coq_Pos.div2 p))
  | Zneg p -> Zneg (Coq_Pos.div2_up p)
  
  (** val quot2 : z -> z **)
  
  let quot2 = function
  | Z0 -> Z0
  | Zpos p ->
    (match p with
     | XH -> Z0
     | _ -> Zpos (Coq_Pos.div2 p))
  | Zneg p ->
    (match p with
     | XH -> Z0
     | _ -> Zneg (Coq_Pos.div2 p))
  
  (** val log2 : z -> z **)
  
  let log2 = function
  | Zpos p0 ->
    (match p0 with
     | XI p -> Zpos (Coq_Pos.size p)
     | XO p -> Zpos (Coq_Pos.size p)
     | XH -> Z0)
  | _ -> Z0
  
  (** val sqrtrem : z -> (z, z) prod **)
  
  let sqrtrem = function
  | Zpos p ->
    let Pair (s, m) = Coq_Pos.sqrtrem p in
    (match m with
     | Coq_Pos.IsPos r -> Pair ((Zpos s), (Zpos r))
     | _ -> Pair ((Zpos s), Z0))
  | _ -> Pair (Z0, Z0)
  
  (** val sqrt : z -> z **)
  
  let sqrt = function
  | Zpos p -> Zpos (Coq_Pos.sqrt p)
  | _ -> Z0
  
  (** val gcd : z -> z -> z **)
  
  let gcd a b =
    match a with
    | Z0 -> abs b
    | Zpos a0 ->
      (match b with
       | Z0 -> abs a
       | Zpos b0 -> Zpos (Coq_Pos.gcd a0 b0)
       | Zneg b0 -> Zpos (Coq_Pos.gcd a0 b0))
    | Zneg a0 ->
      (match b with
       | Z0 -> abs a
       | Zpos b0 -> Zpos (Coq_Pos.gcd a0 b0)
       | Zneg b0 -> Zpos (Coq_Pos.gcd a0 b0))
  
  (** val ggcd : z -> z -> (z, (z, z) prod) prod **)
  
  let ggcd a b =
    match a with
    | Z0 -> Pair ((abs b), (Pair (Z0, (sgn b))))
    | Zpos a0 ->
      (match b with
       | Z0 -> Pair ((abs a), (Pair ((sgn a), Z0)))
       | Zpos b0 ->
         let Pair (g, p) = Coq_Pos.ggcd a0 b0 in
         let Pair (aa, bb) = p in
         Pair ((Zpos g), (Pair ((Zpos aa), (Zpos bb))))
       | Zneg b0 ->
         let Pair (g, p) = Coq_Pos.ggcd a0 b0 in
         let Pair (aa, bb) = p in
         Pair ((Zpos g), (Pair ((Zpos aa), (Zneg bb)))))
    | Zneg a0 ->
      (match b with
       | Z0 -> Pair ((abs a), (Pair ((sgn a), Z0)))
       | Zpos b0 ->
         let Pair (g, p) = Coq_Pos.ggcd a0 b0 in
         let Pair (aa, bb) = p in
         Pair ((Zpos g), (Pair ((Zneg aa), (Zpos bb))))
       | Zneg b0 ->
         let Pair (g, p) = Coq_Pos.ggcd a0 b0 in
         let Pair (aa, bb) = p in
         Pair ((Zpos g), (Pair ((Zneg aa), (Zneg bb)))))
  
  (** val testbit : z -> z -> bool **)
  
  let testbit a = function
  | Z0 -> odd a
  | Zpos p ->
    (match a with
     | Z0 -> False
     | Zpos a0 -> Coq_Pos.testbit a0 (Npos p)
     | Zneg a0 -> negb (N.testbit (Coq_Pos.pred_N a0) (Npos p)))
  | Zneg p -> False
  
  (** val shiftl : z -> z -> z **)
  
  let shiftl a = function
  | Z0 -> a
  | Zpos p -> Coq_Pos.iter p (mul (Zpos (XO XH))) a
  | Zneg p -> Coq_Pos.iter p div2 a
  
  (** val shiftr : z -> z -> z **)
  
  let shiftr a n0 =
    shiftl a (opp n0)
  
  (** val coq_lor : z -> z -> z **)
  
  let coq_lor a b =
    match a with
    | Z0 -> b
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zpos (Coq_Pos.coq_lor a0 b0)
       | Zneg b0 -> Zneg (N.succ_pos (N.ldiff (Coq_Pos.pred_N b0) (Npos a0))))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zneg (N.succ_pos (N.ldiff (Coq_Pos.pred_N a0) (Npos b0)))
       | Zneg b0 ->
         Zneg
           (N.succ_pos (N.coq_land (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0))))
  
  (** val coq_land : z -> z -> z **)
  
  let coq_land a b =
    match a with
    | Z0 -> Z0
    | Zpos a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (Coq_Pos.coq_land a0 b0)
       | Zneg b0 -> of_N (N.ldiff (Npos a0) (Coq_Pos.pred_N b0)))
    | Zneg a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (N.ldiff (Npos b0) (Coq_Pos.pred_N a0))
       | Zneg b0 ->
         Zneg
           (N.succ_pos (N.coq_lor (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0))))
  
  (** val ldiff : z -> z -> z **)
  
  let ldiff a b =
    match a with
    | Z0 -> Z0
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> of_N (Coq_Pos.ldiff a0 b0)
       | Zneg b0 -> of_N (N.coq_land (Npos a0) (Coq_Pos.pred_N b0)))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 ->
         Zneg (N.succ_pos (N.coq_lor (Coq_Pos.pred_N a0) (Npos b0)))
       | Zneg b0 -> of_N (N.ldiff (Coq_Pos.pred_N b0) (Coq_Pos.pred_N a0)))
  
  (** val coq_lxor : z -> z -> z **)
  
  let coq_lxor a b =
    match a with
    | Z0 -> b
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> of_N (Coq_Pos.coq_lxor a0 b0)
       | Zneg b0 ->
         Zneg (N.succ_pos (N.coq_lxor (Npos a0) (Coq_Pos.pred_N b0))))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 ->
         Zneg (N.succ_pos (N.coq_lxor (Coq_Pos.pred_N a0) (Npos b0)))
       | Zneg b0 -> of_N (N.coq_lxor (Coq_Pos.pred_N a0) (Coq_Pos.pred_N b0)))
  
  (** val eq_dec : z -> z -> sumbool **)
  
  let eq_dec x y =
    match x with
    | Z0 ->
      (match y with
       | Z0 -> Left
       | _ -> Right)
    | Zpos x0 ->
      (match y with
       | Zpos p0 -> Coq_Pos.eq_dec x0 p0
       | _ -> Right)
    | Zneg x0 ->
      (match y with
       | Zneg p0 -> Coq_Pos.eq_dec x0 p0
       | _ -> Right)
  
  module Private_BootStrap = 
   struct 
    
   end
  
  (** val leb_spec0 : z -> z -> reflect **)
  
  let leb_spec0 x y =
    iff_reflect (leb x y)
  
  (** val ltb_spec0 : z -> z -> reflect **)
  
  let ltb_spec0 x y =
    iff_reflect (ltb x y)
  
  module Private_OrderTac = 
   struct 
    module IsTotal = 
     struct 
      
     end
    
    module Tac = 
     struct 
      
     end
   end
  
  (** val sqrt_up : z -> z **)
  
  let sqrt_up a =
    match compare Z0 a with
    | Lt -> succ (sqrt (pred a))
    | _ -> Z0
  
  (** val log2_up : z -> z **)
  
  let log2_up a =
    match compare (Zpos XH) a with
    | Lt -> succ (log2 (pred a))
    | _ -> Z0
  
  module Private_NZDiv = 
   struct 
    
   end
  
  module Private_Div = 
   struct 
    module Quot2Div = 
     struct 
      (** val div : z -> z -> z **)
      
      let div =
        quot
      
      (** val modulo : z -> z -> z **)
      
      let modulo =
        rem
     end
    
    module NZQuot = 
     struct 
      
     end
   end
  
  (** val lcm : z -> z -> z **)
  
  let lcm a b =
    abs (mul a (div b (gcd a b)))
  
  (** val eqb_spec : z -> z -> reflect **)
  
  let eqb_spec x y =
    iff_reflect (eqb x y)
  
  (** val b2z : bool -> z **)
  
  let b2z = function
  | True -> Zpos XH
  | False -> Z0
  
  (** val setbit : z -> z -> z **)
  
  let setbit a n0 =
    coq_lor a (shiftl (Zpos XH) n0)
  
  (** val clearbit : z -> z -> z **)
  
  let clearbit a n0 =
    ldiff a (shiftl (Zpos XH) n0)
  
  (** val lnot : z -> z **)
  
  let lnot a =
    pred (opp a)
  
  (** val ones : z -> z **)
  
  let ones n0 =
    pred (shiftl (Zpos XH) n0)
  
  module Private_Tac = 
   struct 
    
   end
  
  module Private_Dec = 
   struct 
    (** val max_case_strong :
        z -> z -> (z -> z -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1)
        -> 'a1 **)
    
    let max_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat n0 (max n0 m) __ (hl __)
       | _ -> compat m (max n0 m) __ (hr __))
    
    (** val max_case :
        z -> z -> (z -> z -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
    
    let max_case n0 m x x0 x1 =
      max_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)
    
    (** val max_dec : z -> z -> sumbool **)
    
    let max_dec n0 m =
      max_case n0 m (fun x y _ h0 -> h0) Left Right
    
    (** val min_case_strong :
        z -> z -> (z -> z -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1)
        -> 'a1 **)
    
    let min_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat m (min n0 m) __ (hr __)
       | _ -> compat n0 (min n0 m) __ (hl __))
    
    (** val min_case :
        z -> z -> (z -> z -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)
    
    let min_case n0 m x x0 x1 =
      min_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)
    
    (** val min_dec : z -> z -> sumbool **)
    
    let min_dec n0 m =
      min_case n0 m (fun x y _ h0 -> h0) Left Right
   end
  
  (** val max_case_strong : z -> z -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let max_case_strong n0 m x x0 =
    Private_Dec.max_case_strong n0 m (fun x1 y _ x2 -> x2) x x0
  
  (** val max_case : z -> z -> 'a1 -> 'a1 -> 'a1 **)
  
  let max_case n0 m x x0 =
    max_case_strong n0 m (fun _ -> x) (fun _ -> x0)
  
  (** val max_dec : z -> z -> sumbool **)
  
  let max_dec =
    Private_Dec.max_dec
  
  (** val min_case_strong : z -> z -> (__ -> 'a1) -> (__ -> 'a1) -> 'a1 **)
  
  let min_case_strong n0 m x x0 =
    Private_Dec.min_case_strong n0 m (fun x1 y _ x2 -> x2) x x0
  
  (** val min_case : z -> z -> 'a1 -> 'a1 -> 'a1 **)
  
  let min_case n0 m x x0 =
    min_case_strong n0 m (fun _ -> x) (fun _ -> x0)
  
  (** val min_dec : z -> z -> sumbool **)
  
  let min_dec =
    Private_Dec.min_dec
 end

(** val z_gt_dec : z -> z -> sumbool **)

let z_gt_dec x y =
  match Z.compare x y with
  | Gt -> Left
  | _ -> Right

(** val z_ge_dec : z -> z -> sumbool **)

let z_ge_dec x y =
  match Z.compare x y with
  | Lt -> Right
  | _ -> Left

(** val z_gt_le_dec : z -> z -> sumbool **)

let z_gt_le_dec x y =
  z_gt_dec x y

(** val z_ge_lt_dec : z -> z -> sumbool **)

let z_ge_lt_dec x y =
  z_ge_dec x y

(** val size0 : nat **)

let size0 =
  S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S O))))))))))))))))))))))))))))))

type digits =
| D0
| D1

type int31 =
| I31 of digits * digits * digits * digits * digits * digits * digits
   * digits * digits * digits * digits * digits * digits * digits * digits
   * digits * digits * digits * digits * digits * digits * digits * digits
   * digits * digits * digits * digits * digits * digits * digits * digits

(** val on : int31 **)

let on =
  I31 (D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0,
    D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0)

(** val in0 : int31 **)

let in0 =
  I31 (D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0,
    D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D1)

(** val sneakr : digits -> int31 -> int31 **)

let sneakr b = function
| I31 (a, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
       a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28,
       a29) ->
  I31 (b, a, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28)

(** val sneakl : digits -> int31 -> int31 **)

let sneakl b = function
| I31 (d, a, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
       a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28) ->
  I31 (a, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, b)

(** val shiftr0 : int31 -> int31 **)

let shiftr0 =
  sneakr D0

(** val twice : int31 -> int31 **)

let twice =
  sneakl D0

(** val twice_plus_one : int31 -> int31 **)

let twice_plus_one =
  sneakl D1

(** val firstr : int31 -> digits **)

let firstr = function
| I31 (x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14,
       x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28,
       d) ->
  d

(** val iszero : int31 -> bool **)

let iszero = function
| I31 (a, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14,
       a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28,
       a29) ->
  (match a with
   | D0 ->
     (match a0 with
      | D0 ->
        (match a1 with
         | D0 ->
           (match a2 with
            | D0 ->
              (match a3 with
               | D0 ->
                 (match a4 with
                  | D0 ->
                    (match a5 with
                     | D0 ->
                       (match a6 with
                        | D0 ->
                          (match a7 with
                           | D0 ->
                             (match a8 with
                              | D0 ->
                                (match a9 with
                                 | D0 ->
                                   (match a10 with
                                    | D0 ->
                                      (match a11 with
                                       | D0 ->
                                         (match a12 with
                                          | D0 ->
                                            (match a13 with
                                             | D0 ->
                                               (match a14 with
                                                | D0 ->
                                                  (match a15 with
                                                   | D0 ->
                                                     (match a16 with
                                                      | D0 ->
                                                        (match a17 with
                                                         | D0 ->
                                                           (match a18 with
                                                            | D0 ->
                                                              (match a19 with
                                                               | D0 ->
                                                                 (match a20 with
                                                                  | D0 ->
                                                                    (match a21 with
                                                                    | D0 ->
                                                                    (match a22 with
                                                                    | D0 ->
                                                                    (match a23 with
                                                                    | D0 ->
                                                                    (match a24 with
                                                                    | D0 ->
                                                                    (match a25 with
                                                                    | D0 ->
                                                                    (match a26 with
                                                                    | D0 ->
                                                                    (match a27 with
                                                                    | D0 ->
                                                                    (match a28 with
                                                                    | D0 ->
                                                                    (match a29 with
                                                                    | D0 ->
                                                                    True
                                                                    | D1 ->
                                                                    False)
                                                                    | D1 ->
                                                                    False)
                                                                    | D1 ->
                                                                    False)
                                                                    | D1 ->
                                                                    False)
                                                                    | D1 ->
                                                                    False)
                                                                    | D1 ->
                                                                    False)
                                                                    | D1 ->
                                                                    False)
                                                                    | D1 ->
                                                                    False)
                                                                    | D1 ->
                                                                    False)
                                                                  | D1 ->
                                                                    False)
                                                               | D1 -> False)
                                                            | D1 -> False)
                                                         | D1 -> False)
                                                      | D1 -> False)
                                                   | D1 -> False)
                                                | D1 -> False)
                                             | D1 -> False)
                                          | D1 -> False)
                                       | D1 -> False)
                                    | D1 -> False)
                                 | D1 -> False)
                              | D1 -> False)
                           | D1 -> False)
                        | D1 -> False)
                     | D1 -> False)
                  | D1 -> False)
               | D1 -> False)
            | D1 -> False)
         | D1 -> False)
      | D1 -> False)
   | D1 -> False)

(** val recr_aux :
    nat -> 'a1 -> (digits -> int31 -> 'a1 -> 'a1) -> int31 -> 'a1 **)

let rec recr_aux n0 case0 caserec i =
  match n0 with
  | O -> case0
  | S next ->
    (match iszero i with
     | True -> case0
     | False ->
       let si = shiftr0 i in
       caserec (firstr i) si (recr_aux next case0 caserec si))

(** val recr : 'a1 -> (digits -> int31 -> 'a1 -> 'a1) -> int31 -> 'a1 **)

let recr case0 caserec i =
  recr_aux size0 case0 caserec i

(** val phi : int31 -> z **)

let phi =
  recr Z0 (fun b x ->
    match b with
    | D0 -> Z.double
    | D1 -> Z.succ_double)

(** val incr : int31 -> int31 **)

let incr =
  recr in0 (fun b si rec0 ->
    match b with
    | D0 -> sneakl D1 si
    | D1 -> sneakl D0 rec0)

(** val compare31 : int31 -> int31 -> comparison **)

let compare31 n0 m =
  Z.compare (phi n0) (phi m)

(** val iter_int31 : int31 -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

let iter_int31 i f =
  recr (fun x -> x) (fun b si rec0 x ->
    match b with
    | D0 -> rec0 (rec0 x)
    | D1 -> f (rec0 (rec0 x))) i

type 'x compare0 =
| LT
| EQ
| GT

module type Coq_OrderedType = 
 sig 
  type t 
  
  val compare : t -> t -> t compare0
  
  val eq_dec : t -> t -> sumbool
 end

module Coq_OrderedTypeFacts = 
 functor (O:Coq_OrderedType) ->
 struct 
  module TO = 
   struct 
    type t = O.t
   end
  
  module IsTO = 
   struct 
    
   end
  
  module OrderTac = MakeOrderTac(TO)(IsTO)
  
  (** val eq_dec : O.t -> O.t -> sumbool **)
  
  let eq_dec =
    O.eq_dec
  
  (** val lt_dec : O.t -> O.t -> sumbool **)
  
  let lt_dec x y =
    match O.compare x y with
    | LT -> Left
    | _ -> Right
  
  (** val eqb : O.t -> O.t -> bool **)
  
  let eqb x y =
    match eq_dec x y with
    | Left -> True
    | Right -> False
 end

module KeyOrderedType = 
 functor (O:Coq_OrderedType) ->
 struct 
  module MO = Coq_OrderedTypeFacts(O)
 end

module type Int = 
 sig 
  type t 
  
  val i2z : t -> z
  
  val _0 : t
  
  val _1 : t
  
  val _2 : t
  
  val _3 : t
  
  val plus : t -> t -> t
  
  val opp : t -> t
  
  val minus : t -> t -> t
  
  val mult : t -> t -> t
  
  val max : t -> t -> t
  
  val gt_le_dec : t -> t -> sumbool
  
  val ge_lt_dec : t -> t -> sumbool
  
  val eq_dec : t -> t -> sumbool
 end

module Z_as_Int = 
 struct 
  type t = z
  
  (** val _0 : z **)
  
  let _0 =
    Z0
  
  (** val _1 : z **)
  
  let _1 =
    Zpos XH
  
  (** val _2 : z **)
  
  let _2 =
    Zpos (XO XH)
  
  (** val _3 : z **)
  
  let _3 =
    Zpos (XI XH)
  
  (** val plus : z -> z -> z **)
  
  let plus =
    Z.add
  
  (** val opp : z -> z **)
  
  let opp =
    Z.opp
  
  (** val minus : z -> z -> z **)
  
  let minus =
    Z.sub
  
  (** val mult : z -> z -> z **)
  
  let mult =
    Z.mul
  
  (** val max : z -> z -> z **)
  
  let max =
    Z.max
  
  (** val gt_le_dec : z -> z -> sumbool **)
  
  let gt_le_dec =
    z_gt_le_dec
  
  (** val ge_lt_dec : z -> z -> sumbool **)
  
  let ge_lt_dec =
    z_ge_lt_dec
  
  (** val eq_dec : z -> z -> sumbool **)
  
  let eq_dec =
    Z.eq_dec
  
  (** val i2z : t -> z **)
  
  let i2z n0 =
    n0
 end

module MakeListOrdering = 
 functor (O:OrderedType) ->
 struct 
  module MO = OrderedTypeFacts(O)
 end

module MakeRaw = 
 functor (I:Int) ->
 functor (X:OrderedType) ->
 struct 
  type elt = X.t
  
  type tree =
  | Leaf
  | Node of I.t * tree * X.t * tree
  
  (** val empty : tree **)
  
  let empty =
    Leaf
  
  (** val is_empty : tree -> bool **)
  
  let is_empty = function
  | Leaf -> True
  | Node (t1, t2, t3, t4) -> False
  
  (** val mem : X.t -> tree -> bool **)
  
  let rec mem x = function
  | Leaf -> False
  | Node (t1, l, k, r) ->
    (match X.compare x k with
     | Eq -> True
     | Lt -> mem x l
     | Gt -> mem x r)
  
  (** val min_elt : tree -> elt option **)
  
  let rec min_elt = function
  | Leaf -> None
  | Node (t1, l, x, r) ->
    (match l with
     | Leaf -> Some x
     | Node (t2, t3, t4, t5) -> min_elt l)
  
  (** val max_elt : tree -> elt option **)
  
  let rec max_elt = function
  | Leaf -> None
  | Node (t1, l, x, r) ->
    (match r with
     | Leaf -> Some x
     | Node (t2, t3, t4, t5) -> max_elt r)
  
  (** val choose : tree -> elt option **)
  
  let choose =
    min_elt
  
  (** val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1 **)
  
  let rec fold f t0 base =
    match t0 with
    | Leaf -> base
    | Node (t1, l, x, r) -> fold f r (f x (fold f l base))
  
  (** val elements_aux : X.t list -> tree -> X.t list **)
  
  let rec elements_aux acc = function
  | Leaf -> acc
  | Node (t0, l, x, r) -> elements_aux (Cons (x, (elements_aux acc r))) l
  
  (** val elements : tree -> X.t list **)
  
  let elements =
    elements_aux Nil
  
  (** val rev_elements_aux : X.t list -> tree -> X.t list **)
  
  let rec rev_elements_aux acc = function
  | Leaf -> acc
  | Node (t0, l, x, r) ->
    rev_elements_aux (Cons (x, (rev_elements_aux acc l))) r
  
  (** val rev_elements : tree -> X.t list **)
  
  let rev_elements =
    rev_elements_aux Nil
  
  (** val cardinal : tree -> nat **)
  
  let rec cardinal = function
  | Leaf -> O
  | Node (t0, l, t1, r) -> S (plus (cardinal l) (cardinal r))
  
  (** val maxdepth : tree -> nat **)
  
  let rec maxdepth = function
  | Leaf -> O
  | Node (t0, l, t1, r) -> S (max (maxdepth l) (maxdepth r))
  
  (** val mindepth : tree -> nat **)
  
  let rec mindepth = function
  | Leaf -> O
  | Node (t0, l, t1, r) -> S (min (mindepth l) (mindepth r))
  
  (** val for_all : (elt -> bool) -> tree -> bool **)
  
  let rec for_all f = function
  | Leaf -> True
  | Node (t0, l, x, r) ->
    (match match f x with
           | True -> for_all f l
           | False -> False with
     | True -> for_all f r
     | False -> False)
  
  (** val exists_ : (elt -> bool) -> tree -> bool **)
  
  let rec exists_ f = function
  | Leaf -> False
  | Node (t0, l, x, r) ->
    (match match f x with
           | True -> True
           | False -> exists_ f l with
     | True -> True
     | False -> exists_ f r)
  
  type enumeration =
  | End
  | More of elt * tree * enumeration
  
  (** val cons : tree -> enumeration -> enumeration **)
  
  let rec cons s e =
    match s with
    | Leaf -> e
    | Node (t0, l, x, r) -> cons l (More (x, r, e))
  
  (** val compare_more :
      X.t -> (enumeration -> comparison) -> enumeration -> comparison **)
  
  let compare_more x1 cont = function
  | End -> Gt
  | More (x2, r2, e3) ->
    (match X.compare x1 x2 with
     | Eq -> cont (cons r2 e3)
     | x -> x)
  
  (** val compare_cont :
      tree -> (enumeration -> comparison) -> enumeration -> comparison **)
  
  let rec compare_cont s1 cont e2 =
    match s1 with
    | Leaf -> cont e2
    | Node (t0, l1, x1, r1) ->
      compare_cont l1 (compare_more x1 (compare_cont r1 cont)) e2
  
  (** val compare_end : enumeration -> comparison **)
  
  let compare_end = function
  | End -> Eq
  | More (e, t0, e0) -> Lt
  
  (** val compare : tree -> tree -> comparison **)
  
  let compare s1 s2 =
    compare_cont s1 compare_end (cons s2 End)
  
  (** val equal : tree -> tree -> bool **)
  
  let equal s1 s2 =
    match compare s1 s2 with
    | Eq -> True
    | _ -> False
  
  (** val subsetl : (tree -> bool) -> X.t -> tree -> bool **)
  
  let rec subsetl subset_l1 x1 s2 = match s2 with
  | Leaf -> False
  | Node (t0, l2, x2, r2) ->
    (match X.compare x1 x2 with
     | Eq -> subset_l1 l2
     | Lt -> subsetl subset_l1 x1 l2
     | Gt ->
       (match mem x1 r2 with
        | True -> subset_l1 s2
        | False -> False))
  
  (** val subsetr : (tree -> bool) -> X.t -> tree -> bool **)
  
  let rec subsetr subset_r1 x1 s2 = match s2 with
  | Leaf -> False
  | Node (t0, l2, x2, r2) ->
    (match X.compare x1 x2 with
     | Eq -> subset_r1 r2
     | Lt ->
       (match mem x1 l2 with
        | True -> subset_r1 s2
        | False -> False)
     | Gt -> subsetr subset_r1 x1 r2)
  
  (** val subset : tree -> tree -> bool **)
  
  let rec subset s1 s2 =
    match s1 with
    | Leaf -> True
    | Node (t0, l1, x1, r1) ->
      (match s2 with
       | Leaf -> False
       | Node (t1, l2, x2, r2) ->
         (match X.compare x1 x2 with
          | Eq ->
            (match subset l1 l2 with
             | True -> subset r1 r2
             | False -> False)
          | Lt ->
            (match subsetl (subset l1) x1 l2 with
             | True -> subset r1 s2
             | False -> False)
          | Gt ->
            (match subsetr (subset r1) x1 r2 with
             | True -> subset l1 s2
             | False -> False)))
  
  type t = tree
  
  (** val height : t -> I.t **)
  
  let height = function
  | Leaf -> I._0
  | Node (h, t0, t1, t2) -> h
  
  (** val singleton : X.t -> tree **)
  
  let singleton x =
    Node (I._1, Leaf, x, Leaf)
  
  (** val create : t -> X.t -> t -> tree **)
  
  let create l x r =
    Node ((I.plus (I.max (height l) (height r)) I._1), l, x, r)
  
  (** val assert_false : t -> X.t -> t -> tree **)
  
  let assert_false =
    create
  
  (** val bal : t -> X.t -> t -> tree **)
  
  let bal l x r =
    let hl = height l in
    let hr = height r in
    (match I.gt_le_dec hl (I.plus hr I._2) with
     | Left ->
       (match l with
        | Leaf -> assert_false l x r
        | Node (t0, ll, lx, lr) ->
          (match I.ge_lt_dec (height ll) (height lr) with
           | Left -> create ll lx (create lr x r)
           | Right ->
             (match lr with
              | Leaf -> assert_false l x r
              | Node (t1, lrl, lrx, lrr) ->
                create (create ll lx lrl) lrx (create lrr x r))))
     | Right ->
       (match I.gt_le_dec hr (I.plus hl I._2) with
        | Left ->
          (match r with
           | Leaf -> assert_false l x r
           | Node (t0, rl, rx, rr) ->
             (match I.ge_lt_dec (height rr) (height rl) with
              | Left -> create (create l x rl) rx rr
              | Right ->
                (match rl with
                 | Leaf -> assert_false l x r
                 | Node (t1, rll, rlx, rlr) ->
                   create (create l x rll) rlx (create rlr rx rr))))
        | Right -> create l x r))
  
  (** val add : X.t -> tree -> tree **)
  
  let rec add x = function
  | Leaf -> Node (I._1, Leaf, x, Leaf)
  | Node (h, l, y, r) ->
    (match X.compare x y with
     | Eq -> Node (h, l, y, r)
     | Lt -> bal (add x l) y r
     | Gt -> bal l y (add x r))
  
  (** val join : tree -> elt -> t -> t **)
  
  let rec join l = match l with
  | Leaf -> add
  | Node (lh, ll, lx, lr) ->
    (fun x ->
      let rec join_aux r = match r with
      | Leaf -> add x l
      | Node (rh, rl, rx, rr) ->
        (match I.gt_le_dec lh (I.plus rh I._2) with
         | Left -> bal ll lx (join lr x r)
         | Right ->
           (match I.gt_le_dec rh (I.plus lh I._2) with
            | Left -> bal (join_aux rl) rx rr
            | Right -> create l x r))
      in join_aux)
  
  (** val remove_min : tree -> elt -> t -> (t, elt) prod **)
  
  let rec remove_min l x r =
    match l with
    | Leaf -> Pair (r, x)
    | Node (lh, ll, lx, lr) ->
      let Pair (l', m) = remove_min ll lx lr in Pair ((bal l' x r), m)
  
  (** val merge : tree -> tree -> tree **)
  
  let merge s1 s2 =
    match s1 with
    | Leaf -> s2
    | Node (t0, t1, t2, t3) ->
      (match s2 with
       | Leaf -> s1
       | Node (t4, l2, x2, r2) ->
         let Pair (s2', m) = remove_min l2 x2 r2 in bal s1 m s2')
  
  (** val remove : X.t -> tree -> tree **)
  
  let rec remove x = function
  | Leaf -> Leaf
  | Node (t0, l, y, r) ->
    (match X.compare x y with
     | Eq -> merge l r
     | Lt -> bal (remove x l) y r
     | Gt -> bal l y (remove x r))
  
  (** val concat : tree -> tree -> tree **)
  
  let concat s1 s2 =
    match s1 with
    | Leaf -> s2
    | Node (t0, t1, t2, t3) ->
      (match s2 with
       | Leaf -> s1
       | Node (t4, l2, x2, r2) ->
         let Pair (s2', m) = remove_min l2 x2 r2 in join s1 m s2')
  
  type triple = { t_left : t; t_in : bool; t_right : t }
  
  (** val t_left : triple -> t **)
  
  let t_left t0 =
    t0.t_left
  
  (** val t_in : triple -> bool **)
  
  let t_in t0 =
    t0.t_in
  
  (** val t_right : triple -> t **)
  
  let t_right t0 =
    t0.t_right
  
  (** val split : X.t -> tree -> triple **)
  
  let rec split x = function
  | Leaf -> { t_left = Leaf; t_in = False; t_right = Leaf }
  | Node (t0, l, y, r) ->
    (match X.compare x y with
     | Eq -> { t_left = l; t_in = True; t_right = r }
     | Lt ->
       let { t_left = ll; t_in = b; t_right = rl } = split x l in
       { t_left = ll; t_in = b; t_right = (join rl y r) }
     | Gt ->
       let { t_left = rl; t_in = b; t_right = rr } = split x r in
       { t_left = (join l y rl); t_in = b; t_right = rr })
  
  (** val inter : tree -> tree -> tree **)
  
  let rec inter s1 s2 =
    match s1 with
    | Leaf -> Leaf
    | Node (t0, l1, x1, r1) ->
      (match s2 with
       | Leaf -> Leaf
       | Node (t1, t2, t3, t4) ->
         let { t_left = l2'; t_in = pres; t_right = r2' } = split x1 s2 in
         (match pres with
          | True -> join (inter l1 l2') x1 (inter r1 r2')
          | False -> concat (inter l1 l2') (inter r1 r2')))
  
  (** val diff : tree -> tree -> tree **)
  
  let rec diff s1 s2 =
    match s1 with
    | Leaf -> Leaf
    | Node (t0, l1, x1, r1) ->
      (match s2 with
       | Leaf -> s1
       | Node (t1, t2, t3, t4) ->
         let { t_left = l2'; t_in = pres; t_right = r2' } = split x1 s2 in
         (match pres with
          | True -> concat (diff l1 l2') (diff r1 r2')
          | False -> join (diff l1 l2') x1 (diff r1 r2')))
  
  (** val union : tree -> tree -> tree **)
  
  let rec union s1 s2 =
    match s1 with
    | Leaf -> s2
    | Node (t0, l1, x1, r1) ->
      (match s2 with
       | Leaf -> s1
       | Node (t1, t2, t3, t4) ->
         let { t_left = l2'; t_in = x; t_right = r2' } = split x1 s2 in
         join (union l1 l2') x1 (union r1 r2'))
  
  (** val filter : (elt -> bool) -> tree -> tree **)
  
  let rec filter f = function
  | Leaf -> Leaf
  | Node (t0, l, x, r) ->
    let l' = filter f l in
    let r' = filter f r in
    (match f x with
     | True -> join l' x r'
     | False -> concat l' r')
  
  (** val partition : (elt -> bool) -> t -> (t, t) prod **)
  
  let rec partition f = function
  | Leaf -> Pair (Leaf, Leaf)
  | Node (t0, l, x, r) ->
    let Pair (l1, l2) = partition f l in
    let Pair (r1, r2) = partition f r in
    (match f x with
     | True -> Pair ((join l1 x r1), (concat l2 r2))
     | False -> Pair ((concat l1 r1), (join l2 x r2)))
  
  (** val ltb_tree : X.t -> tree -> bool **)
  
  let rec ltb_tree x = function
  | Leaf -> True
  | Node (t0, l, y, r) ->
    (match X.compare x y with
     | Gt ->
       (match ltb_tree x l with
        | True -> ltb_tree x r
        | False -> False)
     | _ -> False)
  
  (** val gtb_tree : X.t -> tree -> bool **)
  
  let rec gtb_tree x = function
  | Leaf -> True
  | Node (t0, l, y, r) ->
    (match X.compare x y with
     | Lt ->
       (match gtb_tree x l with
        | True -> gtb_tree x r
        | False -> False)
     | _ -> False)
  
  (** val isok : tree -> bool **)
  
  let rec isok = function
  | Leaf -> True
  | Node (t0, l, x, r) ->
    (match match match isok l with
                 | True -> isok r
                 | False -> False with
           | True -> ltb_tree x l
           | False -> False with
     | True -> gtb_tree x r
     | False -> False)
  
  module MX = OrderedTypeFacts(X)
  
  type coq_R_min_elt =
  | R_min_elt_0 of tree
  | R_min_elt_1 of tree * I.t * tree * X.t * tree
  | R_min_elt_2 of tree * I.t * tree * X.t * tree * I.t * tree * X.t * 
     tree * elt option * coq_R_min_elt
  
  type coq_R_max_elt =
  | R_max_elt_0 of tree
  | R_max_elt_1 of tree * I.t * tree * X.t * tree
  | R_max_elt_2 of tree * I.t * tree * X.t * tree * I.t * tree * X.t * 
     tree * elt option * coq_R_max_elt
  
  module L = MakeListOrdering(X)
  
  (** val flatten_e : enumeration -> elt list **)
  
  let rec flatten_e = function
  | End -> Nil
  | More (x, t0, r) -> Cons (x, (app (elements t0) (flatten_e r)))
  
  type coq_R_bal =
  | R_bal_0 of t * X.t * t
  | R_bal_1 of t * X.t * t * I.t * tree * X.t * tree
  | R_bal_2 of t * X.t * t * I.t * tree * X.t * tree
  | R_bal_3 of t * X.t * t * I.t * tree * X.t * tree * I.t * tree * X.t
     * tree
  | R_bal_4 of t * X.t * t
  | R_bal_5 of t * X.t * t * I.t * tree * X.t * tree
  | R_bal_6 of t * X.t * t * I.t * tree * X.t * tree
  | R_bal_7 of t * X.t * t * I.t * tree * X.t * tree * I.t * tree * X.t
     * tree
  | R_bal_8 of t * X.t * t
  
  type coq_R_remove_min =
  | R_remove_min_0 of tree * elt * t
  | R_remove_min_1 of tree * elt * t * I.t * tree * X.t * tree
     * (t, elt) prod * coq_R_remove_min * t * elt
  
  type coq_R_merge =
  | R_merge_0 of tree * tree
  | R_merge_1 of tree * tree * I.t * tree * X.t * tree
  | R_merge_2 of tree * tree * I.t * tree * X.t * tree * I.t * tree * 
     X.t * tree * t * elt
  
  type coq_R_concat =
  | R_concat_0 of tree * tree
  | R_concat_1 of tree * tree * I.t * tree * X.t * tree
  | R_concat_2 of tree * tree * I.t * tree * X.t * tree * I.t * tree * 
     X.t * tree * t * elt
  
  type coq_R_inter =
  | R_inter_0 of tree * tree
  | R_inter_1 of tree * tree * I.t * tree * X.t * tree
  | R_inter_2 of tree * tree * I.t * tree * X.t * tree * I.t * tree * 
     X.t * tree * t * bool * t * tree * coq_R_inter * tree * coq_R_inter
  | R_inter_3 of tree * tree * I.t * tree * X.t * tree * I.t * tree * 
     X.t * tree * t * bool * t * tree * coq_R_inter * tree * coq_R_inter
  
  type coq_R_diff =
  | R_diff_0 of tree * tree
  | R_diff_1 of tree * tree * I.t * tree * X.t * tree
  | R_diff_2 of tree * tree * I.t * tree * X.t * tree * I.t * tree * 
     X.t * tree * t * bool * t * tree * coq_R_diff * tree * coq_R_diff
  | R_diff_3 of tree * tree * I.t * tree * X.t * tree * I.t * tree * 
     X.t * tree * t * bool * t * tree * coq_R_diff * tree * coq_R_diff
  
  type coq_R_union =
  | R_union_0 of tree * tree
  | R_union_1 of tree * tree * I.t * tree * X.t * tree
  | R_union_2 of tree * tree * I.t * tree * X.t * tree * I.t * tree * 
     X.t * tree * t * bool * t * tree * coq_R_union * tree * coq_R_union
 end

module IntMake = 
 functor (I:Int) ->
 functor (X:OrderedType) ->
 struct 
  module Raw = MakeRaw(I)(X)
  
  module E = 
   struct 
    type t = X.t
    
    (** val compare : t -> t -> comparison **)
    
    let compare =
      X.compare
    
    (** val eq_dec : t -> t -> sumbool **)
    
    let eq_dec =
      X.eq_dec
   end
  
  type elt = X.t
  
  type t_ =
    Raw.t
    (* singleton inductive, whose constructor was Mkt *)
  
  (** val this : t_ -> Raw.t **)
  
  let this t0 =
    t0
  
  type t = t_
  
  (** val mem : elt -> t -> bool **)
  
  let mem x s =
    Raw.mem x (this s)
  
  (** val add : elt -> t -> t **)
  
  let add x s =
    Raw.add x (this s)
  
  (** val remove : elt -> t -> t **)
  
  let remove x s =
    Raw.remove x (this s)
  
  (** val singleton : elt -> t **)
  
  let singleton x =
    Raw.singleton x
  
  (** val union : t -> t -> t **)
  
  let union s s' =
    Raw.union (this s) (this s')
  
  (** val inter : t -> t -> t **)
  
  let inter s s' =
    Raw.inter (this s) (this s')
  
  (** val diff : t -> t -> t **)
  
  let diff s s' =
    Raw.diff (this s) (this s')
  
  (** val equal : t -> t -> bool **)
  
  let equal s s' =
    Raw.equal (this s) (this s')
  
  (** val subset : t -> t -> bool **)
  
  let subset s s' =
    Raw.subset (this s) (this s')
  
  (** val empty : t **)
  
  let empty =
    Raw.empty
  
  (** val is_empty : t -> bool **)
  
  let is_empty s =
    Raw.is_empty (this s)
  
  (** val elements : t -> elt list **)
  
  let elements s =
    Raw.elements (this s)
  
  (** val choose : t -> elt option **)
  
  let choose s =
    Raw.choose (this s)
  
  (** val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1 **)
  
  let fold f s =
    Raw.fold f (this s)
  
  (** val cardinal : t -> nat **)
  
  let cardinal s =
    Raw.cardinal (this s)
  
  (** val filter : (elt -> bool) -> t -> t **)
  
  let filter f s =
    Raw.filter f (this s)
  
  (** val for_all : (elt -> bool) -> t -> bool **)
  
  let for_all f s =
    Raw.for_all f (this s)
  
  (** val exists_ : (elt -> bool) -> t -> bool **)
  
  let exists_ f s =
    Raw.exists_ f (this s)
  
  (** val partition : (elt -> bool) -> t -> (t, t) prod **)
  
  let partition f s =
    let p = Raw.partition f (this s) in Pair ((fst p), (snd p))
  
  (** val eq_dec : t -> t -> sumbool **)
  
  let eq_dec s s' =
    let b = Raw.equal s s' in
    (match b with
     | True -> Left
     | False -> Right)
  
  (** val compare : t -> t -> comparison **)
  
  let compare s s' =
    Raw.compare (this s) (this s')
  
  (** val min_elt : t -> elt option **)
  
  let min_elt s =
    Raw.min_elt (this s)
  
  (** val max_elt : t -> elt option **)
  
  let max_elt s =
    Raw.max_elt (this s)
 end

module type OrderedTypeOrig = 
 Coq_OrderedType

module Update_OT = 
 functor (O:OrderedTypeOrig) ->
 struct 
  type t = O.t
  
  (** val eq_dec : t -> t -> sumbool **)
  
  let eq_dec =
    O.eq_dec
  
  (** val compare : O.t -> O.t -> comparison **)
  
  let compare x y =
    match O.compare x y with
    | LT -> Lt
    | EQ -> Eq
    | GT -> Gt
 end

module Coq_IntMake = 
 functor (I:Int) ->
 functor (X:Coq_OrderedType) ->
 struct 
  module X' = Update_OT(X)
  
  module MSet = IntMake(I)(X')
  
  type elt = X.t
  
  type t = MSet.t
  
  (** val empty : t **)
  
  let empty =
    MSet.empty
  
  (** val is_empty : t -> bool **)
  
  let is_empty =
    MSet.is_empty
  
  (** val mem : elt -> t -> bool **)
  
  let mem =
    MSet.mem
  
  (** val add : elt -> t -> t **)
  
  let add =
    MSet.add
  
  (** val singleton : elt -> t **)
  
  let singleton =
    MSet.singleton
  
  (** val remove : elt -> t -> t **)
  
  let remove =
    MSet.remove
  
  (** val union : t -> t -> t **)
  
  let union =
    MSet.union
  
  (** val inter : t -> t -> t **)
  
  let inter =
    MSet.inter
  
  (** val diff : t -> t -> t **)
  
  let diff =
    MSet.diff
  
  (** val eq_dec : t -> t -> sumbool **)
  
  let eq_dec =
    MSet.eq_dec
  
  (** val equal : t -> t -> bool **)
  
  let equal =
    MSet.equal
  
  (** val subset : t -> t -> bool **)
  
  let subset =
    MSet.subset
  
  (** val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1 **)
  
  let fold x x0 x1 =
    MSet.fold x x0 x1
  
  (** val for_all : (elt -> bool) -> t -> bool **)
  
  let for_all =
    MSet.for_all
  
  (** val exists_ : (elt -> bool) -> t -> bool **)
  
  let exists_ =
    MSet.exists_
  
  (** val filter : (elt -> bool) -> t -> t **)
  
  let filter =
    MSet.filter
  
  (** val partition : (elt -> bool) -> t -> (t, t) prod **)
  
  let partition =
    MSet.partition
  
  (** val cardinal : t -> nat **)
  
  let cardinal =
    MSet.cardinal
  
  (** val elements : t -> elt list **)
  
  let elements =
    MSet.elements
  
  (** val choose : t -> elt option **)
  
  let choose =
    MSet.choose
  
  module MF = 
   struct 
    (** val eqb : X.t -> X.t -> bool **)
    
    let eqb x y =
      match MSet.E.eq_dec x y with
      | Left -> True
      | Right -> False
   end
  
  (** val min_elt : t -> elt option **)
  
  let min_elt =
    MSet.min_elt
  
  (** val max_elt : t -> elt option **)
  
  let max_elt =
    MSet.max_elt
  
  (** val compare : t -> t -> t compare0 **)
  
  let compare s s' =
    let c = compSpec2Type s s' (MSet.compare s s') in
    (match c with
     | CompEqT -> EQ
     | CompLtT -> LT
     | CompGtT -> GT)
  
  module E = 
   struct 
    type t = X.t
    
    (** val compare : t -> t -> t compare0 **)
    
    let compare =
      X.compare
    
    (** val eq_dec : t -> t -> sumbool **)
    
    let eq_dec =
      X.eq_dec
   end
 end

module Make = 
 functor (X:Coq_OrderedType) ->
 Coq_IntMake(Z_as_Int)(X)

module Raw = 
 functor (X:Coq_OrderedType) ->
 struct 
  module MX = Coq_OrderedTypeFacts(X)
  
  module PX = KeyOrderedType(X)
  
  type key = X.t
  
  type 'elt t = (X.t, 'elt) prod list
  
  (** val empty : 'a1 t **)
  
  let empty =
    Nil
  
  (** val is_empty : 'a1 t -> bool **)
  
  let is_empty = function
  | Nil -> True
  | Cons (x, x0) -> False
  
  (** val mem : key -> 'a1 t -> bool **)
  
  let rec mem k = function
  | Nil -> False
  | Cons (p, l) ->
    let Pair (k', e) = p in
    (match X.compare k k' with
     | LT -> False
     | EQ -> True
     | GT -> mem k l)
  
  type 'elt coq_R_mem =
  | R_mem_0 of 'elt t
  | R_mem_1 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list
  | R_mem_2 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list
  | R_mem_3 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list * bool
     * 'elt coq_R_mem
  
  (** val coq_R_mem_rect :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 ->
      'a2) -> 'a1 t -> bool -> 'a1 coq_R_mem -> 'a2 **)
  
  let rec coq_R_mem_rect k f f0 f1 f2 s b = function
  | R_mem_0 s0 -> f s0 __
  | R_mem_1 (s0, k', _x, l) -> f0 s0 k' _x l __ __ __
  | R_mem_2 (s0, k', _x, l) -> f1 s0 k' _x l __ __ __
  | R_mem_3 (s0, k', _x, l, res, r0) ->
    f2 s0 k' _x l __ __ __ res r0 (coq_R_mem_rect k f f0 f1 f2 l res r0)
  
  (** val coq_R_mem_rec :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 ->
      'a2) -> 'a1 t -> bool -> 'a1 coq_R_mem -> 'a2 **)
  
  let rec coq_R_mem_rec k f f0 f1 f2 s b = function
  | R_mem_0 s0 -> f s0 __
  | R_mem_1 (s0, k', _x, l) -> f0 s0 k' _x l __ __ __
  | R_mem_2 (s0, k', _x, l) -> f1 s0 k' _x l __ __ __
  | R_mem_3 (s0, k', _x, l, res, r0) ->
    f2 s0 k' _x l __ __ __ res r0 (coq_R_mem_rec k f f0 f1 f2 l res r0)
  
  (** val mem_rect :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2 **)
  
  let rec mem_rect k f2 f1 f0 f s =
    let f3 = f2 s in
    let f4 = f1 s in
    let f5 = f0 s in
    let f6 = f s in
    (match s with
     | Nil -> f3 __
     | Cons (p, l) ->
       let Pair (t0, e) = p in
       let f7 = f6 t0 e l __ in
       let f8 = fun _ _ ->
         let hrec = mem_rect k f2 f1 f0 f l in f7 __ __ hrec
       in
       let f9 = f5 t0 e l __ in
       let f10 = f4 t0 e l __ in
       (match X.compare k t0 with
        | LT -> f10 __ __
        | EQ -> f9 __ __
        | GT -> f8 __ __))
  
  (** val mem_rec :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2 **)
  
  let mem_rec k =
    mem_rect k
  
  (** val coq_R_mem_correct : key -> 'a1 t -> bool -> 'a1 coq_R_mem **)
  
  let coq_R_mem_correct x x0 res =
    let princ = fun x1 -> mem_rect x1 in
    Obj.magic princ x (fun y _ z0 _ -> R_mem_0 y)
      (fun y y0 y1 y2 _ _ _ z0 _ -> R_mem_1 (y, y0, y1, y2))
      (fun y y0 y1 y2 _ _ _ z0 _ -> R_mem_2 (y, y0, y1, y2))
      (fun y y0 y1 y2 _ _ _ y6 z0 _ -> R_mem_3 (y, y0, y1, y2, (mem x y2),
      (y6 (mem x y2) __))) x0 res __
  
  (** val find : key -> 'a1 t -> 'a1 option **)
  
  let rec find k = function
  | Nil -> None
  | Cons (p, s') ->
    let Pair (k', x) = p in
    (match X.compare k k' with
     | LT -> None
     | EQ -> Some x
     | GT -> find k s')
  
  type 'elt coq_R_find =
  | R_find_0 of 'elt t
  | R_find_1 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list
  | R_find_2 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list
  | R_find_3 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list * 'elt option
     * 'elt coq_R_find
  
  (** val coq_R_find_rect :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2
      -> 'a2) -> 'a1 t -> 'a1 option -> 'a1 coq_R_find -> 'a2 **)
  
  let rec coq_R_find_rect k f f0 f1 f2 s o = function
  | R_find_0 s0 -> f s0 __
  | R_find_1 (s0, k', x, s') -> f0 s0 k' x s' __ __ __
  | R_find_2 (s0, k', x, s') -> f1 s0 k' x s' __ __ __
  | R_find_3 (s0, k', x, s', res, r0) ->
    f2 s0 k' x s' __ __ __ res r0 (coq_R_find_rect k f f0 f1 f2 s' res r0)
  
  (** val coq_R_find_rec :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find -> 'a2
      -> 'a2) -> 'a1 t -> 'a1 option -> 'a1 coq_R_find -> 'a2 **)
  
  let rec coq_R_find_rec k f f0 f1 f2 s o = function
  | R_find_0 s0 -> f s0 __
  | R_find_1 (s0, k', x, s') -> f0 s0 k' x s' __ __ __
  | R_find_2 (s0, k', x, s') -> f1 s0 k' x s' __ __ __
  | R_find_3 (s0, k', x, s', res, r0) ->
    f2 s0 k' x s' __ __ __ res r0 (coq_R_find_rec k f f0 f1 f2 s' res r0)
  
  (** val find_rect :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2 **)
  
  let rec find_rect k f2 f1 f0 f s =
    let f3 = f2 s in
    let f4 = f1 s in
    let f5 = f0 s in
    let f6 = f s in
    (match s with
     | Nil -> f3 __
     | Cons (p, l) ->
       let Pair (t0, e) = p in
       let f7 = f6 t0 e l __ in
       let f8 = fun _ _ ->
         let hrec = find_rect k f2 f1 f0 f l in f7 __ __ hrec
       in
       let f9 = f5 t0 e l __ in
       let f10 = f4 t0 e l __ in
       (match X.compare k t0 with
        | LT -> f10 __ __
        | EQ -> f9 __ __
        | GT -> f8 __ __))
  
  (** val find_rec :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2 **)
  
  let find_rec k =
    find_rect k
  
  (** val coq_R_find_correct :
      key -> 'a1 t -> 'a1 option -> 'a1 coq_R_find **)
  
  let coq_R_find_correct x x0 res =
    let princ = fun x1 -> find_rect x1 in
    Obj.magic princ x (fun y _ z0 _ -> R_find_0 y)
      (fun y y0 y1 y2 _ _ _ z0 _ -> R_find_1 (y, y0, y1, y2))
      (fun y y0 y1 y2 _ _ _ z0 _ -> R_find_2 (y, y0, y1, y2))
      (fun y y0 y1 y2 _ _ _ y6 z0 _ -> R_find_3 (y, y0, y1, y2, (find x y2),
      (y6 (find x y2) __))) x0 res __
  
  (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)
  
  let rec add k x s = match s with
  | Nil -> Cons ((Pair (k, x)), Nil)
  | Cons (p, l) ->
    let Pair (k', y) = p in
    (match X.compare k k' with
     | LT -> Cons ((Pair (k, x)), s)
     | EQ -> Cons ((Pair (k, x)), l)
     | GT -> Cons ((Pair (k', y)), (add k x l)))
  
  type 'elt coq_R_add =
  | R_add_0 of 'elt t
  | R_add_1 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list
  | R_add_2 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list
  | R_add_3 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list * 'elt t
     * 'elt coq_R_add
  
  (** val coq_R_add_rect :
      key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
      (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1
      -> (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add ->
      'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1 coq_R_add -> 'a2 **)
  
  let rec coq_R_add_rect k x f f0 f1 f2 s t0 = function
  | R_add_0 s0 -> f s0 __
  | R_add_1 (s0, k', y, l) -> f0 s0 k' y l __ __ __
  | R_add_2 (s0, k', y, l) -> f1 s0 k' y l __ __ __
  | R_add_3 (s0, k', y, l, res, r0) ->
    f2 s0 k' y l __ __ __ res r0 (coq_R_add_rect k x f f0 f1 f2 l res r0)
  
  (** val coq_R_add_rec :
      key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
      (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1
      -> (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_add ->
      'a2 -> 'a2) -> 'a1 t -> 'a1 t -> 'a1 coq_R_add -> 'a2 **)
  
  let rec coq_R_add_rec k x f f0 f1 f2 s t0 = function
  | R_add_0 s0 -> f s0 __
  | R_add_1 (s0, k', y, l) -> f0 s0 k' y l __ __ __
  | R_add_2 (s0, k', y, l) -> f1 s0 k' y l __ __ __
  | R_add_3 (s0, k', y, l, res, r0) ->
    f2 s0 k' y l __ __ __ res r0 (coq_R_add_rec k x f f0 f1 f2 l res r0)
  
  (** val add_rect :
      key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
      (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1
      -> (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t ->
      'a2 **)
  
  let rec add_rect k x f2 f1 f0 f s =
    let f3 = f2 s in
    let f4 = f1 s in
    let f5 = f0 s in
    let f6 = f s in
    (match s with
     | Nil -> f3 __
     | Cons (p, l) ->
       let Pair (t0, e) = p in
       let f7 = f6 t0 e l __ in
       let f8 = fun _ _ ->
         let hrec = add_rect k x f2 f1 f0 f l in f7 __ __ hrec
       in
       let f9 = f5 t0 e l __ in
       let f10 = f4 t0 e l __ in
       (match X.compare k t0 with
        | LT -> f10 __ __
        | EQ -> f9 __ __
        | GT -> f8 __ __))
  
  (** val add_rec :
      key -> 'a1 -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 ->
      (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1
      -> (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t ->
      'a2 **)
  
  let add_rec k x =
    add_rect k x
  
  (** val coq_R_add_correct :
      key -> 'a1 -> 'a1 t -> 'a1 t -> 'a1 coq_R_add **)
  
  let coq_R_add_correct x x0 x1 res =
    add_rect x x0 (fun y _ z0 _ -> R_add_0 y) (fun y y0 y1 y2 _ _ _ z0 _ ->
      R_add_1 (y, y0, y1, y2)) (fun y y0 y1 y2 _ _ _ z0 _ -> R_add_2 (y, y0,
      y1, y2)) (fun y y0 y1 y2 _ _ _ y6 z0 _ -> R_add_3 (y, y0, y1, y2,
      (add x x0 y2), (y6 (add x x0 y2) __))) x1 res __
  
  (** val remove : key -> 'a1 t -> 'a1 t **)
  
  let rec remove k s = match s with
  | Nil -> Nil
  | Cons (p, l) ->
    let Pair (k', x) = p in
    (match X.compare k k' with
     | LT -> s
     | EQ -> l
     | GT -> Cons ((Pair (k', x)), (remove k l)))
  
  type 'elt coq_R_remove =
  | R_remove_0 of 'elt t
  | R_remove_1 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list
  | R_remove_2 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list
  | R_remove_3 of 'elt t * X.t * 'elt * (X.t, 'elt) prod list * 'elt t
     * 'elt coq_R_remove
  
  (** val coq_R_remove_rect :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 ->
      'a2) -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove -> 'a2 **)
  
  let rec coq_R_remove_rect k f f0 f1 f2 s t0 = function
  | R_remove_0 s0 -> f s0 __
  | R_remove_1 (s0, k', x, l) -> f0 s0 k' x l __ __ __
  | R_remove_2 (s0, k', x, l) -> f1 s0 k' x l __ __ __
  | R_remove_3 (s0, k', x, l, res, r0) ->
    f2 s0 k' x l __ __ __ res r0 (coq_R_remove_rect k f f0 f1 f2 l res r0)
  
  (** val coq_R_remove_rec :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a1 t -> 'a1 coq_R_remove -> 'a2 ->
      'a2) -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove -> 'a2 **)
  
  let rec coq_R_remove_rec k f f0 f1 f2 s t0 = function
  | R_remove_0 s0 -> f s0 __
  | R_remove_1 (s0, k', x, l) -> f0 s0 k' x l __ __ __
  | R_remove_2 (s0, k', x, l) -> f1 s0 k' x l __ __ __
  | R_remove_3 (s0, k', x, l, res, r0) ->
    f2 s0 k' x l __ __ __ res r0 (coq_R_remove_rec k f f0 f1 f2 l res r0)
  
  (** val remove_rect :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2 **)
  
  let rec remove_rect k f2 f1 f0 f s =
    let f3 = f2 s in
    let f4 = f1 s in
    let f5 = f0 s in
    let f6 = f s in
    (match s with
     | Nil -> f3 __
     | Cons (p, l) ->
       let Pair (t0, e) = p in
       let f7 = f6 t0 e l __ in
       let f8 = fun _ _ ->
         let hrec = remove_rect k f2 f1 f0 f l in f7 __ __ hrec
       in
       let f9 = f5 t0 e l __ in
       let f10 = f4 t0 e l __ in
       (match X.compare k t0 with
        | LT -> f10 __ __
        | EQ -> f9 __ __
        | GT -> f8 __ __))
  
  (** val remove_rec :
      key -> ('a1 t -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod
      list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> __ -> 'a2) -> ('a1 t -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> 'a1 t -> 'a2 **)
  
  let remove_rec k =
    remove_rect k
  
  (** val coq_R_remove_correct :
      key -> 'a1 t -> 'a1 t -> 'a1 coq_R_remove **)
  
  let coq_R_remove_correct x x0 res =
    let princ = fun x1 -> remove_rect x1 in
    Obj.magic princ x (fun y _ z0 _ -> R_remove_0 y)
      (fun y y0 y1 y2 _ _ _ z0 _ -> R_remove_1 (y, y0, y1, y2))
      (fun y y0 y1 y2 _ _ _ z0 _ -> R_remove_2 (y, y0, y1, y2))
      (fun y y0 y1 y2 _ _ _ y6 z0 _ -> R_remove_3 (y, y0, y1, y2,
      (remove x y2), (y6 (remove x y2) __))) x0 res __
  
  (** val elements : 'a1 t -> 'a1 t **)
  
  let elements m =
    m
  
  (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)
  
  let rec fold f m acc =
    match m with
    | Nil -> acc
    | Cons (p, m') -> let Pair (k, e) = p in fold f m' (f k e acc)
  
  type ('elt, 'a) coq_R_fold =
  | R_fold_0 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a
  | R_fold_1 of (key -> 'elt -> 'a -> 'a) * 'elt t * 'a * X.t * 'elt
     * (X.t, 'elt) prod list * 'a * ('elt, 'a) coq_R_fold
  
  (** val coq_R_fold_rect :
      (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__ ->
      (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2) -> (key ->
      'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold ->
      'a2 **)
  
  let rec coq_R_fold_rect f f0 f1 m acc a = function
  | R_fold_0 (f2, m0, acc0) -> Obj.magic f __ f2 m0 acc0 __
  | R_fold_1 (f2, m0, acc0, k, e, m', res, r0) ->
    Obj.magic f0 __ f2 m0 acc0 k e m' __ res r0
      (coq_R_fold_rect f f0 f2 m' (f2 k e acc0) res r0)
  
  (** val coq_R_fold_rec :
      (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__ ->
      (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> __ -> ('a1, __) coq_R_fold -> 'a2 -> 'a2) -> (key ->
      'a1 -> 'a3 -> 'a3) -> 'a1 t -> 'a3 -> 'a3 -> ('a1, 'a3) coq_R_fold ->
      'a2 **)
  
  let rec coq_R_fold_rec f f0 f1 m acc a = function
  | R_fold_0 (f2, m0, acc0) -> Obj.magic f __ f2 m0 acc0 __
  | R_fold_1 (f2, m0, acc0, k, e, m', res, r0) ->
    Obj.magic f0 __ f2 m0 acc0 k e m' __ res r0
      (coq_R_fold_rec f f0 f2 m' (f2 k e acc0) res r0)
  
  (** val fold_rect :
      (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__ ->
      (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t
      -> 'a3 -> 'a2 **)
  
  let rec fold_rect f0 f f1 m acc =
    let f2 = Obj.magic f0 __ f1 m acc in
    let f3 = Obj.magic f __ f1 m acc in
    (match m with
     | Nil -> f2 __
     | Cons (p, l) ->
       let Pair (t0, e) = p in
       let f4 = f3 t0 e l __ in
       let hrec = fold_rect f0 f f1 l (f1 t0 e acc) in f4 hrec)
  
  (** val fold_rec :
      (__ -> (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> __ -> 'a2) -> (__ ->
      (key -> 'a1 -> __ -> __) -> 'a1 t -> __ -> X.t -> 'a1 -> (X.t, 'a1)
      prod list -> __ -> 'a2 -> 'a2) -> (key -> 'a1 -> 'a3 -> 'a3) -> 'a1 t
      -> 'a3 -> 'a2 **)
  
  let fold_rec f f0 f1 m acc =
    fold_rect f f0 f1 m acc
  
  (** val coq_R_fold_correct :
      (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 -> ('a1, 'a2)
      coq_R_fold **)
  
  let coq_R_fold_correct x0 x1 x2 res =
    let princ = fun x x3 -> fold_rect x x3 in
    Obj.magic princ (fun _ y0 y1 y2 _ z0 _ -> R_fold_0 (y0, y1, y2))
      (fun _ y0 y1 y2 y3 y4 y5 _ y7 z0 _ -> R_fold_1 (y0, y1, y2, y3, y4, y5,
      (fold y0 y5 (y0 y3 y4 y2)), (y7 (fold y0 y5 (y0 y3 y4 y2)) __))) x0 x1
      x2 res __
  
  (** val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)
  
  let rec equal cmp m m' =
    match m with
    | Nil ->
      (match m' with
       | Nil -> True
       | Cons (p, l) -> False)
    | Cons (p, l) ->
      let Pair (x, e) = p in
      (match m' with
       | Nil -> False
       | Cons (p0, l') ->
         let Pair (x', e') = p0 in
         (match X.compare x x' with
          | EQ ->
            (match cmp e e' with
             | True -> equal cmp l l'
             | False -> False)
          | _ -> False))
  
  type 'elt coq_R_equal =
  | R_equal_0 of 'elt t * 'elt t
  | R_equal_1 of 'elt t * 'elt t * X.t * 'elt * (X.t, 'elt) prod list * 
     X.t * 'elt * (X.t, 'elt) prod list * bool * 'elt coq_R_equal
  | R_equal_2 of 'elt t * 'elt t * X.t * 'elt * (X.t, 'elt) prod list * 
     X.t * 'elt * (X.t, 'elt) prod list * X.t compare0
  | R_equal_3 of 'elt t * 'elt t * 'elt t * 'elt t
  
  (** val coq_R_equal_rect :
      ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1 t
      -> 'a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod list -> __ -> X.t -> 'a1 ->
      (X.t, 'a1) prod list -> __ -> __ -> __ -> bool -> 'a1 coq_R_equal ->
      'a2 -> 'a2) -> ('a1 t -> 'a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod list ->
      __ -> X.t -> 'a1 -> (X.t, 'a1) prod list -> __ -> X.t compare0 -> __ ->
      __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ ->
      'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2 **)
  
  let rec coq_R_equal_rect cmp f f0 f1 f2 m m' b = function
  | R_equal_0 (m0, m'0) -> f m0 m'0 __ __
  | R_equal_1 (m0, m'0, x, e, l, x', e', l', res, r0) ->
    f0 m0 m'0 x e l __ x' e' l' __ __ __ res r0
      (coq_R_equal_rect cmp f f0 f1 f2 l l' res r0)
  | R_equal_2 (m0, m'0, x, e, l, x', e', l', _x) ->
    f1 m0 m'0 x e l __ x' e' l' __ _x __ __
  | R_equal_3 (m0, m'0, _x, _x0) -> f2 m0 m'0 _x __ _x0 __ __
  
  (** val coq_R_equal_rec :
      ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1 t
      -> 'a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod list -> __ -> X.t -> 'a1 ->
      (X.t, 'a1) prod list -> __ -> __ -> __ -> bool -> 'a1 coq_R_equal ->
      'a2 -> 'a2) -> ('a1 t -> 'a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod list ->
      __ -> X.t -> 'a1 -> (X.t, 'a1) prod list -> __ -> X.t compare0 -> __ ->
      __ -> 'a2) -> ('a1 t -> 'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ ->
      'a2) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal -> 'a2 **)
  
  let rec coq_R_equal_rec cmp f f0 f1 f2 m m' b = function
  | R_equal_0 (m0, m'0) -> f m0 m'0 __ __
  | R_equal_1 (m0, m'0, x, e, l, x', e', l', res, r0) ->
    f0 m0 m'0 x e l __ x' e' l' __ __ __ res r0
      (coq_R_equal_rec cmp f f0 f1 f2 l l' res r0)
  | R_equal_2 (m0, m'0, x, e, l, x', e', l', _x) ->
    f1 m0 m'0 x e l __ x' e' l' __ _x __ __
  | R_equal_3 (m0, m'0, _x, _x0) -> f2 m0 m'0 _x __ _x0 __ __
  
  (** val equal_rect :
      ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1 t
      -> 'a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod list -> __ -> X.t -> 'a1 ->
      (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> ('a1 t -> 'a1
      t -> X.t -> 'a1 -> (X.t, 'a1) prod list -> __ -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> X.t compare0 -> __ -> __ -> 'a2) -> ('a1 t ->
      'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t ->
      'a2 **)
  
  let rec equal_rect cmp f2 f1 f0 f m m' =
    let f3 = f2 m m' in
    let f4 = f1 m m' in
    let f5 = f0 m m' in
    let f6 = f m m' in
    let f7 = f6 m __ in
    let f8 = f7 m' __ in
    (match m with
     | Nil ->
       let f9 = f3 __ in
       (match m' with
        | Nil -> f9 __
        | Cons (p, l) -> f8 __)
     | Cons (p, l) ->
       let Pair (t0, e) = p in
       let f9 = f5 t0 e l __ in
       let f10 = f4 t0 e l __ in
       (match m' with
        | Nil -> f8 __
        | Cons (p0, l0) ->
          let Pair (t1, e0) = p0 in
          let f11 = f9 t1 e0 l0 __ in
          let f12 = let _x = X.compare t0 t1 in f11 _x __ in
          let f13 = f10 t1 e0 l0 __ in
          let f14 = fun _ _ ->
            let hrec = equal_rect cmp f2 f1 f0 f l l0 in f13 __ __ hrec
          in
          (match X.compare t0 t1 with
           | EQ -> f14 __ __
           | _ -> f12 __)))
  
  (** val equal_rec :
      ('a1 -> 'a1 -> bool) -> ('a1 t -> 'a1 t -> __ -> __ -> 'a2) -> ('a1 t
      -> 'a1 t -> X.t -> 'a1 -> (X.t, 'a1) prod list -> __ -> X.t -> 'a1 ->
      (X.t, 'a1) prod list -> __ -> __ -> __ -> 'a2 -> 'a2) -> ('a1 t -> 'a1
      t -> X.t -> 'a1 -> (X.t, 'a1) prod list -> __ -> X.t -> 'a1 -> (X.t,
      'a1) prod list -> __ -> X.t compare0 -> __ -> __ -> 'a2) -> ('a1 t ->
      'a1 t -> 'a1 t -> __ -> 'a1 t -> __ -> __ -> 'a2) -> 'a1 t -> 'a1 t ->
      'a2 **)
  
  let equal_rec cmp =
    equal_rect cmp
  
  (** val coq_R_equal_correct :
      ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool -> 'a1 coq_R_equal **)
  
  let coq_R_equal_correct x x0 x1 res =
    equal_rect x (fun y y0 _ _ z0 _ -> R_equal_0 (y, y0))
      (fun y y0 y1 y2 y3 _ y5 y6 y7 _ _ _ y11 z0 _ -> R_equal_1 (y, y0, y1,
      y2, y3, y5, y6, y7, (equal x y3 y7), (y11 (equal x y3 y7) __)))
      (fun y y0 y1 y2 y3 _ y5 y6 y7 _ y9 _ _ z0 _ -> R_equal_2 (y, y0, y1,
      y2, y3, y5, y6, y7, y9)) (fun y y0 y1 _ y3 _ _ z0 _ -> R_equal_3 (y,
      y0, y1, y3)) x0 x1 res __
  
  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)
  
  let rec map f = function
  | Nil -> Nil
  | Cons (p, m') ->
    let Pair (k, e) = p in Cons ((Pair (k, (f e))), (map f m'))
  
  (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)
  
  let rec mapi f = function
  | Nil -> Nil
  | Cons (p, m') ->
    let Pair (k, e) = p in Cons ((Pair (k, (f k e))), (mapi f m'))
  
  (** val option_cons :
      key -> 'a1 option -> (key, 'a1) prod list -> (key, 'a1) prod list **)
  
  let option_cons k o l =
    match o with
    | Some e -> Cons ((Pair (k, e)), l)
    | None -> l
  
  (** val map2_l :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a3 t **)
  
  let rec map2_l f = function
  | Nil -> Nil
  | Cons (p, l) ->
    let Pair (k, e) = p in option_cons k (f (Some e) None) (map2_l f l)
  
  (** val map2_r :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a2 t -> 'a3 t **)
  
  let rec map2_r f = function
  | Nil -> Nil
  | Cons (p, l') ->
    let Pair (k, e') = p in option_cons k (f None (Some e')) (map2_r f l')
  
  (** val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)
  
  let rec map2 f m = match m with
  | Nil -> map2_r f
  | Cons (p, l) ->
    let Pair (k, e) = p in
    let rec map2_aux m' = match m' with
    | Nil -> map2_l f m
    | Cons (p0, l') ->
      let Pair (k', e') = p0 in
      (match X.compare k k' with
       | LT -> option_cons k (f (Some e) None) (map2 f l m')
       | EQ -> option_cons k (f (Some e) (Some e')) (map2 f l l')
       | GT -> option_cons k' (f None (Some e')) (map2_aux l'))
    in map2_aux
  
  (** val combine : 'a1 t -> 'a2 t -> ('a1 option, 'a2 option) prod t **)
  
  let rec combine m = match m with
  | Nil -> map (fun e' -> Pair (None, (Some e')))
  | Cons (p, l) ->
    let Pair (k, e) = p in
    let rec combine_aux m' = match m' with
    | Nil -> map (fun e0 -> Pair ((Some e0), None)) m
    | Cons (p0, l') ->
      let Pair (k', e') = p0 in
      (match X.compare k k' with
       | LT -> Cons ((Pair (k, (Pair ((Some e), None)))), (combine l m'))
       | EQ ->
         Cons ((Pair (k, (Pair ((Some e), (Some e'))))), (combine l l'))
       | GT -> Cons ((Pair (k', (Pair (None, (Some e'))))), (combine_aux l')))
    in combine_aux
  
  (** val fold_right_pair :
      ('a1 -> 'a2 -> 'a3 -> 'a3) -> ('a1, 'a2) prod list -> 'a3 -> 'a3 **)
  
  let fold_right_pair f l i =
    fold_right (fun p -> f (fst p) (snd p)) i l
  
  (** val map2_alt :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> (key,
      'a3) prod list **)
  
  let map2_alt f m m' =
    let m0 = combine m m' in
    let m1 = map (fun p -> f (fst p) (snd p)) m0 in
    fold_right_pair option_cons m1 Nil
  
  (** val at_least_one :
      'a1 option -> 'a2 option -> ('a1 option, 'a2 option) prod option **)
  
  let at_least_one o o' =
    match o with
    | Some e -> Some (Pair (o, o'))
    | None ->
      (match o' with
       | Some e -> Some (Pair (o, o'))
       | None -> None)
  
  (** val at_least_one_then_f :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2 option ->
      'a3 option **)
  
  let at_least_one_then_f f o o' =
    match o with
    | Some e -> f o o'
    | None ->
      (match o' with
       | Some e -> f o o'
       | None -> None)
 end

module Coq_Raw = 
 functor (I:Int) ->
 functor (X:Coq_OrderedType) ->
 struct 
  type key = X.t
  
  type 'elt tree =
  | Leaf
  | Node of 'elt tree * key * 'elt * 'elt tree * I.t
  
  (** val tree_rect :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> I.t -> 'a2)
      -> 'a1 tree -> 'a2 **)
  
  let rec tree_rect f f0 = function
  | Leaf -> f
  | Node (t1, k, e, t2, t3) ->
    f0 t1 (tree_rect f f0 t1) k e t2 (tree_rect f f0 t2) t3
  
  (** val tree_rec :
      'a2 -> ('a1 tree -> 'a2 -> key -> 'a1 -> 'a1 tree -> 'a2 -> I.t -> 'a2)
      -> 'a1 tree -> 'a2 **)
  
  let rec tree_rec f f0 = function
  | Leaf -> f
  | Node (t1, k, e, t2, t3) ->
    f0 t1 (tree_rec f f0 t1) k e t2 (tree_rec f f0 t2) t3
  
  (** val height : 'a1 tree -> I.t **)
  
  let height = function
  | Leaf -> I._0
  | Node (t0, k, e, t1, h) -> h
  
  (** val cardinal : 'a1 tree -> nat **)
  
  let rec cardinal = function
  | Leaf -> O
  | Node (l, k, e, r, t0) -> S (plus (cardinal l) (cardinal r))
  
  (** val empty : 'a1 tree **)
  
  let empty =
    Leaf
  
  (** val is_empty : 'a1 tree -> bool **)
  
  let is_empty = function
  | Leaf -> True
  | Node (t0, k, e, t1, t2) -> False
  
  (** val mem : X.t -> 'a1 tree -> bool **)
  
  let rec mem x = function
  | Leaf -> False
  | Node (l, y, e, r, t0) ->
    (match X.compare x y with
     | LT -> mem x l
     | EQ -> True
     | GT -> mem x r)
  
  (** val find : X.t -> 'a1 tree -> 'a1 option **)
  
  let rec find x = function
  | Leaf -> None
  | Node (l, y, d, r, t0) ->
    (match X.compare x y with
     | LT -> find x l
     | EQ -> Some d
     | GT -> find x r)
  
  (** val create : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)
  
  let create l x e r =
    Node (l, x, e, r, (I.plus (I.max (height l) (height r)) I._1))
  
  (** val assert_false : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)
  
  let assert_false =
    create
  
  (** val bal : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)
  
  let bal l x d r =
    let hl = height l in
    let hr = height r in
    (match I.gt_le_dec hl (I.plus hr I._2) with
     | Left ->
       (match l with
        | Leaf -> assert_false l x d r
        | Node (ll, lx, ld, lr, t0) ->
          (match I.ge_lt_dec (height ll) (height lr) with
           | Left -> create ll lx ld (create lr x d r)
           | Right ->
             (match lr with
              | Leaf -> assert_false l x d r
              | Node (lrl, lrx, lrd, lrr, t1) ->
                create (create ll lx ld lrl) lrx lrd (create lrr x d r))))
     | Right ->
       (match I.gt_le_dec hr (I.plus hl I._2) with
        | Left ->
          (match r with
           | Leaf -> assert_false l x d r
           | Node (rl, rx, rd, rr, t0) ->
             (match I.ge_lt_dec (height rr) (height rl) with
              | Left -> create (create l x d rl) rx rd rr
              | Right ->
                (match rl with
                 | Leaf -> assert_false l x d r
                 | Node (rll, rlx, rld, rlr, t1) ->
                   create (create l x d rll) rlx rld (create rlr rx rd rr))))
        | Right -> create l x d r))
  
  (** val add : key -> 'a1 -> 'a1 tree -> 'a1 tree **)
  
  let rec add x d = function
  | Leaf -> Node (Leaf, x, d, Leaf, I._1)
  | Node (l, y, d', r, h) ->
    (match X.compare x y with
     | LT -> bal (add x d l) y d' r
     | EQ -> Node (l, y, d, r, h)
     | GT -> bal l y d' (add x d r))
  
  (** val remove_min :
      'a1 tree -> key -> 'a1 -> 'a1 tree -> ('a1 tree, (key, 'a1) prod) prod **)
  
  let rec remove_min l x d r =
    match l with
    | Leaf -> Pair (r, (Pair (x, d)))
    | Node (ll, lx, ld, lr, lh) ->
      let Pair (l', m) = remove_min ll lx ld lr in Pair ((bal l' x d r), m)
  
  (** val merge : 'a1 tree -> 'a1 tree -> 'a1 tree **)
  
  let merge s1 s2 =
    match s1 with
    | Leaf -> s2
    | Node (t0, k, e, t1, t2) ->
      (match s2 with
       | Leaf -> s1
       | Node (l2, x2, d2, r2, h2) ->
         let Pair (s2', p) = remove_min l2 x2 d2 r2 in
         let Pair (x, d) = p in bal s1 x d s2')
  
  (** val remove : X.t -> 'a1 tree -> 'a1 tree **)
  
  let rec remove x = function
  | Leaf -> Leaf
  | Node (l, y, d, r, h) ->
    (match X.compare x y with
     | LT -> bal (remove x l) y d r
     | EQ -> merge l r
     | GT -> bal l y d (remove x r))
  
  (** val join : 'a1 tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree **)
  
  let rec join l = match l with
  | Leaf -> add
  | Node (ll, lx, ld, lr, lh) ->
    (fun x d ->
      let rec join_aux r = match r with
      | Leaf -> add x d l
      | Node (rl, rx, rd, rr, rh) ->
        (match I.gt_le_dec lh (I.plus rh I._2) with
         | Left -> bal ll lx ld (join lr x d r)
         | Right ->
           (match I.gt_le_dec rh (I.plus lh I._2) with
            | Left -> bal (join_aux rl) rx rd rr
            | Right -> create l x d r))
      in join_aux)
  
  type 'elt triple = { t_left : 'elt tree; t_opt : 'elt option;
                       t_right : 'elt tree }
  
  (** val triple_rect :
      ('a1 tree -> 'a1 option -> 'a1 tree -> 'a2) -> 'a1 triple -> 'a2 **)
  
  let triple_rect f t0 =
    let { t_left = x; t_opt = x0; t_right = x1 } = t0 in f x x0 x1
  
  (** val triple_rec :
      ('a1 tree -> 'a1 option -> 'a1 tree -> 'a2) -> 'a1 triple -> 'a2 **)
  
  let triple_rec f t0 =
    let { t_left = x; t_opt = x0; t_right = x1 } = t0 in f x x0 x1
  
  (** val t_left : 'a1 triple -> 'a1 tree **)
  
  let t_left t0 =
    t0.t_left
  
  (** val t_opt : 'a1 triple -> 'a1 option **)
  
  let t_opt t0 =
    t0.t_opt
  
  (** val t_right : 'a1 triple -> 'a1 tree **)
  
  let t_right t0 =
    t0.t_right
  
  (** val split : X.t -> 'a1 tree -> 'a1 triple **)
  
  let rec split x = function
  | Leaf -> { t_left = Leaf; t_opt = None; t_right = Leaf }
  | Node (l, y, d, r, h) ->
    (match X.compare x y with
     | LT ->
       let { t_left = ll; t_opt = o; t_right = rl } = split x l in
       { t_left = ll; t_opt = o; t_right = (join rl y d r) }
     | EQ -> { t_left = l; t_opt = (Some d); t_right = r }
     | GT ->
       let { t_left = rl; t_opt = o; t_right = rr } = split x r in
       { t_left = (join l y d rl); t_opt = o; t_right = rr })
  
  (** val concat : 'a1 tree -> 'a1 tree -> 'a1 tree **)
  
  let concat m1 m2 =
    match m1 with
    | Leaf -> m2
    | Node (t0, k, e, t1, t2) ->
      (match m2 with
       | Leaf -> m1
       | Node (l2, x2, d2, r2, t3) ->
         let Pair (m2', xd) = remove_min l2 x2 d2 r2 in
         join m1 (fst xd) (snd xd) m2')
  
  (** val elements_aux :
      (key, 'a1) prod list -> 'a1 tree -> (key, 'a1) prod list **)
  
  let rec elements_aux acc = function
  | Leaf -> acc
  | Node (l, x, d, r, t0) ->
    elements_aux (Cons ((Pair (x, d)), (elements_aux acc r))) l
  
  (** val elements : 'a1 tree -> (key, 'a1) prod list **)
  
  let elements m =
    elements_aux Nil m
  
  (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2 **)
  
  let rec fold f m a =
    match m with
    | Leaf -> a
    | Node (l, x, d, r, t0) -> fold f r (f x d (fold f l a))
  
  type 'elt enumeration =
  | End
  | More of key * 'elt * 'elt tree * 'elt enumeration
  
  (** val enumeration_rect :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2 **)
  
  let rec enumeration_rect f f0 = function
  | End -> f
  | More (k, e0, t0, e1) -> f0 k e0 t0 e1 (enumeration_rect f f0 e1)
  
  (** val enumeration_rec :
      'a2 -> (key -> 'a1 -> 'a1 tree -> 'a1 enumeration -> 'a2 -> 'a2) -> 'a1
      enumeration -> 'a2 **)
  
  let rec enumeration_rec f f0 = function
  | End -> f
  | More (k, e0, t0, e1) -> f0 k e0 t0 e1 (enumeration_rec f f0 e1)
  
  (** val cons : 'a1 tree -> 'a1 enumeration -> 'a1 enumeration **)
  
  let rec cons m e =
    match m with
    | Leaf -> e
    | Node (l, x, d, r, h) -> cons l (More (x, d, r, e))
  
  (** val equal_more :
      ('a1 -> 'a1 -> bool) -> X.t -> 'a1 -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool **)
  
  let equal_more cmp x1 d1 cont = function
  | End -> False
  | More (x2, d2, r2, e3) ->
    (match X.compare x1 x2 with
     | EQ ->
       (match cmp d1 d2 with
        | True -> cont (cons r2 e3)
        | False -> False)
     | _ -> False)
  
  (** val equal_cont :
      ('a1 -> 'a1 -> bool) -> 'a1 tree -> ('a1 enumeration -> bool) -> 'a1
      enumeration -> bool **)
  
  let rec equal_cont cmp m1 cont e2 =
    match m1 with
    | Leaf -> cont e2
    | Node (l1, x1, d1, r1, t0) ->
      equal_cont cmp l1 (equal_more cmp x1 d1 (equal_cont cmp r1 cont)) e2
  
  (** val equal_end : 'a1 enumeration -> bool **)
  
  let equal_end = function
  | End -> True
  | More (k, e, t0, e0) -> False
  
  (** val equal : ('a1 -> 'a1 -> bool) -> 'a1 tree -> 'a1 tree -> bool **)
  
  let equal cmp m1 m2 =
    equal_cont cmp m1 equal_end (cons m2 End)
  
  (** val map : ('a1 -> 'a2) -> 'a1 tree -> 'a2 tree **)
  
  let rec map f = function
  | Leaf -> Leaf
  | Node (l, x, d, r, h) -> Node ((map f l), x, (f d), (map f r), h)
  
  (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 tree -> 'a2 tree **)
  
  let rec mapi f = function
  | Leaf -> Leaf
  | Node (l, x, d, r, h) -> Node ((mapi f l), x, (f x d), (mapi f r), h)
  
  (** val map_option : (key -> 'a1 -> 'a2 option) -> 'a1 tree -> 'a2 tree **)
  
  let rec map_option f = function
  | Leaf -> Leaf
  | Node (l, x, d, r, h) ->
    (match f x d with
     | Some d' -> join (map_option f l) x d' (map_option f r)
     | None -> concat (map_option f l) (map_option f r))
  
  (** val map2_opt :
      (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
      ('a2 tree -> 'a3 tree) -> 'a1 tree -> 'a2 tree -> 'a3 tree **)
  
  let rec map2_opt f mapl mapr m1 m2 =
    match m1 with
    | Leaf -> mapr m2
    | Node (l1, x1, d1, r1, h1) ->
      (match m2 with
       | Leaf -> mapl m1
       | Node (t0, k, y, t1, t2) ->
         let { t_left = l2'; t_opt = o2; t_right = r2' } = split x1 m2 in
         (match f x1 d1 o2 with
          | Some e ->
            join (map2_opt f mapl mapr l1 l2') x1 e
              (map2_opt f mapl mapr r1 r2')
          | None ->
            concat (map2_opt f mapl mapr l1 l2')
              (map2_opt f mapl mapr r1 r2')))
  
  (** val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 tree -> 'a2 tree -> 'a3
      tree **)
  
  let map2 f =
    map2_opt (fun x d o -> f (Some d) o)
      (map_option (fun x d -> f (Some d) None))
      (map_option (fun x d' -> f None (Some d')))
  
  module Proofs = 
   struct 
    module MX = Coq_OrderedTypeFacts(X)
    
    module PX = KeyOrderedType(X)
    
    module L = Raw(X)
    
    type 'elt coq_R_mem =
    | R_mem_0 of 'elt tree
    | R_mem_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t * 
       bool * 'elt coq_R_mem
    | R_mem_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_mem_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t * 
       bool * 'elt coq_R_mem
    
    (** val coq_R_mem_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2)
        -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2 **)
    
    let rec coq_R_mem_rect x f f0 f1 f2 m b = function
    | R_mem_0 m0 -> f m0 __
    | R_mem_1 (m0, l, y, _x, r0, _x0, res, r1) ->
      f0 m0 l y _x r0 _x0 __ __ __ res r1
        (coq_R_mem_rect x f f0 f1 f2 l res r1)
    | R_mem_2 (m0, l, y, _x, r0, _x0) -> f1 m0 l y _x r0 _x0 __ __ __
    | R_mem_3 (m0, l, y, _x, r0, _x0, res, r1) ->
      f2 m0 l y _x r0 _x0 __ __ __ res r1
        (coq_R_mem_rect x f f0 f1 f2 r0 res r1)
    
    (** val coq_R_mem_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1
        tree -> I.t -> __ -> __ -> __ -> bool -> 'a1 coq_R_mem -> 'a2 -> 'a2)
        -> 'a1 tree -> bool -> 'a1 coq_R_mem -> 'a2 **)
    
    let rec coq_R_mem_rec x f f0 f1 f2 m b = function
    | R_mem_0 m0 -> f m0 __
    | R_mem_1 (m0, l, y, _x, r0, _x0, res, r1) ->
      f0 m0 l y _x r0 _x0 __ __ __ res r1
        (coq_R_mem_rec x f f0 f1 f2 l res r1)
    | R_mem_2 (m0, l, y, _x, r0, _x0) -> f1 m0 l y _x r0 _x0 __ __ __
    | R_mem_3 (m0, l, y, _x, r0, _x0, res, r1) ->
      f2 m0 l y _x r0 _x0 __ __ __ res r1
        (coq_R_mem_rec x f f0 f1 f2 r0 res r1)
    
    type 'elt coq_R_find =
    | R_find_0 of 'elt tree
    | R_find_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt option * 'elt coq_R_find
    | R_find_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_find_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt option * 'elt coq_R_find
    
    (** val coq_R_find_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2 **)
    
    let rec coq_R_find_rect x f f0 f1 f2 m o = function
    | R_find_0 m0 -> f m0 __
    | R_find_1 (m0, l, y, d, r0, _x, res, r1) ->
      f0 m0 l y d r0 _x __ __ __ res r1
        (coq_R_find_rect x f f0 f1 f2 l res r1)
    | R_find_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
    | R_find_3 (m0, l, y, d, r0, _x, res, r1) ->
      f2 m0 l y d r0 _x __ __ __ res r1
        (coq_R_find_rect x f f0 f1 f2 r0 res r1)
    
    (** val coq_R_find_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 option -> 'a1 coq_R_find
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 option -> 'a1 coq_R_find -> 'a2 **)
    
    let rec coq_R_find_rec x f f0 f1 f2 m o = function
    | R_find_0 m0 -> f m0 __
    | R_find_1 (m0, l, y, d, r0, _x, res, r1) ->
      f0 m0 l y d r0 _x __ __ __ res r1
        (coq_R_find_rec x f f0 f1 f2 l res r1)
    | R_find_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
    | R_find_3 (m0, l, y, d, r0, _x, res, r1) ->
      f2 m0 l y d r0 _x __ __ __ res r1
        (coq_R_find_rec x f f0 f1 f2 r0 res r1)
    
    type 'elt coq_R_bal =
    | R_bal_0 of 'elt tree * key * 'elt * 'elt tree
    | R_bal_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t
    | R_bal_2 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t
    | R_bal_3 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * 
       I.t
    | R_bal_4 of 'elt tree * key * 'elt * 'elt tree
    | R_bal_5 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t
    | R_bal_6 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t
    | R_bal_7 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * key * 
       'elt * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * 
       I.t
    | R_bal_8 of 'elt tree * key * 'elt * 'elt tree
    
    (** val coq_R_bal_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
        tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ ->
        'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ ->
        __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
        -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ ->
        __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2 **)
    
    let coq_R_bal_rect f f0 f1 f2 f3 f4 f5 f6 f7 l x d r t0 = function
    | R_bal_0 (x0, x1, x2, x3) -> f x0 x1 x2 x3 __ __ __
    | R_bal_1 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
      f0 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __
    | R_bal_2 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
      f1 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __ __
    | R_bal_3 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) ->
      f2 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __ x9 x10 x11 x12 x13 __
    | R_bal_4 (x0, x1, x2, x3) -> f3 x0 x1 x2 x3 __ __ __ __ __
    | R_bal_5 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
      f4 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __
    | R_bal_6 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
      f5 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __ __
    | R_bal_7 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) ->
      f6 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __ x9 x10 x11 x12 x13
        __
    | R_bal_8 (x0, x1, x2, x3) -> f7 x0 x1 x2 x3 __ __ __ __
    
    (** val coq_R_bal_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key ->
        'a1 -> 'a1 tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1
        tree -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ ->
        'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ ->
        __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __
        -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> __ -> 'a2) -> ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> __ ->
        __ -> __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ ->
        __ -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2) -> ('a1
        tree -> key -> 'a1 -> 'a1 tree -> __ -> __ -> __ -> __ -> 'a2) -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_bal -> 'a2 **)
    
    let coq_R_bal_rec f f0 f1 f2 f3 f4 f5 f6 f7 l x d r t0 = function
    | R_bal_0 (x0, x1, x2, x3) -> f x0 x1 x2 x3 __ __ __
    | R_bal_1 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
      f0 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __
    | R_bal_2 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
      f1 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __ __
    | R_bal_3 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) ->
      f2 x0 x1 x2 x3 __ __ x4 x5 x6 x7 x8 __ __ __ x9 x10 x11 x12 x13 __
    | R_bal_4 (x0, x1, x2, x3) -> f3 x0 x1 x2 x3 __ __ __ __ __
    | R_bal_5 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
      f4 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __
    | R_bal_6 (x0, x1, x2, x3, x4, x5, x6, x7, x8) ->
      f5 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __ __
    | R_bal_7 (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) ->
      f6 x0 x1 x2 x3 __ __ __ __ x4 x5 x6 x7 x8 __ __ __ x9 x10 x11 x12 x13
        __
    | R_bal_8 (x0, x1, x2, x3) -> f7 x0 x1 x2 x3 __ __ __ __
    
    type 'elt coq_R_add =
    | R_add_0 of 'elt tree
    | R_add_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt tree * 'elt coq_R_add
    | R_add_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_add_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt tree * 'elt coq_R_add
    
    (** val coq_R_add_rect :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
        key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add ->
        'a2 **)
    
    let rec coq_R_add_rect x d f f0 f1 f2 m t0 = function
    | R_add_0 m0 -> f m0 __
    | R_add_1 (m0, l, y, d', r0, h, res, r1) ->
      f0 m0 l y d' r0 h __ __ __ res r1
        (coq_R_add_rect x d f f0 f1 f2 l res r1)
    | R_add_2 (m0, l, y, d', r0, h) -> f1 m0 l y d' r0 h __ __ __
    | R_add_3 (m0, l, y, d', r0, h, res, r1) ->
      f2 m0 l y d' r0 h __ __ __ res r1
        (coq_R_add_rect x d f f0 f1 f2 r0 res r1)
    
    (** val coq_R_add_rec :
        key -> 'a1 -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key
        -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree ->
        key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1
        coq_R_add -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_add ->
        'a2 **)
    
    let rec coq_R_add_rec x d f f0 f1 f2 m t0 = function
    | R_add_0 m0 -> f m0 __
    | R_add_1 (m0, l, y, d', r0, h, res, r1) ->
      f0 m0 l y d' r0 h __ __ __ res r1
        (coq_R_add_rec x d f f0 f1 f2 l res r1)
    | R_add_2 (m0, l, y, d', r0, h) -> f1 m0 l y d' r0 h __ __ __
    | R_add_3 (m0, l, y, d', r0, h, res, r1) ->
      f2 m0 l y d' r0 h __ __ __ res r1
        (coq_R_add_rec x d f f0 f1 f2 r0 res r1)
    
    type 'elt coq_R_remove_min =
    | R_remove_min_0 of 'elt tree * key * 'elt * 'elt tree
    | R_remove_min_1 of 'elt tree * key * 'elt * 'elt tree * 'elt tree * 
       key * 'elt * 'elt tree * I.t * ('elt tree, (key, 'elt) prod) prod
       * 'elt coq_R_remove_min * 'elt tree * (key, 'elt) prod
    
    (** val coq_R_remove_min_rect :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> ('a1 tree, (key, 'a1) prod) prod -> 'a1 coq_R_remove_min -> 'a2 ->
        'a1 tree -> (key, 'a1) prod -> __ -> 'a2) -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> ('a1 tree, (key, 'a1) prod) prod -> 'a1
        coq_R_remove_min -> 'a2 **)
    
    let rec coq_R_remove_min_rect f f0 l x d r p = function
    | R_remove_min_0 (l0, x0, d0, r1) -> f l0 x0 d0 r1 __
    | R_remove_min_1 (l0, x0, d0, r1, ll, lx, ld, lr, _x, res, r2, l', m) ->
      f0 l0 x0 d0 r1 ll lx ld lr _x __ res r2
        (coq_R_remove_min_rect f f0 ll lx ld lr res r2) l' m __
    
    (** val coq_R_remove_min_rec :
        ('a1 tree -> key -> 'a1 -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> key
        -> 'a1 -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> ('a1 tree, (key, 'a1) prod) prod -> 'a1 coq_R_remove_min -> 'a2 ->
        'a1 tree -> (key, 'a1) prod -> __ -> 'a2) -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> ('a1 tree, (key, 'a1) prod) prod -> 'a1
        coq_R_remove_min -> 'a2 **)
    
    let rec coq_R_remove_min_rec f f0 l x d r p = function
    | R_remove_min_0 (l0, x0, d0, r1) -> f l0 x0 d0 r1 __
    | R_remove_min_1 (l0, x0, d0, r1, ll, lx, ld, lr, _x, res, r2, l', m) ->
      f0 l0 x0 d0 r1 ll lx ld lr _x __ res r2
        (coq_R_remove_min_rec f f0 ll lx ld lr res r2) l' m __
    
    type 'elt coq_R_merge =
    | R_merge_0 of 'elt tree * 'elt tree
    | R_merge_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt * 'elt tree
       * I.t
    | R_merge_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt * 'elt tree
       * I.t * 'elt tree * key * 'elt * 'elt tree * I.t * 'elt tree
       * (key, 'elt) prod * key * 'elt
    
    (** val coq_R_merge_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key, 'a1) prod -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 tree -> 'a1 coq_R_merge -> 'a2 **)
    
    let coq_R_merge_rect f f0 f1 s1 s2 t0 = function
    | R_merge_0 (x, x0) -> f x x0 __
    | R_merge_1 (x, x0, x1, x2, x3, x4, x5) -> f0 x x0 x1 x2 x3 x4 x5 __ __
    | R_merge_2 (x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12,
                 x13, x14) ->
      f1 x x0 x1 x2 x3 x4 x5 __ x6 x7 x8 x9 x10 __ x11 x12 __ x13 x14 __
    
    (** val coq_R_merge_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key, 'a1) prod -> __ -> key -> 'a1 -> __ -> 'a2) -> 'a1 tree -> 'a1
        tree -> 'a1 tree -> 'a1 coq_R_merge -> 'a2 **)
    
    let coq_R_merge_rec f f0 f1 s1 s2 t0 = function
    | R_merge_0 (x, x0) -> f x x0 __
    | R_merge_1 (x, x0, x1, x2, x3, x4, x5) -> f0 x x0 x1 x2 x3 x4 x5 __ __
    | R_merge_2 (x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12,
                 x13, x14) ->
      f1 x x0 x1 x2 x3 x4 x5 __ x6 x7 x8 x9 x10 __ x11 x12 __ x13 x14 __
    
    type 'elt coq_R_remove =
    | R_remove_0 of 'elt tree
    | R_remove_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'elt tree * 'elt coq_R_remove
    | R_remove_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_remove_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'elt tree * 'elt coq_R_remove
    
    (** val coq_R_remove_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 **)
    
    let rec coq_R_remove_rect x f f0 f1 f2 m t0 = function
    | R_remove_0 m0 -> f m0 __
    | R_remove_1 (m0, l, y, d, r0, _x, res, r1) ->
      f0 m0 l y d r0 _x __ __ __ res r1
        (coq_R_remove_rect x f f0 f1 f2 l res r1)
    | R_remove_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
    | R_remove_3 (m0, l, y, d, r0, _x, res, r1) ->
      f2 m0 l y d r0 _x __ __ __ res r1
        (coq_R_remove_rect x f f0 f1 f2 r0 res r1)
    
    (** val coq_R_remove_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree ->
        I.t -> __ -> __ -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 tree -> 'a1 coq_R_remove
        -> 'a2 -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 coq_R_remove -> 'a2 **)
    
    let rec coq_R_remove_rec x f f0 f1 f2 m t0 = function
    | R_remove_0 m0 -> f m0 __
    | R_remove_1 (m0, l, y, d, r0, _x, res, r1) ->
      f0 m0 l y d r0 _x __ __ __ res r1
        (coq_R_remove_rec x f f0 f1 f2 l res r1)
    | R_remove_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
    | R_remove_3 (m0, l, y, d, r0, _x, res, r1) ->
      f2 m0 l y d r0 _x __ __ __ res r1
        (coq_R_remove_rec x f f0 f1 f2 r0 res r1)
    
    type 'elt coq_R_concat =
    | R_concat_0 of 'elt tree * 'elt tree
    | R_concat_1 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
       * 'elt tree * I.t
    | R_concat_2 of 'elt tree * 'elt tree * 'elt tree * key * 'elt
       * 'elt tree * I.t * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt tree * (key, 'elt) prod
    
    (** val coq_R_concat_rect :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key, 'a1) prod -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree ->
        'a1 coq_R_concat -> 'a2 **)
    
    let coq_R_concat_rect f f0 f1 m1 m2 t0 = function
    | R_concat_0 (x, x0) -> f x x0 __
    | R_concat_1 (x, x0, x1, x2, x3, x4, x5) -> f0 x x0 x1 x2 x3 x4 x5 __ __
    | R_concat_2 (x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) ->
      f1 x x0 x1 x2 x3 x4 x5 __ x6 x7 x8 x9 x10 __ x11 x12 __
    
    (** val coq_R_concat_rec :
        ('a1 tree -> 'a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> 'a1
        tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a1 tree ->
        (key, 'a1) prod -> __ -> 'a2) -> 'a1 tree -> 'a1 tree -> 'a1 tree ->
        'a1 coq_R_concat -> 'a2 **)
    
    let coq_R_concat_rec f f0 f1 m1 m2 t0 = function
    | R_concat_0 (x, x0) -> f x x0 __
    | R_concat_1 (x, x0, x1, x2, x3, x4, x5) -> f0 x x0 x1 x2 x3 x4 x5 __ __
    | R_concat_2 (x, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) ->
      f1 x x0 x1 x2 x3 x4 x5 __ x6 x7 x8 x9 x10 __ x11 x12 __
    
    type 'elt coq_R_split =
    | R_split_0 of 'elt tree
    | R_split_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt triple * 'elt coq_R_split * 'elt tree * 'elt option * 'elt tree
    | R_split_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
    | R_split_3 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * I.t
       * 'elt triple * 'elt coq_R_split * 'elt tree * 'elt option * 'elt tree
    
    (** val coq_R_split_rect :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
        -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree ->
        'a1 option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 **)
    
    let rec coq_R_split_rect x f f0 f1 f2 m t0 = function
    | R_split_0 m0 -> f m0 __
    | R_split_1 (m0, l, y, d, r0, _x, res, r1, ll, o, rl) ->
      f0 m0 l y d r0 _x __ __ __ res r1
        (coq_R_split_rect x f f0 f1 f2 l res r1) ll o rl __
    | R_split_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
    | R_split_3 (m0, l, y, d, r0, _x, res, r1, rl, o, rr) ->
      f2 m0 l y d r0 _x __ __ __ res r1
        (coq_R_split_rect x f f0 f1 f2 r0 res r1) rl o rr __
    
    (** val coq_R_split_rec :
        X.t -> ('a1 tree -> __ -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1
        -> 'a1 tree -> I.t -> __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split
        -> 'a2 -> 'a1 tree -> 'a1 option -> 'a1 tree -> __ -> 'a2) -> ('a1
        tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> __ -> __
        -> 'a2) -> ('a1 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t ->
        __ -> __ -> __ -> 'a1 triple -> 'a1 coq_R_split -> 'a2 -> 'a1 tree ->
        'a1 option -> 'a1 tree -> __ -> 'a2) -> 'a1 tree -> 'a1 triple -> 'a1
        coq_R_split -> 'a2 **)
    
    let rec coq_R_split_rec x f f0 f1 f2 m t0 = function
    | R_split_0 m0 -> f m0 __
    | R_split_1 (m0, l, y, d, r0, _x, res, r1, ll, o, rl) ->
      f0 m0 l y d r0 _x __ __ __ res r1
        (coq_R_split_rec x f f0 f1 f2 l res r1) ll o rl __
    | R_split_2 (m0, l, y, d, r0, _x) -> f1 m0 l y d r0 _x __ __ __
    | R_split_3 (m0, l, y, d, r0, _x, res, r1, rl, o, rr) ->
      f2 m0 l y d r0 _x __ __ __ res r1
        (coq_R_split_rec x f f0 f1 f2 r0 res r1) rl o rr __
    
    type ('elt, 'elt') coq_R_map_option =
    | R_map_option_0 of 'elt tree
    | R_map_option_1 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'elt' * 'elt' tree * ('elt, 'elt') coq_R_map_option * 'elt' tree
       * ('elt, 'elt') coq_R_map_option
    | R_map_option_2 of 'elt tree * 'elt tree * key * 'elt * 'elt tree * 
       I.t * 'elt' tree * ('elt, 'elt') coq_R_map_option * 'elt' tree
       * ('elt, 'elt') coq_R_map_option
    
    (** val coq_R_map_option_rect :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 -> __ -> 'a2
        tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 **)
    
    let rec coq_R_map_option_rect f f0 f1 f2 m t0 = function
    | R_map_option_0 m0 -> f0 m0 __
    | R_map_option_1 (m0, l, x, d, r0, _x, d', res0, r1, res, r2) ->
      f1 m0 l x d r0 _x __ d' __ res0 r1
        (coq_R_map_option_rect f f0 f1 f2 l res0 r1) res r2
        (coq_R_map_option_rect f f0 f1 f2 r0 res r2)
    | R_map_option_2 (m0, l, x, d, r0, _x, res0, r1, res, r2) ->
      f2 m0 l x d r0 _x __ __ res0 r1
        (coq_R_map_option_rect f f0 f1 f2 l res0 r1) res r2
        (coq_R_map_option_rect f f0 f1 f2 r0 res r2)
    
    (** val coq_R_map_option_rec :
        (key -> 'a1 -> 'a2 option) -> ('a1 tree -> __ -> 'a3) -> ('a1 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 -> __ -> 'a2
        tree -> ('a1, 'a2) coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a3) -> ('a1 tree -> 'a1 tree -> key ->
        'a1 -> 'a1 tree -> I.t -> __ -> __ -> 'a2 tree -> ('a1, 'a2)
        coq_R_map_option -> 'a3 -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 -> 'a3) -> 'a1 tree -> 'a2 tree -> ('a1, 'a2) coq_R_map_option ->
        'a3 **)
    
    let rec coq_R_map_option_rec f f0 f1 f2 m t0 = function
    | R_map_option_0 m0 -> f0 m0 __
    | R_map_option_1 (m0, l, x, d, r0, _x, d', res0, r1, res, r2) ->
      f1 m0 l x d r0 _x __ d' __ res0 r1
        (coq_R_map_option_rec f f0 f1 f2 l res0 r1) res r2
        (coq_R_map_option_rec f f0 f1 f2 r0 res r2)
    | R_map_option_2 (m0, l, x, d, r0, _x, res0, r1, res, r2) ->
      f2 m0 l x d r0 _x __ __ res0 r1
        (coq_R_map_option_rec f f0 f1 f2 l res0 r1) res r2
        (coq_R_map_option_rec f f0 f1 f2 r0 res r2)
    
    type ('elt, 'elt', 'elt'') coq_R_map2_opt =
    | R_map2_opt_0 of 'elt tree * 'elt' tree
    | R_map2_opt_1 of 'elt tree * 'elt' tree * 'elt tree * key * 'elt
       * 'elt tree * I.t
    | R_map2_opt_2 of 'elt tree * 'elt' tree * 'elt tree * key * 'elt
       * 'elt tree * I.t * 'elt' tree * key * 'elt' * 'elt' tree * I.t
       * 'elt' tree * 'elt' option * 'elt' tree * 'elt'' * 'elt'' tree
       * ('elt, 'elt', 'elt'') coq_R_map2_opt * 'elt'' tree
       * ('elt, 'elt', 'elt'') coq_R_map2_opt
    | R_map2_opt_3 of 'elt tree * 'elt' tree * 'elt tree * key * 'elt
       * 'elt tree * I.t * 'elt' tree * key * 'elt' * 'elt' tree * I.t
       * 'elt' tree * 'elt' option * 'elt' tree * 'elt'' tree
       * ('elt, 'elt', 'elt'') coq_R_map2_opt * 'elt'' tree
       * ('elt, 'elt', 'elt'') coq_R_map2_opt
    
    (** val coq_R_map2_opt_rect :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> I.t ->
        __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1,
        'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 tree -> key ->
        'a2 -> 'a2 tree -> I.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree ->
        __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree ->
        'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 **)
    
    let rec coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 m1 m2 t0 = function
    | R_map2_opt_0 (m3, m4) -> f0 m3 m4 __
    | R_map2_opt_1 (m3, m4, l1, x1, d1, r1, _x) ->
      f1 m3 m4 l1 x1 d1 r1 _x __ __
    | R_map2_opt_2 (m3, m4, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4, l2',
                    o2, r2', e, res0, r0, res, r2) ->
      f2 m3 m4 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ e __
        res0 r0 (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 l1 l2' res0 r0)
        res r2 (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 r1 r2' res r2)
    | R_map2_opt_3 (m3, m4, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4, l2',
                    o2, r2', res0, r0, res, r2) ->
      f3 m3 m4 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ __ res0
        r0 (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 l1 l2' res0 r0) res
        r2 (coq_R_map2_opt_rect f mapl mapr f0 f1 f2 f3 r1 r2' res r2)
    
    (** val coq_R_map2_opt_rec :
        (key -> 'a1 -> 'a2 option -> 'a3 option) -> ('a1 tree -> 'a3 tree) ->
        ('a2 tree -> 'a3 tree) -> ('a1 tree -> 'a2 tree -> __ -> 'a4) -> ('a1
        tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __
        -> __ -> 'a4) -> ('a1 tree -> 'a2 tree -> 'a1 tree -> key -> 'a1 ->
        'a1 tree -> I.t -> __ -> 'a2 tree -> key -> 'a2 -> 'a2 tree -> I.t ->
        __ -> 'a2 tree -> 'a2 option -> 'a2 tree -> __ -> 'a3 -> __ -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3 tree -> ('a1,
        'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> ('a1 tree -> 'a2 tree ->
        'a1 tree -> key -> 'a1 -> 'a1 tree -> I.t -> __ -> 'a2 tree -> key ->
        'a2 -> 'a2 tree -> I.t -> __ -> 'a2 tree -> 'a2 option -> 'a2 tree ->
        __ -> __ -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a3
        tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 -> 'a4) -> 'a1 tree ->
        'a2 tree -> 'a3 tree -> ('a1, 'a2, 'a3) coq_R_map2_opt -> 'a4 **)
    
    let rec coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 m1 m2 t0 = function
    | R_map2_opt_0 (m3, m4) -> f0 m3 m4 __
    | R_map2_opt_1 (m3, m4, l1, x1, d1, r1, _x) ->
      f1 m3 m4 l1 x1 d1 r1 _x __ __
    | R_map2_opt_2 (m3, m4, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4, l2',
                    o2, r2', e, res0, r0, res, r2) ->
      f2 m3 m4 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ e __
        res0 r0 (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 l1 l2' res0 r0)
        res r2 (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 r1 r2' res r2)
    | R_map2_opt_3 (m3, m4, l1, x1, d1, r1, _x, _x0, _x1, _x2, _x3, _x4, l2',
                    o2, r2', res0, r0, res, r2) ->
      f3 m3 m4 l1 x1 d1 r1 _x __ _x0 _x1 _x2 _x3 _x4 __ l2' o2 r2' __ __ res0
        r0 (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 l1 l2' res0 r0) res r2
        (coq_R_map2_opt_rec f mapl mapr f0 f1 f2 f3 r1 r2' res r2)
    
    (** val fold' : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 tree -> 'a2 -> 'a2 **)
    
    let fold' f s =
      L.fold f (elements s)
    
    (** val flatten_e : 'a1 enumeration -> (key, 'a1) prod list **)
    
    let rec flatten_e = function
    | End -> Nil
    | More (x, e0, t0, r) ->
      Cons ((Pair (x, e0)), (app (elements t0) (flatten_e r)))
   end
 end

module Coq0_IntMake = 
 functor (I:Int) ->
 functor (X:Coq_OrderedType) ->
 struct 
  module E = X
  
  module Raw = Coq_Raw(I)(X)
  
  type 'elt bst =
    'elt Raw.tree
    (* singleton inductive, whose constructor was Bst *)
  
  (** val bst_rect : ('a1 Raw.tree -> __ -> 'a2) -> 'a1 bst -> 'a2 **)
  
  let bst_rect f b =
    f b __
  
  (** val bst_rec : ('a1 Raw.tree -> __ -> 'a2) -> 'a1 bst -> 'a2 **)
  
  let bst_rec f b =
    f b __
  
  (** val this : 'a1 bst -> 'a1 Raw.tree **)
  
  let this b =
    b
  
  type 'elt t = 'elt bst
  
  type key = E.t
  
  (** val empty : 'a1 t **)
  
  let empty =
    Raw.empty
  
  (** val is_empty : 'a1 t -> bool **)
  
  let is_empty m =
    Raw.is_empty (this m)
  
  (** val add : key -> 'a1 -> 'a1 t -> 'a1 t **)
  
  let add x e m =
    Raw.add x e (this m)
  
  (** val remove : key -> 'a1 t -> 'a1 t **)
  
  let remove x m =
    Raw.remove x (this m)
  
  (** val mem : key -> 'a1 t -> bool **)
  
  let mem x m =
    Raw.mem x (this m)
  
  (** val find : key -> 'a1 t -> 'a1 option **)
  
  let find x m =
    Raw.find x (this m)
  
  (** val map : ('a1 -> 'a2) -> 'a1 t -> 'a2 t **)
  
  let map f m =
    Raw.map f (this m)
  
  (** val mapi : (key -> 'a1 -> 'a2) -> 'a1 t -> 'a2 t **)
  
  let mapi f m =
    Raw.mapi f (this m)
  
  (** val map2 :
      ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 t -> 'a2 t -> 'a3 t **)
  
  let map2 f m m' =
    Raw.map2 f (this m) (this m')
  
  (** val elements : 'a1 t -> (key, 'a1) prod list **)
  
  let elements m =
    Raw.elements (this m)
  
  (** val cardinal : 'a1 t -> nat **)
  
  let cardinal m =
    Raw.cardinal (this m)
  
  (** val fold : (key -> 'a1 -> 'a2 -> 'a2) -> 'a1 t -> 'a2 -> 'a2 **)
  
  let fold f m i =
    Raw.fold f (this m) i
  
  (** val equal : ('a1 -> 'a1 -> bool) -> 'a1 t -> 'a1 t -> bool **)
  
  let equal cmp m m' =
    Raw.equal cmp (this m) (this m')
 end

module Coq_Make = 
 functor (X:Coq_OrderedType) ->
 Coq0_IntMake(Z_as_Int)(X)

type 'a comparable =
  'a -> 'a -> comparison
  (* singleton inductive, whose constructor was Build_Comparable *)

(** val compare1 : 'a1 comparable -> 'a1 -> 'a1 -> comparison **)

let compare1 comparable0 =
  comparable0

(** val natComparable : nat comparable **)

let natComparable =
  nat_compare

(** val pairComparable :
    'a1 comparable -> 'a2 comparable -> ('a1, 'a2) prod comparable **)

let pairComparable cA cB x y =
  let Pair (xa, xb) = x in
  let Pair (ya, yb) = y in
  (match compare1 cA xa ya with
   | Eq -> compare1 cB xb yb
   | _ -> compare1 cA xa ya)

(** val compare_eqb : 'a1 comparable -> 'a1 -> 'a1 -> bool **)

let compare_eqb c x y =
  match compare1 c x y with
  | Eq -> True
  | _ -> False

(** val compare_eqdec : 'a1 comparable -> 'a1 -> 'a1 -> sumbool **)

let compare_eqdec c x y =
  let c0 = compare1 c x y in
  (match c0 with
   | Eq -> Left
   | _ -> Right)

type 'a finite =
  'a list
  (* singleton inductive, whose constructor was Build_Finite *)

(** val all_list : 'a1 finite -> 'a1 list **)

let all_list finite0 =
  finite0

type 'a alphabet = { alphabetComparable : 'a comparable;
                     alphabetFinite : 'a finite }

(** val alphabetComparable : 'a1 alphabet -> 'a1 comparable **)

let alphabetComparable x = x.alphabetComparable

(** val alphabetFinite : 'a1 alphabet -> 'a1 finite **)

let alphabetFinite x = x.alphabetFinite

type 'a numbered = { inj : ('a -> int31); surj : (int31 -> 'a);
                     inj_bound : int31 }

(** val inj : 'a1 numbered -> 'a1 -> int31 **)

let inj x = x.inj

(** val surj : 'a1 numbered -> int31 -> 'a1 **)

let surj x = x.surj

(** val inj_bound : 'a1 numbered -> int31 **)

let inj_bound x = x.inj_bound

(** val numberedAlphabet : 'a1 numbered -> 'a1 alphabet **)

let numberedAlphabet n0 =
  { alphabetComparable = (fun x y -> compare31 (n0.inj x) (n0.inj y));
    alphabetFinite =
    (fst
      (iter_int31 n0.inj_bound (fun p -> Pair ((Cons ((n0.surj (snd p)),
        (fst p))), (incr (snd p)))) (Pair (Nil, (I31 (D0, D0, D0, D0, D0, D0,
        D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0,
        D0, D0, D0, D0, D0, D0, D0, D0)))))) }

module type ComparableM = 
 sig 
  type t 
  
  val tComparable : t comparable
 end

module OrderedTypeAlt_from_ComparableM = 
 functor (C:ComparableM) ->
 struct 
  type t = C.t
  
  (** val compare : t -> t -> comparison **)
  
  let compare =
    compare1 C.tComparable
 end

module OrderedType_from_ComparableM = 
 functor (C:ComparableM) ->
 struct 
  module Alt = OrderedTypeAlt_from_ComparableM(C)
  
  type t = Alt.t
  
  (** val compare : Alt.t -> Alt.t -> Alt.t compare0 **)
  
  let compare x y =
    match Alt.compare x y with
    | Eq -> EQ
    | Lt -> LT
    | Gt -> GT
  
  (** val eq_dec : Alt.t -> Alt.t -> sumbool **)
  
  let eq_dec x y =
    match Alt.compare x y with
    | Eq -> Left
    | _ -> Right
 end

type 'x arrows_left = __

type 'x arrows_right = __

type tuple = __

(** val uncurry : __ list -> 'a1 arrows_left -> tuple -> 'a1 **)

let rec uncurry args f x =
  match args with
  | Nil -> Obj.magic f
  | Cons (_, q) ->
    let Pair (d, t0) = Obj.magic x in Obj.magic uncurry q f t0 d

module type T = 
 sig 
  type terminal 
  
  type nonterminal 
  
  val coq_TerminalAlph : terminal alphabet
  
  val coq_NonTerminalAlph : nonterminal alphabet
  
  type symbol =
  | T of terminal
  | NT of nonterminal
  
  val symbol_rect :
    (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1
  
  val symbol_rec : (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1
  
  val coq_SymbolAlph : symbol alphabet
  
  type symbol_semantic_type 
  
  type production 
  
  val coq_ProductionAlph : production alphabet
  
  val prod_lhs : production -> nonterminal
  
  val prod_rhs_rev : production -> symbol list
  
  val prod_action : production -> symbol_semantic_type arrows_left
 end

module type Coq_T = 
 sig 
  module Gram : 
   T
  
  type noninitstate 
  
  val coq_NonInitStateAlph : noninitstate alphabet
  
  type initstate 
  
  val coq_InitStateAlph : initstate alphabet
  
  val last_symb_of_non_init_state : noninitstate -> Gram.symbol
  
  type state =
  | Init of initstate
  | Ninit of noninitstate
  
  val state_rect :
    (initstate -> 'a1) -> (noninitstate -> 'a1) -> state -> 'a1
  
  val state_rec : (initstate -> 'a1) -> (noninitstate -> 'a1) -> state -> 'a1
  
  val coq_StateAlph : state alphabet
  
  type lookahead_action =
  | Shift_act of noninitstate
  | Reduce_act of Gram.production
  | Fail_act
  
  val lookahead_action_rect :
    Gram.terminal -> (noninitstate -> __ -> 'a1) -> (Gram.production -> 'a1)
    -> 'a1 -> lookahead_action -> 'a1
  
  val lookahead_action_rec :
    Gram.terminal -> (noninitstate -> __ -> 'a1) -> (Gram.production -> 'a1)
    -> 'a1 -> lookahead_action -> 'a1
  
  type action =
  | Default_reduce_act of Gram.production
  | Lookahead_act of (Gram.terminal -> lookahead_action)
  
  val action_rect :
    (Gram.production -> 'a1) -> ((Gram.terminal -> lookahead_action) -> 'a1)
    -> action -> 'a1
  
  val action_rec :
    (Gram.production -> 'a1) -> ((Gram.terminal -> lookahead_action) -> 'a1)
    -> action -> 'a1
  
  type item = { prod_item : Gram.production; dot_pos_item : nat;
                lookaheads_item : Gram.terminal list }
  
  val item_rect :
    (Gram.production -> nat -> Gram.terminal list -> 'a1) -> item -> 'a1
  
  val item_rec :
    (Gram.production -> nat -> Gram.terminal list -> 'a1) -> item -> 'a1
  
  val prod_item : item -> Gram.production
  
  val dot_pos_item : item -> nat
  
  val lookaheads_item : item -> Gram.terminal list
  
  module GramDefs : 
   sig 
    type token = (Gram.terminal, Gram.symbol_semantic_type) sigT
    
    type parse_tree =
    | Terminal_pt of Gram.terminal * Gram.symbol_semantic_type
    | Non_terminal_pt of Gram.production * token list * tuple
       * parse_tree_list
    and parse_tree_list =
    | Nil_ptl
    | Cons_ptl of Gram.symbol * token list * Gram.symbol_semantic_type
       * parse_tree * Gram.symbol list * token list * tuple * parse_tree_list
    
    val parse_tree_rect :
      (Gram.terminal -> Gram.symbol_semantic_type -> 'a1) -> (Gram.production
      -> token list -> tuple -> parse_tree_list -> 'a1) -> Gram.symbol ->
      token list -> Gram.symbol_semantic_type -> parse_tree -> 'a1
    
    val parse_tree_rec :
      (Gram.terminal -> Gram.symbol_semantic_type -> 'a1) -> (Gram.production
      -> token list -> tuple -> parse_tree_list -> 'a1) -> Gram.symbol ->
      token list -> Gram.symbol_semantic_type -> parse_tree -> 'a1
    
    val parse_tree_list_rect :
      'a1 -> (Gram.symbol -> token list -> Gram.symbol_semantic_type ->
      parse_tree -> Gram.symbol list -> token list -> tuple ->
      parse_tree_list -> 'a1 -> 'a1) -> Gram.symbol list -> token list ->
      tuple -> parse_tree_list -> 'a1
    
    val parse_tree_list_rec :
      'a1 -> (Gram.symbol -> token list -> Gram.symbol_semantic_type ->
      parse_tree -> Gram.symbol list -> token list -> tuple ->
      parse_tree_list -> 'a1 -> 'a1) -> Gram.symbol list -> token list ->
      tuple -> parse_tree_list -> 'a1
    
    val pt_size :
      Gram.symbol -> token list -> Gram.symbol_semantic_type -> parse_tree ->
      nat
    
    val ptl_size :
      Gram.symbol list -> token list -> tuple -> parse_tree_list -> nat
   end
  
  val start_nt : initstate -> Gram.nonterminal
  
  val action_table : state -> action
  
  val goto_table : state -> Gram.nonterminal -> noninitstate option
  
  val past_symb_of_non_init_state : noninitstate -> Gram.symbol list
  
  val past_state_of_non_init_state : noninitstate -> (state -> bool) list
  
  val items_of_state : state -> item list
  
  val nullable_nterm : Gram.nonterminal -> bool
  
  val first_nterm : Gram.nonterminal -> Gram.terminal list
 end

type 'a stream = 'a __stream Lazy.t
and 'a __stream =
| Cons0 of 'a * 'a stream

(** val hd : 'a1 stream -> 'a1 **)

let hd x =
  let Cons0 (a, s) = Lazy.force x in a

(** val tl : 'a1 stream -> 'a1 stream **)

let tl x =
  let Cons0 (a, s) = Lazy.force x in s

module Coq0_Make = 
 functor (A:Coq_T) ->
 struct 
  (** val singleton_state_pred : A.state -> A.state -> bool **)
  
  let singleton_state_pred state0 state' =
    match compare1 A.coq_StateAlph.alphabetComparable state0 state' with
    | Eq -> True
    | _ -> False
  
  (** val past_state_of_state : A.state -> (A.state -> bool) list **)
  
  let past_state_of_state = function
  | A.Init i -> Nil
  | A.Ninit nis -> A.past_state_of_non_init_state nis
  
  (** val head_symbs_of_state : A.state -> A.Gram.symbol list **)
  
  let head_symbs_of_state = function
  | A.Init i -> Nil
  | A.Ninit s ->
    Cons ((A.last_symb_of_non_init_state s),
      (A.past_symb_of_non_init_state s))
  
  (** val head_states_of_state : A.state -> (A.state -> bool) list **)
  
  let head_states_of_state state0 =
    Cons ((singleton_state_pred state0), (past_state_of_state state0))
  
  (** val is_prefix : A.Gram.symbol list -> A.Gram.symbol list -> bool **)
  
  let rec is_prefix l1 l2 =
    match l1 with
    | Nil -> True
    | Cons (t1, q1) ->
      (match l2 with
       | Nil -> False
       | Cons (t2, q2) ->
         (match compare_eqb A.Gram.coq_SymbolAlph.alphabetComparable t1 t2 with
          | True -> is_prefix q1 q2
          | False -> False))
  
  (** val is_shift_head_symbs : unit0 -> bool **)
  
  let is_shift_head_symbs x =
    forallb (fun s ->
      match A.action_table s with
      | A.Default_reduce_act p -> True
      | A.Lookahead_act awp ->
        forallb (fun t0 ->
          match awp t0 with
          | A.Shift_act s2 ->
            is_prefix (A.past_symb_of_non_init_state s2)
              (head_symbs_of_state s)
          | _ -> True) (all_list A.Gram.coq_TerminalAlph.alphabetFinite))
      (all_list A.coq_StateAlph.alphabetFinite)
  
  (** val is_goto_head_symbs : unit0 -> bool **)
  
  let is_goto_head_symbs x =
    forallb (fun s ->
      forallb (fun nt ->
        match A.goto_table s nt with
        | Some s0 ->
          is_prefix (A.past_symb_of_non_init_state s0)
            (head_symbs_of_state s)
        | None -> True) (all_list A.Gram.coq_NonTerminalAlph.alphabetFinite))
      (all_list A.coq_StateAlph.alphabetFinite)
  
  (** val is_prefix_pred :
      (A.state -> bool) list -> (A.state -> bool) list -> bool **)
  
  let rec is_prefix_pred l1 l2 =
    match l1 with
    | Nil -> True
    | Cons (f1, q1) ->
      (match l2 with
       | Nil -> False
       | Cons (f2, q2) ->
         (match forallb (fun x -> implb (f2 x) (f1 x))
                  (all_list A.coq_StateAlph.alphabetFinite) with
          | True -> is_prefix_pred q1 q2
          | False -> False))
  
  (** val is_shift_past_state : unit0 -> bool **)
  
  let is_shift_past_state x =
    forallb (fun s ->
      match A.action_table s with
      | A.Default_reduce_act p -> True
      | A.Lookahead_act awp ->
        forallb (fun t0 ->
          match awp t0 with
          | A.Shift_act s2 ->
            is_prefix_pred (A.past_state_of_non_init_state s2)
              (head_states_of_state s)
          | _ -> True) (all_list A.Gram.coq_TerminalAlph.alphabetFinite))
      (all_list A.coq_StateAlph.alphabetFinite)
  
  (** val is_goto_past_state : unit0 -> bool **)
  
  let is_goto_past_state x =
    forallb (fun s ->
      forallb (fun nt ->
        match A.goto_table s nt with
        | Some s0 ->
          is_prefix_pred (A.past_state_of_non_init_state s0)
            (head_states_of_state s)
        | None -> True) (all_list A.Gram.coq_NonTerminalAlph.alphabetFinite))
      (all_list A.coq_StateAlph.alphabetFinite)
  
  (** val is_state_valid_after_pop :
      A.state -> A.Gram.symbol list -> (A.state -> bool) list -> bool **)
  
  let rec is_state_valid_after_pop state0 to_pop = function
  | Nil -> True
  | Cons (p, pl) ->
    (match to_pop with
     | Nil -> p state0
     | Cons (s, sl) -> is_state_valid_after_pop state0 sl pl)
  
  (** val is_valid_for_reduce : A.state -> A.Gram.production -> bool **)
  
  let is_valid_for_reduce state0 prod0 =
    match is_prefix (A.Gram.prod_rhs_rev prod0) (head_symbs_of_state state0) with
    | True ->
      forallb (fun state_new ->
        match is_state_valid_after_pop state_new (A.Gram.prod_rhs_rev prod0)
                (head_states_of_state state0) with
        | True ->
          (match A.goto_table state_new (A.Gram.prod_lhs prod0) with
           | Some s -> True
           | None ->
             (match state_new with
              | A.Init i ->
                compare_eqb A.Gram.coq_NonTerminalAlph.alphabetComparable
                  (A.Gram.prod_lhs prod0) (A.start_nt i)
              | A.Ninit n0 -> False))
        | False -> True) (all_list A.coq_StateAlph.alphabetFinite)
    | False -> False
  
  (** val is_reduce_ok : unit0 -> bool **)
  
  let is_reduce_ok x =
    forallb (fun s ->
      match A.action_table s with
      | A.Default_reduce_act p -> is_valid_for_reduce s p
      | A.Lookahead_act awp ->
        forallb (fun t0 ->
          match awp t0 with
          | A.Reduce_act p -> is_valid_for_reduce s p
          | _ -> True) (all_list A.Gram.coq_TerminalAlph.alphabetFinite))
      (all_list A.coq_StateAlph.alphabetFinite)
  
  (** val is_safe : unit0 -> bool **)
  
  let is_safe x =
    match match match match is_shift_head_symbs Tt with
                      | True -> is_goto_head_symbs Tt
                      | False -> False with
                | True -> is_shift_past_state Tt
                | False -> False with
          | True -> is_goto_past_state Tt
          | False -> False with
    | True -> is_reduce_ok Tt
    | False -> False
 end

module Coq1_Make = 
 functor (A:Coq_T) ->
 struct 
  type 'a result =
  | Err
  | OK of 'a
  
  (** val result_rect : 'a2 -> ('a1 -> 'a2) -> 'a1 result -> 'a2 **)
  
  let result_rect f f0 = function
  | Err -> f
  | OK x -> f0 x
  
  (** val result_rec : 'a2 -> ('a1 -> 'a2) -> 'a1 result -> 'a2 **)
  
  let result_rec f f0 = function
  | Err -> f
  | OK x -> f0 x
  
  (** val bind : 'a1 result -> ('a1 -> 'a2 result) -> 'a2 result **)
  
  let bind f g =
    match f with
    | Err -> Err
    | OK x -> g x
  
  (** val bind2 :
      ('a1, 'a2) prod result -> ('a1 -> 'a2 -> 'a3 result) -> 'a3 result **)
  
  let bind2 f g =
    match f with
    | Err -> Err
    | OK p -> let Pair (x, y) = p in g x y
  
  (** val app_str : 'a1 list -> 'a1 stream -> 'a1 stream **)
  
  let rec app_str l s =
    match l with
    | Nil -> s
    | Cons (t0, q) -> lazy (Cons0 (t0, (app_str q s)))
  
  type noninitstate_type = A.Gram.symbol_semantic_type
  
  type stack = (A.noninitstate, noninitstate_type) sigT list
  
  (** val state_of_stack : A.initstate -> stack -> A.state **)
  
  let state_of_stack init = function
  | Nil -> A.Init init
  | Cons (s0, l) -> let ExistT (s, n0) = s0 in A.Ninit s
  
  (** val pop :
      A.Gram.symbol list -> stack -> 'a1 arrows_right -> (stack, 'a1) prod
      result **)
  
  let rec pop symbols_to_pop stack_cur action0 =
    match symbols_to_pop with
    | Nil -> OK (Pair (stack_cur, (Obj.magic action0)))
    | Cons (t0, q) ->
      (match stack_cur with
       | Nil -> Err
       | Cons (s, stack_rec) ->
         let ExistT (state_cur, sem) = s in
         (match compare_eqdec A.Gram.coq_SymbolAlph.alphabetComparable
                  (A.last_symb_of_non_init_state state_cur) t0 with
          | Left -> pop q stack_rec (Obj.magic action0 sem)
          | Right -> Err))
  
  type step_result =
  | Fail_sr
  | Accept_sr of A.Gram.symbol_semantic_type * A.GramDefs.token stream
  | Progress_sr of stack * A.GramDefs.token stream
  
  (** val step_result_rect :
      A.initstate -> 'a1 -> (A.Gram.symbol_semantic_type -> A.GramDefs.token
      stream -> 'a1) -> (stack -> A.GramDefs.token stream -> 'a1) ->
      step_result -> 'a1 **)
  
  let step_result_rect init f f0 f1 = function
  | Fail_sr -> f
  | Accept_sr (x, x0) -> f0 x x0
  | Progress_sr (x, x0) -> f1 x x0
  
  (** val step_result_rec :
      A.initstate -> 'a1 -> (A.Gram.symbol_semantic_type -> A.GramDefs.token
      stream -> 'a1) -> (stack -> A.GramDefs.token stream -> 'a1) ->
      step_result -> 'a1 **)
  
  let step_result_rec init f f0 f1 = function
  | Fail_sr -> f
  | Accept_sr (x, x0) -> f0 x x0
  | Progress_sr (x, x0) -> f1 x x0
  
  (** val prod_action' :
      A.Gram.production -> A.Gram.symbol_semantic_type arrows_right **)
  
  let prod_action' p =
    A.Gram.prod_action p
  
  (** val reduce_step :
      A.initstate -> stack -> A.Gram.production -> A.GramDefs.token stream ->
      step_result result **)
  
  let reduce_step init stack_cur production0 buffer =
    bind2
      (pop (A.Gram.prod_rhs_rev production0) stack_cur
        (prod_action' production0)) (fun stack_new sem ->
      match A.goto_table (state_of_stack init stack_new)
              (A.Gram.prod_lhs production0) with
      | Some s ->
        OK (Progress_sr ((Cons ((ExistT (s, sem)), stack_new)), buffer))
      | None ->
        (match stack_new with
         | Nil ->
           (match compare_eqdec A.Gram.coq_NonTerminalAlph.alphabetComparable
                    (A.Gram.prod_lhs production0) (A.start_nt init) with
            | Left -> OK (Accept_sr (sem, buffer))
            | Right -> Err)
         | Cons (s, l) -> Err))
  
  (** val step :
      A.initstate -> stack -> A.GramDefs.token stream -> step_result result **)
  
  let step init stack_cur buffer =
    match A.action_table (state_of_stack init stack_cur) with
    | A.Default_reduce_act production0 ->
      reduce_step init stack_cur production0 buffer
    | A.Lookahead_act awt ->
      let ExistT (term, sem) = hd buffer in
      (match awt term with
       | A.Shift_act state_new ->
         OK (Progress_sr ((Cons ((ExistT (state_new, sem)), stack_cur)),
           (tl buffer)))
       | A.Reduce_act production0 ->
         reduce_step init stack_cur production0 buffer
       | A.Fail_act -> OK Fail_sr)
  
  type parse_result =
  | Fail_pr
  | Timeout_pr
  | Parsed_pr of A.Gram.symbol_semantic_type * A.GramDefs.token stream
  
  (** val parse_result_rect :
      A.initstate -> 'a1 -> 'a1 -> (A.Gram.symbol_semantic_type ->
      A.GramDefs.token stream -> 'a1) -> parse_result -> 'a1 **)
  
  let parse_result_rect init f f0 f1 = function
  | Fail_pr -> f
  | Timeout_pr -> f0
  | Parsed_pr (x, x0) -> f1 x x0
  
  (** val parse_result_rec :
      A.initstate -> 'a1 -> 'a1 -> (A.Gram.symbol_semantic_type ->
      A.GramDefs.token stream -> 'a1) -> parse_result -> 'a1 **)
  
  let parse_result_rec init f f0 f1 = function
  | Fail_pr -> f
  | Timeout_pr -> f0
  | Parsed_pr (x, x0) -> f1 x x0
  
  (** val parse_fix :
      A.initstate -> stack -> A.GramDefs.token stream -> nat -> parse_result
      result **)
  
  let rec parse_fix init stack_cur buffer = function
  | O -> OK Timeout_pr
  | S it ->
    bind (step init stack_cur buffer) (fun r ->
      match r with
      | Fail_sr -> OK Fail_pr
      | Accept_sr (t0, buffer_new) -> OK (Parsed_pr (t0, buffer_new))
      | Progress_sr (s, buffer_new) -> parse_fix init s buffer_new it)
  
  (** val parse :
      A.initstate -> A.GramDefs.token stream -> nat -> parse_result result **)
  
  let parse init buffer n_steps =
    parse_fix init Nil buffer n_steps
 end

module Coq2_Make = 
 functor (A:Coq_T) ->
 functor (Inter:sig 
  type 'a result =
  | Err
  | OK of 'a
  
  val result_rect : 'a2 -> ('a1 -> 'a2) -> 'a1 result -> 'a2
  
  val result_rec : 'a2 -> ('a1 -> 'a2) -> 'a1 result -> 'a2
  
  val bind : 'a1 result -> ('a1 -> 'a2 result) -> 'a2 result
  
  val bind2 :
    ('a1, 'a2) prod result -> ('a1 -> 'a2 -> 'a3 result) -> 'a3 result
  
  val app_str : 'a1 list -> 'a1 stream -> 'a1 stream
  
  type noninitstate_type = A.Gram.symbol_semantic_type
  
  type stack = (A.noninitstate, noninitstate_type) sigT list
  
  val state_of_stack : A.initstate -> stack -> A.state
  
  val pop :
    A.Gram.symbol list -> stack -> 'a1 arrows_right -> (stack, 'a1) prod
    result
  
  type step_result =
  | Fail_sr
  | Accept_sr of A.Gram.symbol_semantic_type * A.GramDefs.token stream
  | Progress_sr of stack * A.GramDefs.token stream
  
  val step_result_rect :
    A.initstate -> 'a1 -> (A.Gram.symbol_semantic_type -> A.GramDefs.token
    stream -> 'a1) -> (stack -> A.GramDefs.token stream -> 'a1) ->
    step_result -> 'a1
  
  val step_result_rec :
    A.initstate -> 'a1 -> (A.Gram.symbol_semantic_type -> A.GramDefs.token
    stream -> 'a1) -> (stack -> A.GramDefs.token stream -> 'a1) ->
    step_result -> 'a1
  
  val prod_action' :
    A.Gram.production -> A.Gram.symbol_semantic_type arrows_right
  
  val reduce_step :
    A.initstate -> stack -> A.Gram.production -> A.GramDefs.token stream ->
    step_result result
  
  val step :
    A.initstate -> stack -> A.GramDefs.token stream -> step_result result
  
  type parse_result =
  | Fail_pr
  | Timeout_pr
  | Parsed_pr of A.Gram.symbol_semantic_type * A.GramDefs.token stream
  
  val parse_result_rect :
    A.initstate -> 'a1 -> 'a1 -> (A.Gram.symbol_semantic_type ->
    A.GramDefs.token stream -> 'a1) -> parse_result -> 'a1
  
  val parse_result_rec :
    A.initstate -> 'a1 -> 'a1 -> (A.Gram.symbol_semantic_type ->
    A.GramDefs.token stream -> 'a1) -> parse_result -> 'a1
  
  val parse_fix :
    A.initstate -> stack -> A.GramDefs.token stream -> nat -> parse_result
    result
  
  val parse :
    A.initstate -> A.GramDefs.token stream -> nat -> parse_result result
 end) ->
 struct 
  module Valid = Coq0_Make(A)
  
  (** val state_stack_of_stack :
      A.initstate -> Inter.stack -> (A.state -> bool) list **)
  
  let state_stack_of_stack init stack0 =
    app
      (map (fun cell -> Valid.singleton_state_pred (A.Ninit (projT1 cell)))
        stack0) (Cons ((Valid.singleton_state_pred (A.Init init)), Nil))
  
  (** val symb_stack_of_stack : Inter.stack -> A.Gram.symbol list **)
  
  let symb_stack_of_stack stack0 =
    map (fun cell -> A.last_symb_of_non_init_state (projT1 cell)) stack0
  
  (** val internal_eq_rew_dep : 'a1 -> 'a2 -> 'a1 -> 'a2 **)
  
  let internal_eq_rew_dep x f y =
    f
  
  (** val parse_with_safe :
      A.initstate -> A.GramDefs.token stream -> nat -> Inter.parse_result **)
  
  let parse_with_safe init buffer n_steps =
    let r = Inter.parse init buffer n_steps in
    (match r with
     | Inter.Err -> assert false (* absurd case *)
     | Inter.OK p -> p)
 end

module Coq3_Make = 
 functor (A:Coq_T) ->
 functor (Inter:sig 
  type 'a result =
  | Err
  | OK of 'a
  
  val result_rect : 'a2 -> ('a1 -> 'a2) -> 'a1 result -> 'a2
  
  val result_rec : 'a2 -> ('a1 -> 'a2) -> 'a1 result -> 'a2
  
  val bind : 'a1 result -> ('a1 -> 'a2 result) -> 'a2 result
  
  val bind2 :
    ('a1, 'a2) prod result -> ('a1 -> 'a2 -> 'a3 result) -> 'a3 result
  
  val app_str : 'a1 list -> 'a1 stream -> 'a1 stream
  
  type noninitstate_type = A.Gram.symbol_semantic_type
  
  type stack = (A.noninitstate, noninitstate_type) sigT list
  
  val state_of_stack : A.initstate -> stack -> A.state
  
  val pop :
    A.Gram.symbol list -> stack -> 'a1 arrows_right -> (stack, 'a1) prod
    result
  
  type step_result =
  | Fail_sr
  | Accept_sr of A.Gram.symbol_semantic_type * A.GramDefs.token stream
  | Progress_sr of stack * A.GramDefs.token stream
  
  val step_result_rect :
    A.initstate -> 'a1 -> (A.Gram.symbol_semantic_type -> A.GramDefs.token
    stream -> 'a1) -> (stack -> A.GramDefs.token stream -> 'a1) ->
    step_result -> 'a1
  
  val step_result_rec :
    A.initstate -> 'a1 -> (A.Gram.symbol_semantic_type -> A.GramDefs.token
    stream -> 'a1) -> (stack -> A.GramDefs.token stream -> 'a1) ->
    step_result -> 'a1
  
  val prod_action' :
    A.Gram.production -> A.Gram.symbol_semantic_type arrows_right
  
  val reduce_step :
    A.initstate -> stack -> A.Gram.production -> A.GramDefs.token stream ->
    step_result result
  
  val step :
    A.initstate -> stack -> A.GramDefs.token stream -> step_result result
  
  type parse_result =
  | Fail_pr
  | Timeout_pr
  | Parsed_pr of A.Gram.symbol_semantic_type * A.GramDefs.token stream
  
  val parse_result_rect :
    A.initstate -> 'a1 -> 'a1 -> (A.Gram.symbol_semantic_type ->
    A.GramDefs.token stream -> 'a1) -> parse_result -> 'a1
  
  val parse_result_rec :
    A.initstate -> 'a1 -> 'a1 -> (A.Gram.symbol_semantic_type ->
    A.GramDefs.token stream -> 'a1) -> parse_result -> 'a1
  
  val parse_fix :
    A.initstate -> stack -> A.GramDefs.token stream -> nat -> parse_result
    result
  
  val parse :
    A.initstate -> A.GramDefs.token stream -> nat -> parse_result result
 end) ->
 struct 
  (** val internal_eq_rew_dep : 'a1 -> 'a2 -> 'a1 -> 'a2 **)
  
  let internal_eq_rew_dep x f y =
    f
 end

module Coq4_Make = 
 functor (A:Coq_T) ->
 struct 
  module TerminalComparableM = 
   struct 
    type t = A.Gram.terminal
    
    (** val tComparable : t comparable **)
    
    let tComparable =
      A.Gram.coq_TerminalAlph.alphabetComparable
   end
  
  module TerminalOrderedType = OrderedType_from_ComparableM(TerminalComparableM)
  
  module StateProdPosComparableM = 
   struct 
    type t = ((A.state, A.Gram.production) prod, nat) prod
    
    (** val tComparable : t comparable **)
    
    let tComparable =
      pairComparable
        (pairComparable A.coq_StateAlph.alphabetComparable
          A.Gram.coq_ProductionAlph.alphabetComparable) natComparable
   end
  
  module StateProdPosOrderedType = OrderedType_from_ComparableM(StateProdPosComparableM)
  
  module TerminalSet = Make(TerminalOrderedType)
  
  module StateProdPosMap = Coq_Make(StateProdPosOrderedType)
  
  (** val nullable_symb : A.Gram.symbol -> bool **)
  
  let nullable_symb = function
  | A.Gram.T t0 -> False
  | A.Gram.NT nt -> A.nullable_nterm nt
  
  (** val nullable_word : A.Gram.symbol list -> bool **)
  
  let nullable_word word =
    forallb nullable_symb word
  
  (** val first_nterm_set : A.Gram.nonterminal -> TerminalSet.t **)
  
  let first_nterm_set nterm =
    fold_left (fun acc t0 -> TerminalSet.add t0 acc) (A.first_nterm nterm)
      TerminalSet.empty
  
  (** val first_symb_set : A.Gram.symbol -> TerminalSet.t **)
  
  let first_symb_set = function
  | A.Gram.T t0 -> TerminalSet.singleton t0
  | A.Gram.NT nt -> first_nterm_set nt
  
  (** val first_word_set : A.Gram.symbol list -> TerminalSet.t **)
  
  let rec first_word_set = function
  | Nil -> TerminalSet.empty
  | Cons (t0, q) ->
    (match nullable_symb t0 with
     | True -> TerminalSet.union (first_symb_set t0) (first_word_set q)
     | False -> first_symb_set t0)
  
  (** val future_of_prod : A.Gram.production -> nat -> A.Gram.symbol list **)
  
  let future_of_prod prod0 dot_pos =
    let rec loop n0 lst =
      match n0 with
      | O -> lst
      | S x ->
        (match loop x lst with
         | Nil -> Nil
         | Cons (y, q) -> q)
    in loop dot_pos (rev' (A.Gram.prod_rhs_rev prod0))
  
  (** val items_map : unit0 -> TerminalSet.t StateProdPosMap.t **)
  
  let items_map x =
    fold_left (fun acc state0 ->
      fold_left (fun acc0 item0 ->
        let key0 = Pair ((Pair (state0, (A.prod_item item0))),
          (A.dot_pos_item item0))
        in
        let data =
          fold_left (fun acc1 t0 -> TerminalSet.add t0 acc1)
            (A.lookaheads_item item0) TerminalSet.empty
        in
        let old =
          match StateProdPosMap.find key0 acc0 with
          | Some x0 -> x0
          | None -> TerminalSet.empty
        in
        StateProdPosMap.add key0 (TerminalSet.union data old) acc0)
        (A.items_of_state state0) acc)
      (all_list A.coq_StateAlph.alphabetFinite) StateProdPosMap.empty
  
  (** val find_items_map :
      TerminalSet.t StateProdPosMap.t -> A.state -> A.Gram.production -> nat
      -> TerminalSet.t **)
  
  let find_items_map items_map0 state0 prod0 dot_pos =
    match StateProdPosMap.find (Pair ((Pair (state0, prod0)), dot_pos))
            items_map0 with
    | Some x -> x
    | None -> TerminalSet.empty
  
  (** val forallb_items :
      TerminalSet.t StateProdPosMap.t -> (A.state -> A.Gram.production -> nat
      -> TerminalSet.t -> bool) -> bool **)
  
  let forallb_items items_map0 p =
    StateProdPosMap.fold (fun key0 set acc ->
      let Pair (p0, pos) = key0 in
      let Pair (st, p1) = p0 in
      (match acc with
       | True -> p st p1 pos set
       | False -> False)) items_map0 True
  
  (** val is_nullable_stable : unit0 -> bool **)
  
  let is_nullable_stable x =
    forallb (fun p ->
      implb (nullable_word (rev' (A.Gram.prod_rhs_rev p)))
        (A.nullable_nterm (A.Gram.prod_lhs p)))
      (all_list A.Gram.coq_ProductionAlph.alphabetFinite)
  
  (** val is_first_stable : unit0 -> bool **)
  
  let is_first_stable x =
    forallb (fun p ->
      TerminalSet.subset (first_word_set (rev' (A.Gram.prod_rhs_rev p)))
        (first_nterm_set (A.Gram.prod_lhs p)))
      (all_list A.Gram.coq_ProductionAlph.alphabetFinite)
  
  (** val is_start_future : TerminalSet.t StateProdPosMap.t -> bool **)
  
  let is_start_future items_map0 =
    forallb (fun init ->
      forallb (fun prod0 ->
        match compare_eqb A.Gram.coq_NonTerminalAlph.alphabetComparable
                (A.Gram.prod_lhs prod0) (A.start_nt init) with
        | True ->
          let lookaheads = find_items_map items_map0 (A.Init init) prod0 O in
          forallb (fun t0 -> TerminalSet.mem t0 lookaheads)
            (all_list A.Gram.coq_TerminalAlph.alphabetFinite)
        | False -> True) (all_list A.Gram.coq_ProductionAlph.alphabetFinite))
      (all_list A.coq_InitStateAlph.alphabetFinite)
  
  (** val is_terminal_shift : TerminalSet.t StateProdPosMap.t -> bool **)
  
  let is_terminal_shift items_map0 =
    forallb_items items_map0 (fun s1 prod0 pos lset ->
      match future_of_prod prod0 pos with
      | Nil -> True
      | Cons (s, l) ->
        (match s with
         | A.Gram.T t0 ->
           (match A.action_table s1 with
            | A.Default_reduce_act p -> False
            | A.Lookahead_act awp ->
              (match awp t0 with
               | A.Shift_act s2 ->
                 TerminalSet.subset lset
                   (find_items_map items_map0 (A.Ninit s2) prod0 (S pos))
               | _ -> False))
         | A.Gram.NT n0 -> True))
  
  (** val is_end_reduce : TerminalSet.t StateProdPosMap.t -> bool **)
  
  let is_end_reduce items_map0 =
    forallb_items items_map0 (fun s prod0 pos lset ->
      match future_of_prod prod0 pos with
      | Nil ->
        (match A.action_table s with
         | A.Default_reduce_act p ->
           compare_eqb A.Gram.coq_ProductionAlph.alphabetComparable p prod0
         | A.Lookahead_act awt ->
           TerminalSet.fold (fun lookahead acc ->
             match awt lookahead with
             | A.Reduce_act p ->
               (match acc with
                | True ->
                  compare_eqb A.Gram.coq_ProductionAlph.alphabetComparable p
                    prod0
                | False -> False)
             | _ -> False) lset True)
      | Cons (s0, l) -> True)
  
  (** val is_non_terminal_goto : TerminalSet.t StateProdPosMap.t -> bool **)
  
  let is_non_terminal_goto items_map0 =
    forallb_items items_map0 (fun s1 prod0 pos lset ->
      match future_of_prod prod0 pos with
      | Nil -> True
      | Cons (s, l) ->
        (match s with
         | A.Gram.T t0 -> True
         | A.Gram.NT nt ->
           (match A.goto_table s1 nt with
            | Some s0 ->
              TerminalSet.subset lset
                (find_items_map items_map0 (A.Ninit s0) prod0 (S pos))
            | None ->
              forallb_items items_map0 (fun s1' prod' pos' x ->
                implb (compare_eqb A.coq_StateAlph.alphabetComparable s1 s1')
                  (match future_of_prod prod' pos' with
                   | Nil -> True
                   | Cons (s0, l0) ->
                     (match s0 with
                      | A.Gram.T t0 -> True
                      | A.Gram.NT nt' ->
                        negb
                          (compare_eqb
                            A.Gram.coq_NonTerminalAlph.alphabetComparable nt
                            nt')))))))
  
  (** val is_start_goto : unit0 -> bool **)
  
  let is_start_goto x =
    forallb (fun init ->
      match A.goto_table (A.Init init) (A.start_nt init) with
      | Some s -> False
      | None -> True) (all_list A.coq_InitStateAlph.alphabetFinite)
  
  (** val is_non_terminal_closed :
      TerminalSet.t StateProdPosMap.t -> bool **)
  
  let is_non_terminal_closed items_map0 =
    forallb_items items_map0 (fun s1 prod0 pos lset ->
      match future_of_prod prod0 pos with
      | Nil -> True
      | Cons (s, q) ->
        (match s with
         | A.Gram.T t0 -> True
         | A.Gram.NT nt ->
           forallb (fun p ->
             match compare_eqb A.Gram.coq_NonTerminalAlph.alphabetComparable
                     (A.Gram.prod_lhs p) nt with
             | True ->
               let lookaheads = find_items_map items_map0 s1 p O in
               (match implb (nullable_word q)
                        (TerminalSet.subset lset lookaheads) with
                | True -> TerminalSet.subset (first_word_set q) lookaheads
                | False -> False)
             | False -> True)
             (all_list A.Gram.coq_ProductionAlph.alphabetFinite)))
  
  (** val is_complete : unit0 -> bool **)
  
  let is_complete x =
    let items_map0 = items_map Tt in
    (match match match match match match match is_nullable_stable Tt with
                                         | True -> is_first_stable Tt
                                         | False -> False with
                                   | True -> is_start_future items_map0
                                   | False -> False with
                             | True -> is_terminal_shift items_map0
                             | False -> False with
                       | True -> is_end_reduce items_map0
                       | False -> False with
                 | True -> is_start_goto Tt
                 | False -> False with
           | True -> is_non_terminal_goto items_map0
           | False -> False with
     | True -> is_non_terminal_closed items_map0
     | False -> False)
 end

module Coq5_Make = 
 functor (A:Coq_T) ->
 functor (Inter:sig 
  type 'a result =
  | Err
  | OK of 'a
  
  val result_rect : 'a2 -> ('a1 -> 'a2) -> 'a1 result -> 'a2
  
  val result_rec : 'a2 -> ('a1 -> 'a2) -> 'a1 result -> 'a2
  
  val bind : 'a1 result -> ('a1 -> 'a2 result) -> 'a2 result
  
  val bind2 :
    ('a1, 'a2) prod result -> ('a1 -> 'a2 -> 'a3 result) -> 'a3 result
  
  val app_str : 'a1 list -> 'a1 stream -> 'a1 stream
  
  type noninitstate_type = A.Gram.symbol_semantic_type
  
  type stack = (A.noninitstate, noninitstate_type) sigT list
  
  val state_of_stack : A.initstate -> stack -> A.state
  
  val pop :
    A.Gram.symbol list -> stack -> 'a1 arrows_right -> (stack, 'a1) prod
    result
  
  type step_result =
  | Fail_sr
  | Accept_sr of A.Gram.symbol_semantic_type * A.GramDefs.token stream
  | Progress_sr of stack * A.GramDefs.token stream
  
  val step_result_rect :
    A.initstate -> 'a1 -> (A.Gram.symbol_semantic_type -> A.GramDefs.token
    stream -> 'a1) -> (stack -> A.GramDefs.token stream -> 'a1) ->
    step_result -> 'a1
  
  val step_result_rec :
    A.initstate -> 'a1 -> (A.Gram.symbol_semantic_type -> A.GramDefs.token
    stream -> 'a1) -> (stack -> A.GramDefs.token stream -> 'a1) ->
    step_result -> 'a1
  
  val prod_action' :
    A.Gram.production -> A.Gram.symbol_semantic_type arrows_right
  
  val reduce_step :
    A.initstate -> stack -> A.Gram.production -> A.GramDefs.token stream ->
    step_result result
  
  val step :
    A.initstate -> stack -> A.GramDefs.token stream -> step_result result
  
  type parse_result =
  | Fail_pr
  | Timeout_pr
  | Parsed_pr of A.Gram.symbol_semantic_type * A.GramDefs.token stream
  
  val parse_result_rect :
    A.initstate -> 'a1 -> 'a1 -> (A.Gram.symbol_semantic_type ->
    A.GramDefs.token stream -> 'a1) -> parse_result -> 'a1
  
  val parse_result_rec :
    A.initstate -> 'a1 -> 'a1 -> (A.Gram.symbol_semantic_type ->
    A.GramDefs.token stream -> 'a1) -> parse_result -> 'a1
  
  val parse_fix :
    A.initstate -> stack -> A.GramDefs.token stream -> nat -> parse_result
    result
  
  val parse :
    A.initstate -> A.GramDefs.token stream -> nat -> parse_result result
 end) ->
 struct 
  module Valid = Coq4_Make(A)
  
  type pt_zipper =
  | Top_ptz
  | Cons_ptl_ptz of A.Gram.symbol * A.GramDefs.token list
     * A.Gram.symbol_semantic_type * A.Gram.symbol list
     * A.GramDefs.token list * tuple * A.GramDefs.parse_tree_list
     * ptl_zipper
  and ptl_zipper =
  | Non_terminal_pt_ptlz of A.Gram.production * A.GramDefs.token list * 
     tuple * pt_zipper
  | Cons_ptl_ptlz of A.Gram.symbol * A.GramDefs.token list
     * A.Gram.symbol_semantic_type * A.GramDefs.parse_tree
     * A.Gram.symbol list * A.GramDefs.token list * tuple * ptl_zipper
  
  (** val pt_zipper_rect :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      'a1 -> (A.Gram.symbol -> A.GramDefs.token list ->
      A.Gram.symbol_semantic_type -> A.Gram.symbol list -> A.GramDefs.token
      list -> tuple -> A.GramDefs.parse_tree_list -> ptl_zipper -> 'a1) ->
      A.Gram.symbol -> A.GramDefs.token list -> A.Gram.symbol_semantic_type
      -> pt_zipper -> 'a1 **)
  
  let pt_zipper_rect init full_word full_sem f f0 hole_symb hole_word hole_sem = function
  | Top_ptz -> f
  | Cons_ptl_ptz (x, x0, x1, x2, x3, x4, x5, x6) -> f0 x x0 x1 x2 x3 x4 x5 x6
  
  (** val pt_zipper_rec :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      'a1 -> (A.Gram.symbol -> A.GramDefs.token list ->
      A.Gram.symbol_semantic_type -> A.Gram.symbol list -> A.GramDefs.token
      list -> tuple -> A.GramDefs.parse_tree_list -> ptl_zipper -> 'a1) ->
      A.Gram.symbol -> A.GramDefs.token list -> A.Gram.symbol_semantic_type
      -> pt_zipper -> 'a1 **)
  
  let pt_zipper_rec init full_word full_sem f f0 hole_symb hole_word hole_sem = function
  | Top_ptz -> f
  | Cons_ptl_ptz (x, x0, x1, x2, x3, x4, x5, x6) -> f0 x x0 x1 x2 x3 x4 x5 x6
  
  (** val ptl_zipper_rect :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      (A.Gram.production -> A.GramDefs.token list -> tuple -> pt_zipper ->
      'a1) -> (A.Gram.symbol -> A.GramDefs.token list ->
      A.Gram.symbol_semantic_type -> A.GramDefs.parse_tree -> A.Gram.symbol
      list -> A.GramDefs.token list -> tuple -> ptl_zipper -> 'a1 -> 'a1) ->
      A.Gram.symbol list -> A.GramDefs.token list -> tuple -> ptl_zipper ->
      'a1 **)
  
  let rec ptl_zipper_rect init full_word full_sem f f0 hole_symbs hole_word hole_sems = function
  | Non_terminal_pt_ptlz (p0, word, semantic_values, p1) ->
    f p0 word semantic_values p1
  | Cons_ptl_ptlz (head_symbolt, wordt, semantic_valuet, p0, head_symbolsq,
                   wordq, semantic_valuesq, p1) ->
    f0 head_symbolt wordt semantic_valuet p0 head_symbolsq wordq
      semantic_valuesq p1
      (Obj.magic (ptl_zipper_rect init full_word full_sem f f0) (Cons
        (head_symbolt, head_symbolsq)) (app wordt wordq) (Pair
        (semantic_valuet, semantic_valuesq)) p1)
  
  (** val ptl_zipper_rec :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      (A.Gram.production -> A.GramDefs.token list -> tuple -> pt_zipper ->
      'a1) -> (A.Gram.symbol -> A.GramDefs.token list ->
      A.Gram.symbol_semantic_type -> A.GramDefs.parse_tree -> A.Gram.symbol
      list -> A.GramDefs.token list -> tuple -> ptl_zipper -> 'a1 -> 'a1) ->
      A.Gram.symbol list -> A.GramDefs.token list -> tuple -> ptl_zipper ->
      'a1 **)
  
  let rec ptl_zipper_rec init full_word full_sem f f0 hole_symbs hole_word hole_sems = function
  | Non_terminal_pt_ptlz (p0, word, semantic_values, p1) ->
    f p0 word semantic_values p1
  | Cons_ptl_ptlz (head_symbolt, wordt, semantic_valuet, p0, head_symbolsq,
                   wordq, semantic_valuesq, p1) ->
    f0 head_symbolt wordt semantic_valuet p0 head_symbolsq wordq
      semantic_valuesq p1
      (Obj.magic (ptl_zipper_rec init full_word full_sem f f0) (Cons
        (head_symbolt, head_symbolsq)) (app wordt wordq) (Pair
        (semantic_valuet, semantic_valuesq)) p1)
  
  (** val ptlz_cost :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      A.Gram.symbol list -> A.GramDefs.token list -> tuple -> ptl_zipper ->
      nat **)
  
  let ptlz_cost init full_word full_sem =
    let rec ptlz_cost0 hole_symbs hole_word hole_sems = function
    | Non_terminal_pt_ptlz (p, word, semantic_values, ptz) ->
      ptz_cost0 (A.Gram.NT (A.Gram.prod_lhs p)) word
        (uncurry (map (Obj.magic __) (rev (A.Gram.prod_rhs_rev p)))
          (A.Gram.prod_action p) semantic_values) ptz
    | Cons_ptl_ptlz (head_symbolt, wordt, semantic_valuet, pt, head_symbolsq,
                     wordq, semantic_valuesq, ptlz') ->
      Obj.magic ptlz_cost0 (Cons (head_symbolt, head_symbolsq))
        (app wordt wordq) (Pair (semantic_valuet, semantic_valuesq)) ptlz'
    and ptz_cost0 hole_symb hole_word hole_sem = function
    | Top_ptz -> O
    | Cons_ptl_ptz (head_symbolt, wordt, semantic_valuet, head_symbolsq,
                    wordq, semantic_valuesq, ptl, ptlz') ->
      plus
        (plus (S O)
          (A.GramDefs.ptl_size head_symbolsq wordq semantic_valuesq ptl))
        (Obj.magic ptlz_cost0 (Cons (head_symbolt, head_symbolsq))
          (app wordt wordq) (Pair (semantic_valuet, semantic_valuesq)) ptlz')
    in ptlz_cost0
  
  (** val ptz_cost :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      A.Gram.symbol -> A.GramDefs.token list -> A.Gram.symbol_semantic_type
      -> pt_zipper -> nat **)
  
  let ptz_cost init full_word full_sem =
    let rec ptlz_cost0 hole_symbs hole_word hole_sems = function
    | Non_terminal_pt_ptlz (p, word, semantic_values, ptz) ->
      ptz_cost0 (A.Gram.NT (A.Gram.prod_lhs p)) word
        (uncurry (map (Obj.magic __) (rev (A.Gram.prod_rhs_rev p)))
          (A.Gram.prod_action p) semantic_values) ptz
    | Cons_ptl_ptlz (head_symbolt, wordt, semantic_valuet, pt, head_symbolsq,
                     wordq, semantic_valuesq, ptlz') ->
      ptlz_cost0 (Cons (head_symbolt, head_symbolsq)) (app wordt wordq) (Pair
        (semantic_valuet, semantic_valuesq)) ptlz'
    and ptz_cost0 hole_symb hole_word hole_sem = function
    | Top_ptz -> O
    | Cons_ptl_ptz (head_symbolt, wordt, semantic_valuet, head_symbolsq,
                    wordq, semantic_valuesq, ptl, ptlz') ->
      plus
        (plus (S O)
          (A.GramDefs.ptl_size head_symbolsq wordq semantic_valuesq ptl))
        (ptlz_cost0 (Cons (head_symbolt, head_symbolsq)) (app wordt wordq)
          (Pair (semantic_valuet, semantic_valuesq)) ptlz')
    in ptz_cost0
  
  type pt_dot =
  | Reduce_ptd of ptl_zipper
  | Shift_ptd of A.Gram.terminal * A.Gram.symbol_semantic_type
     * A.Gram.symbol list * A.GramDefs.token list * tuple
     * A.GramDefs.parse_tree_list * ptl_zipper
  
  (** val pt_dot_rect :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      (ptl_zipper -> 'a1) -> (A.Gram.terminal -> A.Gram.symbol_semantic_type
      -> A.Gram.symbol list -> A.GramDefs.token list -> tuple ->
      A.GramDefs.parse_tree_list -> ptl_zipper -> 'a1) -> pt_dot -> 'a1 **)
  
  let pt_dot_rect init full_word full_sem f f0 = function
  | Reduce_ptd x -> f x
  | Shift_ptd (x, x0, x1, x2, x3, x4, x5) -> f0 x x0 x1 x2 x3 x4 x5
  
  (** val pt_dot_rec :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      (ptl_zipper -> 'a1) -> (A.Gram.terminal -> A.Gram.symbol_semantic_type
      -> A.Gram.symbol list -> A.GramDefs.token list -> tuple ->
      A.GramDefs.parse_tree_list -> ptl_zipper -> 'a1) -> pt_dot -> 'a1 **)
  
  let pt_dot_rec init full_word full_sem f f0 = function
  | Reduce_ptd x -> f x
  | Shift_ptd (x, x0, x1, x2, x3, x4, x5) -> f0 x x0 x1 x2 x3 x4 x5
  
  (** val ptd_cost :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      pt_dot -> nat **)
  
  let ptd_cost init full_word full_sem = function
  | Reduce_ptd ptlz ->
    ptlz_cost init full_word full_sem Nil Nil (Obj.magic Tt) ptlz
  | Shift_ptd (term, sem, symbolsq, wordq, semsq, ptl, ptlz) ->
    plus (plus (S O) (A.GramDefs.ptl_size symbolsq wordq semsq ptl))
      (ptlz_cost init full_word full_sem (Cons ((A.Gram.T term), symbolsq))
        (Cons ((ExistT (term, sem)), wordq)) (Obj.magic (Pair (sem, semsq)))
        ptlz)
  
  (** val ptlz_buffer :
      A.initstate -> A.GramDefs.token list -> A.GramDefs.token stream ->
      A.Gram.symbol_semantic_type -> A.Gram.symbol list -> A.GramDefs.token
      list -> tuple -> ptl_zipper -> A.GramDefs.token stream **)
  
  let ptlz_buffer init full_word buffer_end full_sem =
    let rec ptlz_buffer0 hole_symbs hole_word hole_sems = function
    | Non_terminal_pt_ptlz (p, word, semantic_values, ptz) ->
      ptz_buffer0 (A.Gram.NT (A.Gram.prod_lhs p)) word
        (uncurry (map (Obj.magic __) (rev (A.Gram.prod_rhs_rev p)))
          (A.Gram.prod_action p) semantic_values) ptz
    | Cons_ptl_ptlz (head_symbolt, wordt, semantic_valuet, p, head_symbolsq,
                     wordq, semantic_valuesq, ptlz') ->
      Obj.magic ptlz_buffer0 (Cons (head_symbolt, head_symbolsq))
        (app wordt wordq) (Pair (semantic_valuet, semantic_valuesq)) ptlz'
    and ptz_buffer0 hole_symb hole_word hole_sem = function
    | Top_ptz -> buffer_end
    | Cons_ptl_ptz (head_symbolt, wordt, semantic_valuet, head_symbolsq,
                    wordq, semantic_valuesq, ptl, ptlz') ->
      Inter.app_str wordq
        (Obj.magic ptlz_buffer0 (Cons (head_symbolt, head_symbolsq))
          (app wordt wordq) (Pair (semantic_valuet, semantic_valuesq)) ptlz')
    in ptlz_buffer0
  
  (** val ptz_buffer :
      A.initstate -> A.GramDefs.token list -> A.GramDefs.token stream ->
      A.Gram.symbol_semantic_type -> A.Gram.symbol -> A.GramDefs.token list
      -> A.Gram.symbol_semantic_type -> pt_zipper -> A.GramDefs.token stream **)
  
  let ptz_buffer init full_word buffer_end full_sem =
    let rec ptlz_buffer0 hole_symbs hole_word hole_sems = function
    | Non_terminal_pt_ptlz (p, word, semantic_values, ptz) ->
      ptz_buffer0 (A.Gram.NT (A.Gram.prod_lhs p)) word
        (uncurry (map (Obj.magic __) (rev (A.Gram.prod_rhs_rev p)))
          (A.Gram.prod_action p) semantic_values) ptz
    | Cons_ptl_ptlz (head_symbolt, wordt, semantic_valuet, p, head_symbolsq,
                     wordq, semantic_valuesq, ptlz') ->
      ptlz_buffer0 (Cons (head_symbolt, head_symbolsq)) (app wordt wordq)
        (Pair (semantic_valuet, semantic_valuesq)) ptlz'
    and ptz_buffer0 hole_symb hole_word hole_sem = function
    | Top_ptz -> buffer_end
    | Cons_ptl_ptz (head_symbolt, wordt, semantic_valuet, head_symbolsq,
                    wordq, semantic_valuesq, ptl, ptlz') ->
      Inter.app_str wordq
        (ptlz_buffer0 (Cons (head_symbolt, head_symbolsq)) (app wordt wordq)
          (Pair (semantic_valuet, semantic_valuesq)) ptlz')
    in ptz_buffer0
  
  (** val ptd_buffer :
      A.initstate -> A.GramDefs.token list -> A.GramDefs.token stream ->
      A.Gram.symbol_semantic_type -> pt_dot -> A.GramDefs.token stream **)
  
  let ptd_buffer init full_word buffer_end full_sem = function
  | Reduce_ptd ptlz ->
    ptlz_buffer init full_word buffer_end full_sem Nil Nil (Obj.magic Tt)
      ptlz
  | Shift_ptd (term, sem, symbolsq, wordq, semsq, p, ptlz) ->
    lazy (Cons0 ((ExistT (term, sem)),
      (Inter.app_str wordq
        (ptlz_buffer init full_word buffer_end full_sem (Cons ((A.Gram.T
          term), symbolsq)) (Cons ((ExistT (term, sem)), wordq))
          (Obj.magic (Pair (sem, semsq))) ptlz))))
  
  (** val ptlz_prod :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      A.Gram.symbol list -> A.GramDefs.token list -> tuple -> ptl_zipper ->
      A.Gram.production **)
  
  let rec ptlz_prod init full_word full_sem hole_symbs hole_word hole_sems = function
  | Non_terminal_pt_ptlz (prod0, word, semantic_values, p) -> prod0
  | Cons_ptl_ptlz (head_symbolt, wordt, semantic_valuet, p, head_symbolsq,
                   wordq, semantic_valuesq, ptlz') ->
    Obj.magic (ptlz_prod init full_word full_sem) (Cons (head_symbolt,
      head_symbolsq)) (app wordt wordq) (Pair (semantic_valuet,
      semantic_valuesq)) ptlz'
  
  (** val ptlz_past :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      A.Gram.symbol list -> A.GramDefs.token list -> tuple -> ptl_zipper ->
      A.Gram.symbol list **)
  
  let rec ptlz_past init full_word full_sem hole_symbs hole_word hole_sems = function
  | Non_terminal_pt_ptlz (p, word, semantic_values, p0) -> Nil
  | Cons_ptl_ptlz (s, wordt, semantic_valuet, p, head_symbolsq, wordq,
                   semantic_valuesq, ptlz') ->
    Cons (s,
      (Obj.magic (ptlz_past init full_word full_sem) (Cons (s,
        head_symbolsq)) (app wordt wordq) (Pair (semantic_valuet,
        semantic_valuesq)) ptlz'))
  
  (** val build_pt_dot :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      A.Gram.symbol list -> A.GramDefs.token list -> tuple ->
      A.GramDefs.parse_tree_list -> ptl_zipper -> pt_dot **)
  
  let rec build_pt_dot init full_word full_sem hole_symbs hole_word hole_sems ptl ptlz =
    match ptl with
    | A.GramDefs.Nil_ptl -> Reduce_ptd ptlz
    | A.GramDefs.Cons_ptl (head_symbolt, wordt, semantic_valuet, pt,
                           head_symbolsq, wordq, semantic_valuesq, ptl') ->
      (match pt with
       | A.GramDefs.Terminal_pt (term, sem) ->
         Shift_ptd (term, sem, head_symbolsq, wordq, semantic_valuesq, ptl',
           ptlz)
       | A.GramDefs.Non_terminal_pt (p, word, semantic_values, ptl'') ->
         build_pt_dot init full_word full_sem (rev (A.Gram.prod_rhs_rev p))
           word semantic_values ptl'' (Non_terminal_pt_ptlz (p, word,
           semantic_values, (Cons_ptl_ptz ((A.Gram.NT (A.Gram.prod_lhs p)),
           word,
           (uncurry (map (Obj.magic __) (rev (A.Gram.prod_rhs_rev p)))
             (A.Gram.prod_action p) semantic_values), head_symbolsq, wordq,
           semantic_valuesq, ptl', ptlz)))))
  
  (** val pop_ptlz :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      A.Gram.symbol list -> A.GramDefs.token list -> tuple ->
      A.GramDefs.parse_tree_list -> ptl_zipper -> (A.GramDefs.token list,
      (A.Gram.symbol_semantic_type, (pt_zipper, A.GramDefs.parse_tree) prod)
      sigT) sigT **)
  
  let rec pop_ptlz init full_word full_sem hole_symbs hole_word hole_sems ptl = function
  | Non_terminal_pt_ptlz (prod0, word, sem, ptz) ->
    let sem0 =
      uncurry (map (Obj.magic __) (rev (A.Gram.prod_rhs_rev prod0)))
        (A.Gram.prod_action prod0) sem
    in
    ExistT (word, (ExistT (sem0, (Pair (ptz, (A.GramDefs.Non_terminal_pt
    ((ptlz_prod init full_word full_sem (rev (A.Gram.prod_rhs_rev prod0))
       word sem (Non_terminal_pt_ptlz (prod0, word, sem, ptz))), word, sem,
    ptl)))))))
  | Cons_ptl_ptlz (head_symbolt, wordt, semantic_valuet, pt, head_symbolsq,
                   wordq, semantic_valuesq, ptlz') ->
    Obj.magic (pop_ptlz init full_word full_sem) (Cons (head_symbolt,
      head_symbolsq)) (app wordt wordq) (Pair (semantic_valuet,
      semantic_valuesq)) (A.GramDefs.Cons_ptl (head_symbolt, wordt,
      semantic_valuet, pt, head_symbolsq, wordq, semantic_valuesq, ptl))
      ptlz'
  
  (** val next_ptd :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      pt_dot -> pt_dot option **)
  
  let next_ptd init full_word full_sem = function
  | Reduce_ptd ptlz ->
    let ExistT (x, s) =
      pop_ptlz init full_word full_sem Nil Nil (Obj.magic Tt)
        A.GramDefs.Nil_ptl ptlz
    in
    let ExistT (x0, p) = s in
    let Pair (ptz, pt) = p in
    (match ptz with
     | Top_ptz -> None
     | Cons_ptl_ptz (head_symbolt, wordt, semantic_valuet, head_symbolsq,
                     wordq, semantic_valuesq, ptl, ptlz') ->
       Some
         (build_pt_dot init full_word full_sem head_symbolsq wordq
           semantic_valuesq ptl (Cons_ptl_ptlz (head_symbolt, wordt,
           semantic_valuet, pt, head_symbolsq, wordq, semantic_valuesq,
           ptlz'))))
  | Shift_ptd (term, sem, symbolsq, wordq, semsq, ptl, ptlz) ->
    Some
      (build_pt_dot init full_word full_sem symbolsq wordq semsq ptl
        (Cons_ptl_ptlz ((A.Gram.T term), (Cons ((ExistT (term, sem)), Nil)),
        sem, (A.GramDefs.Terminal_pt (term, sem)), symbolsq, wordq, semsq,
        ptlz)))
  
  (** val init_ptd :
      A.initstate -> A.GramDefs.token list -> A.Gram.symbol_semantic_type ->
      A.GramDefs.parse_tree -> pt_dot **)
  
  let init_ptd init full_word full_sem full_pt =
    let x = Top_ptz in
    (match full_pt with
     | A.GramDefs.Terminal_pt (t0, sem) -> Obj.magic Tt
     | A.GramDefs.Non_terminal_pt (p, word, semantic_values, ptl) ->
       build_pt_dot init full_word full_sem (rev (A.Gram.prod_rhs_rev p))
         word semantic_values ptl (Non_terminal_pt_ptlz (p, word,
         semantic_values, x)))
 end

module Coq6_Make = 
 functor (Aut:Coq_T) ->
 struct 
  module Inter = Coq1_Make(Aut)
  
  module Safe = Coq2_Make(Aut)(Inter)
  
  module Correct = Coq3_Make(Aut)(Inter)
  
  module Complete = Coq5_Make(Aut)(Inter)
  
  (** val complete_validator : unit0 -> bool **)
  
  let complete_validator =
    Complete.Valid.is_complete
  
  (** val safe_validator : unit0 -> bool **)
  
  let safe_validator =
    Safe.Valid.is_safe
  
  (** val parse :
      Aut.initstate -> nat -> Aut.GramDefs.token stream -> Inter.parse_result **)
  
  let parse init n_steps buffer =
    Safe.parse_with_safe init buffer n_steps
 end

type ascii =
| Ascii of bool * bool * bool * bool * bool * bool * bool * bool

type string =
| EmptyString
| String of ascii * string

type rel_addr =
| Pkt_addr of nat
| Mem_addr of nat

type instr =
| Solo_instr of string
| Imm_instr of string * nat
| Offset_instr of string * rel_addr
| Imm_br_instr of string * nat * nat * nat
| Br_instr of string * nat * nat
| Len_instr of string

module Gram = 
 struct 
  type terminal' =
  | BR_OP't
  | EOF't
  | IMM't
  | IMM_BR_OP't
  | IMM_OP't
  | LEN_OP't
  | MEM_ADDR't
  | NEWLINE't
  | OFFSET't
  | OFFSET_OP't
  | PKT_ADDR't
  | SOLO_OP't
  
  type terminal = terminal'
  
  (** val terminalNum : terminal numbered **)
  
  let terminalNum =
    { inj = (fun x ->
      match x with
      | BR_OP't -> on
      | EOF't -> in0
      | IMM't -> twice in0
      | IMM_BR_OP't -> twice_plus_one in0
      | IMM_OP't -> twice (twice in0)
      | LEN_OP't -> twice_plus_one (twice in0)
      | MEM_ADDR't -> twice (twice_plus_one in0)
      | NEWLINE't -> twice_plus_one (twice_plus_one in0)
      | OFFSET't -> twice (twice (twice in0))
      | OFFSET_OP't -> twice_plus_one (twice (twice in0))
      | PKT_ADDR't -> twice (twice_plus_one (twice in0))
      | SOLO_OP't -> twice_plus_one (twice_plus_one (twice in0))); surj =
      (fun n0 ->
      let I31 (d, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13,
               d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25,
               d26, d27, d28, d29) = n0
      in
      (match d with
       | D0 ->
         (match d0 with
          | D0 ->
            (match d1 with
             | D0 ->
               (match d2 with
                | D0 ->
                  (match d3 with
                   | D0 ->
                     (match d4 with
                      | D0 ->
                        (match d5 with
                         | D0 ->
                           (match d6 with
                            | D0 ->
                              (match d7 with
                               | D0 ->
                                 (match d8 with
                                  | D0 ->
                                    (match d9 with
                                     | D0 ->
                                       (match d10 with
                                        | D0 ->
                                          (match d11 with
                                           | D0 ->
                                             (match d12 with
                                              | D0 ->
                                                (match d13 with
                                                 | D0 ->
                                                   (match d14 with
                                                    | D0 ->
                                                      (match d15 with
                                                       | D0 ->
                                                         (match d16 with
                                                          | D0 ->
                                                            (match d17 with
                                                             | D0 ->
                                                               (match d18 with
                                                                | D0 ->
                                                                  (match d19 with
                                                                   | D0 ->
                                                                    (match d20 with
                                                                    | D0 ->
                                                                    (match d21 with
                                                                    | D0 ->
                                                                    (match d22 with
                                                                    | D0 ->
                                                                    (match d23 with
                                                                    | D0 ->
                                                                    (match d24 with
                                                                    | D0 ->
                                                                    (match d25 with
                                                                    | D0 ->
                                                                    (match d26 with
                                                                    | D0 ->
                                                                    (match d27 with
                                                                    | D0 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    BR_OP't
                                                                    | D1 ->
                                                                    EOF't)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    IMM't
                                                                    | D1 ->
                                                                    IMM_BR_OP't))
                                                                    | D1 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    IMM_OP't
                                                                    | D1 ->
                                                                    LEN_OP't)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    MEM_ADDR't
                                                                    | D1 ->
                                                                    NEWLINE't)))
                                                                    | D1 ->
                                                                    (match d27 with
                                                                    | D0 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    OFFSET't
                                                                    | D1 ->
                                                                    OFFSET_OP't)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    PKT_ADDR't
                                                                    | D1 ->
                                                                    SOLO_OP't))
                                                                    | D1 ->
                                                                    BR_OP't))
                                                                    | D1 ->
                                                                    BR_OP't)
                                                                    | D1 ->
                                                                    BR_OP't)
                                                                    | D1 ->
                                                                    BR_OP't)
                                                                    | D1 ->
                                                                    BR_OP't)
                                                                    | D1 ->
                                                                    BR_OP't)
                                                                    | D1 ->
                                                                    BR_OP't)
                                                                   | D1 ->
                                                                    BR_OP't)
                                                                | D1 ->
                                                                  BR_OP't)
                                                             | D1 -> BR_OP't)
                                                          | D1 -> BR_OP't)
                                                       | D1 -> BR_OP't)
                                                    | D1 -> BR_OP't)
                                                 | D1 -> BR_OP't)
                                              | D1 -> BR_OP't)
                                           | D1 -> BR_OP't)
                                        | D1 -> BR_OP't)
                                     | D1 -> BR_OP't)
                                  | D1 -> BR_OP't)
                               | D1 -> BR_OP't)
                            | D1 -> BR_OP't)
                         | D1 -> BR_OP't)
                      | D1 -> BR_OP't)
                   | D1 -> BR_OP't)
                | D1 -> BR_OP't)
             | D1 -> BR_OP't)
          | D1 -> BR_OP't)
       | D1 -> BR_OP't)); inj_bound = (I31 (D0, D0, D0, D0, D0, D0, D0, D0,
      D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0,
      D0, D1, D1, D0, D0)) }
  
  (** val coq_TerminalAlph : terminal alphabet **)
  
  let coq_TerminalAlph =
    numberedAlphabet terminalNum
  
  type nonterminal' =
  | Coq_pinstr'nt
  | Coq_pinstrs'nt
  
  type nonterminal = nonterminal'
  
  (** val nonterminalNum : nonterminal numbered **)
  
  let nonterminalNum =
    { inj = (fun x ->
      match x with
      | Coq_pinstr'nt -> on
      | Coq_pinstrs'nt -> in0); surj = (fun n0 ->
      let I31 (d, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13,
               d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25,
               d26, d27, d28, d29) = n0
      in
      (match d with
       | D0 ->
         (match d0 with
          | D0 ->
            (match d1 with
             | D0 ->
               (match d2 with
                | D0 ->
                  (match d3 with
                   | D0 ->
                     (match d4 with
                      | D0 ->
                        (match d5 with
                         | D0 ->
                           (match d6 with
                            | D0 ->
                              (match d7 with
                               | D0 ->
                                 (match d8 with
                                  | D0 ->
                                    (match d9 with
                                     | D0 ->
                                       (match d10 with
                                        | D0 ->
                                          (match d11 with
                                           | D0 ->
                                             (match d12 with
                                              | D0 ->
                                                (match d13 with
                                                 | D0 ->
                                                   (match d14 with
                                                    | D0 ->
                                                      (match d15 with
                                                       | D0 ->
                                                         (match d16 with
                                                          | D0 ->
                                                            (match d17 with
                                                             | D0 ->
                                                               (match d18 with
                                                                | D0 ->
                                                                  (match d19 with
                                                                   | D0 ->
                                                                    (match d20 with
                                                                    | D0 ->
                                                                    (match d21 with
                                                                    | D0 ->
                                                                    (match d22 with
                                                                    | D0 ->
                                                                    (match d23 with
                                                                    | D0 ->
                                                                    (match d24 with
                                                                    | D0 ->
                                                                    (match d25 with
                                                                    | D0 ->
                                                                    (match d26 with
                                                                    | D0 ->
                                                                    (match d27 with
                                                                    | D0 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Coq_pinstr'nt
                                                                    | D1 ->
                                                                    Coq_pinstrs'nt)
                                                                    | D1 ->
                                                                    Coq_pinstr'nt)
                                                                    | D1 ->
                                                                    Coq_pinstr'nt)
                                                                    | D1 ->
                                                                    Coq_pinstr'nt)
                                                                    | D1 ->
                                                                    Coq_pinstr'nt)
                                                                    | D1 ->
                                                                    Coq_pinstr'nt)
                                                                    | D1 ->
                                                                    Coq_pinstr'nt)
                                                                    | D1 ->
                                                                    Coq_pinstr'nt)
                                                                    | D1 ->
                                                                    Coq_pinstr'nt)
                                                                    | D1 ->
                                                                    Coq_pinstr'nt)
                                                                   | D1 ->
                                                                    Coq_pinstr'nt)
                                                                | D1 ->
                                                                  Coq_pinstr'nt)
                                                             | D1 ->
                                                               Coq_pinstr'nt)
                                                          | D1 ->
                                                            Coq_pinstr'nt)
                                                       | D1 -> Coq_pinstr'nt)
                                                    | D1 -> Coq_pinstr'nt)
                                                 | D1 -> Coq_pinstr'nt)
                                              | D1 -> Coq_pinstr'nt)
                                           | D1 -> Coq_pinstr'nt)
                                        | D1 -> Coq_pinstr'nt)
                                     | D1 -> Coq_pinstr'nt)
                                  | D1 -> Coq_pinstr'nt)
                               | D1 -> Coq_pinstr'nt)
                            | D1 -> Coq_pinstr'nt)
                         | D1 -> Coq_pinstr'nt)
                      | D1 -> Coq_pinstr'nt)
                   | D1 -> Coq_pinstr'nt)
                | D1 -> Coq_pinstr'nt)
             | D1 -> Coq_pinstr'nt)
          | D1 -> Coq_pinstr'nt)
       | D1 -> Coq_pinstr'nt)); inj_bound = (I31 (D0, D0, D0, D0, D0, D0, D0,
      D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0,
      D0, D0, D0, D0, D1, D0)) }
  
  (** val coq_NonTerminalAlph : nonterminal alphabet **)
  
  let coq_NonTerminalAlph =
    numberedAlphabet nonterminalNum
  
  type symbol =
  | T of terminal
  | NT of nonterminal
  
  (** val symbol_rect :
      (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1 **)
  
  let symbol_rect f f0 = function
  | T x -> f x
  | NT x -> f0 x
  
  (** val symbol_rec :
      (terminal -> 'a1) -> (nonterminal -> 'a1) -> symbol -> 'a1 **)
  
  let symbol_rec f f0 = function
  | T x -> f x
  | NT x -> f0 x
  
  (** val coq_SymbolAlph : symbol alphabet **)
  
  let coq_SymbolAlph =
    { alphabetComparable = (fun x y ->
      match x with
      | T x0 ->
        (match y with
         | T y0 -> compare1 coq_TerminalAlph.alphabetComparable x0 y0
         | NT n0 -> Gt)
      | NT x0 ->
        (match y with
         | T t0 -> Lt
         | NT y0 -> compare1 coq_NonTerminalAlph.alphabetComparable x0 y0));
      alphabetFinite =
      (app (map (fun x -> T x) (all_list coq_TerminalAlph.alphabetFinite))
        (map (fun x -> NT x) (all_list coq_NonTerminalAlph.alphabetFinite))) }
  
  type terminal_semantic_type = __
  
  type nonterminal_semantic_type = __
  
  type symbol_semantic_type = __
  
  type production' =
  | Prod'pinstrs'2
  | Prod'pinstrs'1
  | Prod'pinstrs'0
  | Prod'pinstr'6
  | Prod'pinstr'5
  | Prod'pinstr'4
  | Prod'pinstr'3
  | Prod'pinstr'2
  | Prod'pinstr'1
  | Prod'pinstr'0
  
  type production = production'
  
  (** val productionNum : production numbered **)
  
  let productionNum =
    { inj = (fun x ->
      match x with
      | Prod'pinstrs'2 -> on
      | Prod'pinstrs'1 -> in0
      | Prod'pinstrs'0 -> twice in0
      | Prod'pinstr'6 -> twice_plus_one in0
      | Prod'pinstr'5 -> twice (twice in0)
      | Prod'pinstr'4 -> twice_plus_one (twice in0)
      | Prod'pinstr'3 -> twice (twice_plus_one in0)
      | Prod'pinstr'2 -> twice_plus_one (twice_plus_one in0)
      | Prod'pinstr'1 -> twice (twice (twice in0))
      | Prod'pinstr'0 -> twice_plus_one (twice (twice in0))); surj =
      (fun n0 ->
      let I31 (d, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13,
               d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25,
               d26, d27, d28, d29) = n0
      in
      (match d with
       | D0 ->
         (match d0 with
          | D0 ->
            (match d1 with
             | D0 ->
               (match d2 with
                | D0 ->
                  (match d3 with
                   | D0 ->
                     (match d4 with
                      | D0 ->
                        (match d5 with
                         | D0 ->
                           (match d6 with
                            | D0 ->
                              (match d7 with
                               | D0 ->
                                 (match d8 with
                                  | D0 ->
                                    (match d9 with
                                     | D0 ->
                                       (match d10 with
                                        | D0 ->
                                          (match d11 with
                                           | D0 ->
                                             (match d12 with
                                              | D0 ->
                                                (match d13 with
                                                 | D0 ->
                                                   (match d14 with
                                                    | D0 ->
                                                      (match d15 with
                                                       | D0 ->
                                                         (match d16 with
                                                          | D0 ->
                                                            (match d17 with
                                                             | D0 ->
                                                               (match d18 with
                                                                | D0 ->
                                                                  (match d19 with
                                                                   | D0 ->
                                                                    (match d20 with
                                                                    | D0 ->
                                                                    (match d21 with
                                                                    | D0 ->
                                                                    (match d22 with
                                                                    | D0 ->
                                                                    (match d23 with
                                                                    | D0 ->
                                                                    (match d24 with
                                                                    | D0 ->
                                                                    (match d25 with
                                                                    | D0 ->
                                                                    (match d26 with
                                                                    | D0 ->
                                                                    (match d27 with
                                                                    | D0 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Prod'pinstrs'2
                                                                    | D1 ->
                                                                    Prod'pinstrs'1)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Prod'pinstrs'0
                                                                    | D1 ->
                                                                    Prod'pinstr'6))
                                                                    | D1 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Prod'pinstr'5
                                                                    | D1 ->
                                                                    Prod'pinstr'4)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Prod'pinstr'3
                                                                    | D1 ->
                                                                    Prod'pinstr'2)))
                                                                    | D1 ->
                                                                    (match d27 with
                                                                    | D0 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Prod'pinstr'1
                                                                    | D1 ->
                                                                    Prod'pinstr'0)
                                                                    | D1 ->
                                                                    Prod'pinstrs'2)
                                                                    | D1 ->
                                                                    Prod'pinstrs'2))
                                                                    | D1 ->
                                                                    Prod'pinstrs'2)
                                                                    | D1 ->
                                                                    Prod'pinstrs'2)
                                                                    | D1 ->
                                                                    Prod'pinstrs'2)
                                                                    | D1 ->
                                                                    Prod'pinstrs'2)
                                                                    | D1 ->
                                                                    Prod'pinstrs'2)
                                                                    | D1 ->
                                                                    Prod'pinstrs'2)
                                                                   | D1 ->
                                                                    Prod'pinstrs'2)
                                                                | D1 ->
                                                                  Prod'pinstrs'2)
                                                             | D1 ->
                                                               Prod'pinstrs'2)
                                                          | D1 ->
                                                            Prod'pinstrs'2)
                                                       | D1 -> Prod'pinstrs'2)
                                                    | D1 -> Prod'pinstrs'2)
                                                 | D1 -> Prod'pinstrs'2)
                                              | D1 -> Prod'pinstrs'2)
                                           | D1 -> Prod'pinstrs'2)
                                        | D1 -> Prod'pinstrs'2)
                                     | D1 -> Prod'pinstrs'2)
                                  | D1 -> Prod'pinstrs'2)
                               | D1 -> Prod'pinstrs'2)
                            | D1 -> Prod'pinstrs'2)
                         | D1 -> Prod'pinstrs'2)
                      | D1 -> Prod'pinstrs'2)
                   | D1 -> Prod'pinstrs'2)
                | D1 -> Prod'pinstrs'2)
             | D1 -> Prod'pinstrs'2)
          | D1 -> Prod'pinstrs'2)
       | D1 -> Prod'pinstrs'2)); inj_bound = (I31 (D0, D0, D0, D0, D0, D0,
      D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0,
      D0, D0, D0, D1, D0, D1, D0)) }
  
  (** val coq_ProductionAlph : production alphabet **)
  
  let coq_ProductionAlph =
    numberedAlphabet productionNum
  
  (** val prod_contents :
      production -> ((nonterminal, symbol list) prod, symbol_semantic_type
      arrows_left) sigT **)
  
  let prod_contents p =
    let box = fun x x0 -> ExistT (x, x0) in
    (match p with
     | Prod'pinstrs'2 ->
       Obj.magic box (Pair (Coq_pinstrs'nt, (Cons ((T EOF't), Nil))))
         (fun _4 -> Nil)
     | Prod'pinstrs'1 ->
       Obj.magic box (Pair (Coq_pinstrs'nt, (Cons ((T EOF't), (Cons ((NT
         Coq_pinstr'nt), Nil)))))) (fun _4 pinstr -> Cons (pinstr, Nil))
     | Prod'pinstrs'0 ->
       Obj.magic box (Pair (Coq_pinstrs'nt, (Cons ((NT Coq_pinstrs'nt), (Cons
         ((T NEWLINE't), (Cons ((NT Coq_pinstr'nt), Nil))))))))
         (fun rest _4 pinstr -> Cons (pinstr, rest))
     | Prod'pinstr'6 ->
       Obj.magic box (Pair (Coq_pinstr'nt, (Cons ((T OFFSET't), (Cons ((T
         OFFSET't), (Cons ((T BR_OP't), Nil)))))))) (fun b2 b1 opcode ->
         Br_instr (opcode, b1, b2))
     | Prod'pinstr'5 ->
       Obj.magic box (Pair (Coq_pinstr'nt, (Cons ((T OFFSET't), (Cons ((T
         OFFSET't), (Cons ((T IMM't), (Cons ((T IMM_BR_OP't), Nil))))))))))
         (fun b2 b1 imm opcode -> Imm_br_instr (opcode, imm, b1, b2))
     | Prod'pinstr'4 ->
       Obj.magic box (Pair (Coq_pinstr'nt, (Cons ((T MEM_ADDR't), (Cons ((T
         OFFSET_OP't), Nil)))))) (fun offset opcode -> Offset_instr (opcode,
         (Mem_addr offset)))
     | Prod'pinstr'3 ->
       Obj.magic box (Pair (Coq_pinstr'nt, (Cons ((T PKT_ADDR't), (Cons ((T
         OFFSET_OP't), Nil)))))) (fun offset opcode -> Offset_instr (opcode,
         (Pkt_addr offset)))
     | Prod'pinstr'2 ->
       Obj.magic box (Pair (Coq_pinstr'nt, (Cons ((T IMM't), (Cons ((T
         IMM_OP't), Nil)))))) (fun imm opcode -> Imm_instr (opcode, imm))
     | Prod'pinstr'1 ->
       Obj.magic box (Pair (Coq_pinstr'nt, (Cons ((T LEN_OP't), Nil))))
         (fun opcode -> Len_instr opcode)
     | Prod'pinstr'0 ->
       Obj.magic box (Pair (Coq_pinstr'nt, (Cons ((T SOLO_OP't), Nil))))
         (fun opcode -> Solo_instr opcode))
  
  (** val prod_lhs : production -> nonterminal **)
  
  let prod_lhs p =
    fst (projT1 (prod_contents p))
  
  (** val prod_rhs_rev : production -> symbol list **)
  
  let prod_rhs_rev p =
    snd (projT1 (prod_contents p))
  
  (** val prod_action : production -> symbol_semantic_type arrows_left **)
  
  let prod_action p =
    projT2 (prod_contents p)
  
  type token = (terminal, symbol_semantic_type) sigT
  
  type parse_tree =
  | Terminal_pt of terminal * symbol_semantic_type
  | Non_terminal_pt of production * token list * tuple * parse_tree_list
  and parse_tree_list =
  | Nil_ptl
  | Cons_ptl of symbol * token list * symbol_semantic_type * parse_tree
     * symbol list * token list * tuple * parse_tree_list
  
  (** val parse_tree_rect :
      (terminal -> symbol_semantic_type -> 'a1) -> (production -> token list
      -> tuple -> parse_tree_list -> 'a1) -> symbol -> token list ->
      symbol_semantic_type -> parse_tree -> 'a1 **)
  
  let parse_tree_rect f f0 head_symbol word semantic_value = function
  | Terminal_pt (x, x0) -> f x x0
  | Non_terminal_pt (x, x0, x1, x2) -> f0 x x0 x1 x2
  
  (** val parse_tree_rec :
      (terminal -> symbol_semantic_type -> 'a1) -> (production -> token list
      -> tuple -> parse_tree_list -> 'a1) -> symbol -> token list ->
      symbol_semantic_type -> parse_tree -> 'a1 **)
  
  let parse_tree_rec f f0 head_symbol word semantic_value = function
  | Terminal_pt (x, x0) -> f x x0
  | Non_terminal_pt (x, x0, x1, x2) -> f0 x x0 x1 x2
  
  (** val parse_tree_list_rect :
      'a1 -> (symbol -> token list -> symbol_semantic_type -> parse_tree ->
      symbol list -> token list -> tuple -> parse_tree_list -> 'a1 -> 'a1) ->
      symbol list -> token list -> tuple -> parse_tree_list -> 'a1 **)
  
  let rec parse_tree_list_rect f f0 head_symbols word semantic_values = function
  | Nil_ptl -> f
  | Cons_ptl (head_symbolt, wordt, semantic_valuet, p0, head_symbolsq, wordq,
              semantic_valuesq, p1) ->
    f0 head_symbolt wordt semantic_valuet p0 head_symbolsq wordq
      semantic_valuesq p1
      (parse_tree_list_rect f f0 head_symbolsq wordq semantic_valuesq p1)
  
  (** val parse_tree_list_rec :
      'a1 -> (symbol -> token list -> symbol_semantic_type -> parse_tree ->
      symbol list -> token list -> tuple -> parse_tree_list -> 'a1 -> 'a1) ->
      symbol list -> token list -> tuple -> parse_tree_list -> 'a1 **)
  
  let rec parse_tree_list_rec f f0 head_symbols word semantic_values = function
  | Nil_ptl -> f
  | Cons_ptl (head_symbolt, wordt, semantic_valuet, p0, head_symbolsq, wordq,
              semantic_valuesq, p1) ->
    f0 head_symbolt wordt semantic_valuet p0 head_symbolsq wordq
      semantic_valuesq p1
      (parse_tree_list_rec f f0 head_symbolsq wordq semantic_valuesq p1)
  
  (** val pt_size :
      symbol -> token list -> symbol_semantic_type -> parse_tree -> nat **)
  
  let rec pt_size head_symbol word sem = function
  | Terminal_pt (t0, sem0) -> S O
  | Non_terminal_pt (p, word0, semantic_values, l) ->
    S (ptl_size (rev (prod_rhs_rev p)) word0 semantic_values l)
  
  (** val ptl_size :
      symbol list -> token list -> tuple -> parse_tree_list -> nat **)
  
  and ptl_size head_symbols word sems = function
  | Nil_ptl -> O
  | Cons_ptl (head_symbolt, wordt, semantic_valuet, t0, head_symbolsq, wordq,
              semantic_valuesq, q) ->
    plus (pt_size head_symbolt wordt semantic_valuet t0)
      (ptl_size head_symbolsq wordq semantic_valuesq q)
 end
module Coq__1 = Gram

module Aut = 
 struct 
  module Gram = Gram
  
  module GramDefs = Gram
  
  (** val nullable_nterm : Coq__1.nonterminal -> bool **)
  
  let nullable_nterm nt =
    False
  
  (** val first_nterm : Coq__1.nonterminal -> Coq__1.terminal list **)
  
  let first_nterm = function
  | Coq__1.Coq_pinstr'nt ->
    Cons (Coq__1.SOLO_OP't, (Cons (Coq__1.OFFSET_OP't, (Cons
      (Coq__1.LEN_OP't, (Cons (Coq__1.IMM_OP't, (Cons (Coq__1.IMM_BR_OP't,
      (Cons (Coq__1.BR_OP't, Nil)))))))))))
  | Coq__1.Coq_pinstrs'nt ->
    Cons (Coq__1.SOLO_OP't, (Cons (Coq__1.OFFSET_OP't, (Cons
      (Coq__1.LEN_OP't, (Cons (Coq__1.IMM_OP't, (Cons (Coq__1.IMM_BR_OP't,
      (Cons (Coq__1.EOF't, (Cons (Coq__1.BR_OP't, Nil)))))))))))))
  
  type noninitstate' =
  | Nis'20
  | Nis'19
  | Nis'18
  | Nis'17
  | Nis'15
  | Nis'14
  | Nis'13
  | Nis'12
  | Nis'11
  | Nis'10
  | Nis'9
  | Nis'8
  | Nis'7
  | Nis'6
  | Nis'5
  | Nis'4
  | Nis'3
  | Nis'2
  | Nis'1
  
  type noninitstate = noninitstate'
  
  (** val noninitstateNum : noninitstate numbered **)
  
  let noninitstateNum =
    { inj = (fun x ->
      match x with
      | Nis'20 -> on
      | Nis'19 -> in0
      | Nis'18 -> twice in0
      | Nis'17 -> twice_plus_one in0
      | Nis'15 -> twice (twice in0)
      | Nis'14 -> twice_plus_one (twice in0)
      | Nis'13 -> twice (twice_plus_one in0)
      | Nis'12 -> twice_plus_one (twice_plus_one in0)
      | Nis'11 -> twice (twice (twice in0))
      | Nis'10 -> twice_plus_one (twice (twice in0))
      | Nis'9 -> twice (twice_plus_one (twice in0))
      | Nis'8 -> twice_plus_one (twice_plus_one (twice in0))
      | Nis'7 -> twice (twice (twice_plus_one in0))
      | Nis'6 -> twice_plus_one (twice (twice_plus_one in0))
      | Nis'5 -> twice (twice_plus_one (twice_plus_one in0))
      | Nis'4 -> twice_plus_one (twice_plus_one (twice_plus_one in0))
      | Nis'3 -> twice (twice (twice (twice in0)))
      | Nis'2 -> twice_plus_one (twice (twice (twice in0)))
      | Nis'1 -> twice (twice_plus_one (twice (twice in0)))); surj =
      (fun n0 ->
      let I31 (d, d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13,
               d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25,
               d26, d27, d28, d29) = n0
      in
      (match d with
       | D0 ->
         (match d0 with
          | D0 ->
            (match d1 with
             | D0 ->
               (match d2 with
                | D0 ->
                  (match d3 with
                   | D0 ->
                     (match d4 with
                      | D0 ->
                        (match d5 with
                         | D0 ->
                           (match d6 with
                            | D0 ->
                              (match d7 with
                               | D0 ->
                                 (match d8 with
                                  | D0 ->
                                    (match d9 with
                                     | D0 ->
                                       (match d10 with
                                        | D0 ->
                                          (match d11 with
                                           | D0 ->
                                             (match d12 with
                                              | D0 ->
                                                (match d13 with
                                                 | D0 ->
                                                   (match d14 with
                                                    | D0 ->
                                                      (match d15 with
                                                       | D0 ->
                                                         (match d16 with
                                                          | D0 ->
                                                            (match d17 with
                                                             | D0 ->
                                                               (match d18 with
                                                                | D0 ->
                                                                  (match d19 with
                                                                   | D0 ->
                                                                    (match d20 with
                                                                    | D0 ->
                                                                    (match d21 with
                                                                    | D0 ->
                                                                    (match d22 with
                                                                    | D0 ->
                                                                    (match d23 with
                                                                    | D0 ->
                                                                    (match d24 with
                                                                    | D0 ->
                                                                    (match d25 with
                                                                    | D0 ->
                                                                    (match d26 with
                                                                    | D0 ->
                                                                    (match d27 with
                                                                    | D0 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'20
                                                                    | D1 ->
                                                                    Nis'19)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'18
                                                                    | D1 ->
                                                                    Nis'17))
                                                                    | D1 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'15
                                                                    | D1 ->
                                                                    Nis'14)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'13
                                                                    | D1 ->
                                                                    Nis'12)))
                                                                    | D1 ->
                                                                    (match d27 with
                                                                    | D0 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'11
                                                                    | D1 ->
                                                                    Nis'10)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'9
                                                                    | D1 ->
                                                                    Nis'8))
                                                                    | D1 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'7
                                                                    | D1 ->
                                                                    Nis'6)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'5
                                                                    | D1 ->
                                                                    Nis'4))))
                                                                    | D1 ->
                                                                    (match d26 with
                                                                    | D0 ->
                                                                    (match d27 with
                                                                    | D0 ->
                                                                    (match d28 with
                                                                    | D0 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'3
                                                                    | D1 ->
                                                                    Nis'2)
                                                                    | D1 ->
                                                                    (match d29 with
                                                                    | D0 ->
                                                                    Nis'1
                                                                    | D1 ->
                                                                    Nis'20))
                                                                    | D1 ->
                                                                    Nis'20)
                                                                    | D1 ->
                                                                    Nis'20))
                                                                    | D1 ->
                                                                    Nis'20)
                                                                    | D1 ->
                                                                    Nis'20)
                                                                    | D1 ->
                                                                    Nis'20)
                                                                    | D1 ->
                                                                    Nis'20)
                                                                    | D1 ->
                                                                    Nis'20)
                                                                   | D1 ->
                                                                    Nis'20)
                                                                | D1 ->
                                                                  Nis'20)
                                                             | D1 -> Nis'20)
                                                          | D1 -> Nis'20)
                                                       | D1 -> Nis'20)
                                                    | D1 -> Nis'20)
                                                 | D1 -> Nis'20)
                                              | D1 -> Nis'20)
                                           | D1 -> Nis'20)
                                        | D1 -> Nis'20)
                                     | D1 -> Nis'20)
                                  | D1 -> Nis'20)
                               | D1 -> Nis'20)
                            | D1 -> Nis'20)
                         | D1 -> Nis'20)
                      | D1 -> Nis'20)
                   | D1 -> Nis'20)
                | D1 -> Nis'20)
             | D1 -> Nis'20)
          | D1 -> Nis'20)
       | D1 -> Nis'20)); inj_bound = (I31 (D0, D0, D0, D0, D0, D0, D0, D0,
      D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0,
      D1, D0, D0, D1, D1)) }
  
  (** val coq_NonInitStateAlph : noninitstate alphabet **)
  
  let coq_NonInitStateAlph =
    numberedAlphabet noninitstateNum
  
  (** val last_symb_of_non_init_state : noninitstate -> Coq__1.symbol **)
  
  let last_symb_of_non_init_state = function
  | Nis'20 -> Coq__1.T Coq__1.EOF't
  | Nis'19 -> Coq__1.NT Coq__1.Coq_pinstrs'nt
  | Nis'18 -> Coq__1.T Coq__1.NEWLINE't
  | Nis'17 -> Coq__1.NT Coq__1.Coq_pinstr'nt
  | Nis'13 -> Coq__1.T Coq__1.BR_OP't
  | Nis'12 -> Coq__1.T Coq__1.EOF't
  | Nis'9 -> Coq__1.T Coq__1.IMM't
  | Nis'8 -> Coq__1.T Coq__1.IMM_BR_OP't
  | Nis'7 -> Coq__1.T Coq__1.IMM't
  | Nis'6 -> Coq__1.T Coq__1.IMM_OP't
  | Nis'5 -> Coq__1.T Coq__1.LEN_OP't
  | Nis'4 -> Coq__1.T Coq__1.MEM_ADDR't
  | Nis'3 -> Coq__1.T Coq__1.PKT_ADDR't
  | Nis'2 -> Coq__1.T Coq__1.OFFSET_OP't
  | Nis'1 -> Coq__1.T Coq__1.SOLO_OP't
  | _ -> Coq__1.T Coq__1.OFFSET't
  
  type initstate' =
  | Init'0
  
  type initstate = initstate'
  
  (** val initstateNum : initstate numbered **)
  
  let initstateNum =
    { inj = (fun x -> on); surj = (fun n0 -> Init'0); inj_bound = (I31 (D0,
      D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0,
      D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D0, D1)) }
  
  (** val coq_InitStateAlph : initstate alphabet **)
  
  let coq_InitStateAlph =
    numberedAlphabet initstateNum
  
  type state =
  | Init of initstate
  | Ninit of noninitstate
  
  (** val state_rect :
      (initstate -> 'a1) -> (noninitstate -> 'a1) -> state -> 'a1 **)
  
  let state_rect f f0 = function
  | Init x -> f x
  | Ninit x -> f0 x
  
  (** val state_rec :
      (initstate -> 'a1) -> (noninitstate -> 'a1) -> state -> 'a1 **)
  
  let state_rec f f0 = function
  | Init x -> f x
  | Ninit x -> f0 x
  
  (** val coq_StateAlph : state alphabet **)
  
  let coq_StateAlph =
    { alphabetComparable = (fun x y ->
      match x with
      | Init x0 ->
        (match y with
         | Init y0 -> compare1 coq_InitStateAlph.alphabetComparable x0 y0
         | Ninit n0 -> Lt)
      | Ninit x0 ->
        (match y with
         | Init i -> Gt
         | Ninit y0 -> compare1 coq_NonInitStateAlph.alphabetComparable x0 y0));
      alphabetFinite =
      (app
        (map (fun x -> Init x) (all_list coq_InitStateAlph.alphabetFinite))
        (map (fun x -> Ninit x)
          (all_list coq_NonInitStateAlph.alphabetFinite))) }
  
  type lookahead_action =
  | Shift_act of noninitstate
  | Reduce_act of Gram.production
  | Fail_act
  
  (** val lookahead_action_rect :
      Gram.terminal -> (noninitstate -> __ -> 'a1) -> (Gram.production ->
      'a1) -> 'a1 -> lookahead_action -> 'a1 **)
  
  let lookahead_action_rect term f f0 f1 = function
  | Shift_act x -> f x __
  | Reduce_act x -> f0 x
  | Fail_act -> f1
  
  (** val lookahead_action_rec :
      Gram.terminal -> (noninitstate -> __ -> 'a1) -> (Gram.production ->
      'a1) -> 'a1 -> lookahead_action -> 'a1 **)
  
  let lookahead_action_rec term f f0 f1 = function
  | Shift_act x -> f x __
  | Reduce_act x -> f0 x
  | Fail_act -> f1
  
  type action =
  | Default_reduce_act of Gram.production
  | Lookahead_act of (Gram.terminal -> lookahead_action)
  
  (** val action_rect :
      (Gram.production -> 'a1) -> ((Gram.terminal -> lookahead_action) ->
      'a1) -> action -> 'a1 **)
  
  let action_rect f f0 = function
  | Default_reduce_act x -> f x
  | Lookahead_act x -> f0 x
  
  (** val action_rec :
      (Gram.production -> 'a1) -> ((Gram.terminal -> lookahead_action) ->
      'a1) -> action -> 'a1 **)
  
  let action_rec f f0 = function
  | Default_reduce_act x -> f x
  | Lookahead_act x -> f0 x
  
  type item = { prod_item : Gram.production; dot_pos_item : nat;
                lookaheads_item : Gram.terminal list }
  
  (** val item_rect :
      (Gram.production -> nat -> Gram.terminal list -> 'a1) -> item -> 'a1 **)
  
  let item_rect f i =
    let { prod_item = x; dot_pos_item = x0; lookaheads_item = x1 } = i in
    f x x0 x1
  
  (** val item_rec :
      (Gram.production -> nat -> Gram.terminal list -> 'a1) -> item -> 'a1 **)
  
  let item_rec f i =
    let { prod_item = x; dot_pos_item = x0; lookaheads_item = x1 } = i in
    f x x0 x1
  
  (** val prod_item : item -> Gram.production **)
  
  let prod_item x = x.prod_item
  
  (** val dot_pos_item : item -> nat **)
  
  let dot_pos_item x = x.dot_pos_item
  
  (** val lookaheads_item : item -> Gram.terminal list **)
  
  let lookaheads_item x = x.lookaheads_item
  
  (** val start_nt : initstate -> Coq__1.nonterminal **)
  
  let start_nt init =
    Coq__1.Coq_pinstrs'nt
  
  (** val action_table : state -> action **)
  
  let action_table = function
  | Init i ->
    Lookahead_act (fun terminal0 ->
      match Obj.magic terminal0 with
      | Coq__1.BR_OP't -> Shift_act Nis'13
      | Coq__1.EOF't -> Shift_act Nis'12
      | Coq__1.IMM_BR_OP't -> Shift_act Nis'8
      | Coq__1.IMM_OP't -> Shift_act Nis'6
      | Coq__1.LEN_OP't -> Shift_act Nis'5
      | Coq__1.OFFSET_OP't -> Shift_act Nis'2
      | Coq__1.SOLO_OP't -> Shift_act Nis'1
      | _ -> Fail_act)
  | Ninit n0 ->
    (match n0 with
     | Nis'20 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstrs'1)
     | Nis'19 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstrs'0)
     | Nis'18 ->
       Lookahead_act (fun terminal0 ->
         match Obj.magic terminal0 with
         | Coq__1.BR_OP't -> Shift_act Nis'13
         | Coq__1.EOF't -> Shift_act Nis'12
         | Coq__1.IMM_BR_OP't -> Shift_act Nis'8
         | Coq__1.IMM_OP't -> Shift_act Nis'6
         | Coq__1.LEN_OP't -> Shift_act Nis'5
         | Coq__1.OFFSET_OP't -> Shift_act Nis'2
         | Coq__1.SOLO_OP't -> Shift_act Nis'1
         | _ -> Fail_act)
     | Nis'17 ->
       Lookahead_act (fun terminal0 ->
         match Obj.magic terminal0 with
         | Coq__1.EOF't -> Shift_act Nis'20
         | Coq__1.NEWLINE't -> Shift_act Nis'18
         | _ -> Fail_act)
     | Nis'15 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstr'6)
     | Nis'14 ->
       Lookahead_act (fun terminal0 ->
         match Obj.magic terminal0 with
         | Coq__1.OFFSET't -> Shift_act Nis'15
         | _ -> Fail_act)
     | Nis'13 ->
       Lookahead_act (fun terminal0 ->
         match Obj.magic terminal0 with
         | Coq__1.OFFSET't -> Shift_act Nis'14
         | _ -> Fail_act)
     | Nis'12 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstrs'2)
     | Nis'11 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstr'5)
     | Nis'10 ->
       Lookahead_act (fun terminal0 ->
         match Obj.magic terminal0 with
         | Coq__1.OFFSET't -> Shift_act Nis'11
         | _ -> Fail_act)
     | Nis'9 ->
       Lookahead_act (fun terminal0 ->
         match Obj.magic terminal0 with
         | Coq__1.OFFSET't -> Shift_act Nis'10
         | _ -> Fail_act)
     | Nis'8 ->
       Lookahead_act (fun terminal0 ->
         match Obj.magic terminal0 with
         | Coq__1.IMM't -> Shift_act Nis'9
         | _ -> Fail_act)
     | Nis'7 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstr'2)
     | Nis'6 ->
       Lookahead_act (fun terminal0 ->
         match Obj.magic terminal0 with
         | Coq__1.IMM't -> Shift_act Nis'7
         | _ -> Fail_act)
     | Nis'5 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstr'1)
     | Nis'4 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstr'4)
     | Nis'3 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstr'3)
     | Nis'2 ->
       Lookahead_act (fun terminal0 ->
         match Obj.magic terminal0 with
         | Coq__1.MEM_ADDR't -> Shift_act Nis'4
         | Coq__1.PKT_ADDR't -> Shift_act Nis'3
         | _ -> Fail_act)
     | Nis'1 -> Default_reduce_act (Obj.magic Coq__1.Prod'pinstr'0))
  
  (** val goto_table : state -> Coq__1.nonterminal -> noninitstate option **)
  
  let goto_table state0 nt =
    match state0 with
    | Init i ->
      (match nt with
       | Coq__1.Coq_pinstr'nt -> Some Nis'17
       | Coq__1.Coq_pinstrs'nt -> None)
    | Ninit n0 ->
      (match n0 with
       | Nis'18 ->
         (match nt with
          | Coq__1.Coq_pinstr'nt -> Some Nis'17
          | Coq__1.Coq_pinstrs'nt -> Some Nis'19)
       | _ -> None)
  
  (** val past_symb_of_non_init_state :
      noninitstate -> Coq__1.symbol list **)
  
  let past_symb_of_non_init_state = fun _ -> assert false
  
  (** val past_state_of_non_init_state :
      noninitstate -> (state -> bool) list **)
  
  let past_state_of_non_init_state = fun _ -> assert false
  
  (** val items_of_state : state -> item list **)
  
  let items_of_state = fun _ -> assert false
 end

module Parser = Coq6_Make(Aut)

(** val pinstrs :
    nat -> Aut.GramDefs.token stream -> Parser.Inter.parse_result **)

let pinstrs =
  Parser.parse Aut.Init'0

