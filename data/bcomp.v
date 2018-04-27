Require Import Coq.Bool.Bool.
Require Import Coq.Arith.Arith.
Require Import Coq.Vectors.VectorDef.
Require Coq.Vectors.Fin.

Definition fin := Fin.t.

Definition mem := t nat 2048.

Definition Addr := fin 2048.

Record state : Set := mkState {
                          addr_reg : nat;
                          data_reg : nat;
                          cmd_reg : nat;
                          acc : nat;
                          carry : bool;
                          cmd_cnt : nat;
                          memory : mem
                        }.

Fixpoint vec_zeroes {n} : t nat n :=
  match n with
  | 0 => nil nat
  | S n' => cons nat 0 n' vec_zeroes
  end.

Definition empty_state :=
  mkState 0 0 0 0 false 0 vec_zeroes.

Definition set_acc st a' :=
  match st with
  | mkState ar dr cr a c cc mem =>
    mkState ar dr cr a' c cc mem
  end.

Definition set_mem st addr val :=
  match st with
  | mkState ar dr cr a c cc mem =>
    mkState ar dr cr a c cc (replace mem addr val)
  end.

Inductive com : Type :=
| C_ISZ : Addr -> com
| C_AND : Addr -> com
| C_JSR : Addr -> com
| C_MOV : Addr -> com
| C_ADD : Addr -> com
| C_ADC : Addr -> com
| C_SUB : Addr -> com
| C_BCS : Addr -> com
| C_BPL : Addr -> com
| C_BMI : Addr -> com
| C_BEQ : Addr -> com
| C_BR : Addr -> com
                   
| C_HLT : com
| C_NOP : com
| C_CLA : com
| C_CLC : com
| C_CMA : com
| C_CMC : com
| C_ROL : com
| C_ROR : com
| C_INC : com
| C_DEC : com
| C_EI : com
| C_DI : com.
                        
Notation "'AND' x" :=
  (C_AND x) (at level 60).
Notation "'MOV' x" :=
  (C_MOV x) (at level 60).

Notation "'HLT'" := C_HLT (at level 60).
Notation "'NOP'" := C_NOP (at level 60).
Notation "'CLA'" := C_CLA (at level 60).
Notation "'CLC'" := C_CLC (at level 60).
Notation "'CMA'" := C_CMA (at level 60).
Notation "'ROL'" := C_ROL (at level 60).
Notation "'ROR'" := C_ROR (at level 60).
Notation "'INC'" := C_INC (at level 60).
Notation "'DEC'" := C_DEC (at level 60).
Notation "'EI'" := C_EI (at level 60).
Notation "'DI'" := C_DI (at level 60).

Definition bitwise_and x y : nat :=
  match (x,y) with
  | (0,0) => 0
  | _ => 1
  end.

Reserved Notation "c1 '/' st '\\' st'"
                  (at level 40, st at level 39).

Inductive ceval :
  com -> state -> state -> Prop :=
| E_AND : forall st a,
    (AND a) / st \\ set_acc st (bitwise_and (acc st) (proj1_sig (Fin.to_nat a)))
| E_MOV : forall st a,
    (MOV a) / st \\ set_mem st a (acc st)
where "c1 '/' st '\\' st'" := (ceval c1 st st').
