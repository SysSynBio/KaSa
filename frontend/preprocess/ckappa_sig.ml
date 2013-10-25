 (**
  * ckappa_sig.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 01/17/2011
  * Last modification: 02/01/2011
  * * 
  * Signature for prepreprocessing language ckappa 
  *  
  * Copyright 2010,2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "ckappa_sig") message exn (fun () -> default) 
  
module Int_Set_and_Map = Set_and_map.Make (struct type t = int let compare = compare end) 

let local_trace = true 
  
type agent_name = string
type site_name = string 
type internal_state = string 
  
type binding_state = 
    | Free 
    | Lnk_type of agent_name * site_name 

type mixture = 
	| SKIP of mixture 
        | COMMA of agent * mixture 
	| DOT of int * agent * mixture 
	| PLUS of int * agent * mixture 
	| EMPTY_MIX

and agent = {ag_nme:string ; ag_intf:interface ; ag_pos:Tools.pos}
and interface = PORT_SEP of port * interface | EMPTY_INTF
and port = {port_nme:string ; port_int: internal ; port_lnk : link ; port_pos : Tools.pos ; port_free : bool option}
and internal = string list
and link = 
    | LNK_VALUE of (int * agent_name * site_name * int * Tools.pos)
	| FREE 
	| LNK_ANY of Tools.pos 
	| LNK_SOME of Tools.pos
	| LNK_TYPE of ((string * Tools.pos) * (string * Tools.pos))
type rule = 
  {
    lhs: mixture ; 
    arrow:Ast.arrow ; 
    rhs:mixture; 
    k_def:Ast.alg_expr ; 
    k_un:Ast.alg_expr option
  }
type perturbation = Ast.bool_expr * modif_expr list * Tools.pos * Ast.bool_expr option
and modif_expr = 
	| INTRO of (Ast.alg_expr * mixture * Tools.pos) 
	| DELETE of (Ast.alg_expr * mixture * Tools.pos) 
 	| UPDATE of (Ast.str_pos * Ast.alg_expr) (*TODO: pause*)
	| UPDATE_TOK of (Ast.str_pos * Ast.alg_expr) (*TODO: pause*)
	| STOP of (Ast.print_expr list * Tools.pos)
	| SNAPSHOT of (Ast.print_expr list * Tools.pos) (*maybe later of mixture too*)
	| PRINT of ((Ast.print_expr list) * (Ast.print_expr list) * Tools.pos)
	| CFLOW of (Ast.str_pos * Tools.pos) 
	| CFLOWOFF of (Ast.str_pos * Tools.pos)
	| FLUX of Ast.print_expr list * Tools.pos
	| FLUXOFF of Ast.print_expr list * Tools.pos

   
type variable =
	| VAR_KAPPA of (mixture * (string * Tools.pos))
	| VAR_ALG of (Ast.alg_expr * (string * Tools.pos))
   
type compil = 
  {
    variables : variable list; (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
	signatures : (agent * Tools.pos) list ; (*agent signature declaration*)
    rules : (Ast.rule_label * rule) list ; (*rules (possibly named)*)
    observables : Ast.alg_expr list ; (*list of patterns to plot*) 
    init : (Ast.str_pos option * Ast.alg_expr * mixture * Tools.pos) list ; (*initial graph declaration*)
	perturbations : perturbation list
  }
  
  
type ('a,'b) site_type = 
  | Internal of 'a 
  | Binding of 'b 

type site = (site_name,site_name) site_type     
type state = (internal_state,binding_state) site_type  


module State = 
struct
  type t = state 
  let compare = compare
end 

module Dictionary_of_States = Dictionary.Dictionary_of_Ord(State)
 
type internal_state_specification =
  {
   string:internal_state;
   where:Tools.pos list
  }
  
module Site = 
struct 
  type t = site 
  let compare = compare 
end
  
module Kasim_agent_name = 
struct 
  type t = agent_name 
  let compare = compare 
end 
  
module Dictionary_of_agents = Dictionary.Dictionary_of_Ord(Kasim_agent_name)
module Dictionary_of_sites  = Dictionary.Dictionary_of_Ord(Site) 
  
type site_list = 
  {
   used: (site_name list * Tools.pos) list ;
   declared: (site_name list * Tools.pos) list ;
   creation: (site_name list * Tools.pos) list 
  }
   
type site_dic = 
  (unit,unit) Dictionary_of_sites.dictionary
  
type state_dic = (unit,unit) Dictionary_of_States.dictionary
  
type agent_specification = 
  { 
    binding_sites_usage:site_list; 
    marked_sites_usage:site_list;
  }
    
type agent_dic = 
  (unit,unit) Dictionary_of_agents.dictionary

type kappa_handler = 
  {agents_dic : agent_dic; 
   interface_constraints: agent_specification Int_storage.Nearly_inf_Imperatif.t;
   sites: site_dic Int_storage.Nearly_inf_Imperatif.t;
   states_dic: state_dic Int_storage.Nearly_inf_Imperatif.t Int_storage.Nearly_inf_Imperatif.t;
   }

  
type c_agent_name = int
type c_site_name = int 
type c_state = int 
type 'a interval = {min:'a;max:'a}
  
type c_port = 
  { 
    c_site_name : c_site_name ; 
    c_site_position : Tools.pos ;
    c_site_interval : c_state interval ;    
  }
module C_site_map_and_set = Set_and_map.Make(struct type t = c_site_name let compare = compare end)
type c_interface = c_port C_site_map_and_set.map
                                                                           
type c_proper_agent = 
  { 
    c_agent_kasim_id : int ; 
    c_agent_name : c_agent_name ;
    c_agent_interface : c_interface ;
    c_agent_position : Tools.pos ;
  }

type site_address = {agent_index : int ; site : c_site_name }
type c_bond = site_address * site_address 
  

type c_agent = 
   | C_ghost
   | C_agent of c_proper_agent                               

type c_mixture = 
  { 
    c_views:  c_agent Int_storage.Quick_Nearly_inf_Imperatif.t ;
    c_bonds: site_address C_site_map_and_set.map Int_storage.Nearly_inf_Imperatif.t ; 
    c_plus : (int * int) list;
    c_dot  : (int * int) list
    }
      
type c_variable = C_VAR_KAPPA of c_mixture * (string * Tools.pos) | C_VAR_ALG of Ast.alg_expr * (string * Tools.pos)  

type action = Release of c_bond | Bind of c_bond | Half_breaf of site_address   

type c_rule = 
  {
    c_rule_lhs: c_mixture ; 
    c_rule_arrow:Ast.arrow ; 
    c_rule_rhs: c_mixture; 
    c_diff_direct: c_mixture;
    c_diff_reverse: c_mixture;
    c_side_effects: action list;
   }

type c_perturbation = Ast.bool_expr * c_modif_expr * Tools.pos * Ast.bool_expr option
and c_modif_expr = 
	| C_INTRO of (Ast.alg_expr * c_mixture * Tools.pos) 
	| C_DELETE of (Ast.alg_expr * c_mixture * Tools.pos) 
	| C_UPDATE of (string * Tools.pos * Ast.alg_expr * Tools.pos) (*TODO: pause*)
	| C_STOP of Tools.pos
	| C_SNAPSHOT of Tools.pos (*maybe later of mixture too*)

type enriched_rule = 
  {e_rule_label : Ast.rule_label ;
   e_rule_rule : rule ;
   e_rule_c_rule : c_rule }

type enriched_init = 
  {
   e_init_factor : int ; 
   e_init_mixture : mixture ;
   e_init_c_mixture : c_mixture ; 
   e_init_pos : Tools.pos
  } 
  
type c_compil = 
  {
    c_variables : c_variable Int_storage.Nearly_inf_Imperatif.t ; (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
	c_signatures : (agent * Tools.pos) Int_storage.Nearly_inf_Imperatif.t  ; (*agent signature declaration*)
    c_rules : enriched_rule Int_storage.Nearly_inf_Imperatif.t  ; (*rules (possibly named)*)
    c_observables : Ast.alg_expr Int_storage.Nearly_inf_Imperatif.t  ; (*list of patterns to plot*) 
    c_init : enriched_init Int_storage.Nearly_inf_Imperatif.t  ; (*initial graph declaration*)
	c_perturbations : perturbation Int_storage.Nearly_inf_Imperatif.t 
  }
  

   
