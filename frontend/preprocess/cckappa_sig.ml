(**
  * cckappa_sig.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: January, the 17th of 2011
  * Last modification: March, the 30th of 2011
  * * 
  * Signature for prepreprocessing language ckappa 
  *  
  * Copyright 2010,2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "cckappa.sig") message exn (fun () -> default) 
  


type agent_name = int 
type site_name = int

type binding_state = 
    | Free 
    | Lnk_type of agent_name * site_name 

type site = (site_name,site_name) Ckappa_sig.site_type     
type state = (Ckappa_sig.internal_state,binding_state) Ckappa_sig.site_type  
 
module State = 
struct
  type t = state 
  let compare = compare
end 
    
type state_index = int

module Dictionary_of_States = Dictionary.Dictionary_of_Ord(State)
  
type state_dic = (unit,unit) Dictionary_of_States.dictionary
  
type kappa_handler = 
  {nrules: int; 
   nvars: int;
   nagents:int ;
   agents_dic : Ckappa_sig.agent_dic; 
   interface_constraints: Ckappa_sig.agent_specification Int_storage.Nearly_inf_Imperatif.t;
   sites: Ckappa_sig.site_dic Int_storage.Nearly_inf_Imperatif.t;
   states_dic: state_dic Int_storage.Nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t;
   dual: (agent_name * site_name * state_index) Int_storage.Nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif.t;
  }                  
     
type 'a interval = {min:'a;max:'a}
  
type 'state port = 
  { 
    site_name : site_name ; 
    site_position : Tools.pos ;
    site_free : bool option ; 
    site_state : 'state  ;
  }
  

module Site_map_and_set = Set_and_map.Make(struct type t = site_name let compare = compare end)
 
type 'state interface = 'state port Site_map_and_set.map
                                                                           
type 'interface proper_agent = 
  { 
    agent_kasim_id : int ; 
    agent_name : agent_name ;
    agent_interface : 'interface ;
    agent_position : Tools.pos ;
  }

let upgrade_interface ag interface  = 
  {
    agent_kasim_id = ag.agent_kasim_id ;
    agent_name = ag.agent_name ; 
    agent_interface =  interface ;
    agent_position = ag.agent_position 
  }

let map_agent f ag = 
  upgrade_interface 
    ag  
    begin 
      Site_map_and_set.map_map
         (fun port -> {site_free = port.site_free ; 
                       site_name = port.site_name ; 
                       site_position = port.site_position ; 
                       site_state = f port.site_state})
         ag.agent_interface 
    end 

let upgrade_some_interface ag = upgrade_interface ag 
  begin
      Site_map_and_set.map_map (fun x -> (*Some*) x) ag.agent_interface
  end 
  
  
type site_address = {agent_index : int ; site : site_name ; agent_type : agent_name }
type bond = site_address * site_address 
  
let build_address k agent site = {agent_index = k ; site = site ; agent_type = agent}
  
module Address_map_and_set = Set_and_map.Make (struct type t = site_address let compare = compare end)
 
type agent = 
   | Ghost
   | Agent of state_index interval interface proper_agent                               

type agent_sig = state_index list interface proper_agent 
 
type views = agent Int_storage.Quick_Nearly_inf_Imperatif.t 
type diff_views =  state_index interval port Site_map_and_set.map proper_agent  Int_storage.Quick_Nearly_inf_Imperatif.t
type mixture = 
  { 
    c_mixture : Ckappa_sig.mixture ; 
    views:  views ;
    bonds: site_address Site_map_and_set.map Int_storage.Quick_Nearly_inf_Imperatif.t ; 
    plus : (int * int) list;
    dot  : (int * int) list
    }
      
type variable = VAR_KAPPA of mixture * (string * Tools.pos) | VAR_ALG of Ast.alg_expr * (string * Tools.pos)  

type actions = 
  { creation: (int * agent_name) list ;
    remove: (int * unit interface proper_agent * int list) list ; 
    release: bond list ;
    bind: bond list ;
    half_break: (site_address*(state_index interval option)) list ; 
  }
  
let empty_actions = 
  {creation=[];remove=[];release=[];bind=[];half_break=[]}
     

type rule = 
  {
    rule_lhs: mixture ; 
    rule_arrow:Ast.arrow ; 
    rule_rhs: mixture; 
    diff_direct: diff_views; 
    diff_reverse: diff_views;
    actions: actions;
   }


  
type perturbation = Ast.bool_expr * modif_expr * Tools.pos * Ast.bool_expr option
and modif_expr = 
	| INTRO of (Ast.alg_expr * mixture * Tools.pos) 
	| DELETE of (Ast.alg_expr * mixture * Tools.pos) 
	| UPDATE of (string * Tools.pos * Ast.alg_expr * Tools.pos) (*TODO: pause*)
	| STOP of Tools.pos
	| SNAPSHOT of Tools.pos (*maybe later of mixture too*)

type enriched_rule = 
  {e_rule_label : Ast.rule_label ;
   e_rule_rule : Ckappa_sig.rule ;
   e_rule_c_rule : rule }

type enriched_init = 
  {
   e_init_factor : int ; 
   e_init_mixture : Ckappa_sig.mixture ;
   e_init_c_mixture : mixture ; 
   e_init_pos : Tools.pos
  } 
  
type compil = 
  {
    variables : variable Int_storage.Nearly_inf_Imperatif.t ; (*pattern declaration for reusing as variable in perturbations or kinetic rate*)
    signatures : (agent_sig * Tools.pos) Int_storage.Nearly_inf_Imperatif.t  ; (*agent signature declaration*)
    rules : enriched_rule Int_storage.Nearly_inf_Imperatif.t  ; (*rules (possibly named)*)
    observables : Ast.alg_expr Int_storage.Nearly_inf_Imperatif.t  ; (*list of patterns to plot*) 
    init : enriched_init Int_storage.Nearly_inf_Imperatif.t  ; (*initial graph declaration*)
    perturbations : Ckappa_sig.perturbation Int_storage.Nearly_inf_Imperatif.t 
  }
  
