 (**
  * preprocess.ml
  * openkappa
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * 
  * Creation: 01/17/2011
  * Last modification: 01/18/2011
  * * 
  * Translation from kASim ast to ckappa representation,
  *  
  * Copyright 2010 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

let warn parameters mh message exn default = 
     Exception.warn parameters mh (Some "Translate") message exn (fun () -> default) 
  

let local_trace = true 
  
let add_entry parameters id agent site index (error,map) = 
    let error,old_list = 
       Ckappa_sig.Int_Set_and_Map.find_map_option parameters error id map 
    in 
    let old_list = 
        match old_list 
        with
           | None -> []
           | Some list -> list 
    in 
      Ckappa_sig.Int_Set_and_Map.add_map parameters error id ((agent,site,index)::old_list) map 

let rev_ast mixture = 
  let rec aux mixture sol = 
    match mixture with
      | Ast.EMPTY_MIX -> sol 
(*      | Ast.DOT(i,agent,mixture) -> aux mixture (Ast.DOT(i,agent,sol))*)
(*      | Ast.PLUS(i,agent,mixture) -> aux mixture (Ast.PLUS(i,agent,sol))*)
      | Ast.COMMA(agent,mixture) -> aux mixture (Ast.COMMA(agent,sol))
  in aux mixture Ast.EMPTY_MIX 
  
let pop_entry parameters error id map = 
  let error,list = Ckappa_sig.Int_Set_and_Map.find_map parameters error id map 
    in 
    match list with 
      | [a] ->
        let error,map = Ckappa_sig.Int_Set_and_Map.remove_map parameters error id map in 
        error,(a,map)
      | [b;a] -> 
        let error,map = Ckappa_sig.Int_Set_and_Map.add_map parameters error id [a] map in 
        error,(b,map) 
      | _ -> 
        warn parameters error (Some "line 44") Exit (("","",0),map)

let rec scan_interface parameters k agent interface remanent = 
      match interface with 
      | Ast.EMPTY_INTF -> remanent
      | Ast.PORT_SEP (port,interface) -> 
        scan_interface parameters k agent interface 
          (match port.Ast.port_lnk with 
            | Ast.LNK_VALUE (i,_) -> 
                  add_entry parameters i agent port.Ast.port_nme k remanent
            | _ -> remanent)
              
let scan_agent parameters k agent remanent = 
    scan_interface parameters k agent.Ast.ag_nme agent.Ast.ag_intf remanent    

let rec collect_binding_label parameters mixture f k remanent = 
  match mixture with 
  | Ast.COMMA (agent,mixture) (*| Ast.DOT (_,agent,mixture) | Ast.PLUS(_,agent,mixture)*) -> 
           collect_binding_label parameters mixture f (k+1) (scan_agent parameters (f k) agent remanent)
        | Ast.EMPTY_MIX -> remanent 

let translate_lnk_state parameters lnk_state remanent = 
    match lnk_state with 
     | Ast.LNK_VALUE (id,position) ->  
         let error,map = remanent  in 
         let error,((agent,site,index),map) = pop_entry parameters error id map  in 
         Ckappa_sig.LNK_VALUE (index,agent,site,id,position),(error,map)
     | Ast.FREE -> Ckappa_sig.FREE,remanent
     | Ast.LNK_ANY position -> Ckappa_sig.LNK_ANY position,remanent 
     | Ast.LNK_SOME position -> Ckappa_sig.LNK_SOME position,remanent
     | Ast.LNK_TYPE (x,y) -> Ckappa_sig.LNK_TYPE (x,y),remanent

let translate_port parameters port remanent = 
    let lnk,remanent = translate_lnk_state parameters port.Ast.port_lnk remanent in   
     {
       Ckappa_sig.port_nme = port.Ast.port_nme ;
       Ckappa_sig.port_int = port.Ast.port_int ; 
       Ckappa_sig.port_lnk = lnk ;
       port_pos = port.Ast.port_pos;
       Ckappa_sig.port_free = (match port.Ast.port_lnk with Ast.FREE -> Some true | Ast.LNK_ANY _ -> None | Ast.LNK_SOME _ | Ast.LNK_TYPE _ | Ast.LNK_VALUE _ -> Some false ) 
     },
    remanent

let rec translate_interface parameters interface remanent =  
    match interface with 
     | Ast.EMPTY_INTF -> Ckappa_sig.EMPTY_INTF,remanent
     | Ast.PORT_SEP (port,interface) -> 
          let port,remanent = translate_port parameters port remanent in 
          let interface,remanent = translate_interface parameters interface remanent in 
             Ckappa_sig.PORT_SEP (port,interface),remanent  

let translate_agent parameters agent remanent = 
    let interface,remanent = translate_interface parameters agent.Ast.ag_intf remanent in 
    {Ckappa_sig.ag_nme = agent.Ast.ag_nme ;
     Ckappa_sig.ag_intf = interface ;
     Ckappa_sig.ag_pos = agent.Ast.ag_pos;
    },
    remanent 

let rec build_skip k mixture = 
  if k=0 
  then mixture 
  else build_skip (k-1) (Ckappa_sig.SKIP(mixture)) 
  
let rec translate_mixture_zero_zero  parameters mixture remanent tail_size = 
   match mixture with 
     | Ast.EMPTY_MIX -> build_skip tail_size Ckappa_sig.EMPTY_MIX,remanent
     | Ast.COMMA(agent,mixture) ->
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture_zero_zero parameters mixture remanent tail_size  in 
            Ckappa_sig.COMMA(agent,mixture),remanent 
(*      | Ast.DOT(i,agent,mixture) -> 
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture_zero_zero parameters mixture remanent tail_size  in 
            Ckappa_sig.DOT(i,agent,mixture),remanent
      | Ast.PLUS(i,agent,mixture) -> 
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture_zero_zero parameters mixture remanent tail_size  in 
            Ckappa_sig.PLUS(i,agent,mixture),remanent*)
        
let rec translate_mixture_in_rule parameters mixture remanent prefix_size empty_size tail_size = 
   if prefix_size=0 
    then 
      let tail,remanent = translate_mixture_zero_zero parameters mixture remanent tail_size
      in 
        build_skip empty_size tail,remanent 
   else 
      match mixture with 
      | Ast.EMPTY_MIX -> Ckappa_sig.EMPTY_MIX,remanent
      | Ast.COMMA(agent,mixture) ->
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture_in_rule parameters mixture remanent (prefix_size-1) empty_size tail_size  in 
            Ckappa_sig.COMMA(agent,mixture),remanent 
(*      | Ast.DOT(i,agent,mixture) -> 
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture_in_rule parameters mixture remanent (prefix_size-1) empty_size tail_size  in 
            Ckappa_sig.DOT(i,agent,mixture),remanent
      | Ast.PLUS(i,agent,mixture) -> 
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture_in_rule parameters mixture remanent (prefix_size-1) empty_size tail_size  in 
            Ckappa_sig.PLUS(i,agent,mixture),remanent*)

 let rec translate_mixture parameters mixture remanent  = 
    match mixture with 
      | Ast.EMPTY_MIX -> Ckappa_sig.EMPTY_MIX,remanent
      | Ast.COMMA(agent,mixture) ->
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture parameters mixture remanent in 
            Ckappa_sig.COMMA(agent,mixture),remanent 
(*      | Ast.DOT(i,agent,mixture) -> 
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture parameters mixture remanent in 
            Ckappa_sig.DOT(i,agent,mixture),remanent
      | Ast.PLUS(i,agent,mixture) -> 
          let agent,remanent = translate_agent parameters agent remanent in 
          let mixture,remanent = translate_mixture parameters mixture remanent in 
            Ckappa_sig.PLUS(i,agent,mixture),remanent*)
 
let support_agent ag = 
  let name = ag.Ast.ag_nme in 
  let list = 
    let rec scan intf list = 
      match intf with 
      | Ast.EMPTY_INTF -> List.sort compare list 
      | Ast.PORT_SEP (port,intf)-> scan intf ((port.Ast.port_nme)::list)
    in
    scan ag.Ast.ag_intf []
  in 
  name,list 
  
let compatible_agent ag1 ag2 = 
  support_agent ag1 = support_agent ag2
  
let length mixture = 
  let rec aux mixture k = 
    match mixture with 
      | Ast.EMPTY_MIX -> k 
      | Ast.COMMA(_,mixture) (*| Ast.DOT(_,_,mixture) | Ast.PLUS(_,_,mixture)*) -> aux mixture (k+1)
  in aux mixture 0 
  
  
let longuest_prefix mixture1 mixture2 =
  let rec common_prefix mixture1 mixture2 k = 
    match mixture1 with 
      | Ast.EMPTY_MIX -> 
        begin
          k,mixture1,mixture2
        end 
      | Ast.COMMA(agent,mixture) (*| Ast.DOT(_,agent,mixture) | Ast.PLUS(_,agent,mixture)*) ->
        begin
          match mixture2 with 
            | Ast.EMPTY_MIX -> k,mixture1,mixture2
            | Ast.COMMA(agent',mixture') (*| Ast.DOT(_,agent',mixture') | Ast.PLUS(_,agent',mixture')*) -> 
               begin 
                 if compatible_agent agent agent'
                 then  
                   common_prefix mixture mixture' (k+1)
                 else 
                   k,mixture1,mixture2
               end  
        end
  in 
  let common_size,tail_lhs,tail_rhs = common_prefix mixture1 mixture2 0 in 
  common_size,length tail_lhs,length tail_rhs 
        
  


let refine_mixture_in_rule parameters error prefix_size empty_size tail_size mixture = 
     let f i =  if i>prefix_size then i+empty_size else i in 
     let remanent = collect_binding_label parameters mixture f 0 (error,Ckappa_sig.Int_Set_and_Map.empty_map) in  
     let mixture,(error,map) = translate_mixture_in_rule parameters mixture remanent prefix_size empty_size tail_size in
    error,mixture

let refine_mixture parameters error mixture = 
     let remanent = collect_binding_label parameters mixture (fun i -> i) 0 (error,Ckappa_sig.Int_Set_and_Map.empty_map) in  
     let mixture,(error,map) = translate_mixture parameters mixture remanent in
    error,mixture

let refine_init_t parameters error init_t = 
  match 
    init_t 
  with 
    Ast.INIT_MIX(alg_ex,mixture) -> 
      let error,mixture = refine_mixture parameters error mixture in 
      error,Some(alg_ex,mixture)
  | _ -> error,None
    
let refine_agent parameters error agent =
    let remanent = scan_agent parameters 0 agent (error,Ckappa_sig.Int_Set_and_Map.empty_map) in 
    let agent,(error,map) = translate_agent parameters agent remanent in 
      error,agent 
  
      
let refine_var parameters error var = 
  match var with 
    | Ast.VAR_KAPPA (mixture,x)  -> 
        let error,c_mixture = refine_mixture parameters error mixture in 
          error,Ckappa_sig.VAR_KAPPA(c_mixture,x) 
    | Ast.VAR_ALG (x,y) -> 
      error,Ckappa_sig.VAR_ALG(x,y)
      
let translate_compil parameters error compil = 
  let error,var_rev = 
    List.fold_left 
      (fun (error,list) var -> 
        let error,var = refine_var parameters error var in 
          error,(var::list))
    (error,[])
    compil.Ast.variables 
  in 
  let error,signatures_rev = 
    List.fold_left 
      (fun  (error,list) (agent,position)-> 
        let error,agent = refine_agent parameters error agent in 
        error,((agent,position)::list))
      (error,[])
      compil.Ast.signatures       
  in
  let error,rules_rev = 
     List.fold_left
      (fun (error,list) (id,rule) -> 
        let ast_lhs,ast_rhs = rev_ast rule.Ast.lhs,rev_ast rule.Ast.rhs in 
        let prefix,tail_lhs,tail_rhs = longuest_prefix ast_lhs ast_rhs in 
        let error,lhs = refine_mixture_in_rule parameters error prefix 0 tail_rhs ast_lhs in 
        let error,rhs = refine_mixture_in_rule parameters error prefix tail_lhs 0 ast_rhs in 
        error,
        (id,
          {
          Ckappa_sig.lhs = lhs ;
          Ckappa_sig.rhs =  rhs ;
          Ckappa_sig.arrow = rule.Ast.arrow ;
          Ckappa_sig.k_def = rule.Ast.k_def ;
          Ckappa_sig.k_un = rule.Ast.k_un 
          })::list)
    (error,[])
    compil.Ast.rules     
  in 
  let error,init_rev = 
     List.fold_left
      (fun (error,list) (id,init_t,position) -> 
        let error,mixture = refine_init_t parameters error init_t in 
        match mixture 
        with 
          Some (alg,mixture) ->  error,(id,alg,mixture,position)::list
        | None -> error,list)
    (error,[])
    compil.Ast.init    
  in 
  let error,perturbations_rev = 
    List.fold_left
      (fun (error,list) (b,m,p,o) -> 
        let error,m' = 
          List.fold_left 
            (fun (error,list) m -> 
              match m with 
              | Ast.INTRO (a,m,p) -> 
                let error,m' = refine_mixture parameters error (rev_ast m) in 
                error,Ckappa_sig.INTRO(a,m',p)::list
              | Ast.DELETE (a,m,p) -> 
                let error,m' = refine_mixture parameters error (rev_ast m) in 
                error,Ckappa_sig.DELETE(a,m',p)::list
              | Ast.UPDATE (x) -> error,(Ckappa_sig.UPDATE x)::list
              | Ast.STOP (x) -> error,(Ckappa_sig.STOP x)::list
              | Ast.SNAPSHOT p -> error,(Ckappa_sig.SNAPSHOT p)::list 
              | _ -> error,list (*to do*))
            (error,[])
            m
        in
        error,(b,List.rev m',p,o)::list
      )
    (error,[])
    compil.Ast.perturbations
  in 
   {
    Ckappa_sig.variables = List.rev var_rev;
    Ckappa_sig.signatures = List.rev signatures_rev;
    Ckappa_sig.rules = List.rev rules_rev ;      
    Ckappa_sig.observables  = compil.Ast.observables ;        
    Ckappa_sig.init = List.rev init_rev ;      
	Ckappa_sig.perturbations = List.rev perturbations_rev ;
   }

   
