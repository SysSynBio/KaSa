(**
    * kappa.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 01/10/2010
    * Last modification: 01/10/2010
    * * 
    * Signature, handler, and maps module for dealing with internal kappa representation
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)


type agent_name = int
type site_name = int
type state = int 
type halfbond = int 

type rule = {name:string}
  
(*type puzzle_hole = 
    {half_bond:halfbond; 
                                                                        context:(site_name * state) list}*)
type puzzle_hole = int 
  
module SiteMap = Set_and_map.Make (struct type t = site_name let compare = compare end)
 
type environment = state SiteMap.map

 
type kappa_handler = 
  {abstract_halfbond: agent_name -> site_name -> state -> halfbond;
   abstract_puzzle_hole: agent_name -> site_name -> state -> environment -> puzzle_hole;
   dual_of_puzzle_hole: puzzle_hole -> puzzle_hole;
   is_bound: state -> bool;
   print_agent: out_channel -> agent_name -> state SiteMap.map -> unit}
  
let kappa_handler = 
    {abstract_halfbond = (fun  x s state -> 0);
     abstract_puzzle_hole = (fun x s state env -> 0);
     dual_of_puzzle_hole = (fun a -> a);
     is_bound = (fun x -> true);
     print_agent = (fun log x _ -> Printf.fprintf log "%d" x)}
     
 