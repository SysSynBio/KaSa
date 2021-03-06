(**
    * mvbdu_sig.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 22/11/2010
    * Last modification: 19/12/2010
    * * 
    * Signature for memoized function 
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)
  
type ('a,'b,'blist,'c,'d,'f,'g) memoized_fun = 
  {
    f: Remanent_parameters_sig.parameters -> Exception.method_handler-> 'c;
    store: Remanent_parameters_sig.parameters -> Exception.method_handler -> ('f,'b,'blist,'a,'g) handler -> 'd -> 'a Mvbdu_sig.mvbdu -> Exception.method_handler * ('f,'b,'blist,'a,'g) handler;
    get: Remanent_parameters_sig.parameters -> Exception.method_handler -> ('f,'b,'blist,'a,'g) handler -> 'd -> Exception.method_handler * (('f,'b,'blist,'a,'g) handler * 'a Mvbdu_sig.mvbdu option)
  }
and 
  ('f,'b,'c,'d,'e) handler = 
  {
    data: 'f;
    mvbdu_dictionary: 'b;
    list_dictionary: 'c;
    print_cell: out_channel -> string -> 'd Mvbdu_sig.cell -> unit ;
    print_skel: out_channel -> string -> 'd Mvbdu_sig.skeleton -> unit ;
    print_mvbdu: out_channel -> string -> 'd Mvbdu_sig.mvbdu -> unit
  }
    
type 'a pair = 'a * 'a 

type ('a,'b,'blist,'c,'d,'e) unary_memoized_fun = 
  (
    'a,
    'b,
    'blist,
    ('a -> (Exception.method_handler * (Exception.method_handler -> Exception.method_handler * 'c))),
    'a Mvbdu_sig.mvbdu,
    'd,'e) 
  memoized_fun
  
type ('a,'b,'blist,'c,'d,'e) binary_memoized_fun = 
  (
     'a,
     'b,
     'blist,
    (('a -> (Exception.method_handler * ('a,'b,'blist,'c,'d,'e) unary_memoized_fun)) pair),
    'a Mvbdu_sig.mvbdu*'a Mvbdu_sig.mvbdu,
    'd,
    'e
  ) 
    memoized_fun   
  
type ('a,'b,'blist,'c,'d,'e) unary_other_memoized_fun = 
  (
    'a,
    'b,
    'blist,
    'a -> (Exception.method_handler * (Exception.method_handler -> Exception.method_handler * 'a Mvbdu_sig.cell)),
    'd * 'a Mvbdu_sig.mvbdu,
    'c,
    'e
    )
      memoized_fun 
    
type ('a,'b,'blist,'c,'d,'e) reset = 
  {
    empty_list:(Exception.method_handler * ('a,'b,'blist,'c,'d,'e) unary_memoized_fun);
    leaf: 'a -> (Exception.method_handler * (Exception.method_handler -> Exception.method_handler * 'a Mvbdu_sig.cell));
    clean_head: Exception.method_handler * ('a,'b,'blist,'c,'d,'e) unary_memoized_fun;
    build_false: int -> int -> (Exception.method_handler * (Exception.method_handler -> Exception.method_handler * 'a Mvbdu_sig.cell));
    build_true: int -> int -> 'a Mvbdu_sig.mvbdu -> 'a Mvbdu_sig.mvbdu -> 
                       (Exception.method_handler  * (Exception.method_handler  -> Exception.method_handler * 'a Mvbdu_sig.cell))
  }
      
  