(*
 * This source is part of Percepio -- the build system inspector
 *
 * Copyright (C) 2009 Alan Falloon <alan.falloon@gmail.com>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Percepio_parser is the module that translates the stream of system
    events generated by strace(1) and turns it into the tree of
    processes, what files they accessed, and other useful stuff.

    When it finishes it serializes this information out so that it can
    be usefully sliced and diced by other scripts.

*)

open Genlex ;;
open Printf ;;

(** This makes a stream of characters from an input stream. However,
    because Genlex doesn't let us know about newlines, they are
    translated into semicolons. This can be done naively because
    newlines in string constants etc. are escaped by strace.

*)
let translated_char_stream in_chan =
  let f _ =
    try
      begin
        match input_char in_chan with
            '\n' -> Some ';'
          | c ->  Some c
      end
    with End_of_file -> None
  in
  Stream.from f
;;

(** The lexer *)
let lexer =
  make_lexer
    [ ";"
    ; "(" ; ")"
    ; "[" ; "]" ; "~["
    ; "{" ; "}"
    ; "."
    ; ","
    ; "|" ; "&"
    ; "="
    ; "?"
    ]
;;

(** A debug printer for the Genlex tokens *)
let print_token = function
    Kwd ";"  -> printf "\n"
  | Kwd s    -> printf "(K %s)" s
  | Ident s  -> printf "(I %s)" s
  | Int i    -> printf "(N %d)" i
  | Float f  -> printf "(F %f)" f
  | String s -> printf "(S %S)" s
  | Char c   -> printf "(C %C)" c
;;

(** eat tokens and print them until the newline *)
let rec p_print_to_end = parser
    [< 'Kwd ";" >] -> printf "\n";
  | [< 't; rest >] -> print_token t; p_print_to_end rest
;;

(** eat tokens until the newline (semicolon) *)
let rec p_eat_to_nl = parser
    [< 'Kwd ";" >] -> ()
  | [< '_; rest >] -> p_eat_to_nl rest

(** parse the return codes *)
let p_ret_code = parser
    [< 'Kwd "="; 'Int r; ()=p_eat_to_nl >] -> r
;;

(** parse lists of strings *)
let p_str_list =
  let rec p_tail acc = parser
      [< 'Kwd ","; 'String s; rest >] -> p_tail (s::acc) rest
    | [< 'Kwd "]" >] -> List.rev acc
  in
  let p_head = parser
      [< 'String s; rest >] -> p_tail [s] rest
    | [< 'Kwd "]" >] -> []
  in
  parser
      [< 'Kwd "["; rest >] -> p_head rest
;;

(** The operation types *)
type operations =
    Exec of string * string list * string list
  | FailedRead of string
  | Nop
;;

(** parse the different operations *)
let p_operation = parser
    (** execve(exe_name,args,env) = 0/-1 *)
    [< 'Ident "execve"; 'Kwd "("; 'String exe_file; 'Kwd ","; args=p_str_list;
       'Kwd ","; env=p_str_list; 'Kwd ")"; r=p_ret_code >] ->
      if r < 0
      then FailedRead exe_file
      else Exec (exe_file,args,env)

  | (** ignore some calls *)
      [< 'Ident "getcwd"; ()=p_eat_to_nl >]   -> Nop
  | [< 'Ident "statfs"; ()=p_eat_to_nl >]     -> Nop
  | [< 'Ident "wait4"; ()=p_eat_to_nl >]      -> Nop
  | [< 'Ident "vfork"; ()=p_eat_to_nl >]      -> Nop
  | [< 'Ident "fork"; ()=p_eat_to_nl >]       -> Nop
  | [< 'Ident "clone"; ()=p_eat_to_nl >]      -> Nop
  | [< 'Ident "arch_prctl"; ()=p_eat_to_nl >] -> Nop

  | (** catch-all, anything we fail to parse, print *)
      [< ()=p_print_to_end >] -> Nop
;;

(** Parse a line of strace output *)
let p_line = parser
    [< 'Int pid; 'Float _; op=p_operation >] -> pid,op
  | [< >] -> raise End_of_file
;;

let () =
  let chr_stream = translated_char_stream stdin in
  let tok_stream = lexer chr_stream in
    try
      while true do
        ignore (p_line tok_stream)
      done
    with End_of_file -> ()
