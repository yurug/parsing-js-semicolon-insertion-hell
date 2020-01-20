open Utils
open Parser
open Parser.Incremental
open Parser.MenhirInterpreter

let parse lexer =

  let rec parse lexer previous_checkpoint checkpoint =
    match checkpoint with
    | InputNeeded _ ->
       begin
         let (token, lexer) = next_token lexer in
         try_semicolon_rule_3 token lexer checkpoint |> function
         | None ->
            parse lexer (Some checkpoint) (offer checkpoint token)
         | Some (lexer, checkpoint') ->
            parse lexer (Some checkpoint) checkpoint'
       end
    | Accepted ast ->
       ast
    | HandlingError _ | AboutToReduce _ | Shifting _ ->
       parse lexer previous_checkpoint (resume checkpoint)
    | Rejected ->
       match previous_checkpoint with
       | None ->
          failwith "parse_error"
       | Some reboot_checkpoint ->
          (try_semicolon_rule_1 lexer ||| try_semicolon_rule_2 lexer)
          reboot_checkpoint
          |> function
          | None ->
             failwith "parse error"
          | Some (lexer, checkpoint) ->
             parse lexer previous_checkpoint checkpoint


  (* We keep NEWLINEs in the stream of tokens but we hide them to the
     LR(1) parser. *)
  and next_token lexer =
    let (token, lexer) = Lexer.next_token lexer in
    if Lexer.of_menhir_token token = NEWLINE then
      next_token lexer
    else
      (token, lexer)

  and preceded_by token lexer = Lexer.(
    position lexer > 0 && of_menhir_token (read_token ~back:1 lexer) = token
  )

  (**********)
  (* RULE 1 *)
  (**********)

  (*

     When, as the source text is parsed from left to right, a token
     (called the offending token) is encountered that is not allowed
     by any production of the grammar, ...

   *)
  and get_offending_token lexer =
    Lexer.(of_menhir_token (read_token lexer))

  (*

     ... then a semicolon is automatically inserted before
     the offending token ...

  *)
  and insert_semicolon lexer checkpoint =
    if would_insert_empty_statement lexer checkpoint
    || would_shift_a_for_semicolon lexer checkpoint
    then
      (lexer, checkpoint)
    else
      really_insert_semicolon lexer checkpoint

  and really_insert_semicolon lexer checkpoint =
      (Lexer.rewind lexer, offer checkpoint (Lexer.menhir_token SEMICOLON))

  and would_insert_empty_statement lexer checkpoint =
    let (_, checkpoint) = really_insert_semicolon lexer checkpoint in
    match Utils.next_reduction (Some (Lexer.menhir_token EOF)) checkpoint with
    | None ->
       false
    | Some production ->
       match rhs production with
       | [X (T T_SEMICOLON)] -> true
       | _ -> false

  and would_shift_a_for_semicolon lexer checkpoint =
    let (_, checkpoint) = really_insert_semicolon lexer checkpoint in
    match before_next_shift checkpoint with
    | Some before ->
       let is_for = function
         | None ->
            false
         | Some (Element (lrstate, _, _, _)) ->
            match items lrstate with
            | [ (production, dot) ] ->
               begin match rhs production with
               | X (T T_FOR) :: _ ->
                  dot = 3 || dot = 5
               | _ -> false
               end
            | _ -> false
       in
       is_for (get 0 before)
    | None ->
       false

  and try_semicolon_rule_1 lexer checkpoint =
    let offending_token = get_offending_token lexer in

    (*
       ... if one or more of the following conditions is true:

       - The offending token is separated from the previous token by at
         least one LineTerminator.

    *)

    let is_separated_from_previous_token_by_at_least_one_lineterminator () =
      Lexer.(of_menhir_token (read_token ~back:1 lexer)) = NEWLINE
    in

    (*
        - The offending token is }.

     *)

    let offending_token_is_rbrace () =
      offending_token = RBRACE
    in

    (*
        - The previous token is ) and the inserted semicolon would then be
          parsed as the terminating semicolon of a do-while statement
          (13.7.2).
    *)
    let previous_token_is_end_of_do_while_condition () =
      let _, checkpoint = insert_semicolon lexer checkpoint in
      match Utils.next_reduction (Some (Lexer.read_token lexer)) checkpoint with
      | Some production ->
         begin match rhs production with
         | X (T T_DO) :: _ ->
            (* We recognize the do-while production using its first token. *)
            true
         | _ -> false
         end
      | _ -> false
    in

    if is_separated_from_previous_token_by_at_least_one_lineterminator ()
    || offending_token_is_rbrace ()
    || previous_token_is_end_of_do_while_condition ()
    then
      Some (insert_semicolon lexer checkpoint)
    else
      None

  (**********)
  (* RULE 2 *)
  (**********)

  (*

     When, as the source text is parsed from left to right, the end of
     the input stream of tokens is encountered and the parser is
     unable to parse the input token stream as a single instance of
     the goal nonterminal, then a semicolon is automatically inserted
     at the end of the input stream.

  *)
  and try_semicolon_rule_2 =
  let one_shot = ref true in
  fun lexer checkpoint ->
    if get_offending_token lexer = EOF && !one_shot then (
      one_shot := false;
      Some (insert_semicolon lexer checkpoint)
    ) else
      None

  (**********)
  (* RULE 3 *)
  (**********)

  (*

     When, as the source text is parsed from left to right, a token is
     encountered that is allowed by some production of the grammar,
     but the production is a restricted production and the token would
     be the first token for a terminal or nonterminal immediately
     following the annotation “[no LineTerminator here]” within the
     restricted production (and therefore such a token is called a
     restricted token), and the restricted token is separated from the
     previous token by at least one LineTerminator, then a semicolon
     is automatically inserted before the restricted token.

  *)
  and try_semicolon_rule_3 token lexer checkpoint =
    let restricted_production production =
      match rhs production with
      | X (T T_BREAK) :: _ ->
         (* We recognize the production using the first terminal. *)
         true
      | _ ->
         false
    in
    let occurs_in_restricted_production checkpoint =
      let (lexer, checkpoint) = really_insert_semicolon lexer checkpoint in
      match next_reduction (Some token) checkpoint with
      | Some production ->
         if restricted_production production then (
           Some (lexer, checkpoint)
         ) else
           None
      | None ->
         None
   in
   match Lexer.of_menhir_token token with
      | IDENT _ ->
         if preceded_by NEWLINE lexer then
           occurs_in_restricted_production checkpoint
         else
           None
      | _ ->
         None
  in
  parse lexer None (goal Lexing.dummy_pos)

let parse_string s = parse (Lexer.make 3 (Lexing.from_string s))

let score = ref 0

let nb_tests = ref 0

let check flag s =
  incr nb_tests;
  let ok, ko = if flag then "[ OK ]", "[ KO ]" else "[ KO ]", "[ XF ]" in
  let status =
    try
      ignore (parse_string s);
      if flag then incr score;
      ok
    with exn ->
      if not flag then incr score;
      Printf.sprintf "%s (%s)" ko (Printexc.to_string exn)
  in
  Printf.eprintf "%s %s\n" status s

let accept = check true

let reject = check false

let make_test = true

let _tests_ = if make_test then (
  List.iter accept [

      (* Valid and "simple" inputs. *)

      "x = y;";
      "do { x = y; } while (x);";
      "{ x = y; y = z; }";
      "break label;";
      ";";
      "for (x; y; z) x = z;";

      (* Some inputs requiring semicolon insertions. *)

      (* First rule. *)

      (* Subcase 1. *)
      "{ x = y
         y = z; }";

      "{
         x = y
         y = z
       }";

      "{
         x = y
         y = z
         for (x; y; z) z = y
         do {} while (z)
         x = y
       }";

      (* Subcase 2. *)
      "{ x = y }";

      (* Subcase 3. *)
      "{ do { x = y; } while (z) x = y; }";

      (* Second rule. *)
      "x = y";
      "do { x = y; } while (z)";
      "break t";

      (* Third rule. *)
      "{
         break
         x = y
       }"
  ];

  List.iter reject [
      "x = ;";

      "break x y;";

      "{";

      "do { x = y; } while (z";

      "for (x
            y; z) x = y;";

      "for (x; y
            z) x = y;"

  ]
)

let _conclude_ =
  Printf.eprintf "SCORE: %d/%d\n" !score !nb_tests
