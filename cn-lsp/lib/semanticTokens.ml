open! Base
module SemanticTokensLegend = Lsp.Types.SemanticTokensLegend
module SemanticTokenModifiers = Lsp.Types.SemanticTokenModifiers
module SemanticTokenTypes = Lsp.Types.SemanticTokenTypes

module SupportedTokens = struct
  (** A subset of the types enumerated in the LSP documentation, at
      https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens) *)
  type ty = Comment [@@deriving enum, enumerate]

  let ty_to_string (tok_ty : ty) : string =
    match tok_ty with
    | Comment -> "comment"
  ;;

  (* We don't currently support token modifiers, but if we want to, they should
     be defined here, similarly to token types *)
end

(** The token types and modifiers the server supports *)
let legend : SemanticTokensLegend.t =
  let tokenTypes = List.map SupportedTokens.all_of_ty ~f:SupportedTokens.ty_to_string in
  let tokenModifiers = [] in
  SemanticTokensLegend.create ~tokenTypes ~tokenModifiers
;;

type semantic_token =
  { delta_line : int
  ; delta_char : int
  ; length : int
  ; token_type : SemanticTokenTypes.t
  ; token_modifiers : SemanticTokenModifiers.t list
  }

(** A specialized range that only describes a (portion of a) single line *)
type linear_range =
  { line : int
  ; start_char : int
  ; end_char : int option
  }

(** The beginning of a document *)
let origin : linear_range = { line = 0; start_char = 0; end_char = Some 0 }

(** Convert ranges, which may span multiple lines, to linear ranges, which only
    span one line *)
let rec to_lines (ranges : Range.t list) : linear_range list =
  match ranges with
  | [] -> []
  | range :: rest ->
    (match range.end_.line - range.start.line with
     | 0 ->
       let this_line =
         { line = range.start.line
         ; start_char = range.start.character
         ; end_char = Some range.end_.character
         }
       in
       this_line :: to_lines rest
     | _ ->
       let this_line =
         { line = range.start.line; start_char = range.start.character; end_char = None }
       in
       let remainder =
         { range with start = { line = range.start.line + 1; character = 0 } }
       in
       this_line :: to_lines (remainder :: rest))
;;

(** Convert linear ranges to semantic tokens, relative to [prev] *)
let rec to_semantic_tokens (linear_ranges : linear_range list) ~(prev : linear_range)
  : semantic_token list
  =
  match linear_ranges with
  | [] -> []
  | cur :: rest ->
    let delta_line = cur.line - prev.line in
    let delta_char =
      if Int.equal delta_line 0 then cur.start_char - prev.start_char else cur.start_char
    in
    let length = Option.value cur.end_char ~default:999 - cur.start_char in
    let token_type = SemanticTokenTypes.Comment in
    let token_modifiers = [] in
    let tok = { delta_line; delta_char; length; token_type; token_modifiers } in
    tok :: to_semantic_tokens rest ~prev:cur
;;

(** Generate semantic tokens to highlight comments as comments *)
let comment_tokens (uri : Uri.t) : (Lsp.Types.SemanticTokens.t, string) Result.t =
  let ( let@ ) x f = Result.bind x ~f in
  let@ cn_comment_ranges = Parse.document_file_comments uri in
  List.iter cn_comment_ranges ~f:(fun r -> Log.d (Range.to_string r));
  let semantic_tokens = to_semantic_tokens (to_lines cn_comment_ranges) ~prev:origin in
  let comment_index = SupportedTokens.ty_to_enum Comment in
  let modifiers = 0b0 in
  (* This 5-based encoding is defined at
     https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens
  *)
  let lsp_tokens = Array.create ~len:(List.length semantic_tokens * 5) 0 in
  List.iteri semantic_tokens ~f:(fun i tok ->
    let base_idx = i * 5 in
    lsp_tokens.(base_idx + 0) <- tok.delta_line;
    lsp_tokens.(base_idx + 1) <- tok.delta_char;
    lsp_tokens.(base_idx + 2) <- tok.length;
    lsp_tokens.(base_idx + 3) <- comment_index;
    lsp_tokens.(base_idx + 4) <- modifiers;
    ());
  Ok (Lsp.Types.SemanticTokens.create ~data:lsp_tokens ())
;;
