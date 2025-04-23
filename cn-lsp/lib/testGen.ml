open! Base
module Filename = Stdlib.Filename

let spf = Printf.sprintf

module Error = struct
  type t =
    | CnError of LspCn.Error.t
    | CerbError of LspCerb.Error.t
    | TestGenError of exn
    | ProcessError of Process.Error.t
    | CompileError of Process.proc_out

  let to_string (err : t) : string =
    match err with
    | CerbError (loc, cause) -> LspCerb.Error.to_string (loc, cause)
    | CnError e -> LspCn.Error.to_string e
    | TestGenError exn -> "error during test generation: " ^ Exn.to_string exn
    | ProcessError e -> Process.Error.to_string e
    | CompileError e ->
      let unlines = String.concat ~sep:"\n" in
      unlines
        [ spf "Test compilation failed with exit code %i" e.exit_code
        ; "stdout:"
        ; unlines (List.map (String.split_lines e.stdout) ~f:(fun line -> "  " ^ line))
        ; "stderr:"
        ; unlines (List.map (String.split_lines e.stderr) ~f:(fun line -> "  " ^ line))
        ]
  ;;
end

type 'a m = ('a, Error.t) Result.t

let ( let* ) (a : 'a m) (f : 'a -> 'b m) : 'b m = Result.bind a ~f

let lift_cerb (x : 'a LspCerb.m) : 'a m =
  Result.map_error (LspCerb.run x) ~f:(fun (l, c) -> Error.CerbError (l, c))
;;

let lift_cn (x : 'a LspCn.m) : ('a, Error.t) Result.t =
  Result.map_error x ~f:(fun e -> Error.CnError e)
;;

(** Compile tests in [dir], derived from [file_under_test], to produce an
    executable entrypoint to run those tests. *)
let compile_tests ~(dir : string) ~(file_under_test : string) ~(cn_runtime_path : string)
  : (Uri.t, Error.t) Result.t
  =
  (* When CN is asked to test a file `foo.c`, it generates several harness
     files, called `foo_test.c`, `foo-exec.c` (hyphenated, not underscored), and
     `cn.c`. These all get compiled, along with some runtime files, to produce a
     standalone test executable. *)
  let name = Filename.remove_extension (Filename.basename file_under_test) in
  let test_file = Filename.concat dir (name ^ "_test.c") in
  let exec_file = Filename.concat dir (name ^ "-exec.c") in
  let cn_file = Filename.concat dir "cn.c" in
  let entrypoint = Filename.concat dir "tests.out" in
  let cmd = "cc" in
  let args =
    [ "-o"
    ; entrypoint
    ; "-g"
    ; test_file
    ; exec_file
    ; cn_file
    ; Filename.concat cn_runtime_path "libcn_exec.a"
    ; Filename.concat cn_runtime_path "libcn_replica.a"
    ; Filename.concat cn_runtime_path "libcn_test.a"
    ; "-I" ^ Filename.concat cn_runtime_path "include"
      (* [dir] may not be [file_under_test]'s original location, so if the file
         has local includes, this should ensure it can resolve them *)
    ; "-I" ^ Filename.dirname file_under_test
    ]
  in
  Log.d "Compiling tests with arguments:";
  List.iter args ~f:(fun a -> Log.d (spf "  %s" a));
  match Process.read_process cmd (Array.of_list (cmd :: args)) with
  | Error err -> Error (ProcessError err)
  | Ok out ->
    (match out.exit_code with
     | 0 -> Ok (Uri.of_path entrypoint)
     | _ -> Error (CompileError out))
;;

(** Generate tests for all the functions in the given file (or for just
    [fn_name] in particular), and return the executable entrypoint for those
    tests. *)
let generate_tests
  (cerb_env : LspCerb.env)
  (file_under_test : Uri.t)
  ~(cn_runtime_path : string)
  ~(fn_name : string option)
  : Uri.t m
  =
  let output_dir = Filename.get_temp_dir_name () in
  Cn.Sym.executable_spec_enabled := true;
  let file_under_test_path = Uri.to_path file_under_test in
  let without_ownership_checking = false in
  let inherit_loc = true in
  let* cabs_tunit, milicore_prog, (markers_env, ail_prog), _statement_locs =
    lift_cerb (LspCerb.frontend cerb_env file_under_test_path)
  in
  Cn.Check.skip_and_only := [], Option.to_list fn_name;
  let* mucore_prog =
    lift_cn
      (Cn.Core_to_mucore.normalise_file
         ~inherit_loc
         (markers_env, snd ail_prog)
         milicore_prog)
  in
  (* Experimentally, CN seems to assume that this map is empty when beginning
     test generation, and will generate faulty code if it isn't *)
  Cn.Fulminate.Cn_to_ail.(records := RecordMap.empty);
  Cn.Fulminate.Cn_to_ail.augment_record_map (Cn.BaseTypes.Record []);
  let* () =
    try
      Cn.Fulminate.Executable_spec.main
        ~without_ownership_checking
        ~without_loop_invariants:true
        ~with_loop_leak_checks:false
        ~with_test_gen:true
        ~copy_source_dir:false
        file_under_test_path
        ~use_preproc:false
        ail_prog
        None
        (Some output_dir)
        mucore_prog;
      Cn.TestGeneration.run
        ~output_dir
        ~filename:file_under_test_path
        ~without_ownership_checking
        cabs_tunit
        (snd ail_prog)
        mucore_prog;
      Ok ()
    with
    | exn -> Error (TestGenError exn)
  in
  compile_tests ~dir:output_dir ~file_under_test:file_under_test_path ~cn_runtime_path
;;
