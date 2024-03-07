open Unix

let skdir = getenv "HOME" ^ "/.local/share/sk/"

let usage =
  "sk script-name [script-args]\n\
   sk --add script-name script-file\n\
   sk --remove script-name\n\
   sk --list\n\
   sk --help"

let fatal msg =
  let _ = print_endline ("\x1b[0;31mFATAL \x1b[0m" ^ msg) in
  exit 1

type op =
  | List
  | Help
  | Add of { name : string; file : string }
  | Remove of string
  | Run of { cmd : string; args : string list }
[@@deriving show]

(* parse args *)
let parse_args =
  let args = List.tl (Array.to_list Sys.argv) in
  match args with
  | "--add" :: name :: [ file ] -> Add { name; file }
  | "--remove" :: [ name ] -> Remove name
  | [ "--list" ] -> List
  | [ "--help" ] -> Help
  | cmd :: args -> Run { cmd; args }
  | [] -> fatal "no arguments provided"

(* init scripts directory *)
let _ = if not (Sys.file_exists skdir) then mkdir skdir 0o755

let () =
  match parse_args with
  | Help -> print_endline usage
  | List ->
      let _ = Array.map print_endline (Sys.readdir skdir) in
      ()
  | Add { name; file } ->
      let script =
        if Sys.file_exists file then Core.In_channel.read_all file
        else fatal ("script-file " ^ file ^ " does not exist")
      in
      let fd = openfile (skdir ^ name) [ O_RDWR; O_CREAT ] 0x755 in
      let _ = write fd (String.to_bytes script) 0 (String.length script) in
      close fd
  | Remove name ->
      let script = skdir ^ name in
      if Sys.file_exists script then Sys.remove script
      else fatal ("no script named " ^ name)
  | Run { cmd; args } ->
      let _ =
        create_process (skdir ^ cmd) (Array.of_list args) stdin stdout stderr
      in
      ()
