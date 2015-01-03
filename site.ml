open Dsl

let site () =
  dir "" [file "here be dragons" "dragon";
          file "scary stuff" "firedragon";
          dir "more" [info "/!\\ Under construction /!\\"]]
