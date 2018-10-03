let doAnAction = str => Some(print_endline(str));
let hello = 4;
let doit = something => {
  [@else None]
  [%guard let Some(_) = doAnAction(something)];
  /* etc */
  Some("test");
};

let () = switch (doit("test doit")) {
| None => print_endline("None")
| Some(val_) => print_endline("Found " ++ (val_))
};
