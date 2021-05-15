envName:

let
  env = builtins.getEnv envName;
  len = builtins.stringLength env;
in
  len > 0
