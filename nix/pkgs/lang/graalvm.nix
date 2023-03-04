{ pkgs }:

# could be an alternative to ./java.nix and more
# mainly for faster startup and supporting other languages and ecosystem
let
  # thanks to https://github.com/NixOS/nixpkgs/issues/202336#issuecomment-1430134653
  graalvm = (with pkgs;(graalvm17-ce.override {
    products = with graalvmCEPackages; [
      # uncomment as more are required
      ruby-installable-svm-java17 # expose `ruby` binary
      # js-installable-svm-java17
      # llvm-installable-svm-java17
      # native-image-installable-svm-java17
      # nodejs-installable-svm-java17
      # python-installable-svm-java17
      # wasm-installable-svm-java17
    ];
  }));
in
with pkgs;
[
  graalvm # High-Performance Polyglot VM that comes with the binaries like `java` and `javac`, etc. by default
  babashka # Native, fast starting Clojure interpreter for scripting - does not require JVM
  bbin # Install any Babashka script or project with one command
]
