[ # goDeps is a list of Go dependencies.
  {
    # goPackagePath specifies Go package import path.
    goPackagePath = "github.com/fatih/color";
    fetch = {
      # `fetch type` that needs to be used to get package source.
      # If `git` is used there should be `url`, `rev` and `sha256` defined next to it.
      type = "git";
      url = "https://github.com/fatih/color";
      rev = "v1.5.0";
      sha256 = "1hw9hgkfzbzqjhy29pqpk20xggxaqjv45wx8yn69488mw5ph7khh";
    };
  }
]
