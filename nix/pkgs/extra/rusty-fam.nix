{ pkgs }:

with pkgs;
[
  yazi # Blazing fast terminal file manager written in Rust, based on async I/O
  hyperfine # Command-line benchmarking tool
  dua # Tool to conveniently learn about the disk usage of directories
  ripgrep-all # Ripgrep, but also search in PDFs, E-Books, Office documents, zip, tar.gz, and more
  xh # Friendly and fast tool for sending HTTP requests
  fselect # Find files with SQL-like queries
  mprocs # TUI tool to run multiple commands in parallel and show the output of each command separately
  just # Handy way to save and run project-specific commands
  mask # CLI task runner defined by a simple markdown file
  presenterm # Terminal based slideshow tool
  kondo # Save disk space by cleaning unneeded files from software projects
]
# inspired by "Oxidise Your Command Line (2025 Edition) - https://www.youtube.com/watch?v=rWMQ-g2QDsI"
# except the tools that already installed or not interested at the moment
