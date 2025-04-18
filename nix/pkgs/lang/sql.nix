{ pkgs }:

with pkgs;
[
  usql # Universal command-line interface for SQL databases
  litecli # Command-line interface for SQLite
  postgresql # A powerful, open source object-relational database system also comes with psql cli
  pgcli # Alternative command-line interface for PostgreSQL
  steampipe # select * from cloud;
  # thanks to https://www.reddit.com/r/neovim/comments/1gqfvf9/how_do_you_think_about_dadbod/
  lazysql # A cross-platform TUI database management tool written in Go
  rainfrog # A database management TUI for postgres
  harlequin # The SQL IDE for Your Terminal
]
