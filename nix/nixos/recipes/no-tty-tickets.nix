{ ... }:

# basically all ttys to share the sudo cache (not just between tmux panes);
# it's convenient so can be used until there is a good password-less solution is implemented 

let
  # debug with `cat /etc/group`, `sudo -l` and `sudo cat /etc/sudoers`
  noTtyTickets = ''
    # disable tty_ticekts - https://www.sudo.ws/docs/man/sudoers.man/#SUDOERS_OPTIONS
    Defaults:%wheel timestamp_type=global

    # basically the above used to be the below
    # Defaults:%wheel !tty_tickets
  '';
in
{
  security.sudo.extraConfig = noTtyTickets;
  security.sudo-rs.extraConfig = noTtyTickets;
}
