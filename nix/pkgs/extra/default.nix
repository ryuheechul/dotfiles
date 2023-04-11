# programming language related ones should go to ../lang/default.nix and ../lang/support.nix instead

{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  ifEnv = envName: pkgs.lib.optionals (checkEnv envName);
  tag = import ../custom/tag { pkgs = pkgs; };
  hired = import ../custom/hired.nix { pkgs = pkgs; };
  cfn-lint = pkgs.python3.pkgs.cfn-lint;
  hexto256 = import ../custom/hexto256.nix;
  tf-helper = import ../custom/tf-helper.nix { pkgs = pkgs; };
  bat-riffle = import ../custom/bat-riffle { pkgs = pkgs; };
  alacritty-nightly = import ../custom/alacritty-nightly.nix;
in
with pkgs;
[
  # anything "extra" but without optional flag goes here
  hexto256
]
++ ifEnv "MY_NIX_EXTRA_NIGHTLY_ALACRITTY"
  [
    alacritty-nightly
  ]
++ ifEnv "MY_NIX_EXTRA_HIRED"
  [
    hired # A modern take on 'ed'
  ]
++ ifEnv "MY_NIX_EXTRA_AWS"
  [
    # also consider installing `pipx install aws-shell`
    awscli2 # aws cli
    ssm-session-manager-plugin # AWS SSM Plugin
    amazon-ecs-cli # aws ecs cli
    cfn-lint # Checks cloudformation for practices and behaviour that could potentially be improved
  ]
++ ifEnv "MY_NIX_EXTRA_CI"
  [
    circleci-cli # circle ci cli # add checkEnv MY_NIX_EXTRA_CIRCLE_CI
  ]
++ ifEnv "MY_NIX_EXTRA_NETWORK"
  [
    ipcalc # Simple IP network calculator
    socat # Utility for bidirectional data transfer between two independent data channels
    tcpdump # Network sniffer
    ngrep # Network packet analyzer
    tcpflow # TCP stream extractor
    termshark # A terminal user-interface for tshark, inspired by Wireshark.
  ]
++ ifEnv "MY_NIX_EXTRA_TAG"
  [
    tag
  ]
++ ifEnv "MY_NIX_EXTRA_TERRAFORM"
  [
    terraform
    nodePackages.cdktf-cli
    tf-helper
  ]
++ ifEnv "MY_NIX_EXTRA_NOTCURSES"
  [
    # since qrcodegen is marked broken
    (pkgs.notcurses.override {
      qrcodegenSupport = false;
    })
  ]
++ ifEnv "MY_NIX_EXTRA_BAT"
  (with bat-extras; [
    batman
    batgrep
    batdiff
    batwatch
    prettybat
    bat-riffle # A proof-of-concept for a pager-as-a-library. Mainly designed for bat, and not ready for general use.
  ])
++ ifEnv "MY_NIX_EXTRA_LIMA"
  [
    lima # Linux virtual machines (on macOS, in most cases)
  ]
++ ifEnv "MY_NIX_EXTRA_K8S"
  [
    podman # A program for managing pods, containers and container images
    # ```
    # export MY_NIX_EXTRA_PODMAN=1 # to include podman at `home-manager switch`
    #
    # # follow instruction on machine init & start - https://podman.io/getting-started/installation
    # # include these at ~/.local.zshrc
    # export DOCKER_HOST="unix:///${XDG_DATA_HOME}/containers/podman/machine/podman-machine-default/podman.sock" # to `docker` command to work with podman
    # alias docker=podman # or just alias it
    # ```
    docker-client # to shim the absence of docker command for podman in case podman is to replace to docker and act like one
    kubectl # Kubernetes CLI
    k9s # Kubernetes CLI To Manage Your Clusters In Style
    kubernetes-helm # A package manager for kubernetes
    kustomize # Customization of kubernetes YAML configurations
    kompose # A tool to help users who are familiar with docker-compose move to Kubernetes
    minikube # A tool that makes it easy to run Kubernetes locally
    # minikube with podman - https://github.com/containers/podman/issues/12713#issuecomment-1002567777
    # an example podman machine works for minikube - `podman machine init --rootful --cpus 4 --memory 4096 --disk-size 30`
  ]
++ ifEnv "MY_NIX_EXTRA_SSH"
  [
    openssh # An implementation of the SSH protocol
    xorg.xorgserver
    xorg.xauth
    # debug xauth - https://www.linuxquestions.org/questions/linux-newbie-8/warning-no-xauth-data-although-i%27m-using-%60ssh-y%60-4175525755/#post5272443
    # `xauth list`
    # `mcookie | xargs xauth add :0 .`
  ]
++ ifEnv "MY_NIX_EXTRA_WSL"
  [
    wslu # A collection of utilities for Windows 10/11 Linux Subsystems
    # which comes with wslview to enable opening a browser on Windows from terminal
    ruby # An object-oriented language for quick and easy programming
    # and `schasse/tmux-jump` plugin requires it
    ## below are for virtual machine capabilities via:
    ## - virt-manager: to be a "front-end"
    ## - qemu: to provide kvm capabilities for hardware acceleration
    ## - libvirt: to be a "back-end"
    virt-manager # Desktop user interface for managing virtual machines
    # qemu # A generic and open source machine emulator and virtualizer
    # - above might not work as libvirt not be able to find a path
    # - alternatively `apt-get install qemu-system-x86` (in case of debian/ubuntu)
    # libvirt # A toolkit to interact with the virtualization capabilities of recent versions of Linux and other OSes
    # - but wait libvirt will not work with non NixOS distros so in that case do something like below instead
    # - `apt-get install libvirt-daemon-system` (in case of debian/ubuntu)
    # - this also possibly requires logout and login (like terminating tmux server) for `libvirt` group to be effective
  ]
  # add any package to try out (locally more permanent way than `nix-shell -p [package]`
++ lib.optionals (builtins.pathExists ./local-only.nix) (import ./local-only.nix { pkgs = pkgs; })
# # this is actually not working great at least on ubuntu
# # it's probably wise to follow https://tailscale.com/kb/1031/install-linux/ instead
# ++ ifEnv "MY_NIX_EXTRA_TAILSCALE"
# [
#   tailscale
# ]
