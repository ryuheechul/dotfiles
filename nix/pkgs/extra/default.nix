# programming language related ones should go to ../lang/default.nix and ../lang/support.nix instead

{ pkgs }:

let
  checkEnv = import ../../utils/checkEnv.nix;
  ifEnv = envName: pkgs.lib.optionals (checkEnv envName);
  tag = import ../custom/tag { pkgs = pkgs; };
  hired = import ../custom/hired.nix { pkgs = pkgs; };
  cfn-lint = pkgs.python3.pkgs.cfn-lint;
  hexto256 = import ../custom/hexto256.nix;
  termimagenator = import ../custom/termimagenator.nix;
  tf-helper = import ../custom/tf-helper.nix { pkgs = pkgs; };
  bat-riffle = import ../custom/bat-riffle { pkgs = pkgs; };
  alacritty-nightly = import ../custom/alacritty-nightly.nix;
  wsl = import ./wsl.nix { pkgs = pkgs; };
  rsop = import ../custom/rsop.nix { pkgs = pkgs; };
in
with pkgs;
[
  # anything "extra" but without optional flag goes here
  hexto256
  termimagenator
]
++ ifEnv "MY_NIX_EXTRA_NIGHTLY_ALACRITTY" [
  alacritty-nightly
]
++ ifEnv "MY_NIX_EXTRA_HIRED" [
  hired # A modern take on 'ed'
]
++ ifEnv "MY_NIX_EXTRA_AWS" [
  # also consider installing `pipx install aws-shell`
  awscli2 # aws cli
  ssm-session-manager-plugin # AWS SSM Plugin
  amazon-ecs-cli # aws ecs cli
  cfn-lint # Checks cloudformation for practices and behaviour that could potentially be improved
]
++ ifEnv "MY_NIX_EXTRA_CI" [
  circleci-cli # circle ci cli # add checkEnv MY_NIX_EXTRA_CIRCLE_CI
]
++ ifEnv "MY_NIX_EXTRA_NETWORK" [
  ipcalc # Simple IP network calculator
  socat # Utility for bidirectional data transfer between two independent data channels
  tcpdump # Network sniffer
  ngrep # Network packet analyzer
  tcpflow # TCP stream extractor
  termshark # A terminal user-interface for tshark, inspired by Wireshark.
  whois # Intelligent WHOIS client from Debian
]
++ ifEnv "MY_NIX_EXTRA_TAG" [
  tag
]
++ ifEnv "MY_NIX_EXTRA_TERRAFORM" [
  terraform
  nodePackages.cdktf-cli
  tf-helper
]
++ ifEnv "MY_NIX_EXTRA_NOTCURSES" [
  # since qrcodegen is marked broken
  (pkgs.notcurses.override {
    qrcodegenSupport = false;
  })
]
++ ifEnv "MY_NIX_EXTRA_BAT" (
  with bat-extras;
  [
    rsop # Simple, fast & configurable tool to open and preview files
    batman
    batgrep
    batdiff
    batwatch
    prettybat
    bat-riffle # A proof-of-concept for a pager-as-a-library. Mainly designed for bat, and not ready for general use.
  ]
)
++ ifEnv "MY_NIX_EXTRA_LIMA" [
  lima # Linux virtual machines (on macOS, in most cases)
]
++ ifEnv "MY_NIX_EXTRA_K8S" [
  podman # A program for managing pods, containers and container images
  # ```
  # # follow instruction on machine init & start - https://podman.io/getting-started/installation
  # # include these at ~/.local.zshrc
  # export DOCKER_HOST="unix:///${XDG_DATA_HOME}/containers/podman/machine/podman-machine-default/podman.sock" # to `docker` command to work with podman
  # alias docker=podman # or just alias it
  # ```
  docker-client # to shim the absence of docker command for podman in case podman is to replace to docker and act like one
  kubectl # Kubernetes CLI
  kubectx # Fast way to switch between clusters and namespaces in kubectl!
  k9s # Kubernetes CLI To Manage Your Clusters In Style
  kubernetes-helm # A package manager for kubernetes
  kustomize # Customization of kubernetes YAML configurations
  kompose # A tool to help users who are familiar with docker-compose move to Kubernetes
  minikube # A tool that makes it easy to run Kubernetes locally
  kind # Kubernetes IN Docker - local clusters for testing Kubernetes
  colima # Container runtimes with minimal setup powered by lima
  # minikube with podman - https://github.com/containers/podman/issues/12713#issuecomment-1002567777
  # an example podman machine works for minikube - `podman machine init --rootful --cpus 4 --memory 4096 --disk-size 30`
]
++ ifEnv "MY_NIX_EXTRA_SSH" [
  openssh # An implementation of the SSH protocol
  sshocker # Tool for SSH, reverse `sshfs` and port forwarder - see ../linux.nix for `sshfs`
  # - docker volume mounting like experience (as well as port forwarding)
  # - requires `sshfs` to be installed on the remote host
  # - this is better experience for temporary mounting as unmount happens automatically on disconnection
  #   - especially if the remote host has all the dev environments are configured and source code are stored, you can avoid slow disk reads unlike normal `sshfs`
  #     - but typing latency can be a concern, so it's all trade off
  # - example:
  #   - `sshocker [-p 7070:8080] -v .:/tmp/mnt/sshfs remote-host`
  #   - # you should be connected to remote host as if you run it with `ssh`
  #   - `ls /tmp/mnt/sshfs` should list files of client's pwd as it's mounted with reverse       `sshfs`
  #   - now at client side, `curl localhost:7070` should forward traffic to remote host's 8080 port (if you specified)
  #   - `exit` from connection should unmount and stop forwarding just like docker container experience!
  xorg.xorgserver
  xorg.xauth
  # debug xauth - https://www.linuxquestions.org/questions/linux-newbie-8/warning-no-xauth-data-although-i%27m-using-%60ssh-y%60-4175525755/#post5272443
  # `xauth list`
  # `mcookie | xargs xauth add :0 .`
]
++ ifEnv "WSL_DISTRO_NAME" wsl
# add any package to try out (locally more permanent way than `nix-shell -p [package]`
++ lib.optionals (builtins.pathExists ./local-only.nix) (import ./local-only.nix { pkgs = pkgs; })
# # this is actually not working great at least on ubuntu
# # it's probably wise to follow https://tailscale.com/kb/1031/install-linux/ instead
# ++ ifEnv "MY_NIX_EXTRA_TAILSCALE"
# [
#   tailscale
# ]
