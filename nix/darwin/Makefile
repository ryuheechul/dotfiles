arch_for_darwin := aarch64

# use of prepending `path:` for the flake allows to avoid git tree is dirty (and fail) issue
# with the file that is not checked into the git (e.g. ./configuration.nix)

.PHONY: switch
switch: render build
	sudo ./result/sw/bin/darwin-rebuild switch --flake path:.#$(arch_for_darwin)

.PHONY: render
render:
	./gen-configuration.sh

.PHONY: build
build:
	nix build path:.#darwinConfigurations.$(arch_for_darwin).system

.PHONY: update
update:
	nix flake update
