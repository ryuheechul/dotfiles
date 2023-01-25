hostname_for_darwin := aarch64

.PHONY: switch
switch: build ran-prepare
	./result/sw/bin/darwin-rebuild switch --flake .#$(hostname_for_darwin)

.PHONY: build
build:
	nix build .#darwinConfigurations.$(hostname_for_darwin).system

# without this step, `darwin-rebuild switch` will complain and would not complete
ran-prepare:
	sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.orig
	printf 'run\tprivate/var/run\n' | sudo tee -a /etc/synthetic.conf
	/System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util -t
	touch ran-prepare
