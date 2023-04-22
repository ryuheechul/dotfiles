{ pkgs }:
with pkgs;
[
  wslu # A collection of utilities for Windows 10/11 Linux Subsystems
  # which comes with wslview to enable opening a browser on Windows from terminal
  ## below are for virtual machine capabilities via:
  ## - virt-manager: to be a "front-end"
  ## - qemu: to provide kvm capabilities for hardware acceleration
  ## - libvirt: to be a "back-end"
  virt-manager # Desktop user interface for managing virtual machines
  # qemu # A generic and open source machine emulator and virtualizer
  # - installing qemu via nix actually not gets used by libvirt as default path is fixed for the usual path
  # - so `apt-get install qemu-system-x86` (in case of debian/ubuntu) would do the trick
  # libvirt # A toolkit to interact with the virtualization capabilities of recent versions of Linux and other OSes
  # - but wait libvirt services will not work with non NixOS distros so in that case do something like below instead
  # - `apt-get install libvirt-daemon-system` (in case of debian/ubuntu)
  # - this also possibly requires logout and login (like terminating tmux server) for `libvirt` group to be effective
  #
  # My attempts on passing through GPU to qemu guest from WSL host failed with many attempts
  # and the links below are the ones that at least helped me to learn more about related topics
  # as I was quite a layman to almost all concepts including:
  # kvm, vfio, virtio, vt-x, vt-d, gvt-g, mesa, iommu, libvirt, virsh, virt-manager, ovmf etc.
  #
  # here are the links:
  # - https://ubuntuforums.org/showthread.php?t=2326815
  # - https://askubuntu.com/questions/1348975/how-to-use-opengl-3d-acceleration-in-virt-manager-with-ubuntu
  # - https://devblogs.microsoft.com/commandline/d3d12-gpu-video-acceleration-in-the-windows-subsystem-for-linux-now-available/
  # - https://github.com/microsoft/wslg/issues/986
  # - https://www.intel.com/content/www/us/en/download/726609/intel-arc-iris-xe-graphics-whql-windows.html
  # - https://www.reddit.com/r/Surface/comments/gcv2f7/comment/fpe7px0/?utm_source=share&utm_medium=web2x&context=3
  # - https://www.intel.com/content/www/us/en/docs/oneapi/installation-guide-linux/2023-0/configure-wsl-2-for-gpu-workflows.html
  # - https://docs.fedoraproject.org/en-US/quick-docs/using-nested-virtualization-in-kvm/
  # - https://www.intel.com/content/www/us/en/artificial-intelligence/harness-the-power-of-intel-igpu-on-your-machine.html
  # - https://blog.wikichoon.com/2016/01/qemusystem-vs-qemusession.html
  # - https://wiki.libvirt.org/FAQ.html
  # - https://alexbakker.me/post/nixos-pci-passthrough-qemu-vfio.html
  # the below two actually gives a lot more clues on why it was probably impossible at that time (with my hardware configuration)
  # - https://nixos.wiki/wiki/IGVT-g
  # - https://arccompute.com/blog/libvfio-commodity-gpu-multiplexing/

  # is it possible that 3d acceleration didn't work because of my WDDM version was just 2.7 not 2.9 according to this article?
  # upgrading to insider channel might enable the version higher than 2.9 which should expose the gpu to linux distros. Let's see
  # - https://devblogs.microsoft.com/directx/directx-heart-linux/

  # Here are some commands that helps verifying/debugging
  # `ls /dev/kvm`
  # `dmesg | grep DMAR`
  # `cat /proc/cmdline`
  # `lspci -v`
  # `virt-host-validate` provided by libvirt
  # `vainfo -a`
  # `glxinfo -B` `apt install mesa-utils` to show GLX info
]
