{
  rocmOverrideGfx ? "10.3.5",
}:
{ pkgs, ... }:

# although the file name implies it could be generic to any AMD GPU
# but it's actually quite specific to a one device at the moment (6800U/680m)

# originally to be used for ../../containers/meta/ollama-gpu.nix
# ./caddy-over-tailscale.nix can be used to expose the service (open-webui) via tailscale
{
  environment.systemPackages = with pkgs; [
    rocmPackages.clr.icd
    # from here below is to debug
    nvtopPackages.amd
    clinfo # Print all known information about all available OpenCL platforms and devices in the system
    rocmPackages.rocminfo
    rocmPackages.rocm-smi
    memtest_vulkan # Vulkan compute tool for testing video memory stability
  ];

  # https://ollama.com/ - which is responsible for managing and running models
  services.ollama = {
    enable = true;
    rocmOverrideGfx = rocmOverrideGfx;
    # WARN: Why is it not using VRAM? Answered at https://github.com/ollama/ollama/issues/5471#issuecomment-2633000039
    environmentVariables = {
      # thanks to https://github.com/ollama/ollama/pull/6282#issuecomment-2357934042
      HIP_VISIBLE_DEVICES = "1"; # NOTE: very important for the performance; basically force using the iGPU side of the APU and ignore "0" which is CPU
      # test the difference between having this env var and not by running the command below
      # `echo 'why is the sky blue?' | ollama run tinyllama --verbose`
      # `amdgpu_top` to monitor GPU during the run
      # NOTE: somehow GTT is currently only used when CPU is also used (not when only iGPU is used). That maybe being fixed by https://github.com/ollama/ollama/pull/6282
    };

    # host = "0.0.0.0"; # use "SSH LocalForward" instead to delegate the authentication to SSH
    # # WARN: although above conveniently exposes ollama to the network it is not great for security
  };

  # https://openwebui.com/ - Web UI to access ollama models
  services.open-webui = {
    enable = true;
  };

  # for open-webui
  nixpkgs.config.allowUnfree = true;
}
