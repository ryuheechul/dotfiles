{
  rocmOverrideGfx ? "10.3.5",
}:
{ pkgs, ... }:

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
    acceleration = "rocm";
    rocmOverrideGfx = rocmOverrideGfx;
    # WARN: Why is it not using VRAM? Answered at https://github.com/ollama/ollama/issues/5471#issuecomment-2633000039
  };

  # https://openwebui.com/ - Web UI to access ollama models
  services.open-webui = {
    enable = true;
  };
}
