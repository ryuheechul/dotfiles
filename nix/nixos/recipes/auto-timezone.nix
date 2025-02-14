{ lib, ... }:

# https://www.reddit.com/r/NixOS/comments/1411gjs/dynamically_set_the_timezone/
# https://unix.stackexchange.com/questions/778391/geoclue-failed-to-query-location-not-found
# https://beacondb.net/
{
  time.timeZone = lib.mkForce null;
  services.automatic-timezoned.enable = true;
  services.geoclue2.enable = true;
  services.geoclue2.enableDemoAgent = lib.mkForce true;
  services.geoclue2.geoProviderUrl = "https://api.beacondb.net/v1/geolocate";
}
