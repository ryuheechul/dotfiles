use linux/files.nu *

# Function to get test cases
export def get-test-cases [] {
  let files = (get-files)
  [
    {
      name: "Battery Power - Discharging"
      input: {
        adp: $files.adp.off
        bat: $files.bat.discharging
      }
      expected: { source: 'battery', capacity: 0.52, charging: false }
    }
    {
      name: "Battery Power - Discharging"
      input: {
        adp: $files.adp.off
        bat: $files.bat.unknown
      }
      expected: { source: 'battery', capacity: 0.62, charging: false }
    }
    {
      name: "AC Power - Not Charging"
      input: {
        adp: $files.adp.on
        bat: $files.bat.not_charging
      }
      expected: { source: 'AC', capacity: 0.57, charging: false }
    }
    {
      name: "AC Power - Not Charging"
      input: {
        adp: $files.adp.on
        bat: $files.bat.unknown
      }
      expected: { source: 'AC', capacity: 0.62, charging: false }
    }
    {
      name: "AC Power - Charging"
      input: {
        adp: $files.adp.on
        bat: $files.bat.charging
      }
      expected: { source: 'AC', capacity: 0.83, charging: true }
    }
  ]
}
