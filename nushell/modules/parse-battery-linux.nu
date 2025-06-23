# Function to parse battery status output and extract structured data
# Takes a record input (like one of the $input from ./fixtures/linux.nu) and returns:
# { source: 'battery'|'AC', capacity: decimal, charging: bool }
export def parse-battery-status [input: record<adp: string, bat: string> ] {
  # Parse the adapter status to determine power source
  let adp_lines = ($input.adp | lines)
  let adp_online = ($adp_lines | where {|line| $line | str starts-with "POWER_SUPPLY_ONLINE="} | first | str replace "POWER_SUPPLY_ONLINE=" "")

  let source = if ($adp_online == "1") {
    'AC'
  } else {
    'battery'
  }

  # Parse the battery status
  let bat_lines = ($input.bat | lines)

  # Extract capacity
  let capacity_line = ($bat_lines | where {|line| $line | str starts-with "POWER_SUPPLY_CAPACITY="} | first)
  let capacity_raw = ($capacity_line | str replace "POWER_SUPPLY_CAPACITY=" "")
  let capacity = ($capacity_raw | into int) / 100.0

  # Extract charging status
  let status_line = ($bat_lines | where {|line| $line | str starts-with "POWER_SUPPLY_STATUS="} | first)
  let status = ($status_line | str replace "POWER_SUPPLY_STATUS=" "")

  let charging = if ($status == "Charging") {
    true
  } else {
    false
  }

  { source: $source, capacity: $capacity, charging: $charging }
}

# Example usage:
# test-parse-battery-status          # Run all tests
# test-single 0                      # Test first case
# parse-battery-status (mock-bat-info)  # Parse actual battery status



export def battery-status [] {
  bat-info | parse-battery-status $in
}


use std assert
use fixtures/linux.nu *

def mock-bat-info [] {
  let test_cases = (get-test-cases)
  $test_cases | first | get input
}

def bat-info [] {
  # TODO: use real one from /sys/class/power_supply/BAT?/uevent and /sys/class/power_supply/ADP?/uevent
  mock-bat-info
}

# Test the function with all test cases
export def test-parse-battery-status [] {
  print "Testing parse-battery-status function:"
  print ""

  let test_cases = (get-test-cases)
  for test_case in $test_cases {
    let result = parse-battery-status $test_case.input
    try {
      assert equal $result $test_case.expected
      print $"✓ ($test_case.name) - PASSED"
    } catch {
      print $"✗ ($test_case.name) - FAILED"
      print $"  Expected: ($test_case.expected)"
      print $"  Got:      ($result)"
    }
  }

  print ""
  print "🎉 All tests completed!"
}

# Run a single test case by index (0-based)
export def test-single [index: int] {
  let test_cases = (get-test-cases)
  let test_case = ($test_cases | get $index)
  let result = parse-battery-status $test_case.input

  print $"Testing: ($test_case.name)"
  print $"Input: ($test_case.input)"
  print $"Expected: ($test_case.expected)"
  print $"Result: ($result)"

  try {
    assert equal $result $test_case.expected
    print "✓ PASSED"
  } catch {
    print "✗ FAILED"
  }
}
