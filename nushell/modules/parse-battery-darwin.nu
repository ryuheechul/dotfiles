# Function to parse battery status output and extract structured data
# Takes a string input (like output from `pmset -g batt`) and returns:
# { source: 'battery'|'AC', capacity: decimal, charging: bool }
export def parse-battery-status [input: string] {
  let lines = ($input | lines)
  let first_line = ($lines | first)
  let second_line = ($lines | get 1)

  # Extract source from first line
  let source = if ($first_line | str contains "Battery Power") {
    'battery'
  } else {
    'AC'
  }

  # Extract capacity percentage with more robust regex
  let capacity_match = ($second_line | parse --regex '\s+(\d+)%')
  let capacity = if (($capacity_match | length) > 0) {
    ($capacity_match | get 0.capture0 | into int) / 100.0
  } else {
    0.0
  }

  # Determine charging status with more comprehensive checks
  let charging = if ($second_line | str contains "not charging") {
    false
  } else if ($second_line | str contains "discharging") {
    false
  } else if ($second_line | str contains "charged") {
    false
  } else if ($second_line | str contains "charging") {
    true
  } else {
    false
  }

  { source: $source, capacity: $capacity, charging: $charging }
}

# Example usage:
# test-parse-battery-status          # Run all tests
# test-single 0                      # Test first case
# parse-battery-status (pmset -g batt)  # Parse actual battery status

export def battery-status [] {
  pmset -g batt | parse-battery-status $in
}


use std assert
use fixtures/darwin.nu *

# Test the function with all test cases
export def test-parse-battery-status [] {
  print "Testing parse-battery-status function:"
  print ""

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
