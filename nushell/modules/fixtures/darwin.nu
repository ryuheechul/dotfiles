# Test cases with input-output pairs for battery status parsing
export const test_cases = [
  {
    name: "Battery Power - Discharging (no estimate)"
    input: (r#'Now drawing from 'Battery Power'
 -InternalBattery-0 (id=21758051)     91%; discharging; (no estimate) present: true'#)
    expected: { source: 'battery', capacity: 0.91, charging: false }
  }
  {
    name: "Battery Power - Discharging (with time)"
    input: (r#'Now drawing from 'Battery Power'
 -InternalBattery-0 (id=21758051)	78%; discharging; 13:41 remaining present: true'#)
    expected: { source: 'battery', capacity: 0.78, charging: false }
  }
  {
    name: "AC Power - Not Charging"
    input: (r#'Now drawing from 'AC Power'
 -InternalBattery-0 (id=21758051)     92%; AC attached; not charging present: true'#)
    expected: { source: 'AC', capacity: 0.92, charging: false }
  }
  {
    name: "AC Power - Charging"
    input: (r#'Now drawing from 'AC Power'
 -InternalBattery-0 (id=21758051)     93%; charging; (no estimate) present: true'#)
    expected: { source: 'AC', capacity: 0.93, charging: true }
  }
  {
    name: "AC Power - Charging (with time)"
    input: (r#'Now drawing from 'AC Power'
 -InternalBattery-0 (id=21758051)	85%; charging; 2:30 remaining present: true'#)
    expected: { source: 'AC', capacity: 0.85, charging: true }
  }
  {
    name: "AC Power - Fully Charged"
    input: (r#'Now drawing from 'AC Power'
 -InternalBattery-0 (id=21758051)	100%; charged; 0:00 remaining present: true'#)
    expected: { source: 'AC', capacity: 1.0, charging: false }
  }
]
