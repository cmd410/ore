import unittest

import ore

suite "Test engine":
  test "Test simple":
    const input = "-(-1 - 2 * 3 + 4) * 3 / 4 + 5 = {{ -(-1 - 2 * 3 + 4) * 3 / 4 + 5 }}"
    var e = initOreEngine()
    check e.renderString(input) == "-(-1 - 2 * 3 + 4) * 3 / 4 + 5 = 7.25"
  
  test "Test set variable":
    const input = "{% set a = 10 %}{% set b = 32 %}{{ a + b }}"
    var e = initOreEngine()
    check e.renderString(input) == "42"
