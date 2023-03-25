import unittest

import ore


suite "Test Ore Context":
  test "Test simple":
    var e = initOreContext()
    check e.renderString("2 + 2 = {{ 2 + 2 }} Did you know?") == "2 + 2 = 4 Did you know?"

  test "Test multi-char operators":
    var e = initOreContext()
    check e.renderString("{{ 2 <= 3 }}") == "true"
    check e.renderString("{{ true and false }}") == "false"

  test "Test set variable":
    var e = initOreContext()
    check e.renderString("{% set a = 10 %}{% set b = 32 %}{{ a + b }}") == "42"
  
  test "Test not operator":
    var e = initOreContext()
    check e.renderString("{{ not true }}") == "false"

  test "Test lists":
    var e = initOreContext()
    check e.renderString("{{4 in [1, 2,] & [3, 4]}}") == "true"

  test "Test blocks":
    const input = "{% block TestBlock %}Test{% endblock %}"
    var e = initOreContext()
    check e.renderString(input) == "Test"

  test "Test conditinal blocks":
    const input = "Now we can tell what is {% if true %}True{% else %}False{% endif %} and what is {% if false %}True{% elif false %}Impossible{% else %}False{% endif %}"
    var e = initOreContext()
    check e.renderString(input) == "Now we can tell what is True and what is False"

  test "Test space control":
    const input = "this {>}\n  is one \n {<} line"
    var e = initOreContext()
    check e.renderString(input) == "this is one line"

  test "Test file":
    var e = initOreContext()
    check e.renderFile("tests/templates/simple.ore") == "Hello, world!"

  test "Test extends":
    var e = initOreContext()
    check e.renderFile("tests/templates/extends.ore") == "Hello, world!\n2 + 2 = 4"

  test "Test block override":
    var e = initOreContext()
    check e.renderFile("tests/templates/blockOverride.ore") == "Hello, Ore!\n"

  test "Test nested extends":
    var e = initOreContext()
    check e.renderFile("tests/templates/nest3.ore") == "Nest 1 : \"3\" Nest 2 : \"\" Nest 3 : \"\""
  
  test "Test while loop":
    var e = initOreContext()
    check e.renderFile("tests/templates/while.ore") == "10 9 8 7 6 5 4 3 2 1 "
  
  test "Test for loop":
    var e = initOreContext()
    check e.renderString("{% for i in [1,2,3,4] %}{{ i }} {% endfor %}") == "1 2 3 4 "

  test "Test for loop over range":
    var e = initOreContext()
    check e.renderString("{% for i in 0..10 %}{{ i }}{% endfor %}") == "012345678910"
