<h1 style="text-align: center">ORE</h1>

<blockquote style="margin:1rem;background-color: #efb306; color: #272727; border-radius: 0.5rem">
  NOTE: This software is alpha stage. Expect bugs and breaking API and/or syntax changes.
</blockquote>

**Ore** is a runtime capable string templating engine
for nim language.

There are quite a few templating engines out there,
but as far as I've seen none of them is capable to
work at runtime. That's why ore was created.

Why would you need to do runtime string templating?
But of course to template user-provided strings. It's
primary usecase is static site generators, but of course
it can be used for whatever.

<h2 style="text-align: center">Syntax</h2>

Ore strives to have syntax similar to other templating enignes,
such as jinja, to be easy for new users to adopt it
(and to use preexistsing syntax highlighting).

> NOTE: Not all demostrated features are currently implemented,
> what is not ready is marked as WIP

Example:

```jinja
# insert expression
2 + 2 = {{ 2 + 2 * a }}

# define variables
{% set variable = 42 %}

# Template inheritance
{% extend "path/to/other.ore" %}  

# named blocks
{% block name %}   
{% endblock %}

# conditional blocks
{% if condition %}
{% elif other_condition %}
{% else %}
{% endif %}

# for loop blocks (WIP)
{% for i in iterator %}
{% endfor %}

# while loop blocks
{% while condition %}
{% endwhile %}

# Template composition (WIP)
{% insert "path/to/other.ore" arg1=1 arg2="hello" %}

# Space control
this {>}
is one
{<} line
```

<h2 style="text-align: center">Usage</h2>

Here goes a basic usage example:

```nim
import ore

# create OreContext
var ctx = initOreContext()

# Set some variables
ctx.defines:
  a = 42

echo ctx.renderString("Quick math! 2 + 2 = {{ 2 + 2 }}")
echo ctx.renderString("a = {{ a }}")
echo ctx.renderFile("path/to/myTemplate.ore")
```
