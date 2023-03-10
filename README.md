<h1 style="text-align: center">ORE</h1>

<blockquote style="margin:1rem;background-color: #efb306; color: #272727; border-radius: 0.5rem">
  NOTE: This software is UNUSABLE at this stage of development.
  Come later.
</blockquote>

**Ore** is a runtime capable string temlating engine
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

# define variables (WIP)
{% set variable = 42 %}

# Template inheritance (WIP)
{% extend "path/to/other.ore" %}  

# named blocks (WIP)
{% block name %}   
{% endblock %}

# conditional blocks (WIP) 
{% if condition %}  
{% endif %}

# for loop blocks (WIP)
{% for i in iterator %}
{% endfor %}

# while loop blocks (WIP)
{% while condition %}
{% endwhile %}

# Template composition (WIP)
{% insert "path/to/other.ore" arg1=1 arg2="hello" %}
```

<h2 style="text-align: center">Usage</h2>

WIP: currently there is no way to use it, still very early in development. Come back later.
