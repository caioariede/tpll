Introduction
************

**Tpll** is a Template Engine inspired by the `Django Template Language <https://docs.djangoproject.com/en/1.8/topics/templates/#the-django-template-language>`_.


Variables
=========

Variables are surrounded by ``{{`` and ``}}`` like this::

    My first name is {{ first_name }}. My last name is {{ last_name }}.

With the context of::

    [("first_name", CStr "John"), ("last_name", CStr "Doe")]

This template renders to::

    My first name is John. My last name is Doe.


Filters
=======

Filters can be used in values or variables to transform its value.

For example::

    {{ "foo"|upper }}

Will render to::

    FOO

The same way for variables. With the context of::

    [("x", "BAR")]

And the template::

    foo {{ x|lower }}

Will render to::

    foo bar


Tags
====

Tags are surrounded by ``{%`` and ``%}`` like this::

    {% now %}


And can receive multiple arguments::

    {% tag arg1 arg2 arg3 %}

This is the most common format to pass arguments to a tag, but some tags use other formats for usability. As an example, the ``for`` tag::

    {% for arg1 in arg2 %}

Block tags
----------

Tags can also consume blocks containing texts, variables or even other tags,
like the ``for`` tag::

    <ul>
    {% for arg1 in arg2 %}
        <li>Value: {{ arg 1 }}
    {% endfor %}
    </ul>

In this example, the ``for`` tag will iterate over all values in ``arg2``,
rendering everything until the ``endfor`` tag for each iteration.

With the context of::

    [("arg2", CList [(CInt 1, CInt 2, CInt 5)])]

This template renders to::

    <ul>
        <li>Value: 1
        <li>Value: 2
        <li>Value: 5
    </ul>

Comments
========

You comment out a block using the ``{% comment %} ... {% endcomment %}`` tag like this::

    this

    {% comment %} will not be rendered {% endcomment %}

    :)

Will render to::

    this



    :)
