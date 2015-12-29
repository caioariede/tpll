Defaults
********

**Tpll** offers a default collection of template tags and filters.


Default Tags
============

comment
-------

Syntax:

``{% comment %} ... {% endcomment %}``

Ignores everything between ``{% comment %}`` and ``{% endcomment %}``:

.. code-block:: html

    abra{% comment %} X Y {% endcomment %}cadabra

Outputs:

.. code-block:: html

    abracadabra

firstof
-------

Syntax:

``{% firstof arg1 arg2 arg3 ... %}``

Returns the first argument that is not empty:

.. code-block:: html

    abra{% comment %} X Y {% endcomment %}cadabra

Outputs:

.. code-block:: html

    abracadabra

now
---

Syntax:

``{% now %}``

``{% now format %}``

Returns the current date and time. Accepts one argument with the desired
format. The format must use the same syntax and codes defined by the formatTime
function:

.. code-block:: html

    Date/Time: {% now %}

Outputs:

.. code-block:: html

    Date/Time: 2015-11-26 23:11:25

Specifying the format:

.. code-block:: html

    Date/Time: {% now "%Y-%m-%d" %}

Outputs:

.. code-block:: html

    Date/Time: 2015-11-26


for
---

Syntax:

``{% for x in list %} ... {% endfor %}``

``{% for x in list %} ... {% empty %} fallback {% endfor %}``

Iterates over all items in the list. If the list is empty, render the empty
block if present:

.. code-block:: html

    {% for x in list %}{{ x }},{% endfor %}

Outputs:

.. code-block:: html

    1,2,3,5,

Catching empty lists with ``{% empty %}``:

.. code-block:: html

    {% for x in unknown %}{{ x }},{% empty %}foo{% endfor %}

Outputs:

.. code-block:: html

    foo


Default Filters
===============

upper
-----

Syntax:

``{{ arg|upper }}``

Transforms arg to uppercase:

.. code-block:: html

    {{ "bar"|upper }}

Outputs:

.. code-block:: html

    BAR

lower
-----

Syntax:

``{{ arg|lower }}``

Transforms arg to lowercase:

.. code-block:: html

    {{ "FoOoO"|lower }}

Outputs:

.. code-block:: html

    foooo

first
-----

Syntax:

``{{ arg|first }}``

Returns the first item in arg:

.. code-block:: html

    {{ "foo"|first }}

Outputs:

.. code-block:: html

    f

last
----

Syntax:

``{{ arg|last }}``

Returns the last item in arg:

.. code-block:: html

    {{ "bar"|last }}

Outputs:

.. code-block:: html

    r

ljust
-----

Syntax:

``{{ arg|ljust }}``

Left-aligns the value in a field of a given width:

.. code-block:: html

    {{ "foo"|ljust:10 }}

Outputs:

.. code-block:: html

    foo.......

Note: dots were used to represent whitespaces.

rjust
-----

Syntax:

``{{ arg|rjust }}``

Right-aligns the value in a field of a given width:

.. code-block:: html

    {{ "foo"|rjust:10 }}

Outputs:

.. code-block:: html

    .......foo

Note: dots were used to represent whitespaces.
