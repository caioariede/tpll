.. Tpll documentation master file, created by
   sphinx-quickstart on Sun Nov 29 12:46:11 2015.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Tpll's documentation!
================================

**Tpll** is a Template Engine for Haskell that's highly inspired in the
`Django Template Language <https://docs.djangoproject.com/en/1.8/topics/templates/#the-django-template-language>`_.

This is what it looks like:

.. code-block:: html

    <h1>Hello World!</h1>

    <ul>
        {% for item in list %}
        <li>{{ item|upper }}
        {% endfor %}
    </ul>

With the following context:

.. code-block:: haskell

    import Tpll.Context (ctx, ContextValue(CList, CStr, CInt))
    import Tpll.Tags.Default (getAllDefaultTags)

    let ctx' = ctx [("list", CList [CStr "foo", CStr "bar", CInt 42])]

    renderFile "test.html" ctx' getAllDefaultTags


Will output:

.. code-block:: html

    <h1>Hello world!</h1>

    <ul>
        <li>FOO
        <li>BAR
        <li>42
    </ul>


**Table of Contents:**

.. toctree::
   :maxdepth: 2

   intro
   extending



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

