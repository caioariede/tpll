Extending
*********

You can extend Tpll's functionality by adding custom tags and filters.

Creating a custom tag
=====================

Let's create a template tag that outputs a friendly hello message:

.. code-block:: haskell

    import Tpll.Context (Context)
    import Tpll.Tokenizer (Token)
    import Tpll.Tags (TagAction(Render))
    
    helloTag :: Context -> Token -> [Token] -> TagAction
    helloTag ctx' token remainingTokens =
        Render (remainingTokens, return "Hello! :)")

The parameters passed to the template tags are:

1. The current context
2. The token matched by this tag
3. The next (or remaining) tokens

The return value of a template tag is a ``TagAction``, which can be either
``Render`` or ``RenderBlock`` and we describe them below.


Normal tag
----------

The ``helloTag`` example used above is what we call here a **normal tag** and
uses the ``Render`` action:

.. code-block:: haskell

    Render ([Token], IO String)

The arguments passed to this action are the list of tokens not consumed by the
tag and the ``IO String`` to be printed out.


Block tag
---------

The other type of template tag is the **block tag**, that returns a
``RenderBlock`` action. Its most common use is to create a temporary context
that will be used when consuming the block. A good example is the
``{% for ... %}`` tag:

.. code-block:: html

    <ul>
    {% for arg1 in arg2 %}
        <li>Value: {{ arg 1 }}
    {% endfor %}
    </ul>

It switches the context in every iteration of the loop. For this, we use the
``RenderBlock`` action, that's used this way:

.. code-block:: haskell

    RenderBlock ([Context], Context, [Token], String)

Its arguments are:

1. The list of contexts which the block will be iterated over.
2. The context that will be used after the end of the block.
3. The list of the remaining tokens.
4. The string containing a regex pattern that identifies the token where the block must stop.

This is how the ``RenderBlock`` action looks 
This is an example of a ``RenderBlock`` action returned by the ``{% for ... %}`` tag:

.. code-block:: haskell

    RenderBlock (

        -- context stack
        [
            ctx [("foo", CStr "bar"), ("item", CInt 1)],
            ctx [("foo", CStr "bar"), ("item", CInt 2)],
            ctx [("foo", CStr "bar"), ("item", CInt 3)],
        ],

        -- old context
        ctx [("foo", CStr "bar")],

        -- remaining tokens:
        -- {{ foo }}: {{ item }}; {% endfor %}
        [
            Variable { content = "foo", line = 2, raw = "{{ foo }}"},
            Text { content = ": ", line = 2, raw = "" },
            Variable { content = "item", line = 2, raw = "{{ item }}"},
            Text { content = "; ", line = 2, raw = "" },
            Tag { content = "endfor", line = 2, raw = "{% endfor %}"}
        ],

        -- block delimiter
        "endfor|empty"

    )

And the output for this ``RenderBlock`` action:

.. code-block:: html

    bar: 1; bar: 2; bar: 3;
