Extending
*********


Creating custom template tags
=============================

This is how you create the custom template tag ``{% hello %}``:

.. code-block:: haskell

    import Tpll.Context (Context)
    import Tpll.Tokenizer (Token)
    import Tpll.Tags (TagAction(Render))
    
    helloTag :: Context -> Token -> [Token] -> TagAction
    helloTag ctx' token remainingTokens =
        Render (remainingTokens, return "Hello! :)")

As you can see, the implementation is very straightforward, but there are some
details that deserves nitpicking.

First of all, the parameters received by a template tag are:

1. The current context (containing values passed to the template)
2. The matched token
3. The next (or remaining) tokens

Second, the function must return a ``TagAction``, which can be either
``Render`` or ``RenderBlock``, as described below.


Simple rendering
----------------

The ``helloTag`` example used above returns what we call here
*simple rendering* and it's used this way:

.. code-block:: haskell

    Render ([Token], IO String)

The ``Render`` action receives the list of tokens not consumed by the template
tag, and the ``IO String`` to be printed.


Block rendering
---------------

Some template tags needs to consume blocks or even repeat the content of these
blocks. As an example, the ``{% for ... %}`` template tag:

.. code-block:: html

    <ul>
    {% for arg1 in arg2 %}
        <li>Value: {{ arg 1 }}
    {% endfor %}
    </ul>

To simplify our life, we use the ``RenderBlock`` action, that's used this way:

.. code-block:: haskell

    RenderBlock ([Context], Context, [Token], String)

And here is the explanation for each argument:

1. The first is a list of contexts which the block will be iterated over.
2. The second is the context that will be used after the end of the block.
3. The third is the list of the remaining tokens.
4. The fourth is a string containing a regex pattern that identifies the token where the block must end.
