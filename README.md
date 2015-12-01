# tpll
HTML Template Engine for Haskell inspired in Django Template Engine

**Warning:** This project is only for learning purposes. Not intended to be a long-term project, neither maintained. But well, feel free to use or fork it for your needs. :)

### Documentation

The documentation is available online at http://caioariede.github.io/tpll/

### Usage

```haskell
import Tpll.Context (ctx, ContextValue(CStr, CInt, CList))
import Tpll.Tags.Default (getAllDefaultTags)
import Tpll.File (renderFile)


let ctx' = ctx [
  ("a", CStr ""),
  ("b", CInt 42),
  ("list", CList [CStr "foo", CStr "bar"])
]

renderFile "index.html" ctx' getAllDefaultTags
```

**index.html**

```html
<b>{% firstof a b c %}</b>
<ul>
{% for x in list %}
  <li>{{ x|upper }}
{% endfor %}
</ul>
```

**Output:**

```html
<b>42</b>
<ul>
  <li>FOO
  <li>BAR
</ul>
```