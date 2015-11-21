# tpll
HTML Template Engine for Haskell inspired in Django Template Engine

**Warning:** This project is only for learning purposes. Not intended to be a long-term project, neither maintained. But well, feel free to use or fork it for your needs. :)

### Usage

```haskell
import Tpll.Context (ctx)
import Tpll.Tags.Default (getAllDefaultTags)
import Tpll.File (renderFile)


let ctx' = ctx [("a", ""), ("b", "42"), ("c", "")]
renderFile "index.html" ctx' getAllDefaultTags
```

**index.html**

```html
{% firstof a b c %}
```

Result: `42\n`
