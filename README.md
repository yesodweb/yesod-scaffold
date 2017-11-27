## Getting Started

Refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for install instructions.

## Database Setup

After installing Postgres, run:

```
createuser PROJECTNAME_LOWER --password PROJECTNAME --superuser
createdb PROJECTNAME_LOWER
createdb PROJECTNAME_LOWER_test
```

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically be recompiled and redeployed to localhost.

## Tests

```
stack test --flag PROJECTNAME_LOWER:library-only --flag PROJECTNAME_LOWER:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your tests).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts-9.14/hoogle?q=)
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell) 
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.