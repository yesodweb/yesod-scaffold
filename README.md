yesod-scaffold
==============

* master: [![Build Status](https://travis-ci.org/yesodweb/yesod-scaffold.svg?branch=master)](https://travis-ci.org/yesodweb/yesod-scaffold)
* postgres: [![Build Status](https://travis-ci.org/yesodweb/yesod-scaffold.svg?branch=postgres)](https://travis-ci.org/yesodweb/yesod-scaffold)
* mysql: [![Build Status](https://travis-ci.org/yesodweb/yesod-scaffold.svg?branch=mysql)](https://travis-ci.org/yesodweb/yesod-scaffold)
* sqlite: [![Build Status](https://travis-ci.org/yesodweb/yesod-scaffold.svg?branch=sqlite)](https://travis-ci.org/yesodweb/yesod-scaffold)
* simple: [![Build Status](https://travis-ci.org/yesodweb/yesod-scaffold.svg?branch=simple)](https://travis-ci.org/yesodweb/yesod-scaffold)
* mongo: [![Build Status](https://travis-ci.org/yesodweb/yesod-scaffold.svg?branch=mongo)](https://travis-ci.org/yesodweb/yesod-scaffold)
* minimal: [![Build Status](https://travis-ci.org/yesodweb/yesod-scaffold.svg?branch=minimal)](https://travis-ci.org/yesodweb/yesod-scaffold)
* postgres-fay: [![Build Status](https://travis-ci.org/yesodweb/yesod-scaffold.svg?branch=postgres-fay)](https://travis-ci.org/yesodweb/yesod-scaffold)

The Yesod scaffolding, with branches for different versions.

If you want to send a pull request affecting all the different
flavors, please send it as a pull request against the `postgres`
branch. The scripts we use for building the scaffoldings automatically
merge changes from `postgres` to all other branches.

If you have a patch which is backend-specific, please send a pull
request to the appropriate branch, with a comment that this change
applies only to the specific branch.

Build Process
-------------

The code contained by the scaffolding itself is all contained on
separate branches. The master branch contains code for performing
merges between these branches, testing that the branches build
correctly, and generating the .hsfiles used by the
[stack-template repository](https://github.com/commercialhaskell/stack-templates). (Note:
the full list of recognized branches is maintained in the
`app/Main.hs` file on the master branch, in the `branches` value.

The basic workflow for this repository is as follows:

1. Changes are made to one of the branches. If the change is intended
   to be applied to all variants, it should be made to the `postgres`
   branch (as mentioned above).

2. Changes on a branch can be tested with normal Stack commands,
   e.g. `stack test --haddock`.

### For maintainers

If you are a maintainer of this repo, you will additionally need to
merge changes from `postgres` to other branches and deal with updating
the `stack-templates` repo.

__Install the `yesod-scaffold` executable__ by running `stack install`
on the `master` branch.

__Merge changes from `postgres`__ to all other branches by running
`yesod-scaffold merge`. Note that there will often be merge conflicts,
which will need to be resolved and then the command rerun until all
branches merge successfully.

__Confirm that changes build correctly__ by running `yesod-scaffold
build`. Usually you'll want to add `--no-run-tests` if you don't have
databases set up locally. The Travis builds set up the databases for
proper testing, and you can rely on that. If you encounter a build
error on one of the branches, check it out, fix it up, and then rerun
`yesod-scaffold build`.

__Copy the template files__ in the `hsfiles` directory. This directory
is populated after a successful `yesod-scaffold build` run. Place the
`stack-templates` directory as a sibling to the `yesod-scaffold`
directory, and then run the `to-stack-templates.sh` script.
