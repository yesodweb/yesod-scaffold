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
the full list of recognized branches is maintained in the `Shared.hs`
file on the master branch.

The basic workflow for this repository is as follows:

1. Changes are made to one of the branches. If the change is intended
   to be applied to all variants, it should be made to the `postgres`
   branch (as mentioned above).

2. Check out the `master` branch and run `stack build` to generate the
   helper executables.

3. Merge the changes from `postgres` to all other branches by running
   `stack exec yesod-scaffold-merge`. Note that there will often be
   merge conflicts, which will need to be resolved and then the
   command rerun until all branches merge successfully.

4. Run `stack exec yesod-scaffold-build` to compile and run tests in
   all branches. You will need some system libraries and to set up
   some databases for testing.  (NOTE: We should consider automating
   this process and/or using a Docker image for all of this.) After
   this completes successfully, the `hsfiles` directory will be
   populated.

5. The generated .hsfiles should be copied into the stack-templates
   repo, with `yesod-` prefixed to the names.
