# Experimental Scaffold

This is just a branch where I'm playing around with changes that
I repeated make to the scaffold every time I start a new 
project. These changes are as follows:

* Remove support for yesod devel and all widget reloading features.
* Remove special flags from the `.cabal` file.
* Remove all compile-time configuration settings from the application. This
  means that `settings.yml` will not be read at compile time, as it 
  currently is in the scaffold. I find this behavior to be confusing,
  so I took it out.
* Remove tests
* Remove the keter configuration
* Remove classy-prelude and replace its use with a user-defined custom 
  prelude (`Import.Base`).
* Improve out-of-the-box bootstrap support. This means including 
  `bootstrap.js` and jquery with the scaffold.
* Add a `Misc` module for random general-purpose function to go in.
* Add a `Layout` module, which provides an alternative to using
  `defaultLayout` everywhere. The intention is that `defaultLayout`
  should be what gets called when errors occur, but `layout` (or
  `adminLayout` or whatever you want to put in there) should be
  used in all your handlers
* Add a `Task` type synonym and a `Task` module, which provide
  facilities for running things outside of a `Handler` that 
  still need access to things in `App` (like logging and database
  stuff).

I'm not sure that this can or should ever work with with `yesod init`,
so I've provided two script to help set this up. Let's say that you 
want to name your project `appalot`. These would be the steps to get
started:

    wget https://github.com/yesodweb/yesod-scaffold/archive/postgres-no-compile-time-settings-no-devel.zip
    unzip postgres-no-compile-time-settings-no-devel.zip
    rm postgres-no-compile-time-settings-no-devel.zip
    mv postgres-no-compile-time-settings-no-devel appalot
    cd appalot
    ./scripts/name_project appalot
    sudo ./scripts/build_postgres_db appalot

This will create you postgres database, schema, and user as well.
