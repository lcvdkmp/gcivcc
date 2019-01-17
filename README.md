A very useless implementation of a civilized c compiler using the GNU trademark unjustly.

To build the project the cabal package manager it usually comes with the Haskell platform installer
or got to your local distributions package manager (pacman, portage, yum, ...) and install it.

Then to build it simply type the following into the command line:
```
cabal run
```

If this is the first time building the project cabal will probably tell you that you need to first configure the package.
To do this run the following command:
```
cabal configure
```
and then retry.

Now assuming all went well you will now have downloaded all the stuff needed to build the compiler and have also build it.

The cabal build have deposited an executable in the directory dist/build/gcivcc

This is now the build compiler which can be invoked and used to create marvelous programs...
