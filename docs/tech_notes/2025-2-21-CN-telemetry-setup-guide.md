# CN Telemetry: Setup Guide
*SamC, MikeD, ColeS - 2025-02-21*

Telemetry collection for CN works through the VERSE VS Code plugin. Here’s how
to get everything installed and set up. I’m assuming you’re in a working
directory `TELEMETRY` (for example, `/Users/miked/VERSE-Telemetry`), and that you
have already installed VS Code (available [here](https://code.visualstudio.com)).

**Note:** CN’s telemetry will only record invocations of CN made through the VS
Code plugin (not through shell commands, for example). 

## *Step 1:* Clone the `VERSE-Toolchain` repo

```sh
$ git clone https://github.com/GaloisInc/VERSE-Toolchain.git
```

## *Step 2:* Install the CN LSP client 

The CN LSP server client provides syntax highlighting for CN and interacts with
the CN tool and telemetry collection infrastructure. To install it, either from
a pre-built release or from source, follow the instructions
[here](https://github.com/GaloisInc/VERSE-Toolchain/blob/main/cn-client/README.md#installing). 

Installing the client from a release is quicker and easier, but experimental. If
you install the client this way, you do not need to install the server - the
released client includes a pre-built server.

If you build and install the client from source, here's what I'd expect to see:

```sh
$ cd $TELEMETRY/VERSE-Toolchain/cn-client   # go to cn-client directory 
$ npm install
$ npm run compile
$ npm run dist
$ code --install-extension cn-client-0.0.5.vsix   # The version number may change 
```

You should now see a CN plugin in the VS code extensions menu (on the left-hand
side of the editor, or View-\>Extensions in the menu). 

## *Step 3:* Install the CN LSP server and telemetry collection 

If you installed the client from a release, skip to step 4.

If you built the client from source, you'll need to build and install the server
from source as well. To install the CN LSP server and telemetry collection tool,
follow the instructions
[here](https://github.com/GaloisInc/VERSE-Toolchain/blob/main/README.md#installation-and-use).

If you build and install the server from source, here's what I'd expect to see
(assuming you've already installed z3, OCaml, and opam): 

```sh
$ cd $TELEMETRY/VERSE-Toolchain   # go to root of VERSE-Toolchain repository
$ opam update
$ opam switch create . ocaml.5.1.1 --locked -y
$ eval $(opam env)
```

## *Step 4:* Create the telemetry data directory 

Telemetry data is stored in a single fixed directory, regardless which file is
being analyzed with CN. 

```sh
$ mkdir $TELEMETRY/CN-telemetry-data 
```

## *Step 5:* Set up the VS Code client 

We need to tell the telemetry where to store data. Open the CN plugin by
clicking on the Extensions menu, or through View-\>Extensions. Then open the
‘Settings’ menu (click on the cog, then ‘Settings’). Now update the settings as
follows (make sure to substitute your `TELEMETRY` root directory): 

* CN: Telemetry Dir - `<TELEMETRY>/CN-telemetry-data`  
* CN: User ID - *your email address* 

We also need to tell VS Code where to find the CN runtime files. You should be
prompted to pick the opam switch which you installed in Step 3 - see
[here](https://github.com/GaloisInc/VERSE-Toolchain/tree/main/cn-client#running-cn)
for details. In our case, you should pick the switch
`<TELEMETRY>/VERSE-Toolchain/`. If this doesn’t work for some reason, you can
also set the location of the CN files manually in the same way as the telemetry
data. Here are the settings you’ll need: 

* CN: Cerb Runtime - `<TELEMETRY>/VERSE-Toolchain/_opam/lib/cerberus/runtime`  
* CN: Server Path - `<TELEMETRY>/VERSE-Toolchain/_opam/bin/cn-lsp-server`

## *Step 6:* Test everything 

Let’s try this all out on an example. Here’s a piece of C with CN annotations
(from the tutorial, [here](https://rems-project.github.io/cn-tutorial/getting-started/tutorials/basic-usage/#first-function-specification)). 

```c
int add(int x, int y)
/*@ requires let Sum = (i64) x + (i64) y;
             -2147483648i64 <= Sum; Sum <= 2147483647i64; @*/
{
  return x+y;
}
```

Copy this code into a file. VS Code should add syntax highlighting to the CN
annotations - the part inside ‘magic comments’, `/*@ ... @*/`. You should also see
a button above the `add()` function that says ‘Verify add with CN’. When you click
on it, CN runs and verifies that the contract holds. Nice\! This also has the
effect of generating telemetry data. 

**Note:** The ‘Verify _ with CN’ button will only appear *after* you have 
saved the file to disk. 

Now look in the directory `<TELEMETRY>/CN-telemetry-data`. You should see a
file, `profile.json`, and one or more directories, `session-<some date>`. You
can look in the directory to check whether the telemetry data is being
generated. It should be stored in a subdirectory with a hash name, something
like `0x1cdbff2c`. 

## *Step 7:* Run through the tutorials using the IDE

Work through as many of the VERSE tutorial chapters as you can in one hour, or
as much time as you’re willing to devote to this task.

You can find the tutorials
[here](https://rems-project.github.io/cn-tutorial/getting-started/tutorials/). 

**Note:** The tutorial doesn’t refer to the IDE, but rather the command-line
tool. In order to collect telemetry, please always run CN by clicking ‘Verify _
with CN’ in the editor. If you don't see this button above a function, save the
file containing that function. This should cause the lens to (re)appear.

## *Step 8:* Upload your telemetry

Once you’re done, compress and upload your telemetry directory to the VERSE
Google drive [here](https://drive.google.com/drive/folders/1l_LPqeLDqbGCmja7bAB1yspH0oMDed-3).
Use `tar` to compress the folder.

```sh
$ tar -cvf $TELEMETRY/your-name.tar.gz $TELEMETRY/CN-telemetry-data
```

Then open the `$TELEMETRY` directory and upload `your-name.tar.gz` to the
[telemetry directory](https://drive.google.com/drive/folders/1l_LPqeLDqbGCmja7bAB1yspH0oMDed-3)
on Google Drive, replacing `your-name` with your name, of course.
