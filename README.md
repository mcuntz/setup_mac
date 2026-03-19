Author: Matthias Cuntz, mc (at) macu (dot) de  
Modified: 2026-03-14

Inspired by guides of [Dirk
Avery](https://medium.com/faun/zero-to-hero-set-up-your-mac-for-software-development-919ede3df83b),
[Vinicius De
Antoni](https://medium.com/better-programming/setting-up-your-mac-for-web-development-in-2020-659f5588b883),
[Sourabh Bajaj](https://sourabhbajaj.com/mac-setup/), and [Nicolas
Hery](https://github.com/nicolashery/mac-dev-setup).

This is a reminder for myself how to setup a new Mac or updating the
OS, which might be useful for others. The repository includes some of
my dot-files as a reference.

# Table of Contents <span class="tag" tag-name="TOC"><span class="smallcaps">TOC</span></span>

- [How to update the OS](#how-to-update-the-os)
  - [Update via *Software Update*](#update-via-software-update)
- [Setup macOS](#setup-macos)
  - [Check for system updates](#check-for-system-updates)
  - [Xcode](#xcode)
  - [Xcode Command Line Tools (CLT)](#xcode-command-line-tools-clt)
  - [Set preferences of macOS and standard apps](#set-preferences-of-macos-and-standard-apps)
  - [Set default shell \#1](#set-default-shell-1)
  - [App Store](#app-store)
  - [XQuartz](#xquartz)
  - [Homebrew](#homebrew)
  - [Set default shell \#2](#set-default-shell-2)
  - [Homebrew \#2](#homebrew-2)
  - [Emacs](#emacs)
  - [LaTeX](#latex)
  - [Myriad Pro](#myriad-pro)
  - [Freeware](#freeware)
  - [Python](#python)
  - [locate](#locate)
  - [Additional software](#additional-software)
  - [install_fortran_libs](#install_fortran_libs)

# How to update the OS

There are basically two options:

1.  Reformat the startup disk and install a fresh new system, or

2.  Use *Software Update* to update macOS on top of the existing OS.

In both cases, a little bit of preparation helps. I do not use the
first option anymore. It is covered in earlier versions of this
document such as in README14.org. All macOS menu options are for macOS
\>= 13.

## Update via *Software Update*

### Before starting

1.  It is always recommended to make a backup of your personal files
    before an upgrade.

2.  Update all your installed apps in *Applications*. The newest
    versions of the applications will probably already be suitable for
    the new macOS version. It avoids the problem that you cannot open
    anymore the old version of the application on the new macOS just
    to do the update. You'd then have to uninstall/install the
    application again, which might need the licence key, etc.

    For this, open each non-Apple application and *Check for
    Updates…*.

    Use the *App Store…* for all Apple programs and apps downloaded
    from the App Store.

3.  The *Software Update* takes care of all your accounts, passwords,
    etc. The steps before take care of all the installed applications.
    But anything installed from the command line will (most probably)
    not work anymore. So I first uninstall all the things that will be
    installed in the steps below, which are Homebrew (including the
    installed casks), LaTeX, Python/pyenv, and everything installed
    with [installnetcdf](https://github.com/mcuntz/install_netcdf).

    Check the casks installed with Homebrew. This might not work
    anymore because you changed the shell, e.g. to *zsh*. Then copy
    the box below that sets the *HOMEBREW<sub>PREFIX</sub>* into this
    new login terminal ([Homebrew](#homebrew)).

    ``` bash
    brew list --casks
    ```

    Best to note them somewhere (basictex emacs font-source-code-pro
    panoply temurin copilot-cli emacs-app motrix quarto).

    Uninstall all casks (you might have to type your password several
    times for this if not even for each casks):

    ``` bash
    for c in $(brew list --casks) ; do brew uninstall ${c} ; done
    ```
    
    Before uninstalling Homebrew, I also first set back Apple's bash
    shell as default because the newer bash shell used from Homebrew
    will be removed together with the whole Homebrew and you will run
    into trouble if it is still set as your default shell afterwards.

    Set Apple's (old) bash shell as default:

    ``` bash
    chsh -s /bin/bash
    ```

    You have to open a new login terminal for it to take effect.

    Remove Homebrew (password needed):

    ``` bash
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh)"
    ```

    and related files:

    ``` bash
    [[ -f ${HOME}/.fzf.bash ]] && rm -f ${HOME}/.fzf.bash
    ```

    ``` bash
    [[ -f ${HOME}/.fzf.zsh ]] && rm -f ${HOME}/.fzf.zsh
    ```

    ``` bash
    [[ -d /opt/homebrew ]] && sudo rm -r /opt/homebrew
    ```

    Remove (remnants of) LaTeX:

    ``` bash
    sudo rm -r /usr/local/texlive/
    ```

    ``` bash
    sudo rm -r /Library/TeX/
    ```

    Remove pyenv's Python versions:

    ``` bash
    rm -rf ${HOME}/.pyenv
    ```

    Remove everything under `/usr/local`. This seems extreme but Apple
    has nothing installed under `/usr/local`, so all there is comes
    from you or Homebrew. You might change into `/usr/local` and
    remove selected directories by hand. For example, our virus
    scanner at work installs its uninstall-script in `/usr/local`,
    which you probably want to keep. Otherwise:

    ``` bash
    rm -rf /usr/local/*
    ```

### Installing macOS via *Software Update*

Go to *System Settings* \> *General* \> *Software Update*.  Select to
install the new macOS and follow the on-screen instructions.

# Setup macOS

The steps are intended to be done (roughly) in order. You have to open
new login shells several times during the process. If something does
not work after installation, opening a new login shell might do the
trick.

## Check for system updates

Check for updates of macOS in *System Settings* \> *General* \>
*Software Update*.

## Xcode

A full Xcode installation is not always needed. Most often, the Xcode
Command Line Tools (CLT) are enough, for example for Homebrew. But
some development software needs a full Xcode installation such as the
FreePGI Fortran Compiler. So one can, for example, install the Xcode
CLT only, and install the full Xcode only if another program demands
it. Note that installing the full XCode takes considerable time (count
rather half an hour or more).

The full Xcode can be installed from the App Store. You have to open
it once and confirm the Usage Agreement in order to use the bundled
tools. If you install Xcode, it is reasonable to complete the Xcode
installation and the one-time opening before starting with
[Homebrew](#homebrew). Otherwise, it might install the command line
tools CLT twice, but it costs only download bandwidth and time.

While waiting for XCode to install, you can download and install the
other apps from [App Store](#app-store), [XQuartz](#xquartz) and some
[Freeware](#freeware) except
[LaTeXiT](http://www.chachatelier.fr/latexit/), which needs LaTeX
first. You can also [Set preferences of macOS and standard
apps](#set-preferences-of-macos-and-standard-apps).

## Xcode Command Line Tools (CLT)

The normal way to install the XCode Command Line Tools (CLT) from the
terminal would be (watch out for the pop-up window):

``` bash
xcode-select --install
```

On macOS 14 Sonoma, this prints *xcode-select: note: install requested
for command line developer tools* and you have to open *Software
Update* again to install the command line tools.

You might also let [Homebrew](#homebrew) do the job, i.e. it will
install the XCode Command Line Tools if they are missing.

## Set preferences of macOS and standard apps

Set *System Settings* such as: Check for updates of macOS in *System
Settings* \> *General* \> *Software Update*.

- Set computer name in *General* \> *Sharing* \> *Local hostname*

- Unset all in *Desktop & Dock* \> *Mission Control*

- Set *Keyboard* \> *Keyboard Shortcuts* \> *Modifier Keys* \> *Caps
  Lock Key* to *No Action*

Set preferences/settings in standard macOS apps such as:

- Terminal
  - Set *Profiles* \> *Shell* \> *When the shell exists:* to *Close if
    the shell exited cleanly*

  - Unset tickbox *Profiles* \> *Advanced* \> *Set locale environment
    variables on startup*

- Finder
  - Set tickbox *Advanced* \> *Show all filename extensions*

## Set default shell \#1

Apple is now using *zsh* as its default shell. If you want to stay
with *bash*, change it in the terminal:

``` bash
chsh -s /bin/bash
```

To get rid of the nagging reminder that the default shell is now zsh
every time you open a new terminal window, set in your
`.bash_profile`:

``` bash
export BASH_SILENCE_DEPRECATION_WARNING=1
```

My current [.bash_profile](dot-bash_profile) is the file
[dot-bash_profile](dot-bash_profile) in this repository, which uses
specific setup scripts in the folder [.bash.d](dot-bash.d/). There is
also the file [.bashrc](dot-bashrc) for the setup of non-login shells
and for general aliases and functions, which uses also
[.bash.d](dot-bash.d/). And there is the file
[.bashrc.15](dot-bashrc.15) for specific aliases, etc. for macOS 15
(Sequoia).

## App Store

Even when you installed using *Software Update*, you should check for
updates. Do not look only in *App Store…* \> *Updates* but also on
your account (on the bottom left) if there is an update. Xcode, for
example, did not show up in *Updates* and I had to update it from the
account page.

My current App Store programs are:  
Apple Developer, HiddenMe, Keynote, Numbers, Pages, The Unarchiver,
Whatsapp, Windows App, WordService

## XQuartz

XQuartz is the X-window system running on macOS, needed for \*nix GUI
programs. Get it from [XQuartz](http://xquartz.macosforge.org/).

## Homebrew

Install [Homebrew](http://brew.sh) for easy \*nix package
installation.

``` bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

This installs Homebrew into `/usr/local` on Intel Macs and into
`/opt/homebrew` on Macs with Apple Silicon (M1, M2, etc.). You want to
setup you shell so that Homebrew is found automatically. I do this in
[.bash_profile](dot-bash_profile), using the script
[.bash.d/homebrew.sh](dot-bash.d/homebrew.sh). It can happen on
clusters, for example, that the current shell did not inherit from a
login shell. So I also setup Homebrew in [.bashrc](dot-bashrc) if not
done yet.

## Set default shell \#2

Apple moved to *zsh* because of the license change of *bash* from
GPLv2 to GPLv3 with its version 4.0. The current bash shell on macOS
is hence 3.2 from 2007. If you want to use the latest version of
*bash*, install it with Homebrew, "whitelist" the new shell as a login
shell, and choose it as your default login shell:

``` bash
brew install bash
```

``` bash
echo "${HOMEBREW_PREFIX}/bin/bash" | cat /etc/shells - > .ttmmpp ; sudo mv .ttmmpp /etc/shells
```

``` bash
chsh -s ${HOMEBREW_PREFIX}/bin/bash
```

Note that your shell scripts will probably still use the Apple default
bash shell because they often have the shebang line `#!/bin/bash`. The
most portable way to write shell scripts (or any other script) is to
use `#!/usr/bin/env bash` as your shebang. This will take the first
*bash* in your `${PATH}`, which would now be `/usr/local/bin/bash` or
`/opt/homebrew/bin/bash`.

You can now use *bash-completion* with the new bash shell.

``` bash
brew install bash-completion@2
```

You then have to put the following lines in your `.bash_profile` to
use bash-completion:

``` bash
if [[ -f "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh" ]] ; then
    export BASH_COMPLETION_COMPAT_DIR="${HOMEBREW_PREFIX}/etc/bash_completion.d"
    source "${HOMEBREW_PREFIX}/etc/profile.d/bash_completion.sh"
fi
```

Note that these lines have to be after the sourcing of `.fzf.bash` in
your `.bash_profile` if you installed *fzf*, otherwise you get an
error such as `programmable_completion: source: possible retry loop`.

You can do the exact same steps for the zsh shell. Apples version of
*zsh* is rather new but if you want to have the newest developments,
install *zsh* with Homebrew, whitelist it and use it as your default
shell. If you use *zsh*, you might want to check out [Oh My
ZSH](https://ohmyz.sh) for easy configuration of *zsh*.

After a system update such as from *System Preferences* \> *Software
Update*, there might be a link `Relocated Items/` on your Desktop
pointing to `/Users/Shared/Relocated Items`. This is a copy of the
changed `/etc/shells`. As long as Apple does not modify `/etc/shells`
during an update, the edited version of `/etc/shells` stays untouched,
though. One can safely delete the link on the Desktop and also the
directory under `/Users/Shared`. It does not hurt to do a `cat
/etc/shells` in the terminal before, checking that your edits are
still there.

## Homebrew \#2

- **GNU compiler and netCDF software**

  The gcc suite includes *gfortran*. *Cmake* is the build system of
  various software packages. Install *nco*, *ncview*, and *Panoply* to
  work with and visualise netCDF files. This installs the netcdf-C
  version, which comes with *ncdump*, etc. netcdf-C, netcdf-C++, and
  netcdf-Fortran are individual packages. *brew install netcdf* installs the
  netcdf-C package only. See
  [installnetcdf](https://github.com/mcuntz/install_netcdf) for
  Fortran support.

  ``` bash
  brew install gcc
  ```

  ``` bash
  brew install cmake
  ```

  ``` bash
  # geos and gdal take considerable time to install
  for i in geos gdal nco ncview ; do brew install ${i} ; done
  ```

  ``` bash
  brew install cdo
  ```

  ``` bash
  brew install --cask temurin  # Java, for panoply
  ```

  ``` bash
  brew install --cask panoply
  ```

  *HDF5* from Homebrew is not thread-safe so *cdo* will need the -L
  flag if piping, i.e. more than one operator is given to *cdo* in one
  call such as `cdo -timmean -selvar,Tair infile outfile`. I use in my
  `.bashrc`:

  ``` bash
  alias cdo="cdo -L"
  ```

  Note that `cdo -L` instead of purely `cdo` must also be used in
  scripts for piping.

- **Install more practical software**

  Install some more practical software such as, *fd* for a faster
  find, *ripgrep* for grepping across a directory tree, the
  statistical computing environment *R*, or the command-line fuzzy
  finder *fzf* (mostly used with Ctrl-r):

  ``` bash
  brew install htop        # dynamic real-time information of running processes
  ```

  ``` bash
  brew install tree        # visualise folder tree structure
  ```

  ``` bash
  brew install fd          # faster find
  ```

  ``` bash
  brew install bat         # cat with syntax highlighting
  ```

  ``` bash
  brew install ripgrep     # grep across directory tree
  ```

  ``` bash
  brew install fzf         # command-line fuzzy finder
  ```

  ``` bash
  brew install wget        # retrieve files from web servers
  ```

  ``` bash
  brew install ghostscript # postscript and pdf language interpreter
  ```

  ``` bash
  brew install enscript    # convert text files to postscript files
  ```

  ``` bash
  brew install imagemagick # image manipulations
  ```

  ``` bash
  brew install ffmpeg      # for movies
  ```

  ``` bash
  brew install pandoc      # convert between markup languages
  ```

  ``` bash
  brew install pkg-config  # reveal details of installed libraries
  ```

  ``` bash
  brew install graphviz doxygen # documentation for programming languages
  ```

  ``` bash
  brew install subversion  # version control system
  ```

  ``` bash
  brew install git         # version control system
  ```

  ``` bash
  brew install rsync       # better Unicode support in newer rsync versions
  ```

  ``` bash
  brew install r           # statistical computing environment
  ```

  ``` bash
  brew install --cask quarto  # jupyter/Rmarkdown like notebooks
  ```

  ``` bash
  brew install --cask copilot-cli  # MS Copilot on the command line
  ```

  All in one go is:

  ``` bash
  brew install htop tree fd bat ripgrep fzf wget ghostscript \
       enscript imagemagick ffmpeg pandoc pkg-config graphviz \
       doxygen subversion git rsync r
  brew install --cask quarto
  brew install --cask copilot-cli
  ```

  Afterwards some installations of the helpers:

  ``` bash
  ${HOMEBREW_PREFIX}/opt/fzf/install
  ```

## Emacs

I used to use [Aquamacs](http://aquamacs.org), then used
[Spacemacs](https://www.spacemacs.org), and then tried a few other
setups ([doom](https://github.com/doomemacs/doomemacs),
[dotemacs](https://github.com/angrybacon/dotemacs),
[boremacs](https://codeberg.org/kngwyu/boremacs),
[minemacs](https://github.com/abougouffa/minemacs), etc.). Now I am
using my own simple setup by copy/paste different bits from other
setups. This makes loading Emacs pretty fast. My current setup is in
[.emacs.d](dot-emacs.d).

I install Emacs with Homebrew:

``` bash
brew install --cask emacs-app
```

Coming from another Emacs, backup `.emacs` and `.emacs.d`:

``` bash
cd ${HOME}
if [[ -f .emacs ]] ; then mv .emacs .emacs.bak ; fi
if [[ -d .emacs.d ]] ; then mv .emacs.d .emacs.d.bak ; fi
```

Then I install my setup by copying it to `~/.emacs.d`.

I use the font [Source Code
Pro](https://github.com/adobe-fonts/source-code-pro) that is also used
in Spacemacs, install aspell for spell checking, and
[ruff](https://github.com/astral-sh/ruff) for linting Python code:

``` bash
brew install --cask font-source-code-pro
```

``` bash
brew install aspell ruff
```

If you open the new Emacs for the first time, it will install and
byte-compile some packages. This might take some time.

## LaTeX

One can download LaTeX from [MacTeX](https://tug.org/mactex/) or use a
Homebrew cask. I use the BasicTeX installation and add all packages I
ever needed. Thence Homebrew handles the update between years (`brew
upgrade --cask basictex`), which is always a hassle otherwise.

``` bash
brew install --cask basictex
```

Either after a new install or after an upgrade, I install a rather few
LaTeX packages, which I encountered during different projects, journal
templates, etc.:

``` bash
sudo tlmgr update --self ; \
sudo tlmgr install \
 a0poster a4wide acronym adjustbox algorithmicx \
 algorithms anyfontsize apacite appendix arydshln \
 biber biblatex biblatex-apa biblatex-chicago \
 bigfoot blindtext boondox breakurl capt-of changepage \
 changes chemfig cleveref cmbright collectbox \
 collection-fontsrecommended collection-fontutils comment \
 cormorantgaramond csquotes dinbrief doi doublestroke \
 draftwatermark dvipng easy elsarticle enumitem \
 environ etoolbox everypage floatflt floatrow fltpoint \
 fncychap fontawesome5 fontaxes fontinst footmisc  \
 framed gensymb german glossaries glossaries-extra \
 gradientframe helvetic hyphenat ifmtarg kastrup \
 lastpage latexmk lettrine lineno lipsum \
 listingsutf8 makecell marginnote mdframed mdsymbol \
 mhchem minitoc mnsymbol moreverb multirow mwe  \
 ncctools needspace newtx nomencl ntheorem pbox \
 pdfcol pdfcrop pgf pgfgantt placeins preprint program psnfss \
 regexpatch sectsty sidecap simplekv siunitx soul \
 stmaryrd sttools subfigure subfiles supertabular \
 tabfigures tabulary tcolorbox textpos threeparttable \
 tikzfill titlesec titling todonotes truncate type1cm \
 ucs ulem units varwidth vruler wallpaper was wasy \
 wasysym wrapfig xcolor xifthen xkeyval xstring \
 arev bera fira iwona kurier lato ly1 mathastext newtxsf \
 opensans psnfss sansmathfonts sfmath zref authoraftertitle \
 totcount
```

I also install LaTeXML so that Emacs' org-mode, for example, can
convert LaTeX equations to MathML on export:

``` bash
brew install latexml
```

And I (re-)install the font Myriad Pro, also for LaTeX.

## Myriad Pro

I like the Myriad Pro font and AGU journals currently use it. The
Myriad Pro font comes with the Adobe Acrobat Reader.

To install for non-LaTeX programs, one can install in Font Book the
four *otf*-files from the directory '*/Applications/Adobe Acrobat
DC/Adobe Acrobat.app/Contents/Resources/Resource/Font/*' (on older
systems '*/Applications/Adobe Acrobat
Reader.app/Contents/Resources/Resource/Font*)'.

An extended set of glyphs (SemiCondensed, etc.) are given in the zip
file `MyriadPro.zip` in this repository: unzip MyriadPro.zip and drag
the folder with the .otf files into Font Book.

To install Myriad Pro for LaTeX, one launches the following commands
in Terminal:

``` bash
# tools needed for FontPro
sudo tlmgr install fontinst fltpoint tabfigures mnsymbol mdsymbol \
     collection-fontutils

# FontPro tool: copy of https://github.com/sebschub/FontPro
curl -L -o FontPro.zip https://raw.githubusercontent.com/mcuntz/setup_mac/master/FontPro.zip
unzip FontPro.zip
\rm FontPro.zip

# Myriad Pro font, 30 styles
FONT=MyriadPro
curl -L -o ${FONT}.zip https://raw.githubusercontent.com/mcuntz/setup_mac/master/${FONT}.zip
unzip ${FONT}.zip
\rm ${FONT}.zip
if [[ -d __MACOSX ]] ; then \rm -r __MACOSX ; fi
# move them into FontPro directory
mkdir FontPro/otf
mv ${FONT}/${FONT}*.otf FontPro/otf/
rmdir ${FONT}

# make the LaTeX fonts
cd FontPro
./scripts/makeall ${FONT}
echo y | sudo ./scripts/install
sudo updmap-sys --enable Map=${FONT}.map
sudo -H mktexlsr
kpsewhich ${FONT}.map

# clean up
cd ..
\rm -fr FontPro
```

## Freeware

Some essential Freeware:

- [Adobe Reader](https://www.adobe.com/acrobat/pdf-reader.html),
  because Preview sometimes has problems with certain PDFs.

- [AppCleaner](http://www.freemacsoft.net/appcleaner/), for removing
  apps and all their traces.

- [Chrome](https://www.google.com/chrome/), Safari is not always
  supported. Chrome is probably the most supported browser. I
  sometimes also use [Brave](https://brave.com/) on older systems,
  [Opera](https://www.opera.com), and [Firefox Developer
  Edition](https://www.mozilla.org/en-US/firefox/all/).

- [DeepL](https://www.deepl.com/en/translator), translation and
  writing tool.

- [f.lux](https://justgetflux.com/), night shift also for external
  displays.

- [ImageJ](https://imagej.net/ij/), image processing.

- [LaTeXiT](http://www.chachatelier.fr/latexit/), exporting LaTeX
  equations as graphics.

- [LibreOffice](https://www.libreoffice.org/), free office suite.

- [Signal](https://signal.org/), messenger.

- [VLC](http://www.videolan.org/vlc/), video player for all formats.

- [VScodium](https://github.com/VSCodium/vscodium), fully open-source
  VSCode.

- [Zotero](https://www.zotero.org), reference manager.

## Python

Python on macOS can be a real mess at times, as noted by
[XKCD](https://xkcd.com/1987/):

<img src="https://imgs.xkcd.com/comics/python_environment.png"
alt="Python path on my system" data-align="center" width="450"
height="450"/>

macOS Catalina (10.15) still came with Python version 2.7.16 as its
default version. Official support for Python 2 has ended
Januar 2020. So you want to install Python 3. From macOS Big Sur
(11.5) onwards, macOS comes with Python 3 (from macOS 13 Ventura, is
is actually part of the XCode command line tools). But I still
recommend to install Python with *pyenv* and *pyenv-virtualenv*: you
can install different Python versions, use different virtual
environments in different directories (projects) very easily, etc.

*pyenv* and *pyenv-virtualenv* makes that very easy: see the great
(slightly older) article [pyenv: Multi-version Python development on
Mac](https://medium.com/faun/pyenv-multi-version-python-development-on-mac-578736fb91aa)
by Dirk Avery.

To install pyenv and pyenv-virtualenv with Homebrew:

``` bash
brew install openssl readline sqlite3 xz zlib tcl-tk@8 libb2 zstd
brew install pyenv
brew install pyenv-virtualenv
```

See [.bash.d/pyenv.sh](dot-bash.d/pyenv.sh) for the setup in
*.bash_profile* and/or *.bashrc* (cf. [Homebrew](#homebrew) for an
explanation of the "and" setup).

Current available Python version can be listed by 

``` bash
pyenv install --list
```

and installed and switched to simply like this:

``` bash
pyenv install 3.14.3
pyenv global 3.14.3
```

A concise reference is given in on the [pyenv
Github](https://github.com/pyenv/pyenv?tab=readme-ov-file#usage).

I currently install Python actually like this:

``` bash
env LDFLAGS=" \
    -L$(brew --prefix openssl)/lib \
    -L$(brew --prefix readline)/lib \
    -L$(brew --prefix sqlite3)/lib \
    -L$(brew --prefix xz)/lib \
    -L$(brew --prefix zlib)/lib \
    -L$(brew --prefix tcl-tk@8)/lib" \
    CPPFLAGS=" \
    -I$(brew --prefix openssl)/include \
    -I$(brew --prefix readline)/include \
    -I$(brew --prefix sqlite3)/include \
    -I$(brew --prefix xz)/include \
    -I$(brew --prefix zlib)/include \
    -I$(brew --prefix tcl-tk@8)/include" \
    PKG_CONFIG_PATH="$(brew --prefix openssl)/lib/pkgconfig:$(brew --prefix readline)/lib/pkgconfig:$(brew --prefix sqlite3)/lib/pkgconfig:$(brew --prefix xz)/lib/pkgconfig:$(brew --prefix zlib)/lib/pkgconfig:$(brew --prefix tcl-tk@8)/lib/pkgconfig" \
    PYTHON_CONFIGURE_OPTS="--enable-optimizations" \
    pyenv install 3.14.3
```

This uses the newer Tcl/Tk version 8.6 from Homebrew for *tkinter*
(but not v9.0) and turns on profile guided optimization as well as
link time optimization for Python, being then about 10% faster than
without optimization. It takes significantly more time to install
Python with optimization. If you do not use *tkinter* and just want to
try out a Python version, `pyenv install 3.14.3` is just
fine. Otherwise I recommend the optimization because you do not
install Python too often but get a significant gain.


Virtual environments are then created as:

``` bash
pyenv virtualenv 3.14.3 mypy
```

The virtual environment *mypy* can then be used just as any installed
Python version with *pyenv*. For example:

``` bash
pyenv shell mypy
pyenv local mypy
pyenv global mypy
```

- **Essential Python packages**

  I tend to use a Python version with pyenv and install my essential
  packages with *pip*. These are currently in my main environment
  (pystd):  
  numpy, scipy, matplotlib, cartopy, ipython, pandas, cftime,
  netcdf4, statsmodels, scikit-learn,
  dask, xarray, pykdtree, cython, pyshp, six, gdal
  
  And a few more for development:  
  jupyter, xlrd, openpyxl, mpi4py, schwimmbad, f90nml, cdsapi,
  customtkinter, hvplot, icoscp_core, pyflakes, pyperclip, seaborn,
  numpydoc, pytest, pytest-cov, wheel, sphinx, sphinx\_book\_theme.

  This gives the current environment (pystd):

  ``` bash
  # essential subset
  if [[ "$(uname -m)" == "arm64" ]] ; then
      # export OPENBLAS="$(brew --prefix openblas)"
      export HDF5_DIR="$(brew --prefix hdf5)"
      export GEOS_DIR="$(brew --prefix geos)"
      export GEOS_CONFIG="$(brew --prefix geos)/bin/geos-config"
  fi
  ```

  ``` bash
  pyenv virtualenv 3.14.3 pystd
  pyenv global pystd
  ```

  ``` bash
  # test if install works
  python -m pip install numpy
  ```

  ``` bash
  for i in scipy matplotlib ipython pandas cftime netcdf4 \
          statsmodels scikit-learn dask xarray pykdtree \
          cython pyshp six ; do \
      python -m pip install ${i} ; done
  ```

  ``` bash
  # shapely and gdal for cartopy
  # shapely needs to be built from source to link to geos.
  # Uninstall it if already installed
  [[ -z $(python -m pip freeze | grep shapely) ]] && \
      python -m pip uninstall -y shapely
  python -m pip install shapely --no-binary shapely
  ```

  ``` bash
  # gdal needs to know the installed gdal version
  # and install numpy-based raster support
  # test: python3 -c 'from osgeo import gdal_array'
  pip install --no-cache --force-reinstall gdal[numpy]=="$(gdal-config --version).*"
  ```

  ``` bash
  python -m pip install cartopy
  ```

  ``` bash
  # other development packages
  for i in jupyter xlrd openpyxl mpi4py schwimmbad f90nml \
          cdsapi customtkinter hvplot icoscp_core pyflakes \
          pyperclip seaborn numpydoc pytest pytest-cov wheel \
          sphinx sphinx_book_theme ; do \
      python -m pip install ${i} ; done
  ```

  mpi4py will be installed after installing openmpi later.

## locate

Create locate database so that you can search files with the locate
command:

``` bash
sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist
```

This might already be running (*Operation already in progress* or
*Load failed: 5: Input//output error*).

## Additional software

Install additional software from you institution or other payware
similar such as VPN clients, cloud services, etc. For INRAE this is:
WithSecure Antivirus, GlobalProtect VPN, Microsoft Office, the
meticulous Fortran compiler from
[NAG](http://www.nag.co.uk/downloads/npdownloads.asp), and the spell
and grammar checker [Antidote](https://www.antidote.info/en).

## install_fortran_libs

You can install
[netcdf-fortran](https://downloads.unidata.ucar.edu/netcdf/) for the
gfortran compiler with:

``` bash
brew install netcdf-fortran
```

This will automatically update netcdf-fortran for gfortran if a newer
version of netcdf-C and/or netcdf-fortran becomes available.

However, if you use other Fortran compilers then gfortran as well, you
might want to separate the Fortran installations and not use
netcdf-fortran from Homebrew. You can use the script
`install_fortran_libs.sh` in
[installnetcdf](https://github.com/mcuntz/install_netcdf) to install
netCDF4-Fortran and two MPI libraries in separate directories for
different Fortran compilers.  
Set parameters in the section `Setup` of the script.

Note: homebrew upgrades also netcdf-C to newer versions if you install
or update a package that depends on it. Then the netCDF4-Fortran
package installed with `install_fortran_libs.sh` will not work anymore
(it will still link to the old, uninstalled C version). You then have
to rerun the script. I still do it this way to minimize conflicts
between different Fortran compilers (I find them very hard to debug);
and re-installing netCDF4-Fortran with `install_fortran_libs.sh` is
very fast.

- **mpi4py**

After having installed *openmpi*, one can also install *mpi4py* in
Python, for example:

``` bash
env MPICC=/usr/local/openmpi-4.1.7-gfortran/bin/mpicc python -m pip install mpi4py
```

Enjoy your new system!
