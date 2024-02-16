# Emacs configuration

This is my Emacs configuration -- it's fairly simple and does not mean to be anything "special", nor anything "feature complete" by any means. It's just the way I like it and it works for me.

The configuration is designed to be portable, that is, to take care of auto-installing necessary packages and set the up.

I tended to use vanilla key-bindings as much as I could.

The configuration is done in a literate way via the `configuration.org` file, which is loaded by `init.el`. The `custom` section is put in a separate file which is created automatically if one does not exist already.

## Caveat

+ Upon the first installation, it might throw some errors, mainly connected to the necessety to run `M-x package-refresh-contents RET` to sync the repos and install all the packages that need to be installed.

## Roadmap

+ ~~Move LSP configuration from Eglot to `LSP-mode`~~;
+ Install and configure `dap-mode`;
+ ~~Standardize package setup with `use-package`~~
