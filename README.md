# Requirements

I’m using Lean version 3.22.0c ([community fork](https://github.com/leanprover-community/lean)), commit 0ec2a19f1ac9. If you are using older versions of Lean (release ≤3.19.0c), this is will not work because of changes in standard libarry.

Also `netcat` is needed. You can try to configure the start using telnet, but I do not guarantee the functionality.

I designed and tested this on Fedora Linux 27 and Ubuntu 18.04. On macOS, this should also work, because this is also a UNIX-like system. On Windows, you can try using cygwin.

# Running

`lean --run sample-bot.lean <Login> <Password>` (default bot—sample-bot.lean—is configured to work with SASL)

# Code editing

If you want to edit the source code in GNU Emacs, Visual Studio Code, or another editor, the repository directory must be in `LEAN_PATH`.
