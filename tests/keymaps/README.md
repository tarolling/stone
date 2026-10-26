# Test keymaps

XKB keymaps as a Wayland compositor sends them, flattened by xkbcommon, for the fake compositor
in `tests/cli.rs`. `us.xkb` is the keymap of WSLg's compositor (English, US), and `de.xkb` the
same with four keys changed to a German layout's (`y` and `z` swapped, `ö` and `ß`). Their data
comes from xkeyboard-config, under its MIT license.
