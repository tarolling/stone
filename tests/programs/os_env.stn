// reads the variables that os_env.env sets
use os
use os.env
use os as system

print(os.env("STONE_GREETING"))
print(env("STONE_GREETING").len(), system.has_env("STONE_GREETING"))

// an empty value is set, unlike a missing one
print(os.has_env("STONE_EMPTY"), os.env("STONE_EMPTY") == "")
print(os.has_env("STONE_SURELY_MISSING"), os.env("STONE_SURELY_MISSING") == "")

// names libc cannot look up are never set
print(os.has_env(""), os.has_env("STONE_GREETING=hello there"))

// values are new strings, so they can be kept and joined
names = ["STONE_GREETING", "STONE_EMPTY", "STONE_SURELY_MISSING"]
values = []
for name in names;
    values.append(os.env(name))
print(values)
