# Package

version       = "0.1.0"
author        = "Crystal Melting Dot"
description   = "A runtime string templating engine in Nim"
license       = "LGPL-3.0-or-later"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.8"


task docs, "Generate documentation":
  exec "nimble test"
  exec "nim doc --path:src --project --index:on --outdir:docs --git.url:https://github.com/cmd410/ore --git.commit:v" & version & " src/ore.nim"
