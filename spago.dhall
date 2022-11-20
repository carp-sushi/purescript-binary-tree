{ name = "purescript-binary-tree"
, dependencies =
  [ "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "prelude"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
