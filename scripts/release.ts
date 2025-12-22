import { GHRelEasy, CLI } from "gh-rel-easy";

const hconf = new CLI("hconf");

const gh = new GHRelEasy({
  gh: "nalchevanidze/hconf",
  scope: { hconf: "hconf" },
  pkg: (p) => `https://hackage.haskell.org/package/${p}`,
  version: () => hconf.exec("version"),
  next: (b) => hconf.void("next", b && "-b"),
  setup: () => hconf.void("setup"),
});

gh.cli();
