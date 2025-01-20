import siteConfig from "@generated/docusaurus.config";
import Prism from "prismjs";
import "prismjs/components/prism-haskell";

export default function prismIncludeLanguages(PrismObject) {
  console.log(
    "Available languages:",
    Object.keys(PrismObject.languages)
  );

  // Define rex language
  PrismObject.languages.rex = {
    head: {
      pattern: /^""".*$/m,
      greedy: true,
    },
    line: {
      pattern: /'''.*$/m,
      greedy: true,
    },
    page: {
      pattern: /""".*$/m,
      greedy: true,
    },
    note: {
      pattern: /;.*$/m,
      greedy: true,
    },
    cord: {
      pattern: /'[^']*'(?=$|[^'])/,
      greedy: true,
    },
    tape: {
      pattern: /"[^"]*"(?=$|[^"])/,
      greedy: true,
    },
    curl: {
      pattern: /{[^}]*}/,
      greedy: true,
    },
    badc: {
      pattern: /}/,
      greedy: true,
    },
    rine: {
      pattern: /^[\t ]*[$!#%&*+,\-./:<=>\?@\\^`|~]+/m,
      greedy: true,
    },
    rune: {
      pattern: /[$!#%&*+,\-./:<=>\?@\\^`|~]+/,
      greedy: true,
    },
    cnsr: {
      pattern: /(?<![%_a-zA-Z0-9])[A-Z][A-Z_0-9]*(?=$|[^a-zA-Z0-9_])/,
      greedy: true,
    },
    type: {
      pattern:
        /(?<![%_a-zA-Z0-9])[A-Z][_A-Z0-9]*[a-z][_a-zA-Z0-9]*(?=$|[^a-zA-Z0-9_])/,
      greedy: true,
    },
    numb: {
      pattern: /(?<![%_a-zA-Z0-9])[0-9][_a-zA-Z0-9]*/,
      greedy: true,
    },
    word: {
      pattern: /(?<![%_a-zA-Z0-9])[_a-z][_a-zA-Z0-9]*/,
      greedy: true,
    },
    cnst: {
      pattern: /%[_A-Za-z0-9]+/,
      greedy: true,
    },
    nest: {
      pattern: /[[()\]]/,
      greedy: true,
    },
  };

  // alias "sire" code blocks to rex
  PrismObject.languages.sire = PrismObject.languages.rex;

  PrismObject.languages.haskell = Prism.languages.haskell;

  console.log(
    "Languages after rex:",
    Object.keys(PrismObject.languages)
  );
}
