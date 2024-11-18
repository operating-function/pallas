import type { SidebarsConfig } from "@docusaurus/plugin-content-docs";

/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */
const sidebars: SidebarsConfig = {
  // By default, Docusaurus generates a sidebar from the docs folder structure
  //tutorialSidebar: [{type: 'autogenerated', dirName: '.'}],
  explanationSidebar: [
    "explanation/system-overview",
    {
      type: "category",
      label: "PLAN",
      collapsed: false,
      items: [
        "explanation/plan",
        "explanation/plan-data-model",
        "explanation/plan-evaluation-model",
      ],
    },
    "explanation/vm-and-interpreter",
    "explanation/sire-and-rex",
  ],

  referenceSidebar: [
    {
      type: "category",
      label: "Sire",
      collapsed: false,
      items: [
        "reference/sire-runes-macros",
        {
          type: "category",
          label: "Sire Standard Library",
          collapsed: true,
          items: [
            "reference/standard-library/bits-booleans",
            "reference/standard-library/natural-numbers",
            "reference/standard-library/comparisons",
            "reference/standard-library/characters-strings",
            "reference/standard-library/rows",
            "reference/standard-library/lists",
            "reference/standard-library/either",
            "reference/standard-library/maybe",
            "reference/standard-library/sets",
            "reference/standard-library/kv-tables",
            "reference/standard-library/pads",
            "reference/standard-library/bars-byte-arrays",
            "reference/standard-library/types",
          ],
        },
      ],
    },
    {
      type: "category",
      label: "VM and Interpreter",
      collapsed: true,
      items: [
        "reference/vm-and-interpreter/jets-reference",
        "reference/vm-and-interpreter/cogs-reference",
        "reference/vm-and-interpreter/drivers-reference",
        "reference/vm-and-interpreter/jobs-reference",
        "reference/vm-and-interpreter/effects-reference",
      ],
    },
    "reference/plan/plan-reference",
  ],

  tutorialSidebar: [
    {
      type: "category",
      label: "Tutorial",
      collapsed: true,
      items: ["tutorial/intro"],
    },
  ],
};

export default sidebars;