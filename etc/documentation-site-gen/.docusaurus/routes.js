import React from 'react';
import ComponentCreator from '@docusaurus/ComponentCreator';

export default [
  {
    path: '/__docusaurus/debug',
    component: ComponentCreator('/__docusaurus/debug', '5ff'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/config',
    component: ComponentCreator('/__docusaurus/debug/config', '5ba'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/content',
    component: ComponentCreator('/__docusaurus/debug/content', 'a2b'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/globalData',
    component: ComponentCreator('/__docusaurus/debug/globalData', 'c3c'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/metadata',
    component: ComponentCreator('/__docusaurus/debug/metadata', '156'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/registry',
    component: ComponentCreator('/__docusaurus/debug/registry', '88c'),
    exact: true
  },
  {
    path: '/__docusaurus/debug/routes',
    component: ComponentCreator('/__docusaurus/debug/routes', '000'),
    exact: true
  },
  {
    path: '/markdown-page',
    component: ComponentCreator('/markdown-page', '3d7'),
    exact: true
  },
  {
    path: '/',
    component: ComponentCreator('/', '067'),
    routes: [
      {
        path: '/',
        component: ComponentCreator('/', 'ead'),
        routes: [
          {
            path: '/',
            component: ComponentCreator('/', '958'),
            routes: [
              {
                path: '/explanation/plan',
                component: ComponentCreator('/explanation/plan', '2d7'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/explanation/plan-data-model',
                component: ComponentCreator('/explanation/plan-data-model', '823'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/explanation/plan-evaluation-model',
                component: ComponentCreator('/explanation/plan-evaluation-model', '0df'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/explanation/sire-and-rex',
                component: ComponentCreator('/explanation/sire-and-rex', 'dfb'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/explanation/vm-and-interpreter',
                component: ComponentCreator('/explanation/vm-and-interpreter', 'd86'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/philosophy/philosopy-intro',
                component: ComponentCreator('/philosophy/philosopy-intro', '69c'),
                exact: true
              },
              {
                path: '/reference/plan/',
                component: ComponentCreator('/reference/plan/', '2b9'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/sire-runes-macros',
                component: ComponentCreator('/reference/sire-runes-macros', 'b25'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/bars-byte-arrays',
                component: ComponentCreator('/reference/standard-library/bars-byte-arrays', 'd77'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/bits-booleans',
                component: ComponentCreator('/reference/standard-library/bits-booleans', '6b1'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/characters-strings',
                component: ComponentCreator('/reference/standard-library/characters-strings', 'fe8'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/comparisons',
                component: ComponentCreator('/reference/standard-library/comparisons', 'aa0'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/either',
                component: ComponentCreator('/reference/standard-library/either', '9da'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/kv-tables',
                component: ComponentCreator('/reference/standard-library/kv-tables', '435'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/lists',
                component: ComponentCreator('/reference/standard-library/lists', '35e'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/maybe',
                component: ComponentCreator('/reference/standard-library/maybe', '4c5'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/natural-numbers',
                component: ComponentCreator('/reference/standard-library/natural-numbers', '640'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/pads',
                component: ComponentCreator('/reference/standard-library/pads', '50d'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/rex',
                component: ComponentCreator('/reference/standard-library/rex', '358'),
                exact: true
              },
              {
                path: '/reference/standard-library/rows',
                component: ComponentCreator('/reference/standard-library/rows', '7a8'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/sets',
                component: ComponentCreator('/reference/standard-library/sets', '619'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/standard-library/types',
                component: ComponentCreator('/reference/standard-library/types', 'd8c'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/vm-and-interpreter/cogs-reference',
                component: ComponentCreator('/reference/vm-and-interpreter/cogs-reference', '825'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/vm-and-interpreter/drivers-reference',
                component: ComponentCreator('/reference/vm-and-interpreter/drivers-reference', 'e79'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/vm-and-interpreter/effects-reference',
                component: ComponentCreator('/reference/vm-and-interpreter/effects-reference', '344'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/vm-and-interpreter/jets-reference',
                component: ComponentCreator('/reference/vm-and-interpreter/jets-reference', '735'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/reference/vm-and-interpreter/jobs-reference',
                component: ComponentCreator('/reference/vm-and-interpreter/jobs-reference', 'aff'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/tutorial/intro',
                component: ComponentCreator('/tutorial/intro', '50a'),
                exact: true,
                sidebar: "tutorialSidebar"
              },
              {
                path: '/',
                component: ComponentCreator('/', 'b5d'),
                exact: true,
                sidebar: "explanationSidebar"
              }
            ]
          }
        ]
      }
    ]
  },
  {
    path: '*',
    component: ComponentCreator('*'),
  },
];
