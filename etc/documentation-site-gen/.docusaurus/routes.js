import React from 'react';
import ComponentCreator from '@docusaurus/ComponentCreator';

export default [
  {
    path: '/markdown-page',
    component: ComponentCreator('/markdown-page', '3d7'),
    exact: true
  },
  {
    path: '/docs',
    component: ComponentCreator('/docs', '3bb'),
    routes: [
      {
        path: '/docs',
        component: ComponentCreator('/docs', '4c1'),
        routes: [
          {
            path: '/docs',
            component: ComponentCreator('/docs', '517'),
            routes: [
              {
                path: '/docs/explanation/plan',
                component: ComponentCreator('/docs/explanation/plan', 'e99'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/docs/explanation/plan-data-model',
                component: ComponentCreator('/docs/explanation/plan-data-model', 'da1'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/docs/explanation/plan-evaluation-model',
                component: ComponentCreator('/docs/explanation/plan-evaluation-model', 'afe'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/docs/explanation/sire-and-rex',
                component: ComponentCreator('/docs/explanation/sire-and-rex', '02f'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/docs/explanation/system-overview',
                component: ComponentCreator('/docs/explanation/system-overview', '65f'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/docs/explanation/vm-and-interpreter',
                component: ComponentCreator('/docs/explanation/vm-and-interpreter', '2de'),
                exact: true,
                sidebar: "explanationSidebar"
              },
              {
                path: '/docs/philosophy/philosopy-intro',
                component: ComponentCreator('/docs/philosophy/philosopy-intro', '79b'),
                exact: true
              },
              {
                path: '/docs/reference/plan/',
                component: ComponentCreator('/docs/reference/plan/', '510'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/sire-runes-macros',
                component: ComponentCreator('/docs/reference/sire-runes-macros', 'bdc'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/bars-byte-arrays',
                component: ComponentCreator('/docs/reference/standard-library/bars-byte-arrays', '5d0'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/bits-booleans',
                component: ComponentCreator('/docs/reference/standard-library/bits-booleans', 'ec6'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/characters-strings',
                component: ComponentCreator('/docs/reference/standard-library/characters-strings', '39b'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/comparisons',
                component: ComponentCreator('/docs/reference/standard-library/comparisons', 'a46'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/either',
                component: ComponentCreator('/docs/reference/standard-library/either', 'f79'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/kv-tables',
                component: ComponentCreator('/docs/reference/standard-library/kv-tables', '5e4'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/lists',
                component: ComponentCreator('/docs/reference/standard-library/lists', '7d7'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/maybe',
                component: ComponentCreator('/docs/reference/standard-library/maybe', '8c6'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/natural-numbers',
                component: ComponentCreator('/docs/reference/standard-library/natural-numbers', 'f00'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/pads',
                component: ComponentCreator('/docs/reference/standard-library/pads', '1ab'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/rex',
                component: ComponentCreator('/docs/reference/standard-library/rex', 'dc9'),
                exact: true
              },
              {
                path: '/docs/reference/standard-library/rows',
                component: ComponentCreator('/docs/reference/standard-library/rows', 'dad'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/sets',
                component: ComponentCreator('/docs/reference/standard-library/sets', '3cd'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/standard-library/types',
                component: ComponentCreator('/docs/reference/standard-library/types', '4f8'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/vm-and-interpreter/cogs-reference',
                component: ComponentCreator('/docs/reference/vm-and-interpreter/cogs-reference', '2cf'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/vm-and-interpreter/drivers-reference',
                component: ComponentCreator('/docs/reference/vm-and-interpreter/drivers-reference', '868'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/vm-and-interpreter/effects-reference',
                component: ComponentCreator('/docs/reference/vm-and-interpreter/effects-reference', '6cd'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/vm-and-interpreter/jets-reference',
                component: ComponentCreator('/docs/reference/vm-and-interpreter/jets-reference', '71d'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/reference/vm-and-interpreter/jobs-reference',
                component: ComponentCreator('/docs/reference/vm-and-interpreter/jobs-reference', 'fd1'),
                exact: true,
                sidebar: "referenceSidebar"
              },
              {
                path: '/docs/tutorial/intro',
                component: ComponentCreator('/docs/tutorial/intro', '9b8'),
                exact: true,
                sidebar: "tutorialSidebar"
              }
            ]
          }
        ]
      }
    ]
  },
  {
    path: '/',
    component: ComponentCreator('/', 'e5f'),
    exact: true
  },
  {
    path: '*',
    component: ComponentCreator('*'),
  },
];
