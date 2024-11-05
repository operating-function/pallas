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
    path: '/blog',
    component: ComponentCreator('/blog', 'b2f'),
    exact: true
  },
  {
    path: '/blog/archive',
    component: ComponentCreator('/blog/archive', '182'),
    exact: true
  },
  {
    path: '/blog/authors',
    component: ComponentCreator('/blog/authors', '0b7'),
    exact: true
  },
  {
    path: '/blog/authors/all-sebastien-lorber-articles',
    component: ComponentCreator('/blog/authors/all-sebastien-lorber-articles', '4a1'),
    exact: true
  },
  {
    path: '/blog/authors/yangshun',
    component: ComponentCreator('/blog/authors/yangshun', 'a68'),
    exact: true
  },
  {
    path: '/blog/first-blog-post',
    component: ComponentCreator('/blog/first-blog-post', '89a'),
    exact: true
  },
  {
    path: '/blog/long-blog-post',
    component: ComponentCreator('/blog/long-blog-post', '9ad'),
    exact: true
  },
  {
    path: '/blog/mdx-blog-post',
    component: ComponentCreator('/blog/mdx-blog-post', 'e9f'),
    exact: true
  },
  {
    path: '/blog/tags',
    component: ComponentCreator('/blog/tags', '287'),
    exact: true
  },
  {
    path: '/blog/tags/docusaurus',
    component: ComponentCreator('/blog/tags/docusaurus', '704'),
    exact: true
  },
  {
    path: '/blog/tags/facebook',
    component: ComponentCreator('/blog/tags/facebook', '858'),
    exact: true
  },
  {
    path: '/blog/tags/hello',
    component: ComponentCreator('/blog/tags/hello', '299'),
    exact: true
  },
  {
    path: '/blog/tags/hola',
    component: ComponentCreator('/blog/tags/hola', '00d'),
    exact: true
  },
  {
    path: '/blog/welcome',
    component: ComponentCreator('/blog/welcome', 'd2b'),
    exact: true
  },
  {
    path: '/markdown-page',
    component: ComponentCreator('/markdown-page', '3d7'),
    exact: true
  },
  {
    path: '/docs',
    component: ComponentCreator('/docs', '5c4'),
    routes: [
      {
        path: '/docs',
        component: ComponentCreator('/docs', '842'),
        routes: [
          {
            path: '/docs',
            component: ComponentCreator('/docs', 'b1b'),
            routes: [
              {
                path: '/docs/explanation/plan-data-model',
                component: ComponentCreator('/docs/explanation/plan-data-model', 'f21'),
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
                path: '/docs/reference/ref-test',
                component: ComponentCreator('/docs/reference/ref-test', '3bc'),
                exact: true
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
                path: '/docs/reference/standard-library/standard-library-intro',
                component: ComponentCreator('/docs/reference/standard-library/standard-library-intro', '69f'),
                exact: true
              },
              {
                path: '/docs/reference/standard-library/types',
                component: ComponentCreator('/docs/reference/standard-library/types', '4f8'),
                exact: true,
                sidebar: "referenceSidebar"
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
