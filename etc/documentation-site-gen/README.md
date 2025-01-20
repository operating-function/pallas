# Documentation site generation

We use [Docusaurus](https://docusaurus.io/) to generate the documentation static site.

## Installation

If you want to make changes or re-generate the docs:

- have npm installed
- `npm i`
- `npm run start` - gives you a dev server
- `npm run build` - builds the static site

## Deployment

- be in this directory
- have the `vercel` CLI setup
- `npm run build`
- run `vercel --cwd ./build --prod`
- if prompted by vercel to link to a project, choose the appropriate one
