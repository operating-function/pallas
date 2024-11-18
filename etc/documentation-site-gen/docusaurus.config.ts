import { themes as prismThemes } from "prism-react-renderer";
import type { Config } from "@docusaurus/types";
import type * as Preset from "@docusaurus/preset-classic";
import path from "path";

const config: Config = {
  title: "Pallas documentation",
  tagline: "Computer",
  favicon: "img/favicon.ico",
  url: "https://docs.opfn.co",
  baseUrl: "/",
  organizationName: "The Operating Function Company",
  projectName: "Pallas",
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",
  i18n: {
    defaultLocale: "en",
    locales: ["en"],
  },

  presets: [
    [
      "classic",
      {
        docs: {
          path: "../../doc",
          exclude: ["legacy/**"],
          sidebarPath: "./sidebars.ts",
          routeBasePath: "/", // This makes /docs the root
          editUrl:
            "https://github.com/operating-function/pallas/edit/main/doc/",
        },
        // blog: {
        //   showReadingTime: true,
        //   feedOptions: {
        //     type: ["rss", "atom"],
        //     xslt: true,
        //   },
        //   editUrl:
        //     "https://github.com/facebook/docusaurus/tree/main/packages/create-docusaurus/templates/shared/",
        //   onInlineTags: "warn",
        //   onInlineAuthors: "warn",
        //   onUntruncatedBlogPosts: "warn",
        // },
        theme: {
          customCss: [
            "./src/css/custom.css",
            require.resolve("./src/css/prism-rex.css"),
          ],
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    defaultMode: "dark",
    disableSwitch: true,
    respectPrefersColorScheme: false,
    image: "img/docusaurus-social-card.jpg",
    navbar: {
      title: "Pallas docs",
      logo: {
        alt: "opfn logo",
        src: "img/opfn-logo.png",
        srcDark: "img/opfn-logo-light.png",
      },
      items: [
        {
          type: "docSidebar",
          sidebarId: "explanationSidebar",
          position: "left",
          label: "Explanation",
        },
        {
          type: "docSidebar",
          sidebarId: "referenceSidebar",
          position: "left",
          label: "Reference",
        },
        // { to: "/blog", label: "Blog", position: "left" },
        {
          href: "https://github.com/operating-function/pallas",
          label: "GitHub",
          position: "right",
        },
      ],
    },
    footer: {
      style: "dark",
      links: [
        {
          title: "Docs",
          items: [
            {
              label: "System Overview",
              to: "/",
            },
          ],
        },
        {
          title: "Community",
          items: [
            {
              label: "Telegram",
              href: "https://t.me/vaporwareNetwork",
            },
            {
              label: "Twitter",
              href: "https://twitter.com/__vaporware__",
            },
          ],
        },
        {
          title: "More",
          items: [
            {
              label: "opfn",
              to: "https://opfn.co",
            },
            {
              label: "Blog",
              to: "https://blog.vaporware.network",
            },
            {
              label: "GitHub",
              href: "https://github.com/operating-function/pallas",
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} My Project, Inc. Built with Docusaurus.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ["haskell"],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
