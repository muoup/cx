import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const config: Config = {
  title: 'CX Programming Language',
  tagline: 'A C-shaped systems language experiment in linear resources',
  favicon: 'img/favicon.svg',

  // Future flags, see https://docusaurus.io/docs/api/docusaurus-config#future
  future: {
    v4: true, // Improve compatibility with the upcoming Docusaurus v4
  },

  // Set the production url of your site here
  url: 'https://muoup.github.io',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/cx/',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'muoup',
  projectName: 'cx',
  trailingSlash: false,

  onBrokenLinks: 'throw',
  markdown: {
    hooks: {
      onBrokenMarkdownLinks: 'throw',
    },
  },

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      {
        docs: {
          sidebarPath: './sidebars.ts',
          sidebarCollapsible: false,
          // A generated directory's index page becomes the parent of the pages beside it
          async sidebarItemsGenerator({defaultSidebarItemsGenerator, ...args}) {
            const items = await defaultSidebarItemsGenerator(args);
            const id = `${args.item.dirName}/index`;
            const index = args.docs.find((doc) => doc.id === id);
            if (!index) {
              return items;
            }
            const rest = items.filter((item) => !(item.type === 'doc' && item.id === id));
            return [{type: 'category', label: index.title, link: {type: 'doc', id}, items: rest}];
          },
          editUrl:
            'https://github.com/muoup/cx/tree/docs/site/',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    image: 'img/cx-social-card.png',
    colorMode: {
      respectPrefersColorScheme: true,
    },
    navbar: {
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'guidesSidebar',
          position: 'left',
          label: 'Guides',
        },
        {
          type: 'docSidebar',
          sidebarId: 'manualSidebar',
          position: 'left',
          label: 'Manual',
        },
        {
          type: 'docSidebar',
          sidebarId: 'stdlibSidebar',
          position: 'left',
          label: 'Standard Library',
        },
        {
          href: 'https://github.com/muoup/cx',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'light',
      links: [
        {label: 'Guides', to: '/docs/getting-started'},
        {label: 'Manual', to: '/docs/manual/overview'},
        {label: 'Standard Library', to: '/docs/stdlib'},
        {label: 'GitHub', href: 'https://github.com/muoup/cx'},
      ],
      copyright: `© ${new Date().getFullYear()} Zachary Verlardi`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
