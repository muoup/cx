import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */
const sidebars: SidebarsConfig = {
  docsSidebar: [
    'getting-started',
    {
      type: 'category',
      label: 'Language Manual',
      link: {type: 'doc', id: 'manual/index'},
      items: [
        'manual/base-syntax',
        'manual/aggregate-types',
        'manual/member-functions',
        'manual/templates',
        'manual/control-flow',
        'manual/modules',
        'manual/contracts',
        'manual/ownership',
        'manual/safe-functions',
        'manual/c-compatibility',
        'manual/current-limitations',
      ],
    },
    'build-system',
  ],
};

export default sidebars;
