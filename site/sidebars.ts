import type { SidebarsConfig } from "@docusaurus/plugin-content-docs";

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
    gettingStartedSidebar: [
        {
            type: "category",
            label: "Getting Started",
            link: { type: "doc", id: "getting-started/getting-started" },
            items: ["getting-started/getting-started"],
        },
    ],
    manualSidebar: [
        {
            type: "doc",
            id: "manual/overview",
            label: "Language Overview",
        },
        {
            type: "doc",
            id: "manual/build-system",
            label: "1. Build System",
        },
        {
            type: "doc",
            id: "manual/base-syntax",
            label: "2. Base Syntax",
        },
        {
            type: "doc",
            id: "manual/member-functions",
            label: "3. Member Functions",
        },
        {
            type: "doc",
            id: "manual/tagged-unions",
            label: "4. Tagged Unions",
        },
        {
            type: "doc",
            id: "manual/move-semantics",
            label: "5. Move Semantics",
        },
        {
            type: "doc",
            id: "manual/modules",
            label: "6. Modules",
        },
        {
            type: "doc",
            id: "manual/templates",
            label: "7. Templates",
        },
        {
            type: "doc",
            id: "manual/contracts",
            label: "8. Contracts",
        },
    ],
};

export default sidebars;
