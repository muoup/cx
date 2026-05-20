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
            id: "manual/index",
            label: "Language Manual",
        },
        {
            type: "doc",
            id: "manual/base-syntax",
            label: "1. Base Syntax",
        },
        {
            type: "doc",
            id: "manual/member-functions",
            label: "2. Member Functions",
        },
        {
            type: "doc",
            id: "manual/tagged-unions",
            label: "3. Tagged Unions"
        },
        {
            type: "doc",
            id: "manual/move-semantics",
            label: "4. Move Semantics"
        },
        {
            type: "doc",
            id: "manual/modules",
            label: "5. Modules",
        },
        {
            type: "doc",
            id: "manual/templates",
            label: "6. Templates",
        },
        {
            type: "doc",
            id: "manual/control-flow",
            label: "7. Control Flow",
        },
        {
            type: "doc",
            id: "manual/contracts",
            label: "8. Contracts",
        },
        {
            type: "doc",
            id: "manual/ownership",
            label: "9. Ownership",
        },
        {
            type: "doc",
            id: "manual/safe-functions",
            label: "10. Safe Functions",
        },
        {
            type: "doc",
            id: "manual/build-system",
            label: "11. Build System",
        },
        {
            type: "doc",
            id: "manual/current-limitations",
            label: "12. Current Limitations",
        },
    ],
};

export default sidebars;
