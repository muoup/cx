import type {CSSProperties, ReactNode} from "react";
import Layout from "@theme-original/DocItem/Layout";
import type LayoutType from "@theme/DocItem/Layout";
import type {WrapperProps} from "@docusaurus/types";
import {useDoc, useDocsSidebar} from "@docusaurus/plugin-content-docs/client";

import {chapterLabel, sidebarLinks} from "../../../lib/chapters";
import {DocDescriptionContext} from "../../../lib/doc-description";

type Props = WrapperProps<typeof LayoutType>;

// A numbered sidebar label gives the chapter; its h2s become 7.1, 7.2, ...
function useChapter() {
    const {metadata} = useDoc();
    const sidebar = useDocsSidebar();
    const item = sidebarLinks(sidebar?.items ?? []).find((entry) => entry.docId === metadata.id);
    return item && chapterLabel(item.label)?.number;
}

export default function LayoutWrapper(props: Props): ReactNode {
    const {frontMatter} = useDoc();
    const chapter = useChapter();
    const layout = (
        <DocDescriptionContext.Provider value={frontMatter.description}>
            <Layout {...props} />
        </DocDescriptionContext.Provider>
    );

    if (chapter === undefined) {
        return layout;
    }

    const style: CSSProperties = {counterReset: `cx-chapter ${chapter} cx-sec cx-toc`};

    return (
        <div className="cx-chapter" style={style}>
            {layout}
        </div>
    );
}
