import type {CSSProperties, ReactNode} from "react";
import Layout from "@theme-original/DocItem/Layout";
import type LayoutType from "@theme/DocItem/Layout";
import type {WrapperProps} from "@docusaurus/types";
import {useDoc, useDocsSidebar} from "@docusaurus/plugin-content-docs/client";

import {chapterLabel, sidebarLinks} from "../../../lib/chapters";

type Props = WrapperProps<typeof LayoutType>;

// A numbered sidebar label gives the chapter; its h2s become 7.1, 7.2, ...
function useChapter() {
    const {metadata} = useDoc();
    const sidebar = useDocsSidebar();
    const item = sidebarLinks(sidebar?.items ?? []).find((entry) => entry.docId === metadata.id);
    return item && chapterLabel(item.label)?.number;
}

export default function LayoutWrapper(props: Props): ReactNode {
    const chapter = useChapter();

    if (chapter === undefined) {
        return <Layout {...props} />;
    }

    const style: CSSProperties = {counterReset: `chl-chapter ${chapter} chl-sec chl-toc`};

    return (
        <div className="chl-chapter" style={style}>
            <Layout {...props} />
        </div>
    );
}
