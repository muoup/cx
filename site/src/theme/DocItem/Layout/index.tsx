import type {CSSProperties, ReactNode} from "react";
import Layout from "@theme-original/DocItem/Layout";
import type LayoutType from "@theme/DocItem/Layout";
import type {WrapperProps} from "@docusaurus/types";
import {useDoc, useDocsSidebar} from "@docusaurus/plugin-content-docs/client";

type Props = WrapperProps<typeof LayoutType>;

// Numbered sidebar labels ("7. Linear Resources") give the chapter; its h2s become 7.1, 7.2, ...
function useChapter() {
    const {metadata} = useDoc();
    const sidebar = useDocsSidebar();
    const item = sidebar?.items.find(
        (entry) => entry.type === "link" && entry.docId === metadata.id,
    );
    const match = item?.type === "link" ? item.label.match(/^(\d+)\.\s/) : null;
    return match ? Number(match[1]) : undefined;
}

export default function LayoutWrapper(props: Props): ReactNode {
    const chapter = useChapter();

    if (chapter === undefined) {
        return <Layout {...props} />;
    }

    const style: CSSProperties = {counterReset: `cx-chapter ${chapter} cx-sec cx-toc`};

    return (
        <div className="cx-chapter" style={style}>
            <Layout {...props} />
        </div>
    );
}
