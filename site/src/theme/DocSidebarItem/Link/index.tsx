import type {ReactNode} from "react";
import Link from "@theme-original/DocSidebarItem/Link";
import type LinkType from "@theme/DocSidebarItem/Link";
import type {WrapperProps} from "@docusaurus/types";
import {useDocsSidebar} from "@docusaurus/plugin-content-docs/client";

import {chapterLabel} from "../../../lib/chapters";

type Props = WrapperProps<typeof LinkType>;

// In a numbered sidebar, every top-level link gets a number column (empty when unnumbered) so titles align.
export default function LinkWrapper(props: Props): ReactNode {
    const sidebar = useDocsSidebar();
    const numbered = sidebar?.items.some((item) => item.type === "link" && chapterLabel(item.label));
    const chapter = chapterLabel(props.item.label);

    if (!numbered || props.level !== 1) {
        return <Link {...props} />;
    }

    return (
        <Link
            {...props}
            item={chapter ? {...props.item, label: chapter.title} : props.item}
            data-chapter={chapter?.number ?? ""}
        />
    );
}
