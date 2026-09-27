import type {ReactNode} from "react";
import Link from "@theme-original/DocSidebarItem/Link";
import type LinkType from "@theme/DocSidebarItem/Link";
import type {WrapperProps} from "@docusaurus/types";

import {chapterLabel} from "../../../lib/chapters";

type Props = WrapperProps<typeof LinkType>;

// Numbered links show their number in a separate column so the titles align.
export default function LinkWrapper(props: Props): ReactNode {
    const chapter = chapterLabel(props.item.label);

    if (!chapter) {
        return <Link {...props} />;
    }

    return <Link {...props} item={{...props.item, label: chapter.title}} data-chapter={chapter.number} />;
}
