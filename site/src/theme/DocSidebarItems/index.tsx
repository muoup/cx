import type {ReactNode} from "react";
import Items from "@theme-original/DocSidebarItems";
import type ItemsType from "@theme/DocSidebarItems";
import type {WrapperProps} from "@docusaurus/types";

import {chapterLabel, NumberedSidebarContext} from "../../lib/chapters";

type Props = WrapperProps<typeof ItemsType>;

export default function ItemsWrapper(props: Props): ReactNode {
    if (props.level !== 1) {
        return <Items {...props} />;
    }

    const numbered = props.items.some((item) => item.type === "link" && chapterLabel(item.label));

    return (
        <NumberedSidebarContext.Provider value={numbered}>
            <Items {...props} />
        </NumberedSidebarContext.Provider>
    );
}
