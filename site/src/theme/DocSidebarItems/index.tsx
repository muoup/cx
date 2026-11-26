import {useState, type ReactNode} from "react";
import type {PropSidebarItem, PropSidebarItemCategory} from "@docusaurus/plugin-content-docs";
import DocSidebarItems from "@theme-original/DocSidebarItems";
import type {Props} from "@theme/DocSidebarItems";

import styles from "./styles.module.css";

function filterModules(items: PropSidebarItem[], query: string): PropSidebarItem[] {
    return items.flatMap<PropSidebarItem>((item) => {
        if (item.type === "html") {
            return [];
        }

        const sourcePath = item.customProps?.sourcePath;
        const matches = item.href && (item.label.toLowerCase().includes(query) ||
            (typeof sourcePath === "string" && sourcePath.toLowerCase().includes(query)));

        if (item.type === "category") {
            const children = filterModules(item.items, query);
            return matches || children.length > 0 ? [{...item, items: children}] : [];
        }

        return matches ? [item] : [];
    });
}

function StdlibSidebarItems({root, ...props}: Omit<Props, "items"> & {root: PropSidebarItemCategory}): ReactNode {
    const [query, setQuery] = useState("");
    const search = query.trim().toLowerCase();
    const items = search ? filterModules(root.items, search) : root.items;

    return (
        <>
            <li className={styles.searchRow}>
                <input
                    aria-label="Search standard library files"
                    className={styles.search}
                    onChange={(event) => setQuery(event.currentTarget.value)}
                    placeholder="Search standard library files"
                    type="search"
                    value={query}
                />
            </li>
            <DocSidebarItems {...props} items={[{...root, items}]} />
            {search && items.length === 0 ? <li className={styles.empty} role="status">No matching library files.</li> : null}
        </>
    );
}

export default function DocSidebarItemsWrapper({items, ...props}: Props): ReactNode {
    const [root] = items;
    if (props.level === 1 && items.length === 1 && root?.type === "category" && root.customProps?.stdlibSearch === true) {
        return <StdlibSidebarItems {...props} root={root} />;
    }

    return <DocSidebarItems {...props} items={items} />;
}
