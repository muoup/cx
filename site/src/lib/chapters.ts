import type {PropSidebarItem, PropSidebarItemLink} from "@docusaurus/plugin-content-docs";

export function chapterLabel(label: string): {number: number; title: string} | undefined {
    const match = label.match(/^(\d+)\.\s+(.*)$/);
    return match ? {number: Number(match[1]), title: match[2]} : undefined;
}

export function sidebarLinks(items: PropSidebarItem[]): PropSidebarItemLink[] {
    return items.flatMap((item) => (item.type === "category" ? sidebarLinks(item.items) : item.type === "link" ? [item] : []));
}
