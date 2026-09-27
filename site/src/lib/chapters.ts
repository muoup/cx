import {createContext} from "react";

export function chapterLabel(label: string): {number: number; title: string} | undefined {
    const match = label.match(/^(\d+)\.\s+(.*)$/);
    return match ? {number: Number(match[1]), title: match[2]} : undefined;
}

// Set by DocSidebarItems; the sidebar context itself is unavailable in the mobile navbar menu.
export const NumberedSidebarContext = createContext(false);
