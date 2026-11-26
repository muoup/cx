import {useContext, useId, type ReactNode} from "react";
import clsx from "clsx";
import {useDocsSidebar} from "@docusaurus/plugin-content-docs/client";
import DesktopTOC from "@theme-original/DocItem/TOC/Desktop";

import {StdlibOutlineContext} from "../../../../lib/stdlib-outline";
import styles from "./styles.module.css";

export default function DesktopTOCWrapper(): ReactNode {
    const sidebar = useDocsSidebar();
    const outline = useContext(StdlibOutlineContext);
    const contents = useId();

    if (sidebar?.name !== "stdlibSidebar" || !outline) {
        return <DesktopTOC />;
    }

    const {collapsed, setCollapsed} = outline;
    return (
        <aside className={clsx("chl-stdlib-outline", styles.outline)} data-collapsed={collapsed}>
            <button
                aria-controls={contents}
                aria-expanded={!collapsed}
                aria-label="On this page"
                className={styles.toggle}
                onClick={() => setCollapsed(!collapsed)}
                type="button"
            >
                {!collapsed && <span>On this page</span>}
                <span aria-hidden="true" className={styles.chevron} />
            </button>
            <div id={contents} hidden={collapsed}>
                <DesktopTOC />
            </div>
        </aside>
    );
}
