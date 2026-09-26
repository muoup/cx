import type {ReactNode} from "react";
import Link from "@docusaurus/Link";
import {useDocsSidebar, useDocsVersion} from "@docusaurus/plugin-content-docs/client";

import {chapterLabel} from "../../lib/chapters";
import styles from "./styles.module.css";

export default function ChapterList(): ReactNode {
    const sidebar = useDocsSidebar();
    const {docs} = useDocsVersion();

    const chapters = (sidebar?.items ?? []).flatMap((item) => {
        const chapter = item.type === "link" ? chapterLabel(item.label) : undefined;
        return item.type === "link" && chapter ? [{...chapter, href: item.href, docId: item.docId}] : [];
    });

    return (
        <ol className={styles.chapters}>
            {chapters.map(({number, title, href, docId}) => (
                <li key={href}>
                    <Link className={styles.chapter} to={href}>
                        <span className={styles.number}>{number}</span>
                        <span>
                            <span className={styles.title}>{title}</span>
                            {docId && docs[docId]?.description && (
                                <span className={styles.description}>{docs[docId].description}</span>
                            )}
                        </span>
                    </Link>
                </li>
            ))}
        </ol>
    );
}
