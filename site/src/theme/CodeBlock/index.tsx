import {useState, type ReactNode} from "react";
import clsx from "clsx";
import OriginalCodeBlock from "@theme-original/CodeBlock";

import {cxLines} from "../../lib/cx-lines";

type CodeBlockProps = {
    children?: ReactNode;
    className?: string;
    language?: string;
    metastring?: string;
    title?: string;
};

function isCxCodeBlock({className, language}: CodeBlockProps) {
    return language === "cx" || className?.split(/\s+/).includes("language-cx");
}

function codeTitle({metastring, title}: CodeBlockProps) {
    if (title) {
        return title;
    }

    return metastring?.match(/title=(['"])(.*?)\1/)?.[2] ?? "";
}

function CxCopyButton({source}: {source: string}) {
    const [copied, setCopied] = useState(false);

    async function copySource() {
        if (typeof navigator === "undefined" || !navigator.clipboard) {
            return;
        }

        await navigator.clipboard.writeText(source);
        setCopied(true);
        window.setTimeout(() => setCopied(false), 1500);
    }

    return (
        <button className="cx-code-block-copy clean-btn" onClick={copySource} type="button">
            {copied ? "Copied" : "Copy"}
        </button>
    );
}

export default function CodeBlock(props: CodeBlockProps): ReactNode {
    if (!isCxCodeBlock(props)) {
        return <OriginalCodeBlock {...props} />;
    }

    const source = String(props.children ?? "").replace(/\n$/, "");
    const title = codeTitle(props);
    const lines = cxLines(source);

    return (
        <div className={clsx("theme-code-block", "cx-code-block-container")}>
            {title ? <div className="cx-code-block-title">{title}</div> : null}
            <div className="cx-code-block-content">
                <pre className={clsx("cx-code-block", lines.length === 1 && "cx-code-block--single")} tabIndex={0}>
                    <code className="cx-code-lines">
                        {lines.map((line, index) => (
                            <span className="cx-line" data-n={index + 1} key={index}>
                                {line}
                                {"\n"}
                            </span>
                        ))}
                    </code>
                </pre>
                <CxCopyButton source={source} />
            </div>
        </div>
    );
}
