import {useState, type ReactNode} from "react";
import clsx from "clsx";
import OriginalCodeBlock from "@theme-original/CodeBlock";

import {chlLines} from "../../lib/chl-lines";

type CodeBlockProps = {
    children?: ReactNode;
    className?: string;
    language?: string;
    metastring?: string;
    title?: string;
};

function isChlCodeBlock({className, language}: CodeBlockProps) {
    return language === "chl" || className?.split(/\s+/).includes("language-chl");
}

function codeTitle({metastring, title}: CodeBlockProps) {
    if (title) {
        return title;
    }

    return metastring?.match(/title=(['"])(.*?)\1/)?.[2] ?? "";
}

function ChlCopyButton({source}: {source: string}) {
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
        <button className="chl-code-block-copy clean-btn" onClick={copySource} type="button">
            {copied ? "Copied" : "Copy"}
        </button>
    );
}

export default function CodeBlock(props: CodeBlockProps): ReactNode {
    if (!isChlCodeBlock(props)) {
        return <OriginalCodeBlock {...props} />;
    }

    const source = String(props.children ?? "").replace(/\n$/, "");
    const title = codeTitle(props);
    const lines = chlLines(source);

    return (
        <div className={clsx("theme-code-block", "chl-code-block-container")}>
            {title ? <div className="chl-code-block-title">{title}</div> : null}
            <div className="chl-code-block-content">
                <pre className={clsx("chl-code-block", lines.length === 1 && "chl-code-block--single")} tabIndex={0}>
                    <code className="chl-code-lines">
                        {lines.map((line, index) => (
                            <span className="chl-line" data-n={index + 1} key={index}>
                                {line}
                                {"\n"}
                            </span>
                        ))}
                    </code>
                </pre>
                <ChlCopyButton source={source} />
            </div>
        </div>
    );
}
