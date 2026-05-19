import type { ReactNode } from "react";
import Link from "@docusaurus/Link";
import Layout from "@theme/Layout";
import Heading from "@theme/Heading";

import styles from "./index.module.css";

const featureList = [
    "What You See is What Happens",
    "C compatible memory management made safe with linear types",
    "Verified memory safety in the 'safe' subset",
    "First-class contracts and post-condition checking",
    "Predictable C-style data layout and FFI",
    "Opt-in formal verification via FMIR analysis",
    "Tagged unions and pattern matching",
    "Template-based generic programming",
];

const socketSnippet = [
    "import std::io;",
    "import std::optional;",
    "import std::socket;",
    "",
    "opt<c_socket> try_serve(u16 port) {",
    "    c_socket listener =",
    "        c_socket::open(AF_INET, SOCK_STREAM, 0).unwrap();",
    "",
    '    socket_addr addr = socket_addr::ipv4("0.0.0.0", port);',
    "",
    "    if (listener.bind(&addr) < 0) {",
    "        return opt<c_socket>::none();",
    "    }",
    "",
    "    if (listener.listen(128) < 0) {",
    "        return opt<c_socket>::none();",
    "    }",
    "",
    "    return opt<c_socket>::some(move listener);",
    "}",
];

const cxTokenPattern =
    /("[^"]*"|\b(?:import|if|return|move)\b|\b(?:opt|socket|socket_addr|u16|i32)\b|\b(?:std::[A-Za-z_:]+|AF_INET|SOCK_STREAM)\b|\b\d+\b|[A-Za-z_][A-Za-z0-9_]*(?=\())/g;

function cxTokenClass(token: string) {
    if (/^"/.test(token)) return styles.tokenString;
    if (/^\d+$/.test(token)) return styles.tokenNumber;
    if (/^(import|if|return|move)$/.test(token)) return styles.tokenKeyword;
    if (/^(opt|socket|socket_addr|u16|i32)$/.test(token)) {
        return styles.tokenType;
    }
    if (/^(std::[A-Za-z_:]+|AF_INET|SOCK_STREAM)$/.test(token)) {
        return styles.tokenConstant;
    }

    return styles.tokenFunction;
}

function highlightedLine(line: string) {
    const parts: ReactNode[] = [];
    let cursor = 0;

    for (const match of line.matchAll(cxTokenPattern)) {
        const token = match[0];
        const index = match.index ?? 0;

        if (index > cursor) {
            parts.push(line.slice(cursor, index));
        }

        parts.push(
            <span className={cxTokenClass(token)} key={`${index}-${token}`}>
                {token}
            </span>,
        );

        cursor = index + token.length;
    }

    if (cursor < line.length) {
        parts.push(line.slice(cursor));
    }

    return parts;
}

function Hero() {
    return (
        <div className={styles.hero}>
            <div className={styles.heroInner}>
                <div className={styles.heroLeft}>
                    <Heading as="h1" className={styles.heroTitle}>
                        CX
                    </Heading>
                    <div className={styles.heroTagline}>
                        low-level control for safe, traceable, and performant
                        systems
                    </div>
                </div>
                <div className={styles.heroRight}>
                    <div className={styles.getStartedBox}>
                        <div className={styles.getStartedHeader}>
                            RESEARCH PREVIEW
                        </div>
                        <div className={styles.getStartedBody}>
                            <Link
                                className="button button--secondary button--block"
                                to="/docs/getting-started"
                            >
                                INSTALL COMPILER
                            </Link>
                            <div className={styles.getStartedLinks}>
                                <Link to="/docs/manual/">Manual</Link>
                                <Link to="https://github.com/muoup/cx">
                                    GitHub
                                </Link>
                                <Link to="/docs/build-system">
                                    Build System
                                </Link>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}

function MainLayout() {
    return (
        <div className={styles.mainGrid}>
            <div className={styles.contentColumn}>
                <p className={styles.introParagraph}>
                    CX is an experimental systems language for writing direct,
                    C-shaped code with ownership checks that follow every
                    control-flow path.
                </p>

                <ul className={styles.featureBullets}>
                    {featureList.map((f) => (
                        <li key={f}>{f}</li>
                    ))}
                </ul>
            </div>

            <div className={styles.codeColumn}>
                <div className={styles.codeSnippet}>
                    <div className={styles.codeSnippetHeader}>
                        socket_retry.cx
                    </div>
                    <pre>
                        <code>
                            {socketSnippet.map((line, index) => (
                                <span
                                    className={
                                        index === 11
                                            ? styles.errorLine
                                            : undefined
                                    }
                                    key={`${index}-${line}`}
                                >
                                    {highlightedLine(line)}
                                    {"\n"}
                                </span>
                            ))}
                        </code>
                    </pre>
                    <div className={styles.diagnostic}>
                        <div className={styles.diagnosticTitle}>
                            error: `listener` would be leaked on this return
                            path
                        </div>
                        <div>
                            `socket` is linear. Close it, move it, or discharge
                            ownership before leaving `serve`.
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}

export default function Home(): ReactNode {
    return (
        <Layout
            title="The CX Programming Language"
            description="Explicit resource management and formal verification."
        >
            <main className={styles.container}>
                <Hero />
                <MainLayout />
            </main>
        </Layout>
    );
}
