import type { ReactNode } from "react";
import Link from "@docusaurus/Link";
import Layout from "@theme/Layout";
import Heading from "@theme/Heading";

import styles from "./index.module.css";

const featurePanels = [
    {
        title: "Linear Resources",
        body: "No RAII, no garbage collection, all resources are linear and must be destroyed, and destroyed explicitly."
    },
    {
        title: "Modern Features",
        body: "Algebraic data types and templates supported out of the box and kept traceable via a one-symbol one-definition philosophy"
    },
    {
        title: "Safe Subset",
        body: "Mark critical parts of code as safe to prevent undefined behavior and enable enforcement of safety properties."
    },
    {
        title: "C Interop",
        body: "CX is designed as a strict superset of C, all valid C code will be valid CX code."
    },
];

const socketSnippet = [
`import std::io as std;
import std::optional as std;
import std::span as std;
import std::string as std;
import std::functional as std;

import std::net::udp as std::net;

std::opt<std::net::udp_socket> try_serve(u16 port) {
    std::net::endpoint addr = std::net::endpoint::ipv4("0.0.0.0", port)
        |> std::opt::try();

    std::net::udp_socket socket = std::net::udp_socket::open()
        |> std::opt::try();

    socket |> std::net::udp_socket::bind(addr)
        |> std::opt::try();

    const _str& hello = "Hello, world!";
    std::span<const u8> buffer = std::span::str_as_bytes(hello);
    
    std::opt<u64> result = socket 
        |> std::net::udp_socket::send_to(buffer, addr)`,
"        |> std::opt::try();",`
        
    return move socket |> std::opt::some<std::net::udp_socket>();
}`
];

const cxTokenPattern =
    /("[^"]*"|\b(?:import|if|return|move)\b|\b(?:opt|c_socket|socket_addr|u16|i32)\b|\b(?:std::[A-Za-z_:]+|AF_INET|SOCK_STREAM)\b|\b\d+\b|[A-Za-z_][A-Za-z0-9_]*(?=\())/g;

function cxTokenClass(token: string) {
    if (/^"/.test(token)) return styles.tokenString;
    if (/^\d+$/.test(token)) return styles.tokenNumber;
    if (/^(import|if|return|move)$/.test(token)) return styles.tokenKeyword;
    if (/^(opt|c_socket|socket_addr|u16|i32)$/.test(token)) {
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
                                <Link to="https://github.com/muoup/cx">
                                    GitHub
                                </Link>
                                <Link to="/docs/manual/overview">Manual</Link>
                                <Link to="/docs/getting-started">
                                    Getting Started
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
                    CX is a work-in-progress experimental systems language for writing low-level code with modern convenience and
                    compiler-ensured correctness. The language is verbose by design: no implicit destructors, no
                    implicit control-flow, the code you write is the code that runs. 
                </p>

                <div className={styles.featureGrid}>
                    {featurePanels.map((feature) => (
                        <section
                            className={styles.featurePanel}
                            key={feature.title}
                        >
                            <h2>{feature.title}</h2>
                            <p>{feature.body}</p>
                        </section>
                    ))}
                </div>
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
                                        index === 1
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
                            error: `listener` is marked @nodrop but is leaked without cleanup
                        </div>
                    </div>
                </div>
            </div>
        </div>
    );
}

function FoliageBorder() {
    return (
        <div className={styles.foliageBorder} aria-hidden="true">
            <span
                className={`${styles.foliageCluster} ${styles.foliageTopLeft}`}
            />
            <span
                className={`${styles.foliageCluster} ${styles.foliageTopRight}`}
            />
            <span
                className={`${styles.foliageCluster} ${styles.foliageBottomLeft}`}
            />
            <span
                className={`${styles.foliageCluster} ${styles.foliageBottomRight}`}
            />
        </div>
    );
}

export default function Home(): ReactNode {
    return (
        <Layout
            title="The CX Programming Language"
            description="Explicit resource management and formal verification."
            noFooter
            wrapperClassName="landing-page"
        >
            <FoliageBorder />
            <main className={styles.container}>
                <Hero />
                <MainLayout />
            </main>
        </Layout>
    );
}
