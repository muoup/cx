import {useState, type ReactNode} from "react";
import clsx from "clsx";
import Link from "@docusaurus/Link";
import Layout from "@theme/Layout";
import Heading from "@theme/Heading";

import Foliage from "../components/Foliage";
import {cxLines} from "../lib/cx-lines";
import styles from "./index.module.css";

const leakingServer = `import std::{optional, span} as std;
import std::net::{tcp, address} as std::net;

void serve(std::net::tcp_listener& server) {
    while (true) {
        std::net::tcp_stream client = server
            |> std::net::tcp_listener::accept()
            |> std::opt::unwrap_or_else(.{ continue; });

        std::span<const u8> reply = std::span::str_as_bytes("hello\\n");
        client |> std::net::tcp_stream::write(reply)
            |> std::opt::unwrap_or_else(.{ continue; });

        move client |> std::net::tcp_stream::close();
    }
}`;

const fixedServer = `import std::{optional, span} as std;
import std::net::{tcp, address} as std::net;

void serve(std::net::tcp_listener& server) {
    while (true) {
        std::net::tcp_stream client = server
            |> std::net::tcp_listener::accept()
            |> std::opt::unwrap_or_else(.{ continue; });

        std::span<const u8> reply = std::span::str_as_bytes("hello\\n");
        client |> std::net::tcp_stream::write(reply)
            |> std::opt::unwrap_or_else(.{
                move client |> std::net::tcp_stream::close();
                continue;
            });

        move client |> std::net::tcp_stream::close();
    }
}`;

const principles = [
    {
        title: "Linear resources",
        body: <>No RAII and no garbage collector. A <code>@nodrop</code> value must be moved or explicitly destroyed before it goes out of scope.</>,
        code: `struct tcp_stream : @nodrop {
    i32 fd;
};`,
    },
    {
        title: "Modern features",
        body: <>Tagged unions and templates out of the box, kept traceable by a one-symbol, one-definition rule.</>,
        code: `enum union shape {
    circle :: f64,
    point :: void
};`,
    },
    {
        title: "Safe subset",
        body: <>Mark critical code as <code>safe</code> to rule out undefined behavior and state contracts the compiler can check.</>,
        code: `int fn() safe
where
    post(ret): (ret == 1)`,
    },
    {
        title: "C interop",
        body: <>CX is designed as a strict superset of C. C headers and libraries work directly, with no bindings layer.</>,
        code: `extern "C":
i32 close(int fd);`,
    },
];

const status: {area: string; state: "working" | "partial" | "progress"; label: string; notes: ReactNode}[] = [
    {area: "Cranelift backend", state: "working", label: "Working", notes: "Default code generator."},
    {area: "Projects and modules", state: "working", label: "Working", notes: <><code>cx init</code>, <code>cx build</code>, and <code>cx.toml</code>.</>},
    {area: "Linear resources, tagged unions, templates", state: "working", label: "Working", notes: "Covered in the manual."},
    {area: "C99 compatibility", state: "partial", label: "Partial", notes: "Most C compiles unchanged; some features are missing."},
    {area: "LLVM backend", state: "partial", label: "Optional", notes: <>Build with <code>--features backend-llvm</code>.</>},
    {area: "Contracts and safe functions", state: "progress", label: "In progress", notes: "Syntax is still changing."},
];

const installCommands = ["git clone https://github.com/muoup/cx.git", "cd cx && cargo build --release"];

function CopyButton({text}: {text: string}) {
    const [copied, setCopied] = useState(false);

    async function copy() {
        if (typeof navigator === "undefined" || !navigator.clipboard) {
            return;
        }

        await navigator.clipboard.writeText(text);
        setCopied(true);
        window.setTimeout(() => setCopied(false), 1500);
    }

    return (
        <button className={styles.copyButton} onClick={copy} type="button">
            {copied ? "Copied" : "Copy"}
        </button>
    );
}

function Install() {
    return (
        <div className={styles.install}>
            <div className={styles.installHead}>
                <span>Build from source</span>
                <CopyButton text={installCommands.join("\n")} />
            </div>
            <pre>
                {installCommands.map((command) => (
                    <span key={command}>
                        <span className={styles.prompt}>$ </span>
                        {command}
                        {"\n"}
                    </span>
                ))}
            </pre>
        </div>
    );
}

function Code({source, errorLine}: {source: string; errorLine?: number}) {
    return (
        <pre className={clsx("cx-code-block", styles.code)}>
            <code className="cx-code-lines">
                {cxLines(source).map((line, index) => (
                    <span
                        className={clsx("cx-line", index + 1 === errorLine && "cx-line-error")}
                        data-n={index + 1}
                        key={index}
                    >
                        {line}
                        {"\n"}
                    </span>
                ))}
            </code>
        </pre>
    );
}

function LeakDiagnostic() {
    const gutter = (text: string) => <span className={styles.gutter}>{text}</span>;

    return (
        <div className={styles.diagnostic}>
            <span className={styles.error}>error:</span> `client` is marked @nodrop but is leaked without cleanup{"\n"}
            {"  "}{gutter("-->")} server.cx:12:44{"\n"}
            {"   "}{gutter("|")}{"\n"}
            {gutter("12 |")}{"             |> std::opt::unwrap_or_else(.{ continue; });\n"}
            {"   "}{gutter("|")}{" ".repeat(44)}
            <span className={styles.error}>^^^^^^^^ `client` leaks here</span>{"\n"}
            {"   "}{gutter("=")} <span className={styles.help}>help:</span> close it first: move client |&gt; std::net::tcp_stream::close();
        </div>
    );
}

function Example() {
    const [fixed, setFixed] = useState(false);

    return (
        <div className={styles.example}>
            <div className={styles.frame}>
                <div className={styles.frameHead}>
                    <span>server.cx</span>
                    <div className={styles.pills} role="group" aria-label="Example version">
                        <button aria-pressed={!fixed} onClick={() => setFixed(false)} type="button">
                            Leaks
                        </button>
                        <button aria-pressed={fixed} onClick={() => setFixed(true)} type="button">
                            Fixed
                        </button>
                    </div>
                </div>
                {fixed ? <Code source={fixedServer} /> : <Code source={leakingServer} errorLine={12} />}
            </div>
            {fixed ? (
                <div className={clsx(styles.diagnostic, styles.ok)}>
                    <span className={styles.help}>ok:</span> server.cx compiles; every path consumes `client`
                </div>
            ) : (
                <LeakDiagnostic />
            )}
        </div>
    );
}

function Hero() {
    return (
        <section className={styles.hero}>
            <div>
                <Heading as="h1" className={styles.title}>
                    Low-level control for safe, traceable, and performant systems.
                </Heading>
                <p className={styles.lede}>
                    CX is an experimental systems language built as a superset of C. There are no
                    implicit destructors and no hidden control flow: every resource you acquire is
                    released in code you can read, and the compiler checks that you did.
                </p>
                <Install />
                <div className={styles.actions}>
                    <Link className={styles.button} to="/docs/getting-started">
                        Getting started
                    </Link>
                    <Link to="/docs/manual/overview">Read the manual</Link>
                    <Link to="https://github.com/muoup/cx">GitHub</Link>
                </div>
            </div>
            <Example />
        </section>
    );
}

function Principles() {
    return (
        <section className={styles.section}>
            <Heading as="h2" className={styles.eyebrow}>
                <span className={styles.pipe}>|&gt;</span>Principles
            </Heading>
            <div className={styles.principles}>
                {principles.map(({title, body, code}) => (
                    <article className={styles.principle} key={title}>
                        <h3>{title}</h3>
                        <p>{body}</p>
                        <pre>
                            {cxLines(code).map((line, index) => (
                                <span key={index}>
                                    {line}
                                    {"\n"}
                                </span>
                            ))}
                        </pre>
                    </article>
                ))}
            </div>
        </section>
    );
}

function Status() {
    return (
        <section className={styles.section}>
            <Heading as="h2" className={styles.eyebrow}>
                <span className={styles.pipe}>|&gt;</span>Status
            </Heading>
            <div className={styles.tableScroll}>
                <table className={styles.status}>
                    <thead>
                        <tr>
                            <th>Area</th>
                            <th>State</th>
                            <th>Notes</th>
                        </tr>
                    </thead>
                    <tbody>
                        {status.map(({area, state, label, notes}) => (
                            <tr key={area}>
                                <td>{area}</td>
                                <td>
                                    <span className={clsx(styles.state, styles[state])}>{label}</span>
                                </td>
                                <td>{notes}</td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
        </section>
    );
}

export default function Home(): ReactNode {
    return (
        <Layout
            title="The CX Programming Language"
            description="An experimental systems language built as a superset of C, with linear resources and no hidden control flow."
            wrapperClassName="landing-page"
        >
            <Foliage full />
            <main className={styles.page}>
                <Hero />
                <Principles />
                <Status />
            </main>
        </Layout>
    );
}
